(ns nedfryst.core
  (:require [clojure.string :as s]
            [clojure.java.io :as io])
  (:import [java.net URL URLClassLoader]
           [java.nio ByteBuffer ByteOrder]
           [java.io ByteArrayOutputStream File]
           [java.util.zip GZIPOutputStream GZIPInputStream]
           [java.lang.reflect Method]
           [java.lang.management ManagementFactory]
           [java.lang.instrument Instrumentation ClassFileTransformer]
           [com.esotericsoftware.kryo Kryo Serializer]
           [com.esotericsoftware.kryo.serializers DefaultSerializers$StringSerializer DefaultSerializers$ClassSerializer
            FieldSerializer JavaSerializer]
           [com.esotericsoftware.kryo.io Input Output]
           [org.objenesis.strategy StdInstantiatorStrategy]
           [clojure.lang RT Symbol Keyword IPersistentMap IPersistentCollection
            LazySeq AFunction$1 Fn Var Namespace DynamicClassLoader Reflector
            PersistentHashMap$BitmapIndexedNode PersistentHashMap$ArrayNode])
  (:gen-class
   :methods [^:static [premain [String java.lang.instrument.Instrumentation] void]
             ^:static [agentmain [String java.lang.instrument.Instrumentation] void]]))

(set! *warn-on-reflection* true)
(declare ^Instrumentation instrumentation)

(defn -premain [^String args ^Instrumentation instrumentation]
  (def instrumentation instrumentation))

(defn -agentmain [^String args ^Instrumentation instrumentation]
  (def instrumentation instrumentation))

(defn ^String classname [c]
  (cond (instance? Class c) (.getName ^Class c)
        (string? c) c
        :else (classname (type c))))

(defn class-as-resource [cn]
  (let [cn (classname cn)]
    (io/resource (str (s/replace cn "." "/") ".class"))))

(defn ^File agent-jar []
  (let [^URL f (class-as-resource nedfryst.core)]
    (if (= "file" (.getProtocol f))
      (some (fn [^File f] (when (re-find #"nedfryst-.*.jar" (.getName f)) f)) (file-seq (io/file "target")))
      (io/file (URL. (first (s/split (.getFile f) #"!")))))))

(defn pid []
  (first (s/split (.getName (ManagementFactory/getRuntimeMXBean)) #"@")))

(defonce ^URLClassLoader tools-jar-loader
  (URLClassLoader. (into-array URL [(.toURL (io/file (System/getProperty "java.home")
                                                     "../lib/tools.jar"))])))

(defn vm []
  (let [vm-class (.loadClass tools-jar-loader "com.sun.tools.attach.VirtualMachine")]
    (.invoke (.getMethod vm-class "attach"
                         (into-array Class [String])) nil (object-array [(pid)]))))

(defn attach-agent []
  (let [vm (vm)]
    (Reflector/invokeInstanceMember "loadAgent" vm (.getAbsolutePath (agent-jar)))
    (Reflector/invokeNoArgInstanceMember vm "detach")))

(defonce classes (atom {}))

(defn clojure-class? [^bytes bytes]
  (re-find #"clojure/lang/" (String. bytes "ISO-8859-1")))

(defn throwaway-eval? [name]
  (boolean (re-find #"\$eval\d+$" name)))

(defn probable-var-name [cn]
  (symbol (s/replace (.replaceFirst (classname cn) "\\$" "/") "_" "-")))

;; There's kind of an assumption that these will already been loaded before restoring/checkpoint.
;; This seems to work as expected, as long as the fn is already loaded, it gets resolved without any extra work.
;; If we want to autoload it (we probably don't), we need to deal with it in the serializer.
(defn has-source? [name]
  (when-let [file (:file (meta (resolve (probable-var-name name))))]
    (not= "NO_SOURCE_FILE" file)))

;; There was an idea here of having this as a record that got lost.
(defrecord FrozenFn [^String classname ^bytes bytes])

(defn redefine-class
  ([^FrozenFn frozen-fn] (redefine-class (.classname frozen-fn) (.bytes frozen-fn)))
  ([name bytes]
     (let [^DynamicClassLoader loader (RT/baseLoader)]
       (try
         (.loadClass loader name)
         (catch ClassNotFoundException _
           (.defineClass loader name bytes nil))))))

(defn is-class? [bytes]
  (= 0xCAFEBABE (Integer/toUnsignedLong (.getInt (ByteBuffer/wrap bytes)))))

(defn freeze-fn [fn]
  (let [cn (.getName ^Class (type fn))]
    (when-let [bytes (@classes (s/replace cn "." "/"))]
      (FrozenFn. cn bytes))))

(defn class-store [loader name class-being-redefined
                   protection-domain classfile-buffer]
  (when (and (clojure-class? classfile-buffer)
             (not ((some-fn class-as-resource throwaway-eval? has-source?) name)))
    (swap! classes assoc (s/replace name "/" ".") (FrozenFn. (s/replace name "/" ".") classfile-buffer))))

(def class-store-transformer
  (proxy [ClassFileTransformer] []
    (transform [loader name class-being-redefined
                protection-domain classfile-buffer]
      (class-store loader name class-being-redefined
                   protection-domain classfile-buffer)
      nil)))

(defn store-classes []
  (.addTransformer instrumentation class-store-transformer))

(defn dont-store-classes []
  (.removeTransformer instrumentation class-store-transformer))

(def ^Serializer clojure-reader-serializer
  (proxy [Serializer] []
    (write [k ^Output out o]
      (binding [*print-dup* true]
        (.writeString out (pr-str o))))
    (read [k ^Input in o]
      (read-string (.readString in)))))

(def ^Kryo kryo (Kryo.))

(def ^:dynamic *write-full-ns* false)

(doto kryo
  (.setClassLoader (RT/baseLoader))
  (.setInstantiatorStrategy (StdInstantiatorStrategy.))
  (.addDefaultSerializer IPersistentMap FieldSerializer)
  (.addDefaultSerializer IPersistentCollection FieldSerializer)
  (.addDefaultSerializer PersistentHashMap$ArrayNode
                         (doto (FieldSerializer. kryo PersistentHashMap$ArrayNode)
                           (.setIgnoreSyntheticFields false)))
  (.addDefaultSerializer PersistentHashMap$BitmapIndexedNode
                         (doto (FieldSerializer. kryo PersistentHashMap$BitmapIndexedNode)
                           (.setIgnoreSyntheticFields false)))
  (.addDefaultSerializer AFunction$1
                         (doto (FieldSerializer. kryo AFunction$1)
                           (.setIgnoreSyntheticFields false)))
  (.addDefaultSerializer Namespace (proxy [FieldSerializer] [kryo Namespace]
                                     (write [^Kryo k ^Output out o]
                                       (let [^FieldSerializer this this
                                             resolvable-map #(into {} (map (fn [[k v]] [k (pr-str v)]) %))]
                                         (proxy-super write k out o)
                                         (.writeClassAndObject k out (ns-publics o))
                                         (when *write-full-ns*
                                           (.writeClassAndObject k out (resolvable-map (ns-imports o)))
                                           (.writeClassAndObject k out (resolvable-map (ns-refers o)))
                                           (.writeClassAndObject k out (resolvable-map (into {} (map (fn [[k v]] [k (ns-name v)])
                                                                                                     (ns-aliases o))))))))
                                     (read [^Kryo k ^Input in t]
                                       (let [^FieldSerializer this this
                                             ^Namespace ns (create-ns (ns-name (proxy-super read k in t)))]
                                         (->> (.readClassAndObject k in)
                                              (pmap (fn  [[k ^Var v] ]
                                                      (let [dynamic? (.isDynamic v)
                                                            _ (ns-unmap ns k)
                                                            ^Var v (intern ns (if v
                                                                                (with-meta k (dissoc (meta v) :ns :name))
                                                                                k)
                                                                           (when v
                                                                             (.getRawRoot v)))]
                                                        (when dynamic?
                                                          (.setDynamic v)))))
                                              dorun)
                                         (when *write-full-ns*
                                           (doseq [[k v] (.readClassAndObject k in)]
                                             (.importClass ns k (resolve (read-string v))))
                                           (doseq [[k v] (.readClassAndObject k in)]
                                             (ns-unmap ns k)
                                             (.refer ns k (resolve (second (read-string v)))))
                                           (doseq [[k v] (.readClassAndObject k in)]
                                             (.addAlias ns k (the-ns (read-string v)))))
                                         ns))))
  (.register Symbol clojure-reader-serializer)
  (.register Keyword clojure-reader-serializer)
  (.register LazySeq (proxy [FieldSerializer] [kryo LazySeq]
                       (write [^Kryo k ^Output out o]
                         (let [^FieldSerializer this this]
                           (proxy-super write k out (doall o)))))))

(defn enable-freezing []
  (attach-agent)
  (store-classes))

(defn -main [& args])

(defn maybe-realize [x]
  (if (seq? x)
    (doall x)
    x))

(defn freeze [x file]
  (with-open [out (Output. (io/output-stream (io/file file)))]
    (.writeClassAndObject kryo out (object-array (vals @classes)))
    (.writeClassAndObject kryo out (maybe-realize x))))

(defn file-or-resource [f]
  (if (.exists (io/file f))
    (io/file f)
    (io/resource f)))

(def ^Method resolve-class
  (doto (.getDeclaredMethod ClassLoader "resolveClass" (into-array [Class]))
    (.setAccessible true)))

(def ^Method define-class
  (doto (.getDeclaredMethod ClassLoader "defineClass" (into-array [String (type (byte-array 0))
                                                                   Integer/TYPE Integer/TYPE]))
    (.setAccessible true)))

(defn inject-class [loader ^FrozenFn frozen-fn]
  (let [c (.invoke define-class loader
                   (object-array [(.classname frozen-fn) (.bytes frozen-fn)
                                  (int 0) (int (count (.bytes frozen-fn)))]))]
    (.invoke resolve-class loader (object-array [c]))
    c))

(defn thaw [file]
  (with-open [in (Input. (io/input-stream (file-or-resource file)))]
    (doall (pmap (partial inject-class (ClassLoader/getSystemClassLoader))
                 (maybe-realize (.readClassAndObject kryo in))))
    (maybe-realize (.readClassAndObject kryo in))))
