(ns taoensso.nippy
  (:require [taoensso.nippy.utils :as utils]
            [clojure.string :as s])
  (:import [java.io DataInputStream DataOutputStream]
           [java.lang.reflect Modifier]
           [clojure.lang IObj Var Atom Namespace PersistentQueue AFunction$1 AFunction IRecord IPersistentMap]))

;;;; Define type IDs

(def ^:const id-atom         (int 71))
(def ^:const id-var          (int 72))
(def ^:const id-ns           (int 73))
(def ^:const id-afunction$1  (int 74))
(def ^:const id-afunction    (int 75))
(def ^:const id-object-array (int 76))

(def ^:dynamic *ns-freezing* #{})

(freezer Atom id-atom
         (freeze-to-stream!* s @x))

(freezer Var id-var
         (freeze-to-stream!* s (meta x))
         (freeze-to-stream!* s (.hasRoot x))
         (freeze-to-stream!* s (when (.hasRoot x) (.getRawRoot x))))

(freezer Namespace id-ns
         (freeze-to-stream!* s (ns-name x))
         (if (*ns-freezing* x)
           (freeze-to-stream!* s {})
           (binding [*ns-freezing* (conj *ns-freezing* x)]
             (freeze-to-stream!* s (doall (ns-publics x))))))

(def afunction$1-outer
  (doto (.getDeclaredField AFunction$1 "this$0")
    (.setAccessible true)))

;; This is a RestFn created by with-mata on an AFunction, meta gets added back using id-meta.
(freezer AFunction$1 id-afunction$1
         (freeze-to-stream!* s (.get afunction$1-outer x)))

;; Dealing with closures
(freezer AFunction id-afunction
         (let [t (type x)
               fields (remove #(Modifier/isStatic (.getModifiers %)) (.getDeclaredFields t))]
           (freeze-to-stream!* s t)
           (freeze-to-stream!* s (count fields))
           (doseq [f (sort-by #(.getName %) fields)]
             (.setAccessible f true)
             (freeze-to-stream!* s (.get f x)))))

(coll-freezer (type (object-array 0)) id-object-array)

;; Reuse map for IRecords as the extend-type seems to collide.
(freezer IPersistentMap id-map
         (freeze-to-stream!* s (when (instance? IRecord x) (type x)))
         (.writeInt s (* 2 (count x))) ; Encode num kvs
         (doseq [[k v] x]
           (freeze-to-stream!* s k)
           (freeze-to-stream!* s v)))

(defn recreate-record [t m]
  (let [package (.getName (.getPackage t))
        name (.getSimpleName t)]
    ((ns-resolve (symbol package) (symbol (str "map->" name))) m)))

;; The unmarshalling isn't very exsensible. This is almost an exact copy from taoensso.nippy.
;; Note: Nippy doesn't handle identity, might need to add back reference mechanism?
(defn- thaw-from-stream!*
  [^DataInputStream s]
  (let [type-id (.readByte s)]
    (utils/case-eval
     type-id

     id-reader  (read-string (String. (read-bytes! s) "UTF-8"))
     id-bytes   (read-bytes! s)
     id-nil     nil
     id-boolean (.readBoolean s)

     id-char    (.readChar s)
     id-string  (String. (read-bytes! s) "UTF-8")
     id-keyword (keyword (.readUTF s))

     id-list    (apply list (coll-thaw! s)) ; TODO OOMs for big colls
     id-vector  (into  [] (coll-thaw! s))
     id-set     (into #{} (coll-thaw! s))
     id-map     (let [record-type (thaw-from-stream!* s)
                      m (into  {} (coll-thaw-pairs! s))]
                  (if record-type
                    (recreate-record record-type m)
                    m))
     id-coll    (doall (coll-thaw! s))
     id-queue   (into  (PersistentQueue/EMPTY) (coll-thaw! s))

     ;; Changed to deal with IReferences which are IMetas not IObjs
     id-meta (let [m (thaw-from-stream!* s)
                   o (thaw-from-stream!* s)]
               (if (instance? IObj o)
                 (with-meta o m)
                 (doto o (alter-meta! (constantly m)))))

     id-byte    (.readByte s)
     id-short   (.readShort s)
     id-integer (.readInt s)
     id-long    (.readLong s)
     id-bigint  (bigint (read-biginteger! s))

     id-float  (.readFloat s)
     id-double (.readDouble s)
     id-bigdec (BigDecimal. (read-biginteger! s) (.readInt s))

     id-ratio (/ (bigint (read-biginteger! s))
                 (bigint (read-biginteger! s)))

     ;; Added Atom, Var and Namespace
     id-atom (atom (thaw-from-stream!* s))
     id-var (let [m (thaw-from-stream!* s)
                  bound? (thaw-from-stream!* s)
                  v (thaw-from-stream!* s)
                  v (if (:ns m)
                      (intern (:ns m) (with-meta (:name m) m) v)
                      (doto (Var/create v)
                        (alter-meta! (constantly m))))]
              (when-not bound? (.unbindRoot v))
              v)
     id-ns (let [ns (create-ns (thaw-from-stream!* s))
                 publics (doall (thaw-from-stream!* s))]
             ns)
     id-afunction$1 (thaw-from-stream!* s)

     id-afunction (let [t (thaw-from-stream!* s)
                        vars (thaw-from-stream!* s)
                        ctor (first (filter #(= vars (count (.getParameterTypes %)))
                                            (.getDeclaredConstructors t)))]
                    (.newInstance ctor (object-array (repeatedly vars #(thaw-from-stream!* s)))))

     id-object-array  (object-array (coll-thaw! s))

     ;;; DEPRECATED
     id-old-reader (read-string (.readUTF s))
     id-old-string (.readUTF s)
     id-old-map    (apply hash-map (repeatedly (* 2 (.readInt s))
                                               (partial thaw-from-stream!* s)))

     (throw (Exception. (str "Failed to thaw unknown type ID: " type-id))))))
