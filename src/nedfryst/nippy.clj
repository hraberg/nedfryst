(ns taoensso.nippy
  (:require [taoensso.nippy.utils :as utils])
  (:import [java.io DataInputStream DataOutputStream]
           [clojure.lang Var Namespace PersistentQueue]))

;;;; Define type IDs

(def ^:const id-var     (int 71))
(def ^:const id-ns      (int 72))

(def ^:dynamic *ns-freezing* #{})

(freezer Var id-var
         (freeze-to-stream!* s (meta x))
         (freeze-to-stream!* s @x))

(freezer Namespace id-ns
         (freeze-to-stream!* s (ns-name x))
         (if (*ns-freezing* x)
           (freeze-to-stream!* s {})
           (binding [*ns-freezing* (conj *ns-freezing* x)]
             (freeze-to-stream!* s (ns-publics x)))))

;; The unmarshalling isn't very exsensible.
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
     id-map     (into  {} (coll-thaw-pairs! s))
     id-coll    (doall (coll-thaw! s))
     id-queue   (into  (PersistentQueue/EMPTY) (coll-thaw! s))

     id-meta (let [m (thaw-from-stream!* s)
                   o (thaw-from-stream!* s)]
               (if (instance? clojure.lang.IObj o)
                 (with-meta o m)
                 (doto o
                   (alter-meta! (constantly m)))))

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

     id-var (let [m (thaw-from-stream!* s)
                  v (thaw-from-stream!* s)]
              (intern (:ns m) (with-meta (:name m) m) v))

     id-ns (let [ns (create-ns (thaw-from-stream!* s))
                 publics (thaw-from-stream!* s)]
             ns)


     ;;; DEPRECATED
     id-old-reader (read-string (.readUTF s))
     id-old-string (.readUTF s)
     id-old-map    (apply hash-map (repeatedly (* 2 (.readInt s))
                                               (partial thaw-from-stream!* s)))

     (throw (Exception. (str "Failed to thaw unknown type ID: " type-id))))))
