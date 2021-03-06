(ns packss.core
  (:refer-clojure :exclude [record?])
  (:import (java.lang.reflect Method))
  )


;;; ----------------------------------------------------------------
;;; defrecord utility

(defn record? [obj]
  (.isInstance clojure.lang.IRecord obj))
(defn record->map [record]
  (into {} record))
;(defn map->record [class-symbol m]
;  (let [s (symbol (str class-symbol "/create"))]
;    (eval `(~s ~m))))
(defn map->record* [^Class a-class m]
  (let [^Method method (.getMethod a-class "create"
                                   (into-array [clojure.lang.IPersistentMap]))]
    (.invoke method nil (into-array [m]))))
(defn map->record [class-symbol m]
  (map->record* (resolve class-symbol) m))

;;; ----------------------------------------------------------------

(def current-protocol "PACKSS/1.1")

;;; (stack 0) => {:protocol "PACKSS/1.1", ...} ; meta-info
;;; (stack 1) => [mapped-obj obj-type] ; 1 is root
;;; (stack 2) => [mapped-obj obj-type] ; 2 and others are children
;;; (stack 3) => ...
(declare ^:private ^:dynamic stack)
(declare ^:private ^:dynamic cache) ; {obj idx, ...} or {idx obj, ...}
(declare ^:private ^:dynamic ext) ; convert table for user
(declare ^:private ^:dynamic scanner) ; object filter for user
  
(declare obj->idx idx->obj)

;;; ----------------------------------------------------------------

(defn assoc-packss-entry
  [table ^java.lang.Class class1 mapper unmapper & [replacer]]
  (let [k (.getName class1)
        v {:mapper mapper, :unmapper unmapper, :replacer replacer}]
    (assoc (or table {}) k v)))

;;; (make-packss-table
;;;   [ClassObject mapper unmapper]
;;;   [ClassObject2 mapper2 unmapper2 replacer2]
;;;   ...
;;;   )
(defn make-packss-table [& entries]
  (reduce #(apply assoc-packss-entry %1 %2)
          {}
          entries))

;;; internal translate table
(def ^:private built-in-packss-table
  (make-packss-table
    [(class (boolean-array 0)) seq #(boolean-array (map (comp not not) %))]
    [(class (byte-array 0)) seq #(byte-array (map byte %))]
    [(class (short-array 0)) seq #(short-array (map short %))]
    [(class (char-array 0)) seq #(char-array (map char %))]
    [(class (int-array 0)) seq int-array]
    [(class (long-array 0)) seq long-array]
    [(class (float-array 0)) seq float-array]
    [(class (double-array 0)) seq double-array]
    [(class (object-array 0)) #(doall (map obj->idx %)) object-array
     (fn [^"[Ljava.lang.Object;" os]
       (dotimes [i (alength os)]
         (aset os i (idx->obj (aget os i)))))]
    [clojure.lang.Atom #(obj->idx @%) atom #(swap! % idx->obj)]
    [clojure.lang.Ref #(obj->idx @%) ref #(dosync (alter % idx->obj))]
    ))


;;; ----------------------------------------------------------------
;;; class utility

(def ^:dynamic base-packable-classes
  (let [whitelist []
        blacklist [clojure.lang.Var
                   java.sql.Timestamp
                   clojure.lang.Fn
                   java.util.Calendar
                   clojure.lang.Namespace
                   java.util.regex.Pattern
                   java.util.UUID
                   java.lang.Class
                   java.util.Date
                   ]
        print-dupables (set (filter identity (map first (methods print-dup))))]
    (into (apply disj print-dupables blacklist) whitelist)))

(defn unmemoized-class-packable? [^Class a-class & [user-ext]]
  (let [class-name (.getName a-class)]
    (or
      (and user-ext (user-ext class-name))
      (built-in-packss-table class-name)
      (some #(.isAssignableFrom ^Class % a-class) base-packable-classes))))

(def class-packable? (memoize unmemoized-class-packable?))

(defn packable? [obj & [user-ext]]
  (if (nil? obj)
    true
    (class-packable? (class obj) user-ext)))

(defn atomically-immutable? [obj]
  (or
    (instance? clojure.lang.Named obj)
    (instance? String obj)
    (instance? Number obj)
    (instance? Boolean obj)
    (instance? Character obj)
    (and (coll? obj) (not (seq obj))) ; empty-coll?
    ))

;;; ----------------------------------------------------------------
;;; scanner utility

(defn inspect-unpackable-obj [obj & [user-ext]]
  ;(prn (packable? obj user-ext) (class obj))
  (when-not (packable? obj user-ext)
    (throw (ex-info "cannot pack" {:obj obj, :user-ext user-ext})))
  obj)

(defn substitute-nil-for-unpackable-obj [obj & [user-ext]]
  (when (packable? obj user-ext)
    obj))

;;; ----------------------------------------------------------------

(defn- mapping [obj]
  ;; if obj may contains other objs, must mapping recursively
  (let [class-name (if (nil? obj) nil (.getName (class obj)))]
    (if-let [packss-entry (ext class-name)]
      [((:mapper packss-entry) obj) class-name]
      (cond
        (nil? obj) [nil nil]
        ;; NB: record is instance of map!
        (record? obj) [(reduce (fn [prev [k v]]
                                 (assoc prev (obj->idx k) (obj->idx v)))
                               {}
                               obj) class-name]
        (map? obj) [(reduce (fn [prev [k v]]
                              (assoc prev (obj->idx k) (obj->idx v)))
                            {}
                            obj) :map]
        (set? obj) [(reduce (fn [prev one]
                              (conj prev (obj->idx one)))
                            #{}
                            obj) :set]
        (list? obj) [(doall (map obj->idx obj)) :list]
        (vector? obj) [(doall (map obj->idx obj)) :vector]
        (seq? obj) [(doall (map obj->idx obj)) :seq]
        (coll? obj) [(doall (map obj->idx obj)) :coll]
        :else (if-let [packss-entry (built-in-packss-table class-name)]
                [((:mapper packss-entry) obj) class-name]
                [obj nil])))))

(defn- unmapping [mapped-obj mtype]
  (case mtype
    nil mapped-obj
    :map (reduce (fn [prev [k v]]
                   (assoc prev (idx->obj k) (idx->obj v)))
                 {}
                 mapped-obj)
    :set (reduce (fn [prev one]
                   (conj prev (idx->obj one)))
                 #{}
                 mapped-obj)
    :list (apply list (doall (map idx->obj mapped-obj)))
    :vector (vec (doall (map idx->obj mapped-obj)))
    :seq (doall (map idx->obj mapped-obj))
    :coll (vec (doall (map idx->obj mapped-obj)))
    (if-let [packss-entry (or
                             (ext mtype)
                             (built-in-packss-table mtype))]
      ((:unmapper packss-entry) mapped-obj)
      (try
        ;; try unmap defrecord
        (let [m (reduce (fn [prev [k v]]
                          (assoc prev (idx->obj k) (idx->obj v)))
                        {}
                        mapped-obj)]
          (map->record (symbol mtype) m))
        (catch Throwable e
          (throw (ex-info "cannot unmap object" {:obj mapped-obj
                                                 :mtype mtype
                                                 :exception e
                                                 })))))))

(defn- replacing! [obj mtype]
  (if-let [ext-replacer (:replacer (ext mtype))]
    (ext-replacer obj)
    (when-let [replacer (:replacer (built-in-packss-table mtype))]
      (replacer obj))))


;;; NB: cache(map) cannot distinguish Java instance of same value
;;;     (e.g. java.awt.Point).
;;;     Because it preserve by linear.
(defn obj->idx [src-obj]
  ;; TODO: must be safe from stack overflow
  (let [entries (cache src-obj)]
    (if-let [idx (if (number? entries)
                   entries
                   (some (fn [[k v]]
                           (and (identical? src-obj k) v))
                         entries))]
      idx
      (let [new-idx (count stack)
            scanned (scanner src-obj)
            cache-entry-new (if (atomically-immutable? src-obj)
                              new-idx
                              (conj (or entries nil) [src-obj new-idx]))]
        (set! cache (assoc cache src-obj cache-entry-new))
        (let [new-entry (delay
                          (let [[mapped mtype] (mapping scanned)
                                mtype (if (string? mtype)
                                        (obj->idx mtype)
                                        mtype)]
                            [mapped mtype]))]
          (set! stack (conj stack new-entry)) ; reserve to entry
          (force new-entry)
          new-idx)))))

(defn idx->obj [idx]
  ;; TODO: must be safe from stack overflow
  (if-let [cached (cache idx)]
    cached
    (let [[mapped-obj mtype] (stack idx)
          mtype (if (number? mtype) (idx->obj mtype) mtype)
          obj (unmapping mapped-obj mtype)]
      (set! cache (assoc cache idx obj))
      obj)))


(defn- make-meta-info []
  ;; TODO: add more meta-info
  {:protocol current-protocol
   })

;;; TODO: write docstrings
(defn pack [root-obj & [user-ext obj-scanner]]
  ;; TODO: must be thread safe
  (binding [stack [0]
            cache {}
            ext (or user-ext {})
            scanner (or obj-scanner identity)]
    (obj->idx root-obj)
    (into [(make-meta-info)]
          (map deref (rest stack)))))

;;; TODO: write docstrings
(defn unpack [packed & [user-ext]]
  ;; TODO: must be thread safe
  (let [meta-info (packed 0)]
    ;; TODO: check protocol and support old version
    (binding [stack packed
              cache {}
              ext (or user-ext {})]
      ;; extract all stacks
      (dotimes [i (count stack)]
        (when-not (zero? i)
          (idx->obj i)))
      ;; fix mutable objects
      (dotimes [i (count stack)]
        (when-not (zero? i)
          (let [[mapped-obj mtype] (stack i)
                mtype (if (number? mtype) (idx->obj mtype) mtype)]
            (replacing! (cache i) mtype))))
      ;; return root-obj
      (idx->obj 1))))

