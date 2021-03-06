(ns packss.core-test
  (:import [java.awt Point Rectangle])
  (:require [clojure.test :refer :all]
            [packss.core :refer :all]
            [taoensso.nippy :as nippy]
            [clojure.edn :as edn]
            ;[expectations :as test :refer :all]
            ))

(def stress-data
  ;; remove unsupported objects and uncomparable objects
  (dissoc nippy/stress-data
          :throwable :ex-info :exception))

(def stress-data-without-array
  ;; remove unsupported objects, uncomparable objects and array
  (dissoc nippy/stress-data
          :throwable :ex-info :exception :bytes))

(def record-data {:stress-record (:stress-record nippy/stress-data)})

(def mutable-data
  {:atom (atom 1)
   :ref (ref :a)
   :boolean-array (boolean-array [false true false])
   :byte-array (byte-array (map byte [1 2 3]))
   :short-array (short-array (map short [4 5 6]))
   :char-array (char-array (map char [65 66 67]))
   :int-array (int-array (map int [7 8 9]))
   :long-array (long-array (map long [-2 -1 0]))
   :float-array (float-array (map float [1.1 2.2 3.3]))
   :double-array (double-array (map double [4.4 5.5 6.6]))
   :object-array (object-array [4 'a :b "ccc" stress-data-without-array])
   :nested (atom (atom 3))
   })

(def seq-data
  {:list '(1 2 3 a b c)
   :vec [4 5 6 :x :y :z]
   })

(def isomorphic-data (list mutable-data mutable-data))

(def circular-data (atom (object-array [0 nil 2])))
(aset ^"[Ljava.lang.Object;" @circular-data 1 circular-data)
  
(defn array= [a b]
  (or
    (= a b)
    ;; TODO: implicit check array
    (= (seq a) (seq b))))

(deftest stress-test
  (testing "pack->unpack stress-data"
    (is (doall
          (map
            (fn [[k v]]
              (let [v2 (unpack (pack v))]
                (when-not (array= v v2)
                  (prn "cannot match" k v v2)
                  (throw (Exception. "cannot match")))))
            stress-data)))
    (is stress-data (unpack (pack stress-data))))
  (testing "pack->unpack record-data"
    (is (doall
          (map
            (fn [[k v]]
              (let [v2 (unpack (pack v))]
                (when-not (= v v2)
                  (prn "cannot match" k v v2)
                  (throw (Exception. "cannot match")))))
            record-data)))
    (is record-data (unpack (pack record-data))))
  (testing "pack->unpack mutable-data"
    (let [original mutable-data
          target (unpack (pack mutable-data))]
      (is (= @(:atom original) @(:atom target)))
      (is (= @(:ref original) @(:ref target)))
      (is (= (seq (:boolean-array original)) (seq (:boolean-array target))))
      (is (= (seq (:byte-array original)) (seq (:byte-array target))))
      (is (= (seq (:short-array original)) (seq (:short-array target))))
      (is (= (seq (:char-array original)) (seq (:char-array target))))
      (is (= (seq (:int-array original)) (seq (:int-array target))))
      (is (= (seq (:long-array original)) (seq (:long-array target))))
      (is (= (seq (:float-array original)) (seq (:float-array target))))
      (is (= (seq (:double-array original)) (seq (:double-array target))))
      (is (= (seq (:object-array original)) (seq (:object-array target))))
      (is (= @@(:nested original) @@(:nested target)))
      ))
  (testing "pack->unpack seq-data"
    (let [original seq-data
          target (unpack (pack seq-data))]
      (is (= (:list original) (:list target)))
      (is (list? (:list original)))
      (is (list? (:list target)))
      (is (= (:vec original) (:vec target)))
      (is (vector? (:vec original)))
      (is (vector? (:vec target)))
      ))
  (testing "edn-safe?"
    (are [m] (doall
               (map
                 (fn [[k v]]
                   (try
                     (edn/read-string (pr-str (pack v)))
                     (catch Exception e
                       (prn "error at" k v)
                       (throw e))))
                 m))
      stress-data
      record-data
      mutable-data
      seq-data)
    (are [m] (edn/read-string (pr-str (pack m)))
      stress-data
      record-data
      mutable-data
      seq-data)
    )
  (testing "isomorphic?"
    (let [target (unpack (pack isomorphic-data))
          target-a (:int-array (first target))
          target-b (:int-array (fnext target))
          ]
      (is (identical? target-a target-b))
      (aset-int target-a 0 999)
      (is (= (seq target-a) (seq target-b)))
      ))
  (testing "circular?"
    (let [original circular-data
          target (unpack (pack circular-data))
          ]
      (is (= (aget ^"[Ljava.lang.Object;" @original 0)
             (aget ^"[Ljava.lang.Object;" @target 0)))
      (is (= (aget ^"[Ljava.lang.Object;" @original 2)
             (aget ^"[Ljava.lang.Object;" @target 2)))
      (is (= target (aget ^"[Ljava.lang.Object;" @target 1)))
      (is (= @target @(aget ^"[Ljava.lang.Object;" @target 1)))
      ))
  )



;;; TODO: add class include clojure object, for example
(def user-ext-table
  (make-packss-table
    [Point (fn [p] [(.x p) (.y p)]) #(Point. (first %) (second %))]
    [Rectangle
     (fn [r] [(.x r) (.y r) (.width r) (.height r)])
     #(Rectangle. (first %) (second %) (nth % 2) (nth % 3))]
    ))

(defrecord R [a b c])

(def user-ext-data
  {:data [1 :b 'c "4"]
   :record (->R 1 2 3)
   :point (Point. 1 2)
   :rectangle (Rectangle. -1 -2 3 4)
   })

(def nil-filtered-user-ext-data
  {:data [1 :b 'c "4"]
   :record (->R 1 2 3)
   :point nil
   :rectangle nil
   })

(deftest ext-test
  (testing "user-ext test"
    (is (thrown? RuntimeException
                 (edn/read-string (pr-str (pack user-ext-data)))))
    (let [dumped (pr-str (pack user-ext-data user-ext-table))]
      (is (= user-ext-data
             (unpack (edn/read-string dumped) user-ext-table))))))

(deftest scanner-test
  (testing "user-scanner inspect test"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pack user-ext-data nil inspect-unpackable-obj))))
  (testing "user-scanner convert test"
    (is (= (unpack (pack user-ext-data nil substitute-nil-for-unpackable-obj))
           nil-filtered-user-ext-data)))
  )

(deftest packable-test
  (testing "packable? test"
    ;(prn base-packable-classes) ; for debug
    (is (packable? nil))
    (is (packable? 1))
    (is (packable? 1.1))
    (is (packable? \a))
    (is (packable? "b"))
    (is (packable? 'c))
    (is (packable? :d))
    (is (packable? {:a :b}))
    (is (packable? #{:c :d}))
    (is (packable? [0]))
    (is (packable? '(a b c)))
    (is (packable? (take 3 (repeat "zzz"))))
    (is (packable? (int-array [1])))
    (is (packable? (object-array [[[[[[2]]]]]])))
    (is (packable? (atom :z)))
    (is (packable? (ref 1)))
    (is (packable? (->R 1 2 3)))
    (is (not (packable? (Point. 1 2))))
    (is (not (packable? (class class))))
    (is (packable? nil user-ext-table))
    (is (packable? (Point. 1 2) user-ext-table))
    (is (not (packable? (class class) user-ext-table)))
    ))

(def ssd-data
  (let [a (Point. 1 2)
        b (Point. 1 2)]
    {:a-1 a
     :a-2 a
     :b b
     }))

;;; NB: (= (Point. 1 2) (Point. 1 2)) => true
;;;     (identical? (Point. 1 2) (Point. 1 2)) => false
;;;     #{(Point. 1 2), (Point. 1 2)} => exception
(deftest shared-structure-determine-test
  (testing "shared-structure determine test"
    (let [loaded-data (unpack (pack ssd-data user-ext-table) user-ext-table)]
      (is (= (.x ^Point (:a-1 ssd-data)) 1))
      (is (= (.x ^Point (:a-2 ssd-data)) 1))
      (is (= (.x ^Point (:b ssd-data)) 1))
      (is (= (.x ^Point (:a-1 loaded-data)) 1))
      (is (= (.x ^Point (:a-2 loaded-data)) 1))
      (is (= (.x ^Point (:b loaded-data)) 1))
      (set! (.x ^Point (:a-1 ssd-data)) 3)
      (is (= (.x ^Point (:a-1 ssd-data)) 3))
      (is (= (.x ^Point (:a-2 ssd-data)) 3))
      (is (= (.x ^Point (:b ssd-data)) 1))
      (set! (.x ^Point (:a-1 loaded-data)) 4)
      (is (= (.x ^Point (:a-1 loaded-data)) 4))
      (is (= (.x ^Point (:a-2 loaded-data)) 4))
      (is (= (.x ^Point (:b loaded-data)) 1)))))

(defn- get-version []
  (nth (edn/read-string (slurp "project.clj")) 2))

(deftest measure-test
  (testing "measure test"
    (println)
    (println "date:" (str (java.util.Date.)))
    (println "version:" (get-version))
    (let [src-data (repeat 100 mutable-data)
          _ (print "measure pack time: ")
          dumped (pr-str (time (pack src-data)))
          _ (println "measure dumped size:" (alength (.getBytes dumped)))
          _ (print "measure unpack time: ")
          dst-data (edn/read-string dumped)
          restored (time (unpack dst-data))
          ]
      nil)))


