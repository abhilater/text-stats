(ns text-stats.core
  (:require [clojure.java.io :as io]
            [clojure.core.async :as a])
  (:gen-class))

(defn- slurp-resource
  "Given a filename in resource directory,
   it'll read the contents of that filename."
  [filename]
  (-> filename
      io/resource
      slurp))


(defn- create-stop-words-set
  "Creates an in-memory set using the stop-words
   text file from resource"
  [filename]
  (-> filename
      io/resource
      io/reader
      line-seq
      set))


(defn- create-file-line-seq
  [filename]
  (-> filename
      io/resource
      io/reader
      line-seq))


(def stop-words-set (create-stop-words-set "stopwords_en.txt"))


;; Read complete file in memory version (SIMPLE)
(defn- calc-word-freqs
  "Takes a text file input and processes the same to
   remove stop words and then calcs frequency of remaining words"
  [filename]
  (->> filename
       slurp-resource
       (re-seq #"\w+")
       (filter #(not (contains? stop-words-set %)))
       frequencies))


(defn get-top-n
  "Gets the top n used words by sorting the word freq map
  by freq value and then taking the top n eg

  (get-top-n \"alice.txt\" 5)
  => ([\"I\" 543] [\"Alice\" 396] [\"The\" 119] [\"Project\" 81] [\"Gutenberg\" 81])
  "
  [filename n]
  (let [freq-map (calc-word-freqs filename)]
    (take n (into (sorted-map-by (fn [k1 k2]
                                   (compare [(get freq-map k2) k2]
                                            [(get freq-map k1) k1])))
                  freq-map))))


(defn get-bottom-n
  "Gets the bottom n used words by sorting the word freq map
  by freq value and then taking the top n

  (get-bottom-n \"alice.txt\" 5)
  => ([\"000\" 1] [\"1500\" 1] [\"1887\" 1] [\"20\" 1] [\"2001\" 1])
  "
  [filename n]
  (let [freq-map (calc-word-freqs filename)]
    (take n (into (sorted-map-by (fn [k1 k2]
                                   (compare [(get freq-map k1) k1]
                                            [(get freq-map k2) k2])))
                  freq-map))))


;; TODO read file line by line and process

(defn calc-word-freqs-line-by-line [filenames]
  (reduce #(merge-with + %1 %2) {} (->> filenames (mapcat create-file-line-seq)
       (map #(re-seq #"\w+" %))
       (filter #(not (contains? stop-words-set %)))
       (filter #(not (nil? %)))
       (map frequencies))))



;; TODO using transducer

(def xform
  (comp
    (mapcat create-file-line-seq)
    (map #(re-seq #"\w+" %))
    (filter #(not (contains? stop-words-set %)))
    (filter #(not (nil? %)))
    (map frequencies)))


(defn xreducing
  ([] [])
  ([result] result)
  ([result input] (merge-with + result input)))


(defn calc-word-freqs-line-by-line-transducer [filenames]
  (transduce xform xreducing {} filenames))


(defn get-top-n-1
  [filenames n]
  (let [freq-map (calc-word-freqs-line-by-line-transducer filenames)]
    (take n (sort-by val > freq-map))))


(defn get-top-n-2
  [filenames n]
  (let [freq-map (calc-word-freqs-line-by-line filenames)]
    (take n (sort-by val > freq-map))))



; (time (calc-word-freqs-line-by-line (repeat 10 "t8.shakespeare.txt")))
;"Elapsed time: 17104.502851 msecs"
;(time (calc-word-freqs-line-by-line-transducer (repeat 10 "t8.shakespeare.txt")))
;"Elapsed time: 15939.241162 msecs"

;(time (calc-word-freqs-line-by-line (repeat 50 "t8.shakespeare.txt")))
;"Elapsed time: 81843.445115 msecs"
;(time (calc-word-freqs-line-by-line-transducer (repeat 50 "t8.shakespeare.txt")))
;"Elapsed time: 81814.07475 msecs"


;; TODO using transducer and pileline

;(defn- calc-word-freqs-1
;  "Takes a text file input and processes the same to
;   remove stop words and then calcs frequency of remaining
;   words"
;  [filename]
;  (let [c (async/chan 200)]
;    (async/go (doseq [line (create-file-line-seq filename)]
;      (async/>! c (frequencies
;                     (filter #(not (contains? stop-words-set %))
;                             (re-seq #"\w+" line))))))
;    (merge-with + (async/<!! c)))
;
;    )
;
;(defn process-parallel [filename]
;  (a/<!!
;    (a/pipeline
;      (.availableProcessors (Runtime/getRuntime)) ;; Parallelism factor
;      (doto (a/chan) (a/close!))                  ;; Output channel - /dev/null
;      xform
;      (a/to-chan filename))))                        ;; Channel with input data




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


