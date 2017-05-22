(ns text-stats.core
  (:require [clojure.java.io :as io])
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

(def stop-words-set (create-stop-words-set "stopwords_en.txt"))

;; Read complete file in memory version (SIMPLE)
(defn- calc-word-freqs
  "Takes a text file input and processes the same to
   remove stop words and then calcs frequency of remaining
   words"
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


;; TODO using transducer


;; TODO using transducer and pileline




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


