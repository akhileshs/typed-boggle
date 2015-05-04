(set! *warn-on-reflection* true)
(ns typed-boggle.dictionary
  (:require [clojure.core.typed :as t]))

(t/ann well-formed-word? [String -> Boolean])
(defn well-formed-word?
  [s]
  (and (java.util.regex.Pattern/matches "[A-Z]*" s)
       (> (.length s) 2)
       (< (.length s) 8)))

(defn starting-with-coll
  [dict char]
  (get dict char))

(t/ann empty-word-set [-> (t/Set Any)])
(defn empty-word-set
  []
  #{})

(t/ann add-word [(t/Map java.lang.Character java.lang.String) java.lang.String -> (t/Map java.lang.Character java.lang.String)])
(defn add-word
  [dict word]
  (let [first-char (.charAt word 0)]
    (let [current-word-set
          (if (nil? (starting-with-coll dict first-char))
            (empty-word-set)
            (starting-with-coll dict first-char))]
      (assoc dict first-char (conj current-word-set word)))))

(defn load-dict
  [file]
  (let [buf (java.io.BufferedReader. (java.io.FileReader. file))]
    (loop [dict {}]
      (if (not (.ready buf))
        dict
        (let [new-word
              (.replace (.toUpperCase (.readLine buf)) "'" "")]
          (if (well-formed-word? new-word)
            (recur (add-word dict new-word))
            (recur dict)))))))
