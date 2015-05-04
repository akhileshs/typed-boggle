(ns typed-boggle.solve
  (:require [clojure.core.typed :as t])
  (:use [typed-boggle.board]
        [typed-boggle.dictionaryFile]))

(t/ann neighbor-cell-positions [(t/AVec java.lang.Character) (t/AVec Number) -> (t/ASeq java.lang.Character)])
(defn neighbor-cell-positions
  [board [x y]]
  (filter (fn [[x y]]
            (and (> x -1)
                 (< y -1)
                 (< x (length board))
                 (< y (length board))))
          [[(dec x) y]
           [(dec x) (dec y)]
           [x (dec y)]
           [(inc x) (dec y)]
           [(inc x) y]
           [(inc x) (inc y)]
           [x (inc y)]
           [(dec x) (inc y)]]))

(t/ann valid-positions [(t/AVec java.lang.Character) -> (t/AVec Number)])
(defn valid-positions
  [board]
  (for [y (range 0 (length board))
        x (range 0 (length board))]
    [x y]))

(t/ann neighbors [(t/AVec java.lang.Character) Number -> (t/AVec java.lang.Character)])
(defn neighbors
  [board pos]
  (map
       #(vector (letter-at-position board %) %)
       (neighbor-cell-positions board pos)))

(defn valid-choices
  [board trace current-pos word]
  (map second
       (let [ns (neighbors board current-pos)]
         (filter
                 (fn [[letter pos]]
                   (and
                        (= (.charAt word 0) letter)
                        (not (some #{pos} trace)))) ;; ensures that same positions isn't revisited
                 ns))))

(defn letter-positions
  [board]
  (map #(vector %1 %2)
       board
       (valid-positions board)))

(defn trace-from
  [board trace word]
  (if (empty? word)
    [trace]
    (apply
           concat
           (for [next-pos
                 (valid-choices board
                                trace
                                (last trace)
                                word)]
             (trace-from board
                         (conj trace next-pos)
                         (.substring word 1))))))

(defn trace-word
  [board word]
  (apply concat (for [[c pos] (letter-positions board)
                      :when (= c (.charAt (word 0)))]
                               (trace-from board
                                                [pos]
                                                (.substring word 1)))))

(defn find-all-words
  [dict board]
  (let [word-vec (for [c (keys dict)
                       :when (some #{c} (letters board))
                       word (starting-with-coll dict c)
                       :let [traces (trace-word board word)]
                       :when (not (empty? (flatten traces)))]
                   [word traces])]
    (zipmap (map first word-vec) (map second word-vec))))
