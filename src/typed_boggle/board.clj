(ns typed-boggle.board
  (:require [clojure.core.typed :as t]))

(t/ann construct-board [(t/AVec java.lang.Character) -> (t/AVec java.lang.Character)])
(defn construct-board
  [board]
  board)

(t/ann get-letters [(t/AVec java.lang.Character) -> (t/AVec java.lang.Character)])
(defn get-letters
  [board]
  board)

(t/ann ^:no-check length [(t/AVec java.lang.Character) -> t/AnyInteger])
(defn length
  [board]
  (int (Math/sqrt (count board))))

(t/ann ^:no-check letter-at-position [(t/AVec java.lang.Character) (t/AVec t/AnyInteger) -> java.lang.Character])
(defn letter-at-position
  [board [x y]]
  (board (+ x (* y (length board)))))
