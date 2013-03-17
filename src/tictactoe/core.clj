(ns tictactoe.core
  (:require [clojure.test :refer [deftest is]]))

(def board
  [[1 0 1]
   [0 0 1]
   [0 1 0]])

(def empty-board
  [[nil nil nil]
   [nil nil nil]
   [nil nil nil]])

(defn columns [board]
  [(mapv first board)
   (mapv second board)
   (mapv last board)])

(defn diagonals [board]
  (let [row-1 (first board)
        row-2 (second board)
        row-3 (last board)]
  [[(first row-1) (second row-2) (last row-3)]
   [(last row-1) (second row-2) (first row-3)]]))

(defn winner [row]
  (let [sum (reduce + row)]
    (cond (= 3 sum) 1
          (= 0 sum) 0
          :else nil)))

(defn check-for-wins [board]
  (concat (map winner board)
          (map winner (columns board))
          (map winner (diagonals board))))

(defn valid? [[row col piece] board]
  (cond (> row 2) false
        (> col 2) false
        (not= nil (nth (nth board row) col)) false
        :else true))

(defn make-move [move board]
  (let [[row col piece] move]
    (if (valid? move board)
      (let [newrow (assoc (nth board row) col piece)]
        (assoc board row newrow))
      (throw (Exception. (str "Invalid move: player "
                              piece
                              " (" row ", " col ")"))))))
