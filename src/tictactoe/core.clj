(ns tictactoe.core
  (:require [clojure.test :refer [deftest is]]))

(def empty-board
  [[nil nil nil]
   [nil nil nil]
   [nil nil nil]])

(defn columns 
  "Takes a 3x3 tic-tac-toe board (a vector of vectors). 
   Returns a vector of vectors of its columns:

     => (columns [[ 1  0  nil]
                  [ 1 nil  0 ]
                  [ 0 nil nil]])
     
    [[1 1 0] [0 nil nil] [nil 0 nil]]
  "
  [board]
  [(mapv first board)
   (mapv second board)
   (mapv last board)])

(defn diagonals 
  "Takes a 3x3 tic-tac-toe board (a vector of vectors). 
   Returns a vector of vectors of its diagonals:

     => (diagonals [[ 1  0  nil]
                    [ 1 nil  0 ]
                    [ 0 nil nil]])
     
    [[1 nil nil] [nil nil 0]]
  "
  [board]
    (let [row-1 (first board)
        row-2 (second board)
        row-3 (last board)]
  [[(first row-1) (second row-2) (last row-3)]
   [(last row-1) (second row-2) (first row-3)]]))

(defn in-row? 
  "Takes a sequence and element. Returns true if the element is
   found in the sequence. This is a linear scan, used here to
   check if the given element is in a three-element row. Don't
   use this for big rows!
  
     => (in-row? [1 1 nil] nil)
     true

     => (in-row? [1 1 nil] 0)
     false
  "
  [row el]
 (if (empty? row) 
   false
   (if (= (first row) el) 
     true 
     (recur (rest row) el))))

(defn winner 
  "Takes a row vector. Returns winner 1 or 0 if it contains three
   in a row, else nil.
  
     => (winner [1 1 1])
     1

     => (winner [1 1 0])
     nil
  "
  [row]
  (if (in-row? row nil) 
    nil
    (let [sum (reduce + row)]
      (cond (= 3 sum) 1
            (= 0 sum) 0
            :else nil))))

(defn check-for-wins
  "Takes a 3x3 tic-tac-toe board (vector of vectors). Checks for
   winning moves across rows, columns, and diagonals, and returns
   them as a concatenated list.
  
     => (check-for-wins [[1 1 1]
                         [1 0 0]
                         [0 0 1]])

     (1 nil nil nil nil nil  nil nil)
     |--rows--| |-columns-| |-diags-|
  "
  [board]
    (concat (map winner board)
          (map winner (columns board))
          (map winner (diagonals board))))

(defn win?
  "Takes a 3x3 tic-tac-toe board (vector of vectors). Returns true
  if a win is found on the board, else false.
  
     => (win? [[1 1 1]
               [1 0 0]
               [0 0 1]])
     true
  "
  [board]
    (not (empty? (remove nil? (check-for-wins board)))))

(defn valid? 
  "Takes a move (vector of row, column, player) and a 3x3 board.
   Returns false if the space is out of bounds or already occupied,
   else true.

     => (valid? [0 1 1] [[ 0  nil  1 ]
                         [nil nil nil]
                         [nil  0  nil]])
     true

     => (valid? [4 1 1] [[ 0  nil  1 ]
                         [nil nil nil]
                         [nil  0  nil]])
    false
  "
  [[row col _] board]
  (cond (> row 2) false
        (> col 2) false
        (not= nil (nth (nth board row) col)) false
        :else true))

(defn make-move
  "Takes a move (vector of row, column, player) and a 3x3 board.
   Validates the move and returns a new board, or throws an exception.
  "
  [move board]
  (let [[row col piece] move]
    (if (valid? move board)
      (let [newrow (assoc (nth board row) col piece)]
        (assoc board row newrow))
      (throw (Exception. (str "Invalid move: player "
                              piece
                              " (" row ", " col ")"))))))

(defn get-turn
  "Takes a 3x3 board (vector of vectors). Returns the player whose turn
   is next. Note: this does not check for errors or mismatched turns in 
   the board, as it's only used to construct the game tree.
  "
  [board]
  (if (odd? (reduce + (map (fn [x] (if (= nil x) 0 1)) (flatten board))))
    0
    1))

(defn unflatten 
  "Takes a flattened board (vector of length 9). Returns a 3x3 vector of
   vectors.

     => (unflatten [nil 1 0 nil nil nil 1 0 1])
     [[nil  1   0 ]
      [nil nil nil]
      [ 1   0   1 ]]
  "
  [flat-board]
  (let [rows     (cons (first (split-at 3 flat-board)) 
                       (split-at 3 (second (split-at 3 flat-board))))]
        [(vec (first rows))
         (vec (second rows))
         (vec (last rows))]))

(defn boardmap 
  "Takes a  3x3 board (vector of vectors). Applies function f to every
   element in the board.
  "
  [f board]
  (unflatten (map f (flatten board))))

(defn next-boards
  "Takes a 3x3 board (vector of vectors). Generates the next possible moves
   on the given board. Returns a list of flattened boards.
  "
  [board]
  (if (win? board) '()
    (let [piece (get-turn board)
          board (vec (flatten board))]
      (remove nil? (map (fn [i] (when (nil? (board i)) (assoc board i piece)))
                   (range (count board)))))))

(defn score-board
  "Calculates a score for the given board: 1 if it contains a win for player
   0, -1 if it contains a win for player 1, 0 if it's a draw.
  "
  [board]
  (let [wins (check-for-wins board)]
    (cond (in-row? wins 0)  1
          (in-row? wins 1) -1
          :else             0)))

(defn add-leaves 
  "Returns the sum of the leaves of a given tree."
  [tree]
  (if (coll? tree)
    (reduce + 0 (map add-leaves tree))
    tree))

(defn count-leaves 
  "Returns the number of leaves in a given tree."
  [tree]
  (if (coll? tree)
    (reduce + 0 (map count-leaves tree))
    1))

(defn min-leaf [tree]
  (if (coll? tree)
    (apply min (flatten tree))
    tree))

(defn max-leaf [tree]
  (if (coll? tree)
    (apply max (flatten tree))
    tree))

(defn win-in-boards? [boards]
  (not (empty? (remove false? (map win? (map unflatten boards))))))

(defn scr [current-board]
  "Takes a 3x3 game board (vector of vectors). Returns a map of possible
   moves (flattened boards) and their scores (rationals)."
  (defn recursive-score [current-board]
    (if (win? current-board)
      (score-board current-board))
    (let [boards (next-boards current-board)]
      (if (empty? boards)
        (score-board current-board)
        (map recursive-score (map unflatten boards))))))

(defn full? [board]
  (= 9 (count (remove nil? (flatten board)))))

(defn score-move [current-board]
  (if (or (win? current-board) (full? current-board))
    (score-board current-board)
    (let [next-moves (next-boards current-board)]
      (if (= 0 (get-turn current-board))
        (max-leaf (map score-move (map unflatten next-moves)))
        (min-leaf (map score-move (map unflatten next-moves)))))))
      

(defn get-square [row col board]
  (nth (nth board row) col))

(defn opening? [board]
  (= 1 (count (remove nil? (flatten board)))))

(defn opening-move [board]
  (cond (= 1 (get-square 0 0 board)) (make-move [1 1 0] board)
        (= 1 (get-square 0 1 board)) (make-move [2 1 0] board)
        (= 1 (get-square 0 2 board)) (make-move [1 1 0] board)
        (= 1 (get-square 1 0 board)) (make-move [2 0 0] board)
        (= 1 (get-square 1 1 board)) (make-move [2 2 0] board)
        (= 1 (get-square 1 2 board)) (make-move [2 2 0] board)
        (= 1 (get-square 2 0 board)) (make-move [1 1 0] board)
        (= 1 (get-square 2 1 board)) (make-move [2 2 0] board)
        (= 1 (get-square 2 2 board)) (make-move [1 1 0] board)))

(defn best-move [current-board]
  (if (opening? current-board)
    (opening-move current-board)
    (let [movescores (map vector (map score-move (map unflatten (next-boards current-board)))
                                 (map unflatten (next-boards current-board)))]
      (second (last (sort-by first movescores))))))

(defn corner? [board]
  (not (empty? (remove nil? (map (fn [x] (get-square (first x)
                                                     (second x)
                                                     board))
                                   [[0 0] [0 2] 
                                    [2 0] [2 2]])))))

(defn edge? [board]
  (not (empty? (remove nil? (map (fn [x] (get-square (first x)
                                                     (second x)
                                                     board))
                                  [[0 1] [1 0]
                                  [1 2] [2 1]])))))

(defn center? [board]
  (not (nil? (get-square 1 1 board))))
