(ns tictactoe.core-test
  (:use clojure.test
        tictactoe.core))

(deftest get-columns
  (let [board [[1 0 1]
               [0 0 1]
               [0 1 0]]]

    (is (= (columns board)
           [[1 0 0]
            [0 0 1]
            [1 1 0]]))))

(deftest get-diagonals
  (let [board [[1 0 1]
               [0 0 1]
               [0 1 0]]]

    (is (= (diagonals board) 
           [[1 0 0]
            [1 0 0]]))))

(deftest p1-win
  (is (= 1 (winner [1 1 1]))))

(deftest p0-win
  (is (= 0 (winner [0 0 0]))))

(deftest nil-win
  (is (nil? (winner [1 0 0]))))

(deftest check-wins
  (is (= '(1 nil nil nil nil nil nil nil)
         (check-for-wins [[1 1 1]
                          [1 0 0]
                          [0 0 1]]))))

(deftest p0-r1-c0
  (is (= (make-move [1 0 0] empty-board)
         [[nil nil nil]
          [ 0  nil nil]
          [nil nil nil]])))

(deftest p1-r2-c1
  (is (= (make-move [2 1 1] empty-board)
         [[nil nil nil]
          [nil nil nil]
          [nil  1  nil]])))

(deftest invalid-move
  (is (= false
         (valid? [1 1 0] [[nil nil nil]
                          [nil  1  nil]
                          [nil nil nil]]))))

(deftest valid-move
  (is (= true
         (valid? [1 1 0] [[nil nil nil]
                          [ 0  nil  1 ]
                          [nil nil nil]]))))

(deftest row-out-of-bounds
  (is (= false
         (valid? [3 1 1] [[1 0 nil]
                          [0 1  1 ]
                          [1 0  0 ]]))))

(deftest col-out-of-bounds
  (is (= false
         (valid? [1 3 0] [[1 0 nil]
                          [0 1  1 ]
                          [1 0  0 ]]))))

(deftest p1-r0-c2-invalid
  (is (thrown-with-msg? Exception
                        #"Invalid move\: player 1 \(0\, 2\)"
                        (make-move [0 2 1] [[nil nil 0]
                                            [nil nil nil]
                                            [nil nil nil]]))))

(deftest first-turn
  (is (= 1
         (get-turn empty-board))))

(deftest p1-turn
  (is (= 1
         (get-turn [[nil nil nil]
                    [nil  1  nil]
                    [ 0  nil nil]]))))

(deftest p0-turn
  (is (= 0
         (get-turn [[nil  1  nil]
                    [nil nil nil]
                    [nil nil nil]]))))

(deftest board-map
  (is (= [["X" "O" nil]
          ["O" "O" "X"]
          [nil "X" "O"]]
         (boardmap (fn [s] (cond (= nil s) nil
                                 (= 1 s) "X"
                                 (= 0 s) "O"))
                   [[ 1  0 nil]
                    [ 0  0  1 ]
                    [nil 1  0 ]]))))

(deftest unflatten-board
  (is (= [[ 0  nil  0 ]
          [ 1   0   1 ]
          [nil nil  0 ]]
         (unflatten [0 nil 0 1 0 1 nil nil 0]))))

(deftest next-board-empty
  (is (= '([ 1  nil nil nil nil nil nil nil nil] 
           [nil  1  nil nil nil nil nil nil nil] 
           [nil nil  1  nil nil nil nil nil nil] 
           [nil nil nil  1  nil nil nil nil nil] 
           [nil nil nil nil  1  nil nil nil nil] 
           [nil nil nil nil nil  1  nil nil nil] 
           [nil nil nil nil nil nil  1  nil nil] 
           [nil nil nil nil nil nil nil  1  nil] 
           [nil nil nil nil nil nil nil nil 1  ])
         (next-boards empty-board))))

(deftest next-board-p1
  (is (= '([1  0  nil nil nil nil nil nil nil] 
           [1 nil  0  nil nil nil nil nil nil] 
           [1 nil nil  0  nil nil nil nil nil] 
           [1 nil nil nil  0  nil nil nil nil] 
           [1 nil nil nil nil  0  nil nil nil] 
           [1 nil nil nil nil nil  0  nil nil] 
           [1 nil nil nil nil nil nil  0  nil] 
           [1 nil nil nil nil nil nil nil  0 ])
         (next-boards [[ 1  nil nil]
                       [nil nil nil]
                       [nil nil nil]]))))

(deftest next-board-p0
  (is (= '([0  1  nil 1 nil nil nil nil nil] 
           [0 nil  1  1 nil nil nil nil nil] 
           [0 nil nil 1  1  nil nil nil nil] 
           [0 nil nil 1 nil  1  nil nil nil] 
           [0 nil nil 1 nil nil  1  nil nil] 
           [0 nil nil 1 nil nil nil  1  nil] 
           [0 nil nil 1 nil nil nil nil  1 ])
         (next-boards [[ 0  nil nil]
                       [ 1  nil nil]
                       [nil nil nil]]))))

(deftest score-p0-win
  (is (= 1
         (score-board [[ 0   0   0 ]
                       [nil  1   1 ]
                       [nil nil nil]]))))

(deftest score-p1-win
  (is (= -1
         (score-board [[ 1  nil nil]
                       [ 1   0  nil]
                       [ 1  nil  0 ]]))))

(deftest score-no-win
  (is (= 0
         (score-board [[ 1  nil nil]
                       [nil  0  nil]
                       [ 1  nil  0 ]]))))

(deftest win-on-board
  (is (= true
         (win? [[ 1 nil nil]
                [ 1  0  nil]
                [ 1 nil  0 ]]))))

(deftest p0-win-scores
  (is (= [[ 0   1   1 ] 
          [ 0   1  nil] 
          [ 0  nil nil]]
        (best-move [[ 0   1   1 ]
                    [ 0   1  nil]
                    [nil nil nil]]))))

(deftest late-game
  (is (= [[1 1 0] [0 0 nil] [1 nil nil]]
         (best-move [[1 1 0] [nil 0 nil] [1 nil nil]]))))

(deftest open-0-0
  (is (= [[1 nil nil] [nil 0 nil] [nil nil nil]]
         (best-move [[1 nil nil] [nil nil nil] [nil nil nil]]))))

(deftest open-0-1
  (is (= [[nil 1 nil] [nil nil nil] [nil 0 nil]]
         (best-move [[nil 1 nil] [nil nil nil] [nil nil nil]]))))

(deftest open-0-2
  (is (= [[nil nil 1] [nil 0 nil] [nil nil nil]]
         (best-move [[nil nil 1] [nil nil nil] [nil nil nil]]))))

(deftest open-1-0
  (is (= [[nil nil nil] [1 nil nil] [0 nil nil]]
         (best-move [[nil nil nil] [1 nil nil] [nil nil nil]]))))

(deftest open-1-1 
  (is (= [[nil nil nil] [nil 1 nil] [nil nil 0]]
         (best-move [[nil nil nil] [nil 1 nil] [nil nil nil]]))))

(deftest open-1-2
  (is (= [[nil nil nil] [nil nil 1] [nil nil 0]]
         (best-move [[nil nil nil] [nil nil 1] [nil nil nil]]))))

(deftest open-2-0 
  (is (= [[nil nil nil] [nil 0 nil] [1 nil nil]]
         (best-move [[nil nil nil] [nil nil nil] [1 nil nil]]))))

(deftest open-2-1
  (is (= [[nil nil nil] [nil nil nil] [nil 1 0]]
         (best-move [[nil nil nil] [nil nil nil] [nil 1 nil]]))))

(deftest open-2-2 
  (is (= [[nil nil nil] [nil 0 nil] [nil nil 1]]
         (best-move [[nil nil nil] [nil nil nil] [nil nil 1]]))))

(deftest horizontal-l-to-r
  (is (= [[1 1 0] [nil 0 nil] [nil nil nil]]
         (best-move [[1 1 nil] [nil 0 nil] [nil nil nil]]))))

(deftest horizontal-r-to-l
  (is (= [[0 1 1] [nil nil nil] [nil 0 nil]]
         (best-move [[nil 1 1] [nil nil nil] [nil 0 nil]]))))

(deftest horizontal-center
  (is (= [[nil nil nil] [0 1 1] [nil nil 0]]
         (best-move [[nil nil nil] [nil 1 1] [nil nil 0]]))))

(deftest vertical-center
  (is (= [[nil 1 nil] [nil 1 nil] [nil 0 0]]
         (best-move [[nil 1 nil] [nil 1 nil] [nil nil 0]]))))

(deftest l-then-r
  (is (= [[nil nil nil][1 0 1][0 nil nil]]
         (best-move [[nil nil nil] [1 nil 1] [0 nil nil]]))))

(deftest diagonal-squeeze
  (is (= [[0 1 1] [nil 0 nil] [1 0 nil]]
         (best-move [[nil 1 1] [nil 0 nil] [1 0 nil]]))))

(deftest p1-mistake
  (is (= [[1 0 1] [nil 0 nil] [1 0 nil]]
         (best-move [[1 nil 1] [nil 0 nil] [1 0 nil]]))))

(deftest opposite-corners
  (is (= [[nil nil 1] [nil 0 nil] [1 0 nil]]
         (best-move [[nil nil 1] [nil 0 nil] [1 nil nil]]))))

(deftest p0-block-win
  (is (= [[ 1   0  nil] 
          [nil  1  nil] 
          [nil nil  0 ]]
         (best-move [[ 1   0  nil]
                     [nil  1  nil]
                     [nil nil nil]]))))

(deftest get-1-2
  (is (= 0
         (get-square 1 2 [[1 0 1]
                          [1 1 0]
                          [0 0 1]]))))

;(deftest edge
;  (is (= true
;         (edge? [[nil  1  nil]
;                 [nil nil nil]
;                 [nil nil nil]]))))

;(deftest corner
;  (is (= true
;         (corner? [[ 1  nil nil]
;                   [nil nil nil]
;                   [nil nil nil]]))))

;(deftest center
;  (is (= true
;         (center? [[nil nil nil]
;                   [nil  1  nil]
;                   [nil nil nil]]))))

(deftest tree-min
  (is (= 0
         (min-leaf '((10) (9 8 (7) (6 5 (4)) 3) 2 (1 (0)))))))

(deftest tree-max
  (is (= 10
         (max-leaf '((10) (9 8 (7) (6 5 (4)) 3) 2 (1 (0)))))))

(deftest win-in-nextboards
  (is (= true
         (win-in-boards? [[1 1 1 nil 0 0 nil nil nil] 
                          [nil nil nil nil nil nil nil nil nil]]))))

(deftest no-win-in-nextboards
  (is (= false
         (win-in-boards? [[0 1 1 nil 0 0 nil nil nil] 
                          [nil nil nil nil nil nil nil nil nil]]))))

(deftest full-board
  (is (= true
         (full? [[1 1 0] [0 0 1] [1 1 0]]))))

(deftest one-space-left
  (is (= false
         (full? [[nil 1 0] [0 0 1] [1 1 0]]))))

(deftest get-1-1
  (is (= 1
         (get-square 1 1 [[0 nil nil] [nil 1 1] [nil 0 nil]]))))

(deftest get-0-0
  (is (= 0
         (get-square 0 0 [[0 nil nil] [nil 1 1] [nil nil nil]]))))

(deftest get-2-1
  (is (= nil
         (get-square 2 1 [[0 nil nil] [nil 1 1] [nil nil nil]]))))

(deftest opening-position
  (is (= true
         (opening? [[1 nil nil] [nil nil nil] [nil nil nil]]))))

(deftest not-an-opening
  (is (= false
         (opening? [[1 0 nil] [nil nil nil] [nil nil nil]]))))

(deftest all-nil
  (is (= false
         (not-empty-row? [nil nil nil]))))

(deftest one-zero
  (is (= true
         (not-empty-row? [0 nil nil]))))

(deftest col1-win
  (is (= 1
         (get-row-win [nil 1 nil]))))

(deftest right-diag-win
  (is (= [1 [0 0] [1 1] [2 2]]
         (get-win [[1 nil nil] [nil 1 nil] [nil nil 1]]))))

(deftest left-diag-win
  (is (= [1 [0 2] [1 1] [2 0]]
         (get-win [[nil nil 1] [nil 1 nil] [1 nil nil]]))))

(deftest col2-win
  (is (= [0 [0 2] [1 2] [2 2]]
         (get-win [[nil nil 0] [nil nil 0] [nil nil 0]]))))

(deftest row1-win
  (is (= [0 [1 0] [1 1] [1 2]]
         (get-win [[nil nil nil] [0 0 0] [nil nil nil]]))))

(deftest no-win
  (is (= nil
         (get-win [[1 0 1] [1 0 1] [0 1 0]]))))

(deftest symmetric-boards
  (is (= [[[ 0   1  nil]
           [nil nil  1 ]
           [ 1   0  nil]]
          [[nil  1  nil]
           [ 1  nil  0]
           [ 0  nil  1]]
          [[nil  0   1]
           [ 1  nil nil]
           [nil  1   0]]]
         (all-rotations [[ 1  nil  0]
                         [ 0  nil  1]
                         [nil  1  nil]])))) 
