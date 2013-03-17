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
