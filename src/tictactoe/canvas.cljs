(ns tictactoe.canvas
  (:require [tictactoe.core :refer [make-move 
                                    empty-board 
                                    best-move
                                    valid?
                                    win?
                                    draw?
                                    get-win]]))

(def board (atom empty-board))

(defn get-canvas 
  "Don't tell anyone this is actually just document.getElementById()."
  [id]
    (.getElementById js/document id))

(defn reset-canvas 
  "Clears the canvas."
  []
  (let [canvas (get-canvas "tictactoe-board")]
    (.setAttribute canvas "width" (.-width canvas))))

(defn show-reset
  "Displays the 'play again' link under the board."
  []
  (.setAttribute (.getElementById js/document "reset") "class" "visible"))

(defn new-board
  "Draws a new game board and sets the state atom to an empty board. Loads canvas
  drawing functions and listens for user clicks. Called on DOMContentLoaded."
  []
  (let [canvas (get-canvas "tictactoe-board")
        context (.getContext canvas "2d")
        width (.-width canvas)
        step (/ (- width 10) 3)
        column-coords (for [x (range (+ step 10) width step)
                         y [10 (- width 10)]]
                      (list x y))]

  (swap! board (fn [] empty-board))

  (defn draw-line 
    "Canvas line wrapper. Takes start and end coordinates and a map of style attributes."
    [start end style]
    (set! (.-lineWidth context) (style :width))
    (set! (.-strokeStyle context) (style :color))
    (.beginPath context)
    (.moveTo context (first start) (second start))
    (.lineTo context (first end) (second end))
    (.stroke context))

  (defn draw-grid
    "Draws the tic-tac-toe grid. Will resize the grid based on the size of the
    canvas element."
    [line-width]
    (doseq [points (concat (split-at 2 column-coords)
                           (split-at 2 (map reverse column-coords)))]
      (draw-line (first points) (second points) {:width 5 :color "#999"})))
  
  (defn row-coordinate 
    "Converts a row in the tic-tac-toe grid to a canvas coordinate."
    [row]
    (+ (* step row) 20))

  (defn col-coordinate
    "Converts a column in the tic-tac-toe grid to a canvas coordinate."
    [col]
    (+ 30 (* step col)))
  
  (defn canvas->grid 
    "Converts a canvas coordinate to a tic-tac-toe (row, col) grid square"
    [coord]
    (reverse (map (fn [x] (.floor js/Math (/ x step))) coord)))

  (defn grid->canvas
    "Converts a (row, col) grid square to a canvas coordinate"
    [coord]
    (reverse [(row-coordinate (first coord)) (col-coordinate (second coord))]))

  (defn draw-move 
    "Draws the given piece in (row, col) on the tic-tac-toe grid."
    [row col piece]
    (.drawImage context 
                (if (= piece "X") 
                  (.getElementById js/document "x-img")
                  (.getElementById js/document "o-img"))
                (col-coordinate col) 
                (row-coordinate row)
                (* 0.7 step)
                (* 0.9 step))) 

  (defn win-col-coord [col]
    (+ (col-coordinate col) (* 0.35 step)))

  (defn win-row-coord [row]
    (+ (row-coordinate row) (* 0.5 step)))

  (defn draw-win
    "Draws a line connecting a three-in-a-row on the board."
    [win]
    (let [style (if (= 1 (first win))
                  {:width 10 :color "#4768D6"}
                  {:width 10 :color "#5BA600"})]
      (cond (= :row (second win))
            (draw-line [10 (win-row-coord (last win))]
                       [(- width 10) (win-row-coord (last win))]
                       style)
            (= :col (second win))
            (draw-line [(win-col-coord (last win)) 10]
                       [(win-col-coord (last win)) (- width 10)]
                       style)
            (= :diag (second win))
            (if (= 0 (last win))
              (draw-line [10 10]
                         [(- width 10) (- width 10)]
                         style)
              (draw-line [(- width 10) 10]
                         [10 (- width 10)]
                         style)))))

  (defn update-board
    "Takes a 3x3 game board, as returned by one of the core game functions,
    and draws it to the canvas. Connects winning three-in-a-rows if found
    on the given board."
    [newboard]
    
    (defn update-iter [pieces row col]
      (when (not (empty? pieces))
        (let [piece (first pieces)]
          (when piece
            (draw-move row col (if (= 1 piece) "X" "O"))))
        (if (= 2 col) 
          (update-iter (rest pieces) (inc row) 0)
          (update-iter (rest pieces) row (inc col)))))

    (update-iter (flatten newboard) 0 0)
    
    (when (or (win? newboard) (draw? newboard))
      (show-reset)
      (when (win? newboard)
        (draw-win (get-win newboard))))
    (swap! board (fn [] newboard)))

  (defn add-click-listener 
    "Adds a click listener to the canvas element that calls the given function."
    [f]
    (.addEventListener canvas "click" f false))

  (defn canvas-coords 
    "Returns the coordinates of a click on the canvas."
    [e]
    (if (and (.-pageX e) (.-pageY e))
      [(- (.-pageX e)
          (.-offsetLeft canvas))
       (- (.-pageY e)
          (.-offsetTop canvas))]
      [(- (+ (.clientX e)
             (.-scrollLeft (.-body js/document))
              (.-scrollLeft (.-documentElement js/document)))
          (.-offsetLeft canvas))
       (- (+ (.clientY e)
             (.-scrollTop (.-body js/document))
             (.-scrollTop (.-documentElement js/document)))
          (.-offsetTop canvas))]))

  (defn process-move [move]
    (update-board (make-move move @board))
    (update-board (best-move @board)))

  (defn canvas-click 
    "Called when the user clicks on the canvas. If the square is unoccupied, updates
    the board and generates a new opponent move."
    [e]
    (let [grid-coords (canvas->grid (canvas-coords e))
          move [(first grid-coords) (second grid-coords) 1]
          board @board]
        (when (and (valid? move board) (not (win? board)))
          (draw-move (first move) (second move) "X")
          (process-move move))))

  (draw-grid 5)
  (add-click-listener canvas-click)
  (.addEventListener (.getElementById js/document "reset-link") 
                     "click" 
                     (fn [] (reset-canvas)
                            (new-board)))))

(.addEventListener js/window "load" new-board)
