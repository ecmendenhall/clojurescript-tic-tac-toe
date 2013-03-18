(ns tictactoe.canvas
  (:require [tictactoe.core :refer [make-move empty-board best-move]]))

(defn get-canvas [id]
    (.getElementById js/document id))

(defn new-board []
  (def canvas  (get-canvas "tictactoe-board"))
  (def context (.getContext canvas "2d"))
  (def width   (.-width canvas))
  (def step    (/ (- width 10) 3))
  (def column-coords (for [x (range (+ step 10) width step)
                           y [10 (- width 10)]]
                      (list x y)))
  (def board (atom empty-board))
  
  (defn draw-line [start end]
    (.moveTo context (first start) (second start))
    (.lineTo context (first end) (second end)))

  (defn line-style [style]
    (set! (.-lineWidth context) (style :width))
    (set! (.-strokeStyle context) (style :color)))

  (defn draw-grid [line-width]
    (doseq [points (concat (split-at 2 column-coords)
                           (split-at 2 (map reverse column-coords)))]
      (draw-line (first points) (second points)))
    
    (line-style {:width 5 :color "#000"})
    (.stroke context))
  
  (defn row-coordinate [row]
    (* step (+ 1 row)))

  (defn col-coordinate [col]
    (+ 30 (* step col)))

  (defn draw-move [row col piece]
    (set! (.-font context) (str (+ 20 step) "px Menlo"))
    (set! (.-fillStyle context) "black")
    (.fillText context piece (col-coordinate col) (row-coordinate row)))

  (defn clear [row col]
    (let [c (col-coordinate col)
          r (row-coordinate row)]
      (.beginPath context)
      (.rect context c (+ 20 (- r step)) (- step 30) (- step 15))
      (set! (.-fillStyle context) "white")
      (.fill context)))

  (defn update-board [newboard]
    (swap! board (fn [] newboard))
    (.log js/console (clj->js @board))
    (defn update-iter [pieces row col]
      (when (not (empty? pieces))
        (clear row col)
        (let [piece (first pieces)]
          (when piece
            (draw-move row col (if (= 1 piece) "X" "O"))))
        (if (= 2 col) 
          (update-iter (rest pieces) (inc row) 0)
          (update-iter (rest pieces) row (inc col)))))
    (update-iter (flatten newboard) 0 0))

  (defn add-click-listener [f]
    (.addEventListener canvas "click" f false))

  (defn canvas-coords [e]
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

  (defn canvas->grid [coord]
    (reverse (map (fn [x] (.floor js/Math (/ x step))) coord)))

  (defn canvas-click [e]
    (let [grid-coords (canvas->grid (canvas-coords e))]
      (.log js/console (clj->js grid-coords))
      (.log js/console (clj->js @board))
      (update-board (make-move [(first grid-coords) (second grid-coords) 1]
                               @board))
      (update-board (best-move @board))))

  (draw-grid 5)
  (add-click-listener canvas-click))

(.addEventListener js/window "DOMContentLoaded" new-board)
