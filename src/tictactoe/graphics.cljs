(ns tictactoe.graphics)

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
    (.fillText context piece (col-coordinate col) (row-coordinate row)))

  (draw-grid 5)
  
  (draw-move 0 0 "X")
  (draw-move 0 1 "O")
  (draw-move 0 2 "X")
  (draw-move 1 0 "O")
  (draw-move 1 1 "X")
  (draw-move 1 2 "X")
  (draw-move 2 0 "O")
  (draw-move 2 1 "X")
  (draw-move 2 2 "O"))

(.addEventListener js/window "DOMContentLoaded" new-board)
