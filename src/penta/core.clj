(ns penta.core
  (:require [hiccup.core :as up]))

(def tau (* 2 Math/PI))

(def organism-colors
  [:yellow :red :blue :brown :green :purple :gray])

(defn circle
  [[x y radius color]]
  [:circle {:cx x :cy y :r radius :fill color}])

(defn circle-index
  [[x-axis y-axis] [x-offset y-offset] radius color index]
  (let [x (+ x-offset (* x-axis index radius 2))
        y (+ y-offset (* y-axis index radius 2))]
    (circle [x y radius color])))

(defn axis
  [symmetry index]
  (let [ratio (/ index symmetry)
        unit (* ratio tau)
        x-axis (Math/cos unit)
        y-axis (Math/sin unit)]
    [x-axis y-axis]))

(defn beam
  [radius colors axis next]
  (let [rings (count colors)
        offset (* rings radius 2)]
    (map
     (partial circle-index axis [offset offset] radius)
     colors
     (range (count colors)))))

(defn beams
  [symmetry radius colors]
  (let [axes (map (partial axis symmetry) (range symmetry))
        next-axes (drop 1 (cycle axes))]
    (mapcat
     (partial beam radius colors)
     axes
     next-axes)))

(defn layout
  [symmetry radius colors]
  (let [field (* 4 radius (count colors))]
    [:svg {:width field :height field}
     [:g (beams symmetry radius colors)]]))

(defn render
  [symmetry radius colors path]
  (let [radiate (layout symmetry radius colors)
        out (up/html radiate)]
    (spit path out)))

