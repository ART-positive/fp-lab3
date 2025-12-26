(ns fp3.interpolation)

(defrecord Point [x y])

(defmulti interpolate
  "Multimethod for different interpolation algorithms"
  (fn [method points x] method))

(defmethod interpolate :linear
  [_ points x]
  (let [[p0 p1] (take-last 2 points)
        x0 (:x p0) y0 (:y p0)
        x1 (:x p1) y1 (:y p1)]
    (if (or (nil? x0) (nil? x1))
      nil
      (if (= x0 x1)
        y0
        (+ y0 (* (- y1 y0) (/ (- x x0) (- x1 x0))))))))

(defmethod interpolate :lagrange
  [_ points x]
  (let [n (count points)]
    (loop [i 0
           result 0.0]
      (if (< i n)
        (let [xi (:x (nth points i))
              yi (:y (nth points i))
              li (loop [j 0
                        term 1.0]
                   (if (< j n)
                     (if (= i j)
                       (recur (inc j) term)
                       (let [xj (:x (nth points j))]
                         (if (= xi xj)
                           (recur (inc j) term)
                           (recur (inc j) (* term (/ (- x xj) (- xi xj)))))))
                     term))]
          (recur (inc i) (+ result (* yi li))))
        result))))