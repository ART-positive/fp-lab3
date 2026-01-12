(ns fp3.interpolation)

(defrecord Point [x y])

(def ^:private eps 1e-9)

(defn- almost= [a b] (< (Math/abs (- a b)) eps))

(defmulti interpolate (fn [method points x] method))

(defmethod interpolate :linear
  [_ points x]
  (let [[p0 p1] (take-last 2 points)]
    (when (and p0 p1)
      (let [x0 (:x p0) y0 (:y p0)
            x1 (:x p1) y1 (:y p1)]
        (cond
          (almost= x x0) y0
          (almost= x x1) y1
          (almost= x0 x1) (throw (Exception. "Duplicate x"))
          :else (+ y0 (* (- y1 y0) (/ (- x x0) (- x1 x0)))))))))

(defmethod interpolate :lagrange
  [_ points x]
  (let [n (count points)]
    (reduce
     (fn [acc i]
       (let [xi (:x (nth points i))
             yi (:y (nth points i))]
         (+ acc (* yi
                   (reduce
                    (fn [term j]
                      (let [xj (:x (nth points j))]
                        (if (= i j)
                          term
                          (if (almost= xi xj)
                            (throw (Exception. "Duplicate x in Lagrange"))
                            (* term (/ (- x xj) (- xi xj)))))))
                    1.0
                    (range n))))))
     0.0
     (range n))))
