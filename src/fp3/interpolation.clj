(ns fp3.interpolation)

(defrecord Point [x y])

(def ^:private eps 1e-9)

(defn- almost= [a b]
  (and (number? a) (number? b) (< (Math/abs (- a b)) eps)))

(defmulti interpolate (fn [context] (:algorithm context)))

(defmethod interpolate :linear
  [{:keys [p1 p2 x]}]
  (let [x1 (:x p1) y1 (:y p1)
        x2 (:x p2) y2 (:y p2)]
    (cond
      (almost= x x1) y1
      (almost= x x2) y2
      (almost= x1 x2) (throw (Exception. "Duplicate x"))
      :else (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1)))))))

(defmethod interpolate :lagrange
  [{:keys [points x]}]
  (let [n (count points)]
    (when (>= n 2)
      (reduce
       (fn [acc i]
         (let [xi (:x (nth points i))
               yi (:y (nth points i))]
           (+ acc
              (* yi
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
       (range n)))))
