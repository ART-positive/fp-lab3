(ns fp3.interpolation-test
  (:require [clojure.test :refer :all]
            [fp3.interpolation :as interp :refer [->Point]]))

(deftest linear-interpolation-basic
  (let [p0 (->Point 0.0 0.0)
        p1 (->Point 1.0 1.0)]
    (is (= 0.0 (interp/interpolate {:algorithm :linear :p1 p0 :p2 p1 :x 0.0})))
    (is (= 1.0 (interp/interpolate {:algorithm :linear :p1 p0 :p2 p1 :x 1.0})))
    (is (= 0.5 (interp/interpolate {:algorithm :linear :p1 p0 :p2 p1 :x 0.5})))
    (is (= 0.25 (interp/interpolate {:algorithm :linear :p1 p0 :p2 p1 :x 0.25})))))

(deftest linear-degenerate
  (testing "If nodes share the same x-coordinate"
    (let [p0 (->Point 1.0 2.0)
          p1 (->Point 1.0 3.0)]
      (is (= 2.0 (interp/interpolate {:algorithm :linear :p1 p0 :p2 p1 :x 1.0})))
      (is (thrown? Exception
                   (interp/interpolate {:algorithm :linear :p1 p0 :p2 p1 :x 1.5}))))))

(deftest lagrange-interpolation-basic
  (testing "Exact nodes and intermediate points (quadratic)"
    (let [pts [(->Point 0.0 0.0) (->Point 1.0 1.0) (->Point 2.0 4.0)]]
      (is (= 0.0 (interp/interpolate {:algorithm :lagrange :points pts :x 0.0})))
      (is (= 1.0 (interp/interpolate {:algorithm :lagrange :points pts :x 1.0})))
      (is (= 4.0 (interp/interpolate {:algorithm :lagrange :points pts :x 2.0})))
      (is (= 0.25 (interp/interpolate {:algorithm :lagrange :points pts :x 0.5})))
      (is (= 2.25 (interp/interpolate {:algorithm :lagrange :points pts :x 1.5}))))))

(deftest lagrange-duplicate-x
  (testing "Duplicate x in nodes causes exception"
    (let [pts [(->Point 0.0 0.0) (->Point 0.0 1.0) (->Point 1.0 2.0)]]
      (is (thrown? Exception
                   (interp/interpolate {:algorithm :lagrange :points pts :x 0.5}))))))
