(ns fp3.interpolation-test
  (:require [clojure.test :refer :all]
            [fp3.interpolation :refer :all :as interp]))

(deftest linear-interpolation-basic
  (let [points [(->Point 0.0 0.0) (->Point 1.0 1.0)]]
    (is (= (interpolate :linear points 0.0) 0.0))
    (is (= (interpolate :linear points 1.0) 1.0))
    (is (= (interpolate :linear points 0.5) 0.5))
    (is (= (interpolate :linear points 0.25) 0.25))))

(deftest linear-degenerate
  (testing "If x equals first node, linear returns y0 even when nodes share same x"
    (let [points [(->Point 1.0 2.0) (->Point 1.0 3.0)]]
      (is (= (interpolate :linear points 1.0) 2.0)))))

(deftest lagrange-interpolation-basic
  (testing "Exact nodes"
    (let [points [(->Point 0.0 0.0) (->Point 1.0 1.0) (->Point 2.0 4.0)]]
      (is (= (interpolate :lagrange points 0.0) 0.0))
      (is (= (interpolate :lagrange points 1.0) 1.0))
      (is (= (interpolate :lagrange points 2.0) 4.0))))

  (testing "Intermediate points for quadratic"
    (let [points [(->Point 0.0 0.0) (->Point 1.0 1.0) (->Point 2.0 4.0)]]
      (is (= (interpolate :lagrange points 0.5) 0.25))
      (is (= (interpolate :lagrange points 1.5) 2.25)))))
