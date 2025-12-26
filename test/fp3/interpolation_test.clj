(ns fp3.interpolation-test
  (:require [clojure.test :refer :all]
            [fp3.interpolation :refer [->Point interpolate]]))

(deftest linear-interpolation-test
  (testing "Basic linear interpolation"
    (let [points [(->Point 0.0 0.0) (->Point 1.0 1.0)]]
      (is (= (interpolate :linear points 0.5) 0.5))
      (is (= (interpolate :linear points 0.0) 0.0))
      (is (= (interpolate :linear points 1.0) 1.0))
      (is (= (interpolate :linear points 0.25) 0.25))))

  (testing "Linear interpolation with same x values"
    (let [points [(->Point 1.0 2.0) (->Point 1.0 3.0)]]
      (is (= (interpolate :linear points 1.0) 2.0))))

  (testing "Linear interpolation with nil values"
    (let [points [(->Point nil 2.0) (->Point 1.0 3.0)]]
      (is (nil? (interpolate :linear points 0.5))))
    (let [points [(->Point 1.0 2.0) (->Point nil 3.0)]]
      (is (nil? (interpolate :linear points 0.5)))))

  (testing "Linear interpolation with negative values"
    (let [points [(->Point -1.0 -1.0) (->Point 1.0 1.0)]]
      (is (= (interpolate :linear points 0.0) 0.0))
      (is (= (interpolate :linear points -0.5) -0.5))
      (is (= (interpolate :linear points 0.5) 0.5)))))

(deftest lagrange-interpolation-test
  (testing "Lagrange interpolation with exact points"
    (let [points [(->Point 0.0 0.0) (->Point 1.0 1.0) (->Point 2.0 4.0)]]
      (is (= (interpolate :lagrange points 0.0) 0.0))
      (is (= (interpolate :lagrange points 1.0) 1.0))
      (is (= (interpolate :lagrange points 2.0) 4.0))))

  (testing "Lagrange interpolation at intermediate points"
    (let [points [(->Point 0.0 0.0) (->Point 1.0 1.0) (->Point 2.0 4.0)]]
      (is (= (interpolate :lagrange points 0.5) 0.25))
      (is (= (interpolate :lagrange points 1.5) 2.25))))

  (testing "Lagrange interpolation with same x values"
    (let [points [(->Point 0.0 0.0) (->Point 0.0 1.0) (->Point 1.0 2.0)]]
      (is (number? (interpolate :lagrange points 0.5)))))

  (testing "Lagrange interpolation with quadratic function"
    (let [points [(->Point 0.0 0.0) (->Point 1.0 1.0) (->Point 2.0 4.0) (->Point 3.0 9.0)]]
      (is (= (interpolate :lagrange points 1.5) 2.25))
      (is (= (interpolate :lagrange points 2.5) 6.25))
      (is (= (interpolate :lagrange points 3.0) 9.0))))

  (testing "Lagrange interpolation with negative values"
    (let [points [(->Point -1.0 1.0) (->Point 0.0 0.0) (->Point 1.0 1.0)]]
      (is (= (interpolate :lagrange points -0.5) 0.25))
      (is (= (interpolate :lagrange points 0.5) 0.25)))))