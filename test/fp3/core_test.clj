(ns fp3.core-test
  (:require [clojure.test :refer :all]
            [fp3.core :as core]
            [fp3.interpolation :as interp]))

(deftest parse-point-test
  (testing "Valid point parsing"
    (is (= (core/parse-point "1.0 2.0") (interp/->Point 1.0 2.0)))
    (is (= (core/parse-point "1 2") (interp/->Point 1.0 2.0)))
    (is (= (core/parse-point "   1.5   3.7   ") (interp/->Point 1.5 3.7))))

  (testing "Invalid point parsing"
    (is (nil? (core/parse-point "")))
    (is (nil? (core/parse-point "1")))
    (is (nil? (core/parse-point "1 2 3")))
    (is (nil? (core/parse-point "a b")))
    (is (nil? (core/parse-point "1.0 abc")))))

(deftest validate-point-order-test
  (testing "Empty points list"
    (is (true? (core/validate-point-order [] (interp/->Point 1.0 1.0)))))

  (testing "Valid increasing order"
    (let [points [(interp/->Point 1.0 1.0) (interp/->Point 2.0 2.0)]]
      (is (true? (core/validate-point-order points (interp/->Point 3.0 3.0))))
      (is (true? (core/validate-point-order points (interp/->Point 2.1 2.1))))))

  (testing "Invalid non-increasing order"
    (let [points [(interp/->Point 1.0 1.0) (interp/->Point 2.0 2.0)]]
      (is (false? (core/validate-point-order points (interp/->Point 2.0 3.0))))
      (is (false? (core/validate-point-order points (interp/->Point 1.5 3.0)))))))

(deftest generate-x-values-test
  (testing "Normal generation"
    (is (= (vec (core/generate-x-values 0.0 5.0 1.0))
           [0.0 1.0 2.0 3.0 4.0 5.0]))
    (is (= (vec (core/generate-x-values 1.0 3.0 0.5))
           [1.0 1.5 2.0 2.5 3.0]))
    (is (= (vec (core/generate-x-values 0.0 2.5 0.5))
           [0.0 0.5 1.0 1.5 2.0 2.5])))

  (testing "Edge cases"
    (is (nil? (core/generate-x-values nil 5.0 1.0)))
    (is (nil? (core/generate-x-values 0.0 nil 1.0)))
    (is (nil? (core/generate-x-values 0.0 5.0 0.0)))
    (is (nil? (core/generate-x-values 0.0 5.0 -1.0)))
    (is (= (vec (core/generate-x-values 5.0 5.0 1.0))
           [5.0]))))

(deftest interpolate-for-algorithm-test
  (testing "Linear interpolation with enough points"
    (let [points [(interp/->Point 0.0 0.0) (interp/->Point 1.0 1.0)]
          config {:window 2 :method :linear}]
      (is (= (core/interpolate-for-algorithm :linear config points 0.5 nil)
             [[:linear 0.0 0.0] [:linear 0.5 0.5] [:linear 1.0 1.0]]))
      (is (= (core/interpolate-for-algorithm :linear config points 0.5 0.5)
             [[:linear 1.0 1.0]]))))

  (testing "Lagrange interpolation with enough points"
    (let [points [(interp/->Point 0.0 0.0) (interp/->Point 1.0 1.0)
                  (interp/->Point 2.0 4.0) (interp/->Point 3.0 9.0)]
          config {:window 4 :method :lagrange}]
      (is (seq (core/interpolate-for-algorithm :lagrange config points 1.0 nil)))
      (is (= (count (core/interpolate-for-algorithm :lagrange config points 1.0 nil)) 4))))

  (testing "Insufficient points"
    (let [points [(interp/->Point 0.0 0.0)]
          linear-config {:window 2 :method :linear}
          lagrange-config {:window 4 :method :lagrange}]
      (is (nil? (core/interpolate-for-algorithm :linear linear-config points 0.5 nil)))
      (is (nil? (core/interpolate-for-algorithm :lagrange lagrange-config points 0.5 nil)))))

  (testing "Error handling in interpolation - same x values"
    (let [points [(interp/->Point 1.0 1.0) (interp/->Point 1.0 2.0)]  ; Same x values
          config {:window 2 :method :linear}]
      ;; Linear interpolation returns y0 when x0 == x1 (according to your implementation)
      (let [result (core/interpolate-for-algorithm :linear config points 1.0 nil)]
        ;; It should return the point at x=1.0 with y=1.0
        (is (= result [[:linear 1.0 1.0]]))))))

(deftest process-points-test
  (testing "Process points with linear algorithm"
    (let [points [(interp/->Point 0.0 0.0) (interp/->Point 1.0 1.0)]
          selected-algos {:linear {:window 2 :method :linear}}
          max-window 2
          step 0.5
          last-x-printed nil]
      (is (= (core/process-points points selected-algos max-window step last-x-printed)
             [[:linear 0.0 0.0] [:linear 0.5 0.5] [:linear 1.0 1.0]]))))

  (testing "Process points with multiple algorithms"
    (let [points [(interp/->Point 0.0 0.0) (interp/->Point 1.0 1.0)
                  (interp/->Point 2.0 4.0) (interp/->Point 3.0 9.0)]
          selected-algos {:linear {:window 2 :method :linear}
                          :lagrange {:window 4 :method :lagrange}}
          max-window 4
          step 1.0
          last-x-printed nil]
      (let [results (core/process-points points selected-algos max-window step last-x-printed)]
        (is (seq results))
        (is (some #(= (first %) :linear) results))
        (is (some #(= (first %) :lagrange) results)))))

  (testing "Insufficient points"
    (let [points [(interp/->Point 0.0 0.0)]
          selected-algos {:linear {:window 2 :method :linear}}
          max-window 2
          step 0.5
          last-x-printed nil]
      (is (nil? (core/process-points points selected-algos max-window step last-x-printed))))))