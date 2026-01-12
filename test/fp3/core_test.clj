(ns fp3.core-test
  (:require [clojure.test :refer :all]
            [fp3.core :as core]
            [fp3.interpolation :as interp]))

(deftest parse-point-test
  (testing "Valid parses"
    (is (= (core/parse-point "1.0 2.0") (interp/->Point 1.0 2.0)))
    (is (= (core/parse-point "1 2")   (interp/->Point 1.0 2.0)))
    (is (= (core/parse-point "  3.5   4.25 ") (interp/->Point 3.5 4.25))))

  (testing "Behavior with single or extra tokens (matches current parse-point)"
    (is (= (core/parse-point "1") (interp/->Point 1.0 nil)))
    (is (= (core/parse-point "1 2 3") (interp/->Point 1.0 2.0))))

  (testing "Invalid parses -> nil"
    (is (nil? (core/parse-point "")))
    (is (nil? (core/parse-point "a b")))
    (is (nil? (core/parse-point "1 x")))))
