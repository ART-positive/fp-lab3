(ns fp3.core
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [fp3.interpolation :as interp])
  (:gen-class))

(def default-algorithms
  {:linear   {:window 2 :method :linear}
   :lagrange {:window 4 :method :lagrange}})

(def cli-options
  [["-l" "--linear" "Use linear interpolation"]
   ["-g" "--lagrange" "Use Lagrange interpolation"]
   ["-s" "--step STEP" "Step size"
    :id :step
    :default 1.0
    :parse-fn #(Double/parseDouble %)
    :validate [#(pos? %) "Step must be > 0"]]
   ["-wg" "--w-lagrange SIZE" "Window size for Lagrange interpolation (integer >= 2)"
    :id :w-lagrange
    :parse-fn #(Integer/parseInt %)
    :validate [#(>= % 2) "Lagrange window size must be at least 2"]]
   ["-h" "--help" "Show help"]])

(defn usage [summary]
  (str "Usage: fp3 [options]\n\nOptions:\n" summary))

(defn error-msg [errors]
  (str "Errors:\n" (str/join \newline errors)))

(defn validate-args [args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) {:exit (usage summary)}
      errors {:exit (error-msg errors)}
      (and (not (:linear options)) (not (:lagrange options)))
      {:exit "Specify at least one algorithm"}
      :else {:options options})))

(defn parse-point [line]
  (try
    (let [[x y] (map #(Double/parseDouble %)
                     (str/split (str/trim line) #"\s+"))]
      (interp/->Point x y))
    (catch Exception _ nil)))

(defn ks-for-window [origin step x-min x-max]
  (let [k-min (long (Math/ceil (/ (- x-min origin) step)))
        k-max (long (Math/floor (/ (- x-max origin) step)))]
    (range k-min (inc k-max))))

(defn produce-for-algo [algo config points step origin last-k]
  (when (and origin (>= (count points) (:window config)))
    (let [window (take-last (:window config) points)
          x-min (:x (first window))
          x-max (:x (last window))]
      (->> (ks-for-window origin step x-min x-max)
           (filter #(or (nil? last-k) (> % last-k)))
           (keep (fn [k]
                   (let [x (+ origin (* k step))]
                     (try
                       (let [result (case (:method config)
                                      :linear (let [[p0 p1] (take-last 2 window)]
                                                (interp/interpolate {:algorithm :linear
                                                                     :p1 p0
                                                                     :p2 p1
                                                                     :x x}))
                                      :lagrange (interp/interpolate {:algorithm :lagrange
                                                                     :points window
                                                                     :x x}))]
                         (when result
                           [algo x result k]))
                       (catch Exception _ nil)))))
           vec))))

(defn -main [& args]
  (let [result (validate-args args)]
    (if (:exit result)
      (do (println (:exit result)) (System/exit 0))
      (let [{:keys [step w-lagrange]} (:options result)
            selected-order (->> [:linear :lagrange]
                                (filter #(get (:options result) %))
                                vec)
            base-selected (select-keys default-algorithms selected-order)
            effective (if (and (contains? base-selected :lagrange) w-lagrange)
                        (assoc-in base-selected [:lagrange :window] w-lagrange)
                        base-selected)
            max-window (if (seq effective) (apply max (map :window (vals effective))) 0)]

        (println "Ready. Enter points (x y). Empty line to exit.")

        (loop [points []
               origin nil
               last-ks (zipmap (keys effective) (repeat nil))]

          (print "> ") (flush)

          (let [line (read-line)]
            (cond
              (nil? line)
              (do (println "EOF. Exiting.") (System/exit 0))

              (str/blank? (str/trim line))
              (do (println "Exiting...") (System/exit 0))

              :else
              (let [trimmed (str/trim line)]
                (if-let [pt (parse-point trimmed)]
                  (if (and (seq points)
                           (<= (:x pt) (:x (last points))))
                    (do
                      (println "x must increase")
                      (recur points origin last-ks))

                    (let [origin' (or origin (:x pt))
                          new-points (conj points pt)
                          points' (->> new-points
                                       (take-last max-window)
                                       vec)
                          produced (into {}
                                         (for [[algo cfg] effective]
                                           [algo (produce-for-algo
                                                  algo cfg points'
                                                  step origin'
                                                  (get last-ks algo))]))
                          new-last-ks
                          (reduce-kv
                           (fn [m algo pts]
                             (if (seq pts)
                               (assoc m algo (last (map last pts)))
                               m))
                           last-ks
                           produced)]

                      (doseq [[algo pts] produced
                              [_ x y _] pts]
                        (printf "%s: %.3f %.3f\n" (name algo) x y))
                      (flush)

                      (recur points' origin' new-last-ks)))

                  (do
                    (println "Invalid format")
                    (recur points origin last-ks)))))))))))
