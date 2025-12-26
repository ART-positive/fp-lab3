(ns fp3.core
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [fp3.interpolation :as interp])
  (:gen-class))

(def algorithms
  {:linear   {:window 2 :method :linear}
   :lagrange {:window 4 :method :lagrange}})

(def cli-options
  [["-l" "--linear" "Use linear interpolation"]
   ["-g" "--lagrange" "Use Lagrange interpolation"]
   ["-s" "--step STEP" "Step size"
    :id :step
    :default 1.0
    :validate [#(pos? %) "Step must be > 0"]
    :parse-fn #(Double/parseDouble %)]
   ["-h" "--help" "Show help"]])

(defn- usage [summary]
  (str "Usage: fp3 [options]\n\nOptions:\n" summary))

(defn- error-msg [errors]
  (str "The following errors occurred while parsing your command:\n"
       (str/join \newline errors)))

(defn- validate-args
  [args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) {:exit-message (usage summary) :ok? true}
      errors {:exit-message (error-msg errors) :ok? false}
      (and (not (:linear options))
           (not (:lagrange options)))
      {:exit-message "Please specify at least one interpolation method\n" :ok? false}
      :else {:options options})))

(defn parse-point
  "Parse point from string in format 'x y'"
  [line]
  (let [tokens (-> line str/trim (str/split #"\s+"))]
    (when (= (count tokens) 2)
      (try
        (let [x (Double/parseDouble (first tokens))
              y (Double/parseDouble (second tokens))]
          (interp/->Point x y))
        (catch NumberFormatException _ nil)))))

(defn validate-point-order
  "Check that new point's x is greater than previous (strictly increasing)"
  [points new-point]
  (if (empty? points)
    true
    (let [last-x (:x (last points))]
      (> (:x new-point) last-x))))

(defn generate-x-values
  "Generate x values from x-start to x-end with given step"
  [x-start x-end step]
  (when (and x-start x-end step (> step 0))
    (take-while #(<= % x-end)
                (iterate #(+ % step) x-start))))

(defn interpolate-for-algorithm
  [algo-name {:keys [window method]} points step last-x-printed]
  (when (>= (count points) window)
    (let [window-points (vec (take-last window points))
          x-start (:x (first window-points))
          x-end   (:x (last window-points))]
      (when (and (number? x-start) (number? x-end) (pos? step))
        (->> (generate-x-values x-start x-end step)
             (filter #(or (nil? last-x-printed) (> % last-x-printed)))
             (map (fn [x]
                    (try
                      (let [y (interp/interpolate method window-points x)]
                        (when (and (some? y) (number? y))
                          [algo-name x y]))
                      (catch Exception _
                        nil))))
             (filter some?)
             (seq))))))

(defn process-points
  "Process accumulated points and return results"
  [points selected-algos max-window step last-x-printed]
  (when (>= (count points) 2)
    (let [window (take-last max-window points)]
      (->> selected-algos
           (mapcat (fn [[algo-name config]]
                     (interpolate-for-algorithm
                      algo-name config window step last-x-printed)))
           (seq)))))

(defn -main
  [& args]
  (let [{:keys [options exit-message ok?]} (validate-args args)]
    (cond
      exit-message
      (do (println exit-message)
          (System/exit (if ok? 0 1)))

      :else
      (let [step (:step options)
            selected-algos (->> [:linear :lagrange]
                                (filter #(get options %))
                                (select-keys algorithms))
            max-window (->> selected-algos vals (map :window) (apply max 0))]
        (when (empty? selected-algos)
          (println "No algorithms selected")
          (System/exit 1))

        (println "Ready. Enter points line-by-line in format: x y (blank line to exit).")
        (println "Points must be in strictly increasing order by x.")

        ;; Основной цикл с простым состоянием
        (loop [points []
               last-x-printed nil]
          (print "> ")
          (flush)
          (if-let [line (read-line)]
            (let [trimmed-line (str/trim line)]
              (cond
                (str/blank? trimmed-line)
                (println "Exiting...")

                :else
                (if-let [point (parse-point trimmed-line)]
                  (if (validate-point-order points point)
                    (let [new-points (conj points point)
                          results (process-points new-points selected-algos max-window step last-x-printed)
                          new-last-x (when (seq results)
                                       (-> results last second))]
                      (when results
                        (doseq [[algo-name x y] results]
                          (printf "%s: %.3f %.3f\n" (name algo-name) x y))
                        (flush))
                      (recur new-points (or new-last-x last-x-printed)))
                    (do
                      (println "Error: Points must be in strictly increasing order by x.")
                      (recur points last-x-printed)))
                  (do
                    (println "Invalid format. Please enter: x y")
                    (recur points last-x-printed)))))
            (println "Exiting...")))))))