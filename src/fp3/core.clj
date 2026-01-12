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
    :parse-fn #(Double/parseDouble %)
    :validate [#(pos? %) "Step must be > 0"]]
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
    (let [[x y] (map #(Double/parseDouble %) (str/split (str/trim line) #"\s+"))]
      (interp/->Point x y))
    (catch Exception _ nil)))

(defn ks-for-window [origin step x-min x-max]
  (let [eps 1e-9
        k-min (long (Math/ceil (/ (- x-min origin) step)))
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
                       (when-let [y (interp/interpolate (:method config) window x)]
                         [algo x y k])
                       (catch Exception _ nil)))))
           vec))))

(defn -main [& args]
  (let [result (validate-args args)]
    (if (:exit result)
      (do (println (:exit result)) (System/exit 0))
      (let [{:keys [step]} (:options result)
            selected (->> [:linear :lagrange]
                          (filter #(get (:options result) %))
                          (select-keys algorithms))
            max-window (apply max (map :window (vals selected)))]
        (println "Ready. Enter points (x y). Empty line to exit.")

        (loop [points [] origin nil last-ks (zipmap (keys selected) (repeat nil))]
          (print "> ") (flush)
          (if-let [line (read-line)]
            (let [trimmed (str/trim line)]
              (cond
                (str/blank? trimmed)
                (println "Exiting...")

                :else
                (if-let [pt (parse-point trimmed)]
                  (if (and (seq points) (<= (:x pt) (:x (last points))))
                    (do (println "x must increase") (recur points origin last-ks))
                    (let [origin' (or origin (:x pt))
                          new-points (conj points pt)
                          points' (if (> (count new-points) max-window)
                                    (vec (take-last max-window new-points))
                                    (vec new-points))

                          produced (into {}
                                         (for [[algo config] selected]
                                           [algo (produce-for-algo algo config points' step origin' (last-ks algo))]))

                          new-last-ks (reduce-kv
                                       (fn [m algo pts]
                                         (if-let [last-k (last (map last pts))]
                                           (assoc m algo last-k)
                                           m))
                                       last-ks
                                       produced)]

                      (doseq [[algo _] selected
                              [_ x y] (produced algo)]
                        (printf "%s: %.3f %.3f\n" (name algo) x y))
                      (flush)
                      (recur points' origin' new-last-ks)))

                  (do (println "Invalid format") (recur points origin last-ks)))))
            (println "EOF")))))))
