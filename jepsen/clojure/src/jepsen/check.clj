(ns jepsen.check
  "Offline checker runner for sqlocaml Jepsen histories.

   Reads an EDN-format history file produced by the OCaml harness
   and dispatches it to the appropriate Jepsen/Elle checker.

   Usage:
     clojure -M -m jepsen.check <history.edn> [options]

   Options:
     --workload list-append (default)
     --consistency snapshot-isolation (default)
     --output <path>       Write analysis results to file (default: stdout)"

  (:require [clojure.tools.cli :as cli]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [elle.list-append :as elle-la]
            [jepsen.checker :as checker]
            [jepsen.history :as history]))

;; ---------------------------------------------------------------------------
;; History parsing
;; ---------------------------------------------------------------------------

(defn parse-history
  "Read one EDN entry per line from `path` into a vector of Jepsen history maps.

   The OCaml harness emits each operation as a single-line EDN map:
     {:type :invoke/:ok/:fail/:info, :f <string>, :value <...>,
      :process <int>, :index <int>, :time <int64-ns>}

   Elle expects :time in nanoseconds (Long), :index as Long, :process as Long."
  [path]
  (with-open [rdr (io/reader path)]
    (doall
      (map-indexed
        (fn [i line]
          (let [m (edn/read-string line)]
            (-> m
                (update :time (fn [t] (when t (long t))))
                (update :index (fn [i] (long i)))
                (update :process (fn [p] (long p)))
                (assoc :type (keyword (:type m)))
                (update :f keyword))))
        (line-seq rdr)))))

;; ---------------------------------------------------------------------------
;; Workload checkers
;; ---------------------------------------------------------------------------

(defn check-list-append
  "Run Elle's list-append checker with snapshot-isolation consistency model.
   Returns a map of analysis results."
  [history opts]
  (let [checker (elle-la/checker
                  {:consistency-models [:snapshot-isolation]})}
    (checker/check checker {:test {:clock :real}} history nil)))

(def checkers
  "Map of workload name -> checker function"
  {:list-append check-list-append})

;; ---------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------

(defn report-result
  "Pretty-print the checker result."
  [result output-file]
  (let [out (if output-file (io/writer output-file) *out*)]
    (binding [*out* out]
      (println "=== Jepsen Checker Result ===")
      (println)
      (if (:valid? result)
        (println "RESULT: VALID — no anomalies detected")
        (println "RESULT: INVALID — anomalies found"))
      (println)
      (println "--- Full analysis ---")
      (pp/pprint result)
      (println))
    (when output-file
      (.close out)
      (println "Wrote analysis to" output-file))))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(def cli-options
  [["-w" "--workload NAME" "Checker workload (list-append)"
    :default :list-append
    :parse-fn keyword]
   ["-o" "--output PATH"   "Output file for analysis results"]
   ["-h" "--help"          "Show help"]])

(defn usage [summary]
  (println "sqlocaml Jepsen checker — offline history analysis")
  (println)
  (println "Usage: clojure -M -m jepsen.check <history.edn> [options]")
  (println)
  (println summary)
  (println)
  (println "Workloads: list-append (default) | bank | set | counter"))

(defn -main [& args]
  (let [{:keys [options arguments summary errors]}
        (cli/parse-opts args cli-options)]
    (when errors
      (println "Errors:" errors)
      (usage summary)
      (System/exit 1))
    (when (:help options)
      (usage summary)
      (System/exit 0))
    (when (empty? arguments)
      (println "Missing: history.edn path")
      (usage summary)
      (System/exit 1))
    (let [history-path (first arguments)
          _ (println "Reading history from" history-path)
          history (parse-history history-path)
          _ (println (str "Parsed " (count history) " history entries"))
          workload (:workload options)
          checker-fn (get checkers workload)]
      (if (nil? checker-fn)
        (do (println "Unknown workload:" workload)
            (usage summary)
            (System/exit 1))
        (let [result (checker-fn history options)]
          (report-result result (:output options))
          (if (:valid? result)
            (System/exit 0)
            (System/exit 1)))))))
