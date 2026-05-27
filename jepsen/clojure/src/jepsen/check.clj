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
            [clojure.set :as set]
            [elle.list-append :as elle-la]
            [jepsen.checker :as checker]
            [jepsen.history :as history]))

;; ---------------------------------------------------------------------------
;; History parsing
;; ---------------------------------------------------------------------------

(defn parse-history
  "Read one EDN entry per line from path into a vector of Jepsen history maps."
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
  "Run Elle's list-append checker with snapshot-isolation consistency model."
  [history opts]
  (let [checker (elle-la/checker
                  {:consistency-models [:snapshot-isolation]})]
    (checker/check checker {:test {:clock :real}} history nil)))

;; ---------------------------------------------------------------------------
;; Bank checker: total-conservation invariant
;; ---------------------------------------------------------------------------

(defn- bank-total-invariant
  [history]
  (let [reads (filter #(and (= :ok (:type %))
                            (= :read (:f %))
                            (vector? (:value %)))
                      history)
        totals (map (fn [op]
                      (let [pairs (:value op)]
                        (reduce + (map second pairs))))
                    reads)]
    (if (empty? totals)
      {:valid? true :note "no read operations found"}
      (let [expected (first totals)
            all-match (every? #(= expected %) totals)]
        {:valid? all-match
         :expected-total expected
         :totals-seen (distinct totals)
         :reads-checked (count totals)
         :anomaly (when-not all-match
                    "total balance changed across reads")}))))

(defn check-bank
  [history opts]
  (let [result (bank-total-invariant history)]
    {:valid? (:valid? result)
     :workload "bank"
     :checker "total-conservation"
     :details result}))

;; ---------------------------------------------------------------------------
;; Set checker: acked elements present, no fabricated elements
;; ---------------------------------------------------------------------------

(defn- set-durability-check
  [history]
  (let [acked (set (for [op history
                         :when (and (= :ok (:type op))
                                    (= :add (:f op)))]
                     (:value op)))
        final-reads (filter #(and (= :ok (:type %))
                                  (= :read (:f %))
                                  (vector? (:value %)))
                            history)
        final-set (if (seq final-reads)
                    (set (:value (last final-reads)))
                    #{})
        lost (set/difference acked final-set)
        fabricated (set/difference final-set acked)]
    {:valid? (and (empty? lost) (empty? fabricated))
     :acked-count (count acked)
     :final-count (count final-set)
     :lost (seq lost)
     :fabricated (seq fabricated)
     :anomaly (cond
                (and (seq lost) (seq fabricated))
                (str "Lost " (count lost) " elements, fabricated "
                     (count fabricated))
                (seq lost)
                (str "Lost " (count lost) " acked elements")
                (seq fabricated)
                (str "Fabricated " (count fabricated) " elements"))}))

(defn check-set
  [history opts]
  (let [result (set-durability-check history)]
    {:valid? (:valid? result)
     :workload "set"
     :checker "durability"
     :details result}))

;; ---------------------------------------------------------------------------
;; Counter checker: monotonic reads, final value within bounds
;; ---------------------------------------------------------------------------

(defn- counter-check
  [history]
  (let [adds (filter #(and (= :ok (:type %)) (= :add (:f %))) history)
        reads (filter #(and (= :ok (:type %)) (= :read (:f %))) history)
        acked (count adds)
        read-vals (keep (fn [op]
                          (let [v (:value op)]
                            (when (and (vector? v) (= 2 (count v)))
                              (second v))))
                        reads)
        read-vals (remove nil? read-vals)
        monotonic (or (empty? read-vals)
                      (apply <= read-vals))
        final-val (last read-vals)
        in-bounds (if final-val
                    (<= 0 final-val acked)
                    true)]
    {:valid? (and monotonic in-bounds)
     :acked acked
     :reads-checked (count read-vals)
     :monotonic monotonic
     :final-value final-val
     :in-bounds in-bounds
     :anomaly (cond
                (not monotonic) "reads not monotonic"
                (not in-bounds) (str "final value " final-val
                                     " not in [0, " acked "]"))}))

(defn check-counter
  [history opts]
  (let [result (counter-check history)]
    {:valid? (:valid? result)
     :workload "counter"
     :checker "monotonic-bounds"
     :details result}))

;; ---------------------------------------------------------------------------
;; Checker registry
;; ---------------------------------------------------------------------------

(def checkers
  {:list-append check-list-append
   :bank        check-bank
   :set         check-set
   :counter     check-counter})

;; ---------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------

(defn report-result
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
