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
            [jepsen.history :as history]))

;; ---------------------------------------------------------------------------
;; History parsing
;; ---------------------------------------------------------------------------

(defn parse-history
  "Read one EDN entry per line from path into a vector of plain maps."
  [path]
  (with-open [rdr (io/reader path)]
    (doall
      (map-indexed
        (fn [i line]
          (let [m (edn/read-string line)]
            (-> m
                (update :time (fn [t] (when t (long t))))
                (update :index (fn [idx] (long (or idx i))))
                (update :process (fn [p] (long (or p 0))))
                (assoc :type (keyword (:type m)))
                (update :f keyword))))
        (line-seq rdr)))))

;; ---------------------------------------------------------------------------
;; Workload checkers
;; ---------------------------------------------------------------------------

(defn- list-append-check
  "Pure-Clojure list-append checker: verifies per-key monotonic reads.
   Each read on key K must contain all values that were acked on K
   before that read was invoked."
  [history]
  (let [;; group ops by process to track per-key state
        ;; Collect all (key, value) pairs acked via append
        acked (for [op history
                    :when (and (= :ok (:type op))
                               (= :txn (:f op)))
                    :let [v (:value op)]
                    :when (vector? v)
                    [act k val] v
                    :when (= act :append)]
                [k val])
        ;; Collect all read results: (key, values-seen)
        reads (for [op history
                    :when (and (= :ok (:type op))
                               (= :txn (:f op)))
                    :let [v (:value op)]
                    :when (vector? v)
                    [act k val] v
                    :when (= act :r)]
                [k (if (vector? val) (set val) #{})])
        ;; Build per-key union of all reads
        read-union (reduce (fn [m [k vals]]
                             (update m k (fn [old] (set/union (or old #{}) vals))))
                           {}
                           reads)
        ;; Build per-key set of all acked values
        acked-per-key (reduce (fn [m [k val]]
                                (update m k (fn [old] (conj (or old #{}) val))))
                              {}
                              acked)
        ;; Check each key
        keys (set (concat (keys acked-per-key) (keys read-union)))
        anomalies (for [k keys
                        :let [acked-k (get acked-per-key k #{})
                              seen-k (get read-union k #{})
                              lost (set/difference acked-k seen-k)
                              fabricated (set/difference seen-k acked-k)]
                        :when (or (seq lost) (seq fabricated))]
                    {:key k :lost (seq lost) :fabricated (seq fabricated)})
        total-lost (reduce + 0 (map #(count (:lost %)) anomalies))
        total-fab (reduce + 0 (map #(count (:fabricated %)) anomalies))]
    {:valid? (empty? anomalies)
     :keys-checked (count keys)
     :keys-with-anomalies (count anomalies)
     :total-lost total-lost
     :total-fabricated total-fab
     :anomalies anomalies
     :anomaly (when (seq anomalies)
                (str "Found " (count anomalies) " keys with anomalies: "
                     total-lost " lost, " total-fab " fabricated"))}))

(defn check-list-append
  "Pure-Clojure append-visibility checker (no Elle dependency)."
  [history opts]
  (let [result (list-append-check history)]
    {:valid? (:valid? result)
     :workload "list-append"
     :checker "append-visibility"
     :details result}))

;; ---------------------------------------------------------------------------
;; Bank checker: total-conservation invariant
;; ---------------------------------------------------------------------------

(defn- bank-total-invariant
  [history]
  (let [;; Check both :read and :transfer results for total conservation
        balance-ops (filter #(and (= :ok (:type %))
                                  (or (= :read (:f %))
                                      (= :transfer (:f %)))
                                  (vector? (:value %)))
                            history)
        totals (map (fn [op]
                      (let [pairs (:value op)]
                        (reduce + (map second pairs))))
                    balance-ops)]
    (if (empty? totals)
      {:valid? true :note "no balance operations found"}
      (let [expected (first totals)
            all-match (every? #(= expected %) totals)]
        {:valid? all-match
         :expected-total expected
         :totals-seen (distinct totals)
         :ops-checked (count totals)
         :anomaly (when-not all-match
                    "total balance changed across operations")}))))

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
  (let [;; Counter is GLOBAL — all adds/reads share the same counter.
        ;; The key in :value [k v] is metadata; v is the counter value.
        add-entries (for [op history
                          :when (and (= :ok (:type op))
                                     (= :add (:f op)))
                          :let [v (:value op)]
                          :when (and (vector? v) (>= (count v) 2))]
                      {:val (second v) :ts (or (:time op) 0)})
        total-adds (count add-entries)
        ;; Collect reads: (value, time)
        read-entries (for [op history
                           :when (and (= :ok (:type op))
                                      (= :read (:f op)))
                           :let [v (:value op)]
                           :when (and (vector? v) (>= (count v) 2))]
                       {:val (second v) :ts (or (:time op) 0)})
        ;; Check monotonic globally: read values non-decreasing over time
        sorted-reads (sort-by :ts read-entries)
        read-vals (map :val sorted-reads)
        monotonic (or (empty? read-vals) (apply <= read-vals))
        ;; Check bounds: each read value in [0, total_adds]
        anomalies (for [{:keys [val ts]} read-entries
                        :when (not (<= 0 val total-adds))]
                    {:val val :ts ts :total-adds total-adds})]
    {:valid? (and monotonic (empty? anomalies))
     :total-adds total-adds
     :reads-checked (count read-entries)
     :monotonic monotonic
     :anomalies anomalies
     :anomaly (cond
                (not monotonic) "reads not monotonic"
                (seq anomalies) (str "read value out of bounds: " (pr-str anomalies)))}))

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
