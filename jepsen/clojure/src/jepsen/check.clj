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
  "Pure-Clojure list-append checker with temporal ordering.
   For each key:
   1. No dirty reads: a read's values must all be acked at or before that read.
   2. No lost updates: the final read for each key must contain all acked values
      (union covers all committed appends).
   Catches dirty_read and lost_update negative controls."
  [history]
  (let [;; Per-key acked appends with timestamps
        acked (for [op history
                    :when (and (= :ok (:type op))
                               (= :txn (:f op)))
                    :let [v (:value op)
                          ts (or (:time op) 0)]
                    :when (vector? v)
                    [act k val] v
                    :when (= act :append)]
                {:k k :val val :ts ts})
        ;; Per-key reads with timestamps
        reads (for [op history
                    :when (and (= :ok (:type op))
                               (= :txn (:f op)))
                    :let [v (:value op)
                          ts (or (:time op) 0)]
                    :when (vector? v)
                    [act k val] v
                    :when (= act :r)]
                {:k k :vals (if (vector? val) (set val) #{}) :ts ts})
        ;; All keys referenced
        keys (set (concat (map :k acked) (map :k reads)))
        ;; Per-key sorted acks
        acked-by-key (reduce (fn [m {:keys [k val ts]}]
                               (update m k (fn [vs] (conj (or vs []) {:val val :ts ts}))))
                             {} acked)
        ;; Build per-key union of all reads (for lost-update check)
        read-union (reduce (fn [m {:keys [k vals]}]
                             (update m k set/union (or (get m k) #{}) vals))
                           {} reads)
        ;; Build per-key set of all acked values
        acked-set (reduce (fn [m {:keys [k val]}]
                             (update m k (fn [s] (conj (or s #{}) val))))
                           {} acked)
        ;; Check each key
        anomalies (mapcat
                    (fn [k]
                      (let [reads-k (sort-by :ts (filter #(= k (:k %)) reads))
                            acks-k (sort-by :ts (get acked-by-key k []))
                            last-read-vals (if (seq reads-k) (:vals (last reads-k)) #{})
                            ;; Values acked at or before time T
                            acked-before (fn [t]
                                           (set (map :val (filter #(<= (:ts %) t) acks-k))))
                            ;; Check 1: dirty reads — each read only sees values acked at or before its time
                            dirty-reads (for [{:keys [vals ts]} reads-k
                                              :let [valid (acked-before ts)
                                                    extras (set/difference vals valid)]
                                              :when (seq extras)]
                                          {:key k :type :dirty-read
                                           :read-vals (seq vals)
                                           :extras (seq extras)
                                           :read-ts ts
                                           :acked-until (seq (acked-before ts))})
                            ;; Check 2: lost updates — last read must cover all acked values
                            acked-all (get acked-set k #{})
                            lost (set/difference acked-all last-read-vals)
                            lost-update (when (seq lost)
                                          {:key k :type :lost-update
                                           :lost (seq lost)
                                           :acked-all (seq acked-all)
                                           :last-read-vals (seq last-read-vals)})
                            ;; Check 3: fabricated — anything in reads never acked at all
                            union-k (get read-union k #{})
                            fabricated (set/difference union-k acked-all)
                            fab (when (seq fabricated)
                                  {:key k :type :fabricated
                                   :fabricated (seq fabricated)})]
                        (remove nil? (concat dirty-reads [lost-update fab]))))
                    keys)]
    {:valid? (empty? anomalies)
     :keys-checked (count keys)
     :anomalies anomalies
     :anomaly (when (seq anomalies)
                (let [by-type (group-by :type anomalies)]
                  (str (count anomalies) " anomalies: "
                       (count (get by-type :dirty-read)) " dirty-reads, "
                       (count (get by-type :lost-update)) " lost-updates, "
                       (count (get by-type :fabricated)) " fabricated")))}))

(defn check-list-append
  "Pure-Clojure append-temporal checker (no Elle dependency)."
  [history opts]
  (let [result (list-append-check history)]
    {:valid? (:valid? result)
     :workload "list-append"
     :checker "append-temporal"
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
  (let [;; Counter is PER-KEY — each key has an independent counter.
        ;; :value [k v] where k=key, v=counter_value for that key.
        add-entries (for [op history
                          :when (and (= :ok (:type op))
                                     (= :add (:f op)))
                          :let [v (:value op)]
                          :when (and (vector? v) (>= (count v) 2))]
                      {:k (first v) :val (second v) :ts (or (:time op) 0)})
        ;; Count adds per key
        add-counts (reduce (fn [m {:keys [k]}]
                             (update m k (fnil inc 0)))
                           {}
                           add-entries)
        ;; Collect reads: (key, value, time)
        read-entries (for [op history
                           :when (and (= :ok (:type op))
                                      (= :read (:f op)))
                           :let [v (:value op)]
                           :when (and (vector? v) (>= (count v) 2))]
                       {:k (first v) :val (second v) :ts (or (:time op) 0)})
        ;; Per-key monotonic: read values non-decreasing for each key
        read-seqs (reduce (fn [m {:keys [k val ts]}]
                            (update m k (fn [vs] (conj (or vs []) [ts val]))))
                          {}
                          read-entries)
        monotonic (every? (fn [[k pairs]]
                            (let [vals (map second (sort-by first pairs))]
                              (or (empty? vals) (apply <= vals))))
                          read-seqs)
        ;; Per-key bounds: each read in [0, total_adds_for_key]
        ;; and sufficient: read value >= # of adds to that key completed before it
        anomalies (for [{:keys [k val ts]} read-entries
                        :let [total-k (get add-counts k 0)
                              adds-before (count (filter #(and (= k (:k %))
                                                               (< (:ts %) ts))
                                                         add-entries))]
                        :when (or (not (<= 0 val total-k))
                                  (< val adds-before))]
                    {:key k :val val :ts ts
                     :total total-k :adds-before adds-before})]
    {:valid? (and monotonic (empty? anomalies))
     :adds-per-key add-counts
     :reads-checked (count read-entries)
     :monotonic monotonic
     :anomalies anomalies
     :anomaly (cond
                (not monotonic) "reads not monotonic"
                (seq anomalies) (str "read anomalies: " (pr-str anomalies)))}))

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
