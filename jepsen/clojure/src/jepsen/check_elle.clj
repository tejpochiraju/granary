(ns jepsen.check-elle
  "Elle SI checker for list-append histories.

   Performs full dependency-graph analysis using Jepsen's Elle library
   (elle 0.2.6+).  Catches G0, G1a/b/c, G-single, G2-item, G2 (write skew),
   lost update, dirty update, incompatible orders.

   This is loaded dynamically so check.clj can work without Elle when only
   the pure-Clojure checkers are needed."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]))

;; ---------------------------------------------------------------------------
;; History filtering for Elle
;; ---------------------------------------------------------------------------

(defn client-history
  "Filter history to client txn completions (process >= 0, :f = :txn).
   Returns ops with :value normalized to keyword micro-ops (:r, :append)."
  [history]
  (keep (fn [op]
          (when (and (contains? #{:ok :fail} (:type op))
                     (<= 0 (:process op 0))
                     (= :txn (:f op)))
            ;; Ensure :value contains keyword micro-ops
            (let [v (:value op)]
              (when (vector? v)
                (assoc op :value
                       (mapv (fn [mop]
                               (if (vector? mop)
                                 (let [f (first mop)]
                                   (if (keyword? f)
                                     mop
                                     (into [(keyword (name f))] (rest mop))))
                                 mop))
                             v))))))
        history))

;; ---------------------------------------------------------------------------
;; Elle list-append check
;; ---------------------------------------------------------------------------

(defn run-elle
  "Run Elle's list-append SI checker on a parsed Jepsen history.
   Options map may contain:
     :consistency     Consistency model keyword (default :snapshot-isolation)
     :directory       Where to output Elle's HTML report (default nil)

   Returns a map with :valid?, :summary, :anomaly-types, :anomaly-count,
   and :details (the raw Elle result)."
  [history opts]
  (try
    ;; Dynamically require Elle & Jepsen at runtime (use eval to avoid
    ;; compile-time resolution of elle.list-append and jepsen.history)
    (eval '(clojure.core/require 'elle.list-append))
    (eval '(clojure.core/require 'jepsen.history))
    (let [elle-check-fn (resolve (symbol "elle.list-append" "check"))
          _ (when (nil? elle-check-fn)
              (throw (Exception.
                       "elle.list-append/check not found — is elle 0.2.6+ in deps.edn?")))
          jh-history (resolve (symbol "jepsen.history" "history"))
          ;; Filter to client txn completions only
          clients (client-history history)
          _ (println (str "Elle: analyzing " (count clients)
                          " client txn completions"))
          ;; Wrap in Jepsen History object
          jepsen-history (jh-history (vec clients))
          ;; Run Elle at the requested consistency model
          consistency (or (:consistency opts) :snapshot-isolation)
          elle-opts (cond-> {:consistency-models [consistency]}
                      (:directory opts)
                      (assoc :directory (:directory opts)))
          elle-result (elle-check-fn elle-opts jepsen-history)
          ;; Extract anomalies
          valid? (:valid? elle-result)
          anomaly-types (:anomaly-types elle-result)
          anomalies (:anomalies elle-result)
          anomaly-count (count anomalies)
          ;; Build summary
          summary (str
                    (if valid? "VALID" "INVALID")
                    " — Elle SI (" (name consistency) ") check completed"
                    (when anomaly-types
                      (str ", types: " (pr-str anomaly-types)))
                    (when (pos? anomaly-count)
                      (str ", " anomaly-count " anomaly instances")))]
      {:valid? valid?
       :workload "list-append"
       :checker (str "elle-" (name consistency))
       :summary summary
       :anomaly-types anomaly-types
       :anomaly-count anomaly-count
       :details elle-result})
    (catch Exception e
      {:valid? :unknown
       :workload "list-append"
       :checker "elle-error"
       :error (str "Elle check failed: " (.getMessage e))
       :details {:exception (str e)}})))
