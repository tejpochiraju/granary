(** The granary side of the TPC-derived cross-engine benchmark (#482).

    Shared by [bench_tpch.ml] and [test_tpch_smoke.ml] — both drive the same
    granary engine under {!Bench_report.ENGINE}, differing only in which
    reference engine (if any) they compare against. Raises [Failure] on any
    granary error; callers that want an Alcotest-flavoured failure message
    catch and re-report it themselves. *)

include Bench_report.ENGINE
