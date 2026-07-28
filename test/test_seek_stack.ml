(** #235 regression: [Store.seek_next] must not let a synchronously-resolving
    cursor grow the OCaml stack without bound.

    The streaming probes added in #232/#234 consume a cursor with a recursive
    [let rec gather () = match%lwt S.seek_next cur with ... ; gather ()].
    [match%lwt] is [Lwt.bind]; when [seek_next] returns an ALREADY-DETERMINED
    promise — the [Mem] backend (always) or a cache-resident B+-tree page — the
    continuation runs synchronously, so each [gather ()] nests one frame on the
    OCaml stack instead of returning to a trampoline.  Streaming K matches then
    costs O(K) stack, and a pathologically common term/value overflows it.

    The fix makes [seek_next] splice an [Lwt.pause] every [seek_pause_interval]
    hits: the spliced promise is not yet determined, so [Lwt.bind] defers the
    continuation to the scheduler and the stack unwinds.

    These tests are machine-independent and do not rely on actually triggering a
    [Stack_overflow] (which depends on the process stack limit):

    - [test_seek_next_yields] inspects [Lwt.state] of each [seek_next] promise
      on the synchronous [Mem] path.  WITHOUT the fix every promise is [Return]
      (zero cooperative yields); WITH it, at least one is [Sleep] — the pause
      that resets the stack.  It also checks the yields recur (not just once),
      so the stack stays bounded across a long run, and that every entry is
      streamed exactly once in order.

    - [test_large_drain_completes] consumes a large run through the exact
      vulnerable [gather] pattern and asserts it finishes with the right count.
      On a default 8 MB stack this overflows without the fix; everywhere it
      validates correctness at scale. *)

open Lwt.Syntax
module S = Granary_store.Store

let run = Lwt_main.run
let key_of i = Bytes.of_string (Printf.sprintf "%012d" i)
let tid = 0

(* Build an in-memory store holding [n] entries under [tid] with ascending,
   fixed-width keys (so [seek_ge ""] streams all of them in order). *)
let build_mem n =
  let s = S.create () in
  let* tx = S.rw_begin s in
  let rec ins i =
    if i >= n
    then Lwt.return_unit
    else
      let* () = S.put tx tid (key_of i) (Bytes.of_string "v") in
      ins (i + 1)
  in
  let* () = ins 0 in
  let* () = S.commit tx in
  Lwt.return s
;;

(* Stream every entry, inspecting whether each [seek_next] promise is already
   determined.  Returns (number consumed, number of cooperative yields). *)
let stream_counting_yields s =
  let* tx = S.ro_begin s in
  let* cur = S.seek_ge tx tid (Bytes.of_string "") in
  let yields = ref 0 in
  let rec loop consumed =
    let p = S.seek_next cur in
    (match Lwt.state p with
     | Lwt.Sleep -> incr yields
     | Lwt.Return _ | Lwt.Fail _ -> ());
    let* kv = p in
    match kv with
    | None -> Lwt.return consumed
    | Some _ -> loop (consumed + 1)
  in
  let* consumed = loop 0 in
  S.seek_close cur;
  let* () = S.ro_end tx in
  Lwt.return (consumed, !yields)
;;

let test_seek_next_yields () =
  (* Comfortably larger than the internal pause interval (256) so several
     boundaries are crossed; if that interval is ever raised above [n] this
     test must be revisited. *)
  let n = 2000 in
  let consumed, yields =
    run
      (let* s = build_mem n in
       stream_counting_yields s)
  in
  Alcotest.(check int) "streamed every entry exactly once" n consumed;
  (* The core guard: the synchronous Mem path yielded at least once.  Without
     the #235 fix this is 0. *)
  Alcotest.(check bool)
    (Printf.sprintf
       "seek_next yielded cooperatively (got %d yields over %d hits)"
       yields
       n)
    true
    (yields >= 1);
  (* And it recurs — the stack is reset periodically, not once. *)
  Alcotest.(check bool)
    (Printf.sprintf "yields recur to bound the stack (got %d)" yields)
    true
    (yields >= 2)
;;

(* The exact vulnerable consumer shape from exec.ml's index/FTS probes. *)
let drain_all s =
  let* tx = S.ro_begin s in
  let* cur = S.seek_ge tx tid (Bytes.of_string "") in
  let count = ref 0 in
  let rec gather () =
    let* kv = S.seek_next cur in
    match kv with
    | None -> Lwt.return_unit
    | Some _ ->
      incr count;
      gather ()
  in
  let* () = gather () in
  S.seek_close cur;
  let* () = S.ro_end tx in
  Lwt.return !count
;;

let test_large_drain_completes () =
  (* Large enough to overflow a default 8 MB stack via the synchronous Mem
     path if the pause hook were removed. *)
  let n = 300_000 in
  let count =
    run
      (let* s = build_mem n in
       drain_all s)
  in
  Alcotest.(check int) "drained the full run without stack overflow" n count
;;

let () =
  Alcotest.run
    "seek_stack"
    [ ( "stack-safety"
      , [ Alcotest.test_case
            "seek_next yields to bound the stack"
            `Quick
            test_seek_next_yields
        ; Alcotest.test_case
            "large synchronous drain completes"
            `Slow
            test_large_drain_completes
        ] )
    ]
;;
