type t =
  | Txn_begin of { txn_id : int64 }
  | Txn_commit of
      { txn_id : int64
      ; frames : int
      }
  | Txn_rollback of { txn_id : int64 }
  | Savepoint_begin of
      { txn_id : int64
      ; name : string
      }
  | Savepoint_release of
      { txn_id : int64
      ; name : string
      }
  | Savepoint_rollback of
      { txn_id : int64
      ; name : string
      }
  | Wal_append of
      { txn_id : int64
      ; base_idx : int
      ; count : int
      }
  | Wal_reset of { epoch : int64 }
  | Checkpoint_begin of { target_frames : int }
  | Checkpoint_end of { pages_migrated : int }

let label = function
  | Txn_begin _ -> "BEGIN"
  | Txn_commit _ -> "COMMIT"
  | Txn_rollback _ -> "ROLLBACK"
  | Savepoint_begin _ -> "SP_BEGIN"
  | Savepoint_release _ -> "SP_RELEASE"
  | Savepoint_rollback _ -> "SP_ROLLBACK"
  | Wal_append _ -> "WAL_APPEND"
  | Wal_reset _ -> "WAL_RESET"
  | Checkpoint_begin _ -> "CKPT_BEGIN"
  | Checkpoint_end _ -> "CKPT_END"
;;

let txn_id = function
  | Txn_begin { txn_id }
  | Txn_commit { txn_id; _ }
  | Txn_rollback { txn_id }
  | Savepoint_begin { txn_id; _ }
  | Savepoint_release { txn_id; _ }
  | Savepoint_rollback { txn_id; _ }
  | Wal_append { txn_id; _ } -> Some txn_id
  | Wal_reset _ | Checkpoint_begin _ | Checkpoint_end _ -> None
;;

let pp fmt ev =
  let tag = label ev in
  match ev with
  | Txn_begin { txn_id } | Txn_rollback { txn_id } ->
    Format.fprintf fmt "%s txn=%Ld" tag txn_id
  | Txn_commit { txn_id; frames } ->
    Format.fprintf fmt "%s txn=%Ld frames=%d" tag txn_id frames
  | Savepoint_begin { txn_id; name }
  | Savepoint_release { txn_id; name }
  | Savepoint_rollback { txn_id; name } ->
    Format.fprintf fmt "%s txn=%Ld name=%s" tag txn_id name
  | Wal_append { txn_id; base_idx; count } ->
    Format.fprintf fmt "%s txn=%Ld base=%d count=%d" tag txn_id base_idx count
  | Wal_reset { epoch } -> Format.fprintf fmt "%s epoch=%Ld" tag epoch
  | Checkpoint_begin { target_frames } ->
    Format.fprintf fmt "%s target=%d" tag target_frames
  | Checkpoint_end { pages_migrated } ->
    Format.fprintf fmt "%s migrated=%d" tag pages_migrated
;;
