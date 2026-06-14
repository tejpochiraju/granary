module Row = Sqlocaml_encoding.Row

type t =
  | Int_col of
      { values : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
      ; nulls : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
      ; len : int
      }
  | Real_col of
      { values : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
      ; nulls : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
      ; len : int
      }
  | Text_col of
      { dict : string array
      ; dict_tbl : (string, int) Hashtbl.t
      ; indices : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
      ; nulls : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
      ; len : int
      }
  | Blob_col of
      { values : bytes array
      ; nulls : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
      ; len : int
      }

let create ty cap =
  match ty with
  | Row.Integer ->
    Int_col
      { values = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout cap
      ; nulls = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout cap
      ; len = 0
      }
  | Row.Real ->
    Real_col
      { values = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout cap
      ; nulls = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout cap
      ; len = 0
      }
  | Row.Text ->
    Text_col
      { dict = [||]
      ; dict_tbl = Hashtbl.create 16
      ; indices = Bigarray.Array1.create Bigarray.int Bigarray.c_layout cap
      ; nulls = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout cap
      ; len = 0
      }
  | Row.Blob ->
    Blob_col
      { values = [||]
      ; nulls = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout cap
      ; len = 0
      }
;;

let length col =
  match col with
  | Int_col { len; _ } | Real_col { len; _ } | Text_col { len; _ } | Blob_col { len; _ }
    -> len
;;

let pp fmt col =
  match col with
  | Int_col { len; _ } -> Format.fprintf fmt "Col.Int(%d)" len
  | Real_col { len; _ } -> Format.fprintf fmt "Col.Real(%d)" len
  | Text_col { len; _ } -> Format.fprintf fmt "Col.Text(%d)" len
  | Blob_col { len; _ } -> Format.fprintf fmt "Col.Blob(%d)" len
;;

let resize_bigarray kind layout cur new_len =
  let dim = Bigarray.Array1.dim cur in
  if dim >= new_len
  then cur
  else (
    let cap = max new_len (dim * 2) in
    let new_arr = Bigarray.Array1.create kind layout cap in
    let dst_view = Bigarray.Array1.sub new_arr 0 dim in
    Bigarray.Array1.blit cur dst_view;
    new_arr)
;;

let resize_nulls nulls new_len =
  resize_bigarray Bigarray.int8_unsigned Bigarray.c_layout nulls new_len
;;

let grow_bytes_array arr new_len =
  let cur_len = Array.length arr in
  if cur_len >= new_len
  then arr
  else
    Array.init
      (max new_len (cur_len * 2))
      (fun i -> if i < cur_len then arr.(i) else Bytes.empty)
;;

let append_value_null col =
  let idx = length col in
  let new_len = idx + 1 in
  match col with
  | Int_col c ->
    let nulls = resize_nulls c.nulls new_len in
    Bigarray.Array1.set nulls idx 1;
    Int_col { values = c.values; nulls; len = new_len }
  | Real_col c ->
    let nulls = resize_nulls c.nulls new_len in
    Bigarray.Array1.set nulls idx 1;
    Real_col { values = c.values; nulls; len = new_len }
  | Text_col c ->
    let nulls = resize_nulls c.nulls new_len in
    Bigarray.Array1.set nulls idx 1;
    Text_col
      { dict = c.dict; dict_tbl = c.dict_tbl; indices = c.indices; nulls; len = new_len }
  | Blob_col c ->
    let nulls = resize_nulls c.nulls new_len in
    let values = grow_bytes_array c.values new_len in
    Bigarray.Array1.set nulls idx 1;
    Blob_col { values; nulls; len = new_len }
;;

let append_value col v =
  match col, v with
  | _, Row.V_null -> append_value_null col
  | Int_col c, Row.V_int n ->
    let idx = c.len in
    let new_len = idx + 1 in
    let values = resize_bigarray Bigarray.int64 Bigarray.c_layout c.values new_len in
    let nulls = resize_nulls c.nulls new_len in
    Bigarray.Array1.set values idx n;
    Bigarray.Array1.set nulls idx 0;
    Int_col { values; nulls; len = new_len }
  | Real_col c, Row.V_real f ->
    let idx = c.len in
    let new_len = idx + 1 in
    let values = resize_bigarray Bigarray.float64 Bigarray.c_layout c.values new_len in
    let nulls = resize_nulls c.nulls new_len in
    Bigarray.Array1.set values idx f;
    Bigarray.Array1.set nulls idx 0;
    Real_col { values; nulls; len = new_len }
  | Text_col c, Row.V_text s ->
    let idx = c.len in
    let new_len = idx + 1 in
    let dict, dict_tbl, dict_idx =
      match Hashtbl.find_opt c.dict_tbl s with
      | Some i -> c.dict, c.dict_tbl, i
      | None ->
        let i = Array.length c.dict in
        let dict = Array.append c.dict [| s |] in
        Hashtbl.add c.dict_tbl s i;
        dict, c.dict_tbl, i
    in
    let indices = resize_bigarray Bigarray.int Bigarray.c_layout c.indices new_len in
    let nulls = resize_nulls c.nulls new_len in
    Bigarray.Array1.set indices idx dict_idx;
    Bigarray.Array1.set nulls idx 0;
    Text_col { dict; dict_tbl; indices; nulls; len = new_len }
  | Blob_col c, Row.V_blob b ->
    let idx = c.len in
    let new_len = idx + 1 in
    let values = grow_bytes_array c.values new_len in
    let nulls = resize_nulls c.nulls new_len in
    values.(idx) <- b;
    Bigarray.Array1.set nulls idx 0;
    Blob_col { values; nulls; len = new_len }
  | _ -> failwith "Col.append_value: type mismatch"
;;

let get_value col idx =
  match col with
  | Int_col { values; nulls; _ } ->
    if Bigarray.Array1.get nulls idx <> 0
    then Row.V_null
    else Row.V_int (Bigarray.Array1.get values idx)
  | Real_col { values; nulls; _ } ->
    if Bigarray.Array1.get nulls idx <> 0
    then Row.V_null
    else Row.V_real (Bigarray.Array1.get values idx)
  | Text_col { dict; indices; nulls; _ } ->
    if Bigarray.Array1.get nulls idx <> 0
    then Row.V_null
    else Row.V_text dict.(Bigarray.Array1.get indices idx)
  | Blob_col { values; nulls; _ } ->
    if Bigarray.Array1.get nulls idx <> 0 then Row.V_null else Row.V_blob values.(idx)
;;

let append_batch col rows =
  let n = Array.length rows in
  if n = 0
  then col
  else (
    match col with
    | Int_col c ->
      let new_len = c.len + n in
      let values = resize_bigarray Bigarray.int64 Bigarray.c_layout c.values new_len in
      let nulls = resize_nulls c.nulls new_len in
      Array.iteri
        (fun i v ->
           let idx = c.len + i in
           match v with
           | Row.V_int n ->
             Bigarray.Array1.set values idx n;
             Bigarray.Array1.set nulls idx 0
           | Row.V_null -> Bigarray.Array1.set nulls idx 1
           | _ -> failwith "Col.append_batch: type mismatch (expected int)")
        rows;
      Int_col { values; nulls; len = new_len }
    | Real_col c ->
      let new_len = c.len + n in
      let values = resize_bigarray Bigarray.float64 Bigarray.c_layout c.values new_len in
      let nulls = resize_nulls c.nulls new_len in
      Array.iteri
        (fun i v ->
           let idx = c.len + i in
           match v with
           | Row.V_real f ->
             Bigarray.Array1.set values idx f;
             Bigarray.Array1.set nulls idx 0
           | Row.V_null -> Bigarray.Array1.set nulls idx 1
           | _ -> failwith "Col.append_batch: type mismatch (expected real)")
        rows;
      Real_col { values; nulls; len = new_len }
    | Text_col c ->
      let new_len = c.len + n in
      let dict = ref c.dict in
      let dict_tbl = Hashtbl.copy c.dict_tbl in
      let dict_idx_for s =
        match Hashtbl.find_opt dict_tbl s with
        | Some i -> i
        | None ->
          let i = Array.length !dict in
          dict := Array.append !dict [| s |];
          Hashtbl.add dict_tbl s i;
          i
      in
      let indices = resize_bigarray Bigarray.int Bigarray.c_layout c.indices new_len in
      let nulls = resize_nulls c.nulls new_len in
      Array.iteri
        (fun i v ->
           let idx = c.len + i in
           match v with
           | Row.V_text s ->
             Bigarray.Array1.set indices idx (dict_idx_for s);
             Bigarray.Array1.set nulls idx 0
           | Row.V_null -> Bigarray.Array1.set nulls idx 1
           | _ -> failwith "Col.append_batch: type mismatch (expected text)")
        rows;
      Text_col { dict = !dict; dict_tbl; indices; nulls; len = new_len }
    | Blob_col c ->
      let new_len = c.len + n in
      let values = grow_bytes_array c.values new_len in
      let nulls = resize_nulls c.nulls new_len in
      Array.iteri
        (fun i v ->
           let idx = c.len + i in
           match v with
           | Row.V_blob b ->
             values.(idx) <- b;
             Bigarray.Array1.set nulls idx 0
           | Row.V_null -> Bigarray.Array1.set nulls idx 1
           | _ -> failwith "Col.append_batch: type mismatch (expected blob)")
        rows;
      Blob_col { values; nulls; len = new_len })
;;

let of_values schema rows =
  let schema_arr = Array.of_list schema in
  let ncols = Array.length schema_arr in
  let nrows = Array.length rows in
  Array.init ncols (fun i ->
    let col_ty = schema_arr.(i).Row.ty in
    let col = create col_ty nrows in
    let col_rows = Array.init nrows (fun r -> rows.(r).(i)) in
    append_batch col col_rows)
;;
