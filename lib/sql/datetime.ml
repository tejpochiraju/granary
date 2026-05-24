(** Pure-OCaml date/time parsing and formatting for SQL date/time functions.
    No Unix dependencies — clock is injected via optional argument. *)

type dt = {
  year  : int;
  month : int;
  day   : int;
  hour  : int;
  min   : int;
  sec   : float;
}

(* ---- Julian Day Number arithmetic ----------------------------------- *)

let jdn_of_ymd y m d =
  let a = (14 - m) / 12 in
  let y' = y + 4800 - a in
  let m' = m + 12 * a - 3 in
  d + (153 * m' + 2) / 5 + 365 * y' + y' / 4 - y' / 100 + y' / 400 - 32045

let ymd_of_jdn j =
  let a = j + 32044 in
  let b = (4 * a + 3) / 146097 in
  let c = a - (146097 * b) / 4 in
  let d = (4 * c + 3) / 1461 in
  let e = c - (1461 * d) / 4 in
  let m = (5 * e + 2) / 153 in
  let day   = e - (153 * m + 2) / 5 + 1 in
  let month = m + 3 - 12 * (m / 10) in
  let year  = 100 * b + d - 4800 + m / 10 in
  (year, month, day)

(* JD at midnight = integer JDN - 0.5 (JDN counts from noon). *)
let jd_of_dt dt =
  let base = float_of_int (jdn_of_ymd dt.year dt.month dt.day) -. 0.5 in
  let day_frac =
    (float_of_int dt.hour
     +. float_of_int dt.min /. 60.0
     +. dt.sec /. 3600.0) /. 24.0
  in
  base +. day_frac

let dt_of_jd jd =
  let shifted = jd +. 0.5 in
  let jdn  = int_of_float (floor shifted) in
  let frac = shifted -. float_of_int jdn in
  let total_sec = frac *. 86400.0 in
  let hour = int_of_float total_sec / 3600 in
  let min  = (int_of_float total_sec mod 3600) / 60 in
  let sec  = total_sec -. float_of_int (hour * 3600 + min * 60) in
  let (year, month, day) = ymd_of_jdn jdn in
  { year; month; day; hour; min; sec }

let unix_epoch_jd = float_of_int (jdn_of_ymd 1970 1 1) -. 0.5

let unix_of_jd jd = (jd -. unix_epoch_jd) *. 86400.0
let jd_of_unix t  = unix_epoch_jd +. t /. 86400.0

(* ---- Parsing ------------------------------------------------------- *)

let parse_int2 s i = int_of_string (String.sub s i 2)
let parse_int4 s i = int_of_string (String.sub s i 4)

let parse_date_part s =
  try
    if String.length s < 10 then None
    else
      let y = parse_int4 s 0 in
      let m = parse_int2 s 5 in
      let d = parse_int2 s 8 in
      if s.[4] <> '-' || s.[7] <> '-' then None
      else Some (y, m, d)
  with Failure _ | Invalid_argument _ -> None

let parse_time_part s =
  try
    if String.length s < 5 then None
    else
      let h  = parse_int2 s 0 in
      let mi = parse_int2 s 3 in
      if s.[2] <> ':' then None
      else if String.length s = 5 then Some (h, mi, 0.0)
      else if s.[5] <> ':' then None
      else begin
        let sec_str = String.sub s 6 (String.length s - 6) in
        let sec = float_of_string sec_str in
        Some (h, mi, sec)
      end
  with Failure _ | Invalid_argument _ -> None

let parse ?(now : (unit -> float) option) ts =
  let ts = String.trim ts in
  if String.lowercase_ascii ts = "now" then
    (match now with
     | None   -> Error "clock not available for 'now'"
     | Some f -> Ok (dt_of_jd (jd_of_unix (f ()))))
  else if String.length ts >= 10 && ts.[4] = '-' then begin
    match parse_date_part ts with
    | None -> Error ("invalid date: " ^ ts)
    | Some (y, m, d) ->
      let rest_start =
        if String.length ts > 10 && (ts.[10] = ' ' || ts.[10] = 'T')
        then Some 11 else None
      in
      (match rest_start with
       | None ->
         Ok { year = y; month = m; day = d; hour = 0; min = 0; sec = 0.0 }
       | Some i ->
         let time_part = String.sub ts i (String.length ts - i) in
         (match parse_time_part time_part with
          | None -> Error ("invalid time in: " ^ ts)
          | Some (h, mi, sec) ->
            Ok { year = y; month = m; day = d; hour = h; min = mi; sec }))
  end
  else if String.length ts >= 5 && ts.[2] = ':' then begin
    match parse_time_part ts with
    | None -> Error ("invalid time: " ^ ts)
    | Some (h, mi, sec) ->
      Ok { year = 2000; month = 1; day = 1; hour = h; min = mi; sec }
  end
  else begin
    match float_of_string_opt ts with
    | None   -> Error ("unrecognised time string: " ^ ts)
    | Some f ->
      if Float.is_finite f then begin
        if String.contains ts '.' && f > 1000.0 then
          Ok (dt_of_jd f)
        else
          Ok (dt_of_jd (jd_of_unix f))
      end else Error ("invalid numeric time: " ^ ts)
  end

(* ---- Formatting ---------------------------------------------------- *)

let pad2 n = if n < 10 then "0" ^ string_of_int n else string_of_int n

let pad3 n =
  if n < 10 then "00" ^ string_of_int n
  else if n < 100 then "0" ^ string_of_int n
  else string_of_int n

let pad4 n =
  if n < 1000 then "0" ^ pad3 n else string_of_int n

let to_date dt =
  pad4 dt.year ^ "-" ^ pad2 dt.month ^ "-" ^ pad2 dt.day

let to_time dt =
  pad2 dt.hour ^ ":" ^ pad2 dt.min ^ ":" ^ pad2 (int_of_float dt.sec)

let to_datetime dt = to_date dt ^ " " ^ to_time dt

let to_julianday dt = jd_of_dt dt

let to_unixepoch dt = Int64.of_float (unix_of_jd (jd_of_dt dt))

let is_leap_year y =
  (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0

let days_in_month y m =
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11               -> 30
  | 2 -> if is_leap_year y then 29 else 28
  | _ -> 0

let day_of_year y m d =
  let rec go mo acc =
    if mo >= m then acc + d
    else go (mo + 1) (acc + days_in_month y mo)
  in
  go 1 0

let strftime fmt dt =
  let buf = Buffer.create (String.length fmt) in
  let n = String.length fmt in
  let i = ref 0 in
  while !i < n do
    if fmt.[!i] = '%' && !i + 1 < n then begin
      incr i;
      (match fmt.[!i] with
       | 'Y' -> Buffer.add_string buf (pad4 dt.year)
       | 'm' -> Buffer.add_string buf (pad2 dt.month)
       | 'd' -> Buffer.add_string buf (pad2 dt.day)
       | 'H' -> Buffer.add_string buf (pad2 dt.hour)
       | 'M' -> Buffer.add_string buf (pad2 dt.min)
       | 'S' -> Buffer.add_string buf (pad2 (int_of_float dt.sec))
       | 'f' ->
         let whole = int_of_float dt.sec in
         let frac  = dt.sec -. float_of_int whole in
         Buffer.add_string buf
           (Printf.sprintf "%02d.%06.0f" whole (frac *. 1_000_000.0))
       | 'j' ->
         Buffer.add_string buf (pad3 (day_of_year dt.year dt.month dt.day))
       | 's' ->
         Buffer.add_string buf (Int64.to_string (to_unixepoch dt))
       | '%' -> Buffer.add_char buf '%'
       | c   -> Buffer.add_char buf '%'; Buffer.add_char buf c);
      incr i
    end else begin
      Buffer.add_char buf fmt.[!i];
      incr i
    end
  done;
  Buffer.contents buf

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
