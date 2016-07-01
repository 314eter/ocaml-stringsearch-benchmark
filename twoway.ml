let name = "twoway"

let rec substrings_match str1 str2 pos1 pos2 length =
  match length with
  | 0 -> true
  | _ when str1.[pos1] != str2.[pos2] -> false
  | _ -> substrings_match str1 str2 (pos1 + 1) (pos2 + 1) (length - 1)

let max_suffix ~lt pattern patternlength =
  let rec loop maxpos respos reslen period =
    if respos + reslen = patternlength then
      (maxpos, period)
    else
      let a' = pattern.[maxpos + reslen] in
      let a = pattern.[respos + reslen] in
      if lt a a' then
        let respos = respos + reslen + 1 in
        loop maxpos respos 0 (respos - maxpos)
      else if a != a' then
        loop respos (respos + 1) 0 1
      else if reslen + 1 = period then
        loop maxpos (respos + reslen + 1) 0 period
      else
        loop maxpos respos (reslen + 1) period
  in loop 0 1 0 1

let critical_position text length =
  match max_suffix ( < ) text length, max_suffix ( > ) text length with
  | (m1, _) as mp1, (m2, _) when m1 > m2 -> mp1
  | _, mp2 -> mp2

let search_exact pattern patternlength text maxpos critical period skip pos =
  let pos = ref pos in
  let skip = ref skip in
  let i = ref 0 in
  while !pos <= maxpos && !i >= 0 do
    i := max critical !skip;
    while !i < patternlength && String.unsafe_get text (!pos + !i) = String.unsafe_get pattern !i do incr i done;
    if !i < patternlength then begin
      pos := !pos + max (!i - critical + 1) (!skip - period + 1);
      skip := 0
    end else begin
      i := critical - 1;
      while !i >= 0 && String.unsafe_get text (!pos + !i) = String.unsafe_get pattern !i do decr i done;
      if !i >= 0 then begin
        pos := !pos + period;
        skip := patternlength - period;
      end
    end
  done;
  if !i < 0 then Some !pos else None

let search_inexact pattern patternlength text maxpos critical period _ pos =
  let pos = ref pos in
  let i = ref 0 in
  while !pos <= maxpos && !i >= 0 do
    i := critical;
    while !i < patternlength && String.unsafe_get text (!pos + !i) = String.unsafe_get pattern !i do incr i done;
    if !i < patternlength then
      pos := !pos + !i - critical + 1
    else begin
      i := critical - 1;
      while !i >= 0 && String.unsafe_get text (!pos + !i) = String.unsafe_get pattern !i do decr i done;
      if !i >= 0 then
        pos := !pos + period
    end
  done;
  if !i < 0 then Some !pos else None

let find_all pattern =
  let patternlength = String.length pattern in
  let (critical, period) = critical_position pattern patternlength in
  let (search, period) =
    if 2 * critical < patternlength then
      if substrings_match pattern pattern 0 period critical then
        (search_exact, period)
      else
        (search_inexact, patternlength - critical + 1)
    else
      (search_inexact, critical + 1) in
  fun text ->
    let maxpos = String.length text - patternlength in
    let rec loop count skip pos =
      match search pattern patternlength text maxpos critical period skip pos with
      | Some pos -> loop (count + 1) (patternlength - period) (pos + period)
      | None -> count
    in loop 0 0 0
