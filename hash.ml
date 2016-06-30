let name = "hash"

exception Found of int

let start_search pattern patternlength text =
  let hash = ref 0 in
  let identical = ref true in
  for pos = 0 to patternlength - 1 do
    let patternchar = String.unsafe_get pattern pos |> int_of_char in
    let textchar = String.unsafe_get text pos |> int_of_char in
    hash := !hash + textchar - patternchar;
    identical := !identical && patternchar = textchar
  done;
  (!hash, !identical)

let search pattern patternlength text maxpos hash pos =
  let hash = ref hash in
  let i = ref 0 in
  for pos = pos + 1 to maxpos do
    hash := !hash - (String.unsafe_get text (pos - 1) |> int_of_char) + (String.unsafe_get text (pos + patternlength - 1) |> int_of_char);
    if !hash = 0 then begin
      i := 0;
      while String.unsafe_get text (pos + !i) = String.unsafe_get pattern !i do
        incr i;
        if !i = patternlength then raise (Found pos)
      done
    end
  done

let find_all pattern text =
  let patternlength = String.length pattern in
  let maxpos = String.length text - patternlength in
  let (hash, identical) = start_search pattern patternlength text in
  let count = if identical then 1 else 0 in
  let rec loop count hash pos =
    match search pattern patternlength text maxpos hash pos with
    | exception Found pos -> loop (count + 1) 0 pos
    | () -> count
  in loop count hash 0
