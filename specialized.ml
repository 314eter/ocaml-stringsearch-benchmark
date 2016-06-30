let name = "specialized"

exception Found = Naive.Found

let search1 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    then raise (Found pos)
  done

let search2 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    then raise (Found pos)
  done

let search3 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    && String.unsafe_get text (pos + 2) = String.unsafe_get pattern 2
    then raise (Found pos)
  done

let search4 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    && String.unsafe_get text (pos + 2) = String.unsafe_get pattern 2
    && String.unsafe_get text (pos + 3) = String.unsafe_get pattern 3
    then raise (Found pos)
  done

let search5 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    && String.unsafe_get text (pos + 2) = String.unsafe_get pattern 2
    && String.unsafe_get text (pos + 3) = String.unsafe_get pattern 3
    && String.unsafe_get text (pos + 4) = String.unsafe_get pattern 4
    then raise (Found pos)
  done

let search6 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    && String.unsafe_get text (pos + 2) = String.unsafe_get pattern 2
    && String.unsafe_get text (pos + 3) = String.unsafe_get pattern 3
    && String.unsafe_get text (pos + 4) = String.unsafe_get pattern 4
    && String.unsafe_get text (pos + 5) = String.unsafe_get pattern 5
    then raise (Found pos)
  done

let search7 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    && String.unsafe_get text (pos + 2) = String.unsafe_get pattern 2
    && String.unsafe_get text (pos + 3) = String.unsafe_get pattern 3
    && String.unsafe_get text (pos + 4) = String.unsafe_get pattern 4
    && String.unsafe_get text (pos + 5) = String.unsafe_get pattern 5
    && String.unsafe_get text (pos + 6) = String.unsafe_get pattern 6
    then raise (Found pos)
  done

let search8 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    && String.unsafe_get text (pos + 2) = String.unsafe_get pattern 2
    && String.unsafe_get text (pos + 3) = String.unsafe_get pattern 3
    && String.unsafe_get text (pos + 4) = String.unsafe_get pattern 4
    && String.unsafe_get text (pos + 5) = String.unsafe_get pattern 5
    && String.unsafe_get text (pos + 6) = String.unsafe_get pattern 6
    && String.unsafe_get text (pos + 7) = String.unsafe_get pattern 7
    then raise (Found pos)
  done

let search9 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    && String.unsafe_get text (pos + 2) = String.unsafe_get pattern 2
    && String.unsafe_get text (pos + 3) = String.unsafe_get pattern 3
    && String.unsafe_get text (pos + 4) = String.unsafe_get pattern 4
    && String.unsafe_get text (pos + 5) = String.unsafe_get pattern 5
    && String.unsafe_get text (pos + 6) = String.unsafe_get pattern 6
    && String.unsafe_get text (pos + 7) = String.unsafe_get pattern 7
    && String.unsafe_get text (pos + 8) = String.unsafe_get pattern 8
    then raise (Found pos)
  done

let search10 pattern text maxpos pos =
  for pos = pos to maxpos do
    if String.unsafe_get text pos = String.unsafe_get pattern 0
    && String.unsafe_get text (pos + 1) = String.unsafe_get pattern 1
    && String.unsafe_get text (pos + 2) = String.unsafe_get pattern 2
    && String.unsafe_get text (pos + 3) = String.unsafe_get pattern 3
    && String.unsafe_get text (pos + 4) = String.unsafe_get pattern 4
    && String.unsafe_get text (pos + 5) = String.unsafe_get pattern 5
    && String.unsafe_get text (pos + 6) = String.unsafe_get pattern 6
    && String.unsafe_get text (pos + 7) = String.unsafe_get pattern 7
    && String.unsafe_get text (pos + 8) = String.unsafe_get pattern 8
    && String.unsafe_get text (pos + 9) = String.unsafe_get pattern 9
    then raise (Found pos)
  done

let find_all pattern =
  let patternlength = String.length pattern in
  let search =
    match patternlength with
    | 1 -> search1 pattern
    | 2 -> search2 pattern
    | 3 -> search3 pattern
    | 4 -> search4 pattern
    | 5 -> search5 pattern
    | 6 -> search6 pattern
    | 7 -> search7 pattern
    | 8 -> search8 pattern
    | 9 -> search9 pattern
    | 10 -> search10 pattern
    | _ -> Naive.search pattern patternlength in
  fun text ->
    let maxpos = String.length text - patternlength in
    let rec loop count pos =
      match search text maxpos pos with
      | exception Found pos -> loop (count + 1) (pos + 1)
      | () -> count
    in loop 0 0
