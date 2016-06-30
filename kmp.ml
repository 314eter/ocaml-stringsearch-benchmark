let name = "kmp"

exception Found of int

let search pattern patternlength text textlength table k i =
  let k = ref k in
  let i = ref i in
  while !k < textlength do
    while !i >= 0 && String.unsafe_get text !k != String.unsafe_get pattern !i do
      i := if !i = 0 then -1 else Array.unsafe_get table !i
    done;
    incr k; incr i;
    if !i = patternlength then raise (Found (!k - patternlength))
  done;
  !i

let init pattern patternlength =
  let table = Array.make patternlength 0 in
  let k = ref 1 and i = ref 0 in
  while !k < patternlength do
    if String.unsafe_get pattern !k != String.unsafe_get pattern !i then begin
      Array.unsafe_set table !k !i;
      i := if !i = 0 then -1 else Array.unsafe_get table !i;
      while !i >= 0 && String.unsafe_get pattern !k != String.unsafe_get pattern !i do
        i := if !i = 0 then -1 else Array.unsafe_get table !i
      done
    end else
      Array.unsafe_set table !k (if !i = 0 then -1 else Array.unsafe_get table !i);
    incr k; incr i;
  done;
  Array.unsafe_set table 0 !i;
  table

let find_all pattern =
  let patternlength = String.length pattern in
  let table = init pattern patternlength in
  fun text ->
    let textlength = String.length text in
    let rec loop count k i =
      match search pattern patternlength text textlength table k i with
      | exception Found pos -> loop (count + 1) (pos + patternlength) (Array.unsafe_get table 0)
      | _ -> count in
    loop 0 0 0
