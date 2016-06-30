let name = "naive"

exception Found of int

let search pattern patternlength text maxpos pos =
  let i = ref 0 in
  for pos = pos to maxpos do
    i := 0;
    while String.unsafe_get text (pos + !i) = String.unsafe_get pattern !i do
      incr i;
      if !i = patternlength then raise (Found pos)
    done;
  done

let find_all pattern text =
  let patternlength = String.length pattern in
  let maxpos = String.length text - patternlength in
  let rec loop count pos =
    match search pattern patternlength text maxpos pos with
    | exception Found pos -> loop (count + 1) (pos + 1)
    | () -> count
  in loop 0 0
