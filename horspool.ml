let name = "horspool"

let init pattern maxi =
  let table = Array.make 256 (maxi + 1) in
  for i = 0 to maxi - 1 do
    Array.unsafe_set table (String.unsafe_get pattern i |> int_of_char) (maxi - i)
  done;
  table

let search pattern maxi text maxpos table skip pos =
  let pos = ref pos in
  let i = ref 0 in
  while !pos <= maxpos && !i != -1 do
    i := maxi;
    while !i != -1 && String.unsafe_get text (!pos + !i) = String.unsafe_get pattern !i do decr i done;
    if !i != -1 then
      pos := !pos + if !i = maxi then Array.unsafe_get table (String.unsafe_get text (!pos + maxi) |> int_of_char) else skip
  done;
  if !i = -1 then Some !pos else None

let find_all pattern =
  let maxi = String.length pattern - 1 in
  let table = init pattern maxi in
  let skip = Array.unsafe_get table (String.unsafe_get pattern maxi |> int_of_char) in
  fun text ->
    let maxpos = String.length text - maxi - 1 in
    let rec loop count pos =
      match search pattern maxi text maxpos table skip pos with
      | Some i -> loop (count + 1) (i + 1)
      | None -> count
    in loop 0 0
