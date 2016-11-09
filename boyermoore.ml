let name = "boyermoore"

let init_bc pattern maxi =
  let table = Array.make 256 (-1) in
  for i = 0 to maxi do
    Array.unsafe_set table (String.unsafe_get pattern i |> int_of_char) i
  done;
  table

let init_suffixes pattern maxi =
  let suffixes = Array.make (maxi + 1) (maxi + 1) in
  let left = ref maxi in
  let right = ref maxi in
  for i = maxi - 1 downto 0 do
    let l = Array.unsafe_get suffixes (i + maxi - !right) in
    if i > !left && l < i - !left then
      Array.unsafe_set suffixes i l
    else begin
      if i < !left then left := i;
      right := i;
      while !left >= 0 && String.unsafe_get pattern !left = String.unsafe_get pattern (!left + maxi - !right) do
        decr left
      done;
      Array.unsafe_set suffixes i (!right - !left)
    end
  done;
  suffixes

let init_gs pattern maxi =
  let table = Array.make (maxi + 1) (maxi + 1) in
  let suffixes = init_suffixes pattern maxi in
  let left = ref 0 in
  for i = maxi downto 0 do
    if Array.unsafe_get suffixes i = i + 1 then
      while !left < maxi - i do
        if Array.unsafe_get table !left = (maxi + 1) then
          Array.unsafe_set table !left (maxi - i);
        incr left
      done
  done;
  for i = 0 to maxi - 1 do
    Array.unsafe_set table (maxi - Array.unsafe_get suffixes i) (maxi - i)
  done;
  table

let search pattern maxi text maxpos table_bc table_gs pos =
  let pos = ref pos in
  let i = ref 0 in
  while !pos <= maxpos && !i != -1 do
    i := maxi;
    while !i != -1 && String.unsafe_get text (!pos + !i) = String.unsafe_get pattern !i do decr i done;
    if !i != -1 then
      let shift_bc = !i - Array.unsafe_get table_bc (String.unsafe_get text (!pos + !i) |> int_of_char) in
      let shift_gs = Array.unsafe_get table_gs !i in
      pos := !pos + max shift_bc shift_gs
  done;
  if !i = -1 then Some !pos else None

let find_all pattern =
  let maxi = String.length pattern - 1 in
  let table_bc = init_bc pattern maxi in
  let table_gs = init_gs pattern maxi in
  fun text ->
    let maxpos = String.length text - maxi - 1 in
    let rec loop count pos =
      match search pattern maxi text maxpos table_bc table_gs pos with
      | Some pos -> loop (count + 1) (pos + Array.unsafe_get table_gs 0)
      | None -> count
    in loop 0 0
