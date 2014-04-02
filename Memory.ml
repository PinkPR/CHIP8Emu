let size = 4096
let map = Array.make size 0

let get n =
  Array.get map n

let set n x =
  Array.set map n x
