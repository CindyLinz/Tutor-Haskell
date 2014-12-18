

let add = \a -> \b -> a + b
in
  add 3 4
  (\a -> (\b -> a + b)) 3 4
  (\b -> 3 + b) 4
  3 + 4
  7

let ad = (let add a b = a + b in add 3) in ad 4
