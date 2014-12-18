main = do
  putStrLn "input a b c for a*x^2 + b*x + c = 0"

  line <- getLine
  let [a, b, c] = words line

  putStrLn ("got a=" ++ a ++ " b=" ++ b ++ " c=" ++ c)

  xxx <- let xx = 5 in return (xx + 3)

  let
    det0 = b0 * b0 - 4 * a0 * c0
    a0 = read a
    b0 = read b
    c0 = read c
    r0 = -b0 / (2 * a0)

  if det0 < 0
  then putStrLn "沒有實根"
  else if det0 == 0
  then putStrLn (show r0)
  else putStrLn (show (r0 + sqrt det0 / (2 * a0)) ++ ", " ++ show (r0 - sqrt det0 / (2 * a0)))
