main = do
  a <- getLine
  b <- getLine
  putStrLn (a ++ " + " ++ b ++ " = " ++ show ((read a :: Double) + read b))
