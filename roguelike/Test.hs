import Data.Sequence as Seq

main = do
  let x = Seq.fromList "ABC"
  putStrLn $ show $ Seq.viewl x
  let (a :< _) = (Seq.viewl x)
  putStrLn $ [a] ++ "ttt"
  -- let str = (Seq.viewl x) ++ "something"
  let xx = Seq.drop 1 x
  putStrLn $ show $ Seq.viewl xx
  let xxx = (><) xx (fromList "DE")
  putStrLn $ show $ Seq.viewl xxx
