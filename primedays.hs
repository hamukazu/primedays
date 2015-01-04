import Data.Time.Calendar
import System.Environment (getArgs)
isPrime n=not $ or $ map (\a->n `mod` a==0)
            $ takeWhile (\i->i*i<=n) [2..]
primeDays y=map (\a->filter (/= '-') $ show a) $ takeWhile (< fromGregorian (y+1) 1 1) 
      $ iterate (\d->addDays 1 d) $ fromGregorian y 1 1
main=do
  args<-getArgs
  case args of
    y:_ ->mapM_ (\a->putStrLn a) $ filter (isPrime.read) $ primeDays $ read y
    _ ->putStrLn "Usage: primedays <year>"