import System.Random

main = do
  gen <- getStdGen

rnd_select :: [a] -> Int -> [a]
rnd_select [] _ = []
rnd_select xs y = take y $ xs !! (randomRs (0, length xs) gen)
