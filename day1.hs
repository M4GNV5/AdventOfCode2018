import Data.List
import Data.Maybe

part1 = sum

part2 xs = snd $ fromJust result
    where
        frequencies = scanl (+) 0 (cycle xs)
        result      = find (\(i,v) -> v `elem` take i frequencies) $ zip [0..] frequencies

removePlus ('+':xs) = xs
removePlus xs       = xs
main = do
    input <- readFile "input/day1.txt"
    let input' = map (read . removePlus) $ lines input

    putStrLn $ show $ part1 input'
    putStrLn $ show $ part2 input'
