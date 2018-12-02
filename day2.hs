import Data.List
import Data.Maybe

hasCharNTimes n xs = any (\c -> n == (length $ elemIndices c xs)) xs

part1 xs = (length two) * (length three)
    where
        two     = filter (hasCharNTimes 2) xs
        three   = filter (hasCharNTimes 3) xs

part2 xs = snd $ fromJust $ find (\(c,_) -> c == 1) allComparasions
    where
        allComparasions     = concat $ map (\x -> map (compareBoxes x) xs) xs
        compareBoxes a b    = (diffCount, withoutDiff)
            where
                comparasion = zipWith (==) a b
                diffCount   = length $ filter (not . id) comparasion
                withoutDiff = map snd $ filter fst $ zip comparasion a

main = do
    input <- readFile "input/day2.txt"
    let input'  = lines input

    putStrLn $ show $ part1 input'
    putStrLn $ show $ part2 input'
