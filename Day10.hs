convertInner :: [Char] -> [Char] -> Char -> Int -> String
convertInner xs soFar lastChar lastCount
  | null xs 		= newSoFar
  | head xs == lastChar = convertInner (tail xs) soFar lastChar (lastCount + 1)
  | otherwise		= convertInner (tail xs) newSoFar (head xs) 1
  where newSoFar = lastChar : (head (show lastCount)) : soFar

convert :: [Char] -> [Char]
convert string = reverse $ convertInner (tail string) [] (head string) 1

main = putStrLn $ show $ length ((iterate convert "1113122113") !! 40)
