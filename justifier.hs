tamanhoLinhas :: [String]-> [Int]
tamanhoLinhas [] = []
tamanhoLinhas (x:xs) = (length x : tamanhoLinhas xs)


bigestLine :: [String] -> Int
bigestLine [] = 0
bigestLine x = maximum (tamanhoLinhas x)

lineSize :: [String] -> Int
lineSize (x:xs) = (length x) + lineSize xs
lineSize [] = 0

countWords :: [String] -> Int
countWords [] = 0
countWords (x:xs) = 1 + countWords xs

breakLines :: [String] -> [[String]]
breakLines (x:xs) = (words x) : breakLines xs
breakLines [] = []


spaces :: Int -> Int -> String
spaces 0 0 = []
spaces x y = if x > 0 then ' ' : spaces (x-1) y else if y > 0 then ' ' : spaces x 0 else []


insertSpaces :: Int -> Int -> [String] -> String
insertSpaces n rest [] = []
insertSpaces n rest (x:xs) = 
  if length (x:xs) > 1
    then (x ++ (spaces n rest)) ++ (insertSpaces n (rest-1) xs)
	else (x ++ (insertSpaces n (rest-1) xs))
	
	
justifica :: String -> String
justifica (x:xs) = unlines (insertIntoLines (breakLines((lines (x:xs))))    (bigestLine(lines(x:xs)))  )


insertIntoLines :: [[String]] -> Int -> [String]
insertIntoLines [] b = []
insertIntoLines (x:[]) b = insertSpaces 1 0 x : insertIntoLines [] b
insertIntoLines (x:xs) b = 
  (insertSpaces 
               (quot ((b - (lineSize x)))   ((countWords x) - 1))  --1o argumento insertSpace
               	 (mod ((b - (lineSize x)))   ((countWords x) - 1))  --2o argumento
           	                                                                     x) : (insertIntoLines xs b) 
																				 
