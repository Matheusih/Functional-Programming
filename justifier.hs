import Data.List

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs


separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas x = lines x





separaPalavras :: String -> [String]
separaPalavras [] = []
separaPalavras x = words x



insertSpace :: Int -> String -> String
insertSpace n (x:xs) = 
   let (a,b) = splitAt n (x:xs) in a ++ " " ++ b

   


removeEspacos :: String -> String
removeEspacos [] = []
removeEspacos x = [ xs | xs <- x , xs /= ' ']











doubList :: [Int] -> [Int]
doubList x = map (2*) x



matches :: Int -> [Int] -> [Int]
matches n [] = []
matches n (x:xs) =
   if x == n then x : matches n xs
    else matches n xs

elemento :: Int -> [Int] -> Bool
elemento n x = if length ( matches n x ) > 0 then True else False


par:: Int->Bool
par a = mod a 2== 0 

selectEvens :: [Int] -> [Int]
selectEvens [] = []
selectEvens x = [ xs | xs <- x , par xs]


filterF :: (a->Bool) -> [a] -> [a]
filterF f [] = []
filterF f (x:xs) = 
  if f x == True 
    then x : filterF f xs 
    else filterF f xs
	
menores :: Int -> [Int] -> [Int]
menores a [] = []
menores a l = [x | x <- l, x < a]

maiores :: Int -> [Int] -> [Int]
maiores a [] = []
maiores a l = [x | x <- l, x > a]

foldr2 :: (a->b->b) ->b ->[a]->b
foldr2 f e [] = e
foldr2 f e (x:xs) = f x (foldr2 f e xs)


insEnd ::  Int ->[Int] -> [Int]
insEnd a [] = [a]
insEnd a (x:xs) =  (x:xs)++[a] 


inverte :: [Int] -> [Int]
inverte x = foldr (insEnd) [] x


media:: [Float] -> Float
media [] = 0
media x = sum x / (fromIntegral $ length x)








countWords :: [String] -> Int
countWords [] = 0
countWords (x:xs) = 1 + countWords xs


spaces :: Int -> Int -> String
spaces 0 0 = []
spaces x y = if x > 0 then ' ' : spaces (x-1) y else if y > 0 then ' ' : spaces x 0 else []


insertSpaces :: Int -> Int -> [String] -> String
insertSpaces n rest [] = []
insertSpaces n rest (x:xs) = 
  if length (x:xs) > 1
    then (x ++ (spaces n rest)) ++ (insertSpaces n (rest-1) xs)
	else (x ++ (insertSpaces n (rest-1) xs))

lineSize :: [String] -> Int
lineSize (x:xs) = (length x) + lineSize xs
lineSize [] = 0


calcSpaces :: Int -> Int -> Int
calcSpaces x y = (x-y)

breakLines :: [String] -> [[String]]
breakLines (x:xs) = (words x) : breakLines xs
breakLines [] = []

insertIntoLines :: [[String]] -> Int -> [String]
insertIntoLines [] b = []
insertIntoLines (x:xs) b = 
  (insertSpaces 
               (quot ((b - (lineSize x)))   ((countWords x) - 1))  --1o argumento insertSpace
               	 (mod ((b - (lineSize x)))   ((countWords x) - 1))  --2o argumento
           	                                                                     x) : (insertIntoLines xs b) 



		
justifyText :: String -> String
justifyText (x:xs) = unlines (insertIntoLines (breakLines((lines (x:xs))))    (bigestLine(lines(x:xs)))  )




tamanhoLinhas :: [String]-> [Int]
tamanhoLinhas [] = []
tamanhoLinhas (x:xs) = (length x : tamanhoLinhas xs)


bigestLine :: [String] -> Int
bigestLine [] = 0
bigestLine x = maximum (tamanhoLinhas x)
