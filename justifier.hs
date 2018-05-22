import Data.List

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs


separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas x = lines x


tamanhoLinhas :: [String]-> [Int]
tamanhoLinhas [] = []
tamanhoLinhas (x:xs) = (length x : tamanhoLinhas xs)


tamanhoMaiorLinha :: [String] -> Int
tamanhoMaiorLinha [] = 0
tamanhoMaiorLinha x = maximum (tamanhoLinhas x)


separaPalavras :: String -> [String]
separaPalavras [] = []
separaPalavras x = words x



insertSpace :: Int -> String -> String
insertSpace n (x:xs) = 
   let (a,b) = splitAt n (x:xs) in a ++ " " ++ b

   
insertSpaces :: [Int] -> Int -> Int -> String -> ([Int],Int, Int, String)
insertSpaces (a:az) i n [] = ((a:az),i,n,[])
insertSpaces (a:az) i 0 (x:xs) = ((a:az),0, 0, (x:xs))
insertSpaces (a:az) i n (x:xs) 
    | i == last (a:az) = insertSpaces (a:az) a (n-1) (insertSpace i (x:xs))
    | otherwise insertSpaces (a:az) F (n-1) (insertSpace i (x:xs))
        where F = head ( drop 1 ( dropWhile(/= i) (a:az)))
