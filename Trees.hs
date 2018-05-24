main = putStrLn "No errors!"

data Estacao = Verao | Inverno
data Temperatura = Frio | Calor

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

printTemp :: Temperatura -> String
printTemp Frio = "Frio"
printTemp _ = "Calor"


blabla :: (a,b,c) -> a
blabla (a,b,c) = a


data Arvore = Folha | Nodo Int Arvore Arvore 
  deriving(Eq,Show)
  
minhaArvore :: Arvore
minhaArvore = Nodo 10 (Nodo 14 ( Nodo 1 Folha Folha) Folha) Folha

somaArvore :: Arvore -> Int
somaArvore Folha = 0
somaArvore (Nodo n arv1 arv2) = n + somaArvore arv1 + somaArvore arv2

myTree :: Arvore
myTree = Nodo 10 ( Nodo 6 Folha Folha ) ( Nodo 4 (Nodo 2 Folha Folha) (Nodo 5 Folha Folha))


times2tree :: Arvore -> Arvore
times2tree Folha = Folha
times2tree (Nodo x arv1 arv2) = Nodo (2*x) (times2tree arv1) (times2tree arv2)

maxTree :: Arvore -> Int
maxTree Folha = 0
maxTree (Nodo n a1 a2) = max (n) (max (maxTree a1) (maxTree a2))


existsT :: Int -> Arvore -> Bool
existsT x Folha = False
existsT x (Nodo n a1 a2) 
  | x == n = True
  | otherwise = (existsT x a1) || (existsT x a2)
  
howManyT :: Int -> Arvore -> Int
howManyT x Folha = 0
howManyT x (Nodo n a1 a2) 
  | x == n = 1 + (howManyT x a1) + (howManyT x a2)
  | otherwise = (howManyT x a1) + (howManyT x a2)
  
  
refleteArvore :: Arvore -> Arvore
refleteArvore (Nodo n a1 a2)
-- ou polimorfica:
--data ArvoreP a = Folha | Nodo a (ArvoreP a) (ArvoreP a)
