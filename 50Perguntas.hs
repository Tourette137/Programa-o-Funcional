-- 50 Perguntas de Programação Funcional


--1
enumeraFromTo :: Int -> Int -> [Int]
enumeraFromTo x y | x>y = []
                  | otherwise = x : enumeraFromTo (x+1) y



--2
enumeraFromThenTo :: Int -> Int -> Int -> [Int]
enumeraFromThenTo a b c | a <= c    = a : enumeraFromThenTo (a+b-1) b c
                        | otherwise = [a]



--3
uneLista :: [a] -> [a] -> [a]
uneLista [] b     = b
uneLista (h:t) b  = h : uneLista t b



--4
elementoLista :: [a] -> Int -> a
elementoLista [] _           = []
elementoLista (h:t) 0        = h
elementoLista (h:t) x  | x>0 = elementoLista t (x-1)



-- 5
inverteLista :: [a] -> [a]
inverteLista []    = []
inverteLista (h:t) = inverteLista t ++ [h]



--6
retirarLista :: Int -> [a] -> [a]
retirarLista 0 f     = []
retirarLista x []    = []
retirarLista x (h:t) = h : retirarLista (x-1) t



--7
tiraLista :: Int -> [a] -> [a]
tiraLista x []    = []
tiraLista x (h:t) | x==0 = (h:t)
                  | x>=1 = tiraLista (x-1) t



--8
zipar :: [a] -> [b] -> [(a,b)]
zipar a []           = []
zipar [] b           = []
zipar (h1:t1)(h2:t2) = (h1,h2) : zipar t1 t2



--9
temElemento :: Eq a => a -> [a] -> Bool
temElemento x []    = False
temElemento x (h:t) | x==h = True
                    | otherwise = temElemento x t



--10
replicar :: Int -> a -> [a]
replicar 0 a  = []
replicar x a  = a : replicar (x-1) a



--11
interLista :: a -> [a] -> [a]
interLista x []    = []
interLista x [f]   = [f]
interLista x (h:t) = h : x : interLista x t



--12
agrupa :: Eq a => [a] -> [[a]]
agrupa []    = []
agrupa (h:t) = (aux1 h t) : agrupa(drop(length(aux1 h t))(h:t))

aux1 :: Eq a => a -> [a] -> [a]
aux1 a []    = [a]
aux1 a (h:t) | (a == h)  = (a : (aux1 h t))
             | otherwise = (aux1 a [])



--13
concatena :: [[a]] -> [a]
concatena []    = []
concatena (h:t) = h ++ concatena t



--14
myInits :: [a] -> [[a]]
myInits x = aux2 0 x

aux2 :: Int -> [a] -> [[a]]
aux2 i x | i < length(x) = (take i x) : aux2 (i+1) x
         | otherwise     = [x]



--15
cauda :: [a] -> [[a]]
cauda []     = [[]]
cauda (x:xs) = (x:xs) : cauda xs



--16
ePrefixo2 :: Eq a => [a] -> [a] -> Bool
ePrefixo2 [] _          = True
ePrefixo2 _ []          = False
ePrefixo2 (y:ys) (x:xs) | (x==y)    = (ePrefixo2 ys xs)
                        | otherwise = False



--17
eSufixo :: Eq a => [a] -> [a] -> Bool
eSufixo [] _          = True
eSufixo _ []          = False
eSufixo (y:ys) (x:xs) = ePrefixo2 (inverteLista(y:ys)) (inverteLista(x:xs))



--18
eSquenciaDe :: Eq a => [a] -> [a] -> Bool
eSquenciaDe [] _          = True
eSquenciaDe _ []          = False
eSquenciaDe (y:ys) (x:xs) | (y==x)    = (eSquenciaDe (ys) (xs))
                          | otherwise = (eSquenciaDe (y:ys) (xs))
                     


--19
elementoIndice :: Eq a => a -> [a] -> [Int]
elementoIndice _ []     = []
elementoIndice y (x:xs) = aux4 0 y (x:xs)

aux4 :: Eq a => Int -> a -> [a] -> [Int]
aux4 _ _ []     = []
aux4 h y (x:xs) | (y==x)    = h : aux4 (h+1) y xs
                | otherwise = aux4 (h+1) y xs



--20
noob :: Eq a => [a] -> [a]
noob []     = []
noob (x:xs) = x : noob (aux5 x xs)

aux5 :: Eq a => a -> [a] -> [a]
aux5 _ []     = []
aux5 y (x:xs) | (y==x)    = aux5 y xs
              | otherwise = x : aux5 y xs



--21
apagar :: Eq a => a -> [a] -> [a]
apagar _ [] = []
apagar y (x:xs) | (y==x)    = xs
                | otherwise = x : (apagar y xs)



--22
removeLista2 :: Eq a => [a] -> [a] -> [a]
removeLista2 a []     = a
removeLista2 [] _     = []
removeLista2 x (y:ys) = removeLista2 (apagar y x) ys



--23
uniao :: Eq a => [a] -> [a] -> [a]
uniao a [] = a
uniao [] a = a
uniao x y  = x ++ removeLista2 y x 



--24
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] a     = []
intersecao a []     = []
intersecao (x:xs) y | (temElemento x y == True) = (x : intersecao xs y)
                    | otherwise                 = intersecao xs y



--25
inserir :: Ord a => a -> [a] -> [a]
inserir x []     = [x]
inserir y (x:xs) | (y>x)     = x : (inserir y xs)
                 | otherwise = y : x : xs



--26
unePalavras :: [String] -> String
unePalavras [a]    = a
unePalavras (x:xs) = x ++ " " ++ unePalavras xs



--27
juntaString :: [String] -> String
juntaString [a]    = a ++ "\n"
juntaString (x:xs) = x ++ "\n" ++ juntaString xs



--28
oMaior :: Ord a => [a] -> Int
oMaior []     = 0
oMaior (x:xs) | (aux6 x xs == True) = 0
              | otherwise           = oMaior xs + 1

aux6 :: Ord a => a -> [a] -> Bool
aux6 x []     = True 
aux6 x (h:hs) | x>=h = aux6 x hs
              | otherwise = False



--29
eRepetido :: Eq a => [a] -> Bool
eRepetido []       = False
eRepetido (x:xs) | (aux7 x xs == True) = True
                 | otherwise           = eRepetido xs

aux7 :: Eq a => a -> [a] -> Bool
aux7 x []     = False
aux7 h (x:xs) | (h==x)    = True
              | otherwise = aux7 h xs



--30
myAlgarismos :: [Char] -> [Char]
myAlgarismos x = intersecao x "0123456789"



--31
posicaoImpar :: [a] -> [a]
posicaoImpar []       = []
posicaoImpar [x]      = []
posicaoImpar (x:y:xs) = y : posicaoImpar xs



--32
posicaoPar :: [a] -> [a]
posicaoPar []       = []
posicaoPar [x]      = [x]
posicaoPar (x:y:xs) = x : posicaoPar (xs)



--33
crescente :: Ord a => [a] -> Bool
crescente []       = True
crescente [x]      = True
crescente (x:y:xs) | (x<=y) = crescente (y:xs)
                   | otherwise = False



--34
iSorteio :: Ord a => [a] -> [a]
iSorteio []     = []
iSorteio (x:xs) = inserir x (iSorteio xs)



--35
minor :: String -> String -> Bool
minor [] []         = False
minor x []          = False
minor [] x          = True
minor (x:xs) (y:ys) | (x<y)     = True
                    | (x>y)     = False
                    | otherwise = minor xs ys



--36
pertenceConjunto :: Eq a => a -> [(a,Int)] -> Bool
pertenceConjunto _ []     = False
pertenceConjunto h (x:xs) | (h==(fst x)) = True
                          | otherwise    = pertenceConjunto h xs



--37 
lengthConjunto :: [(a,Int)] -> Int
lengthConjunto []     = 0
lengthConjunto (x:xs) = (snd x) + lengthConjunto xs 



--38
converteMSet :: [(a,Int)] -> [a]
converteMSet []     = []
converteMSet (x:xs) = replicar (snd x) (fst x) ++ converteMSet xs



--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet h []         = [(h,1)]
insereMSet h ((p,s):xs) | (h==p)    = (p,(s+1)) : xs
                        | otherwise = (p,s) : insereMSet h xs



--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet h []         = []
removeMSet h ((p,s):xs) | (h==p) && (s==1) = xs
                        | (h==p) && (s>1)  = (p,(s-1)) : xs
                        | otherwise = (p,s) : removeMSet h xs



--41
constroiMSet ::  Ord a => [a] -> [(a,Int)]
constroiMSet []     = []
constroiMSet (x:xs) = (x, aux12 x (x:xs) 0) : constroiMSet (aux13 x xs)

aux12 :: Eq a => a -> [a] -> Int -> Int
aux12 x [] h     = h
aux12 x (y:ys) h | (x==y)    = aux12 x ys h+1
                 | otherwise = aux12 x ys h

aux13 :: Eq a => a -> [a] -> [a]
aux13 x []     = []
aux13 x (y:ys) | (x==y)    = aux13 x ys
               | otherwise = y : aux13 x ys



--42
myPartitionEithers ::  [Either a b] -> ([a],[b])
myPartitionEithers x = (lefts x, rightg x)
---------------------------------------
rightg :: [Either a b] -> [b]
rightg [] = []
rightg (x:xs) | (isItRight x) = fromEitherR x : rightg xs
              | otherwise     = rightg xs

isItRight :: Either a b -> Bool
isItRight (Left a)  = False
isItRight (Right b) = True

fromEitherR :: Either a b -> b
fromEitherR (Right b) = b
---------------------------------------
lefts :: [Either a b] -> [a]
lefts [] = []
lefts (x:xs) | (isItLeft x) = fromEitherL x : lefts xs
             | otherwise    = lefts xs

isItLeft :: Either a b -> Bool
isItLeft (Left a)  = True
isItLeft (Right b) = False

fromEitherL :: Either a b -> a
fromEitherL (Left a) = a



--43
myCatMaybes :: [Maybe a] -> [a]
myCatMaybes []     = []
myCatMaybes (x:xs) | (seraQueESo x) = (opahESo x) : myCatMaybes xs
                   | otherwise      = myCatMaybes xs

seraQueESo :: Maybe a -> Bool
seraQueESo (Just a)  = True
seraQueESo (Nothing) = False

opahESo :: Maybe a -> a
opahESo (Just a) = a



--44
data Movimento = Norte | Sul | Este | Oeste
                deriving Show


posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) []      = (x,y)
posicao (x,y) [Norte] = (x,(y+1))
posicao (x,y) [Sul]   = (x,(y-1))
posicao (x,y) [Este]  = ((x+1),y)
posicao (x,y) [Oeste] = ((x-1),y)
posicao (x,y) (m:ms)  = posicao (posicao (x,y) [m]) ms



--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) = aux45 (x2-x1) ++ aux46 (y2-y1)

aux45 :: Int -> [Movimento]
aux45 x | (x==0) = []
        | (x>0)  = replicar x Este
        | (x<0)  = replicar (-x) Oeste

aux46 :: Int -> [Movimento]
aux46 y | (y==0) = []
        | (y>0)  = replicar y Norte
        | (y<0)  = replicar (-y) Sul



--46
vertical :: [Movimento] -> Bool
vertical (Sul : [])   = True
vertical (Norte : []) = True
vertical (Oeste : _)  = False
vertical (Este : _)   = False
vertical (Sul : m)    = vertical m
vertical (Norte : m)  = vertical m



--47
data Posicao = Pos Int Int
             deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral (x:xs) = aux47 x xs


aux47 :: Posicao -> [Posicao] -> Posicao
aux47 a []     = a
aux47 x (h:hs) | ((proximidade(x)) > (proximidade (h))) = aux47 h hs
               | otherwise = aux47 x hs

proximidade :: Posicao -> Int
proximidade (Pos x y) = (abs(x)+abs(y))



--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ []    = []
vizinhos x (h:t) | ((aux48 x h) || (aux4848 x h) == True) = h : vizinhos x t
                 | otherwise                              = vizinhos x t

aux48 :: Posicao -> Posicao -> Bool
aux48 (Pos a b) (Pos x y) | (a==(x-1) && b==y) || (a==(x+1) && b==y) = True
                          | otherwise = False

aux4848 :: Posicao -> Posicao -> Bool
aux4848 (Pos a b) (Pos x y) | (b==(y-1) && a==x) || (b==(y+1) && a==x) = True
                            | otherwise = False



--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada []      = False
mesmaOrdenada (x:xs)  = aux4949 x xs

aux4949 :: Posicao -> [Posicao] -> Bool
aux4949 (Pos a b) []             = True
aux4949 (Pos a b) (Pos x y : hs) | (b==y)    = aux4949 (Pos a b) hs
                                 | otherwise = False



--50
data Semaforo = Verde | Amarelo | Vermelho
              deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = length (filtraVermelho l) >= 3

filtraVermelho :: [Semaforo] -> [Semaforo]
filtraVermelho []            = []
filtraVermelho (Vermelho:xs) = Vermelho : filtraVermelho xs
filtraVermelho (_:xs)        = filtraVermelho xs