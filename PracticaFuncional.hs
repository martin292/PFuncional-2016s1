------------------------------------------------------------------------------------------------
--PRACTICA 1 -
------------------------------------------------------------------------------------------------

--1)

seven :: a -> Int
seven _ = 7

sign :: Int -> Int
sign n | n == 0    = 0
       | n > 0     = 1
       | otherwise = -1

absolute :: Int -> Int
absolute n | sign n == -1 = -1 * n
	         | otherwise    = n

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

dividesTo :: Int -> Int -> Bool
dividesTo 0 _ = False
dividesTo x y = mod y x == 0

isMultiple :: Int -> Int -> Bool
isMultiple x y = dividesTo y x

isCommonDivisor :: Int -> (Int, Int) -> Bool
isCommonDivisor n (x, y) = (dividesTo n x) && (dividesTo n y)

isCommonMult :: Int -> (Int, Int) -> Bool
isCommonMult n (x, y) = (isMultiple n x) && (isMultiple n y)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


--2)

{-
f x = let (y,z) = (x,x) 
			in y

greaterThan (x,y) = if x > y then True else False

f (x,y) = let z = x + y 
					in g (z,y) 
					where g (a,b) = a - b
-}

f x = x

greaterThan (x, y) = x > y

f' (x, y) = (x+y)-x


--3)

{-
power4 x = let sqr y = y * y
					 in sqr (sqr x)
-}

power4 x = x*x*x*x

--power4 x = twice sqr x


--4)

--T(0) = T(1) = 1
--T(n) = T(n − 1) + T(n − 2)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

--5)

{-
""equivalencia""

eficiencia

corrección

claridad

modificabilidad

terminación

generalidad

simplicidad
-}

------------------------------------------------------------------------------------------------
--PRACTICA 2 -
------------------------------------------------------------------------------------------------

--1)

{-

a) Bool
True
False

b) (Int, Int)
(1, 2)
(3, 4)

c) Char -> Int
\c -> 1
\c -> 2

d) (Int, Char) -> Bool
\(n, c) -> True
\(n, c) -> False

e) (Int -> Int) -> Int
? 

f ) (Bool -> Bool, Int)
(\b -> True, 1)
(\b -> False, 2)

g) a -> Bool
\a -> True
\a -> False

h) c -> c
id

-}

--2)

--Permite realizar chequeos en tiempo de compilación, previniendo bugs.

--detección de errores comunes
--documentación
--especificación rudimentaria
--oportunidades de optimización en compilación


--3)

{-
seven :: a -> Int

sign :: Int -> Int

absolute :: Int -> Int

and' :: Bool -> Bool -> Bool

or' :: Bool -> Bool -> Bool

not' :: Bool -> Bool

xor' :: Bool -> Bool -> Bool

dividesTo :: Int -> Int -> Bool

isMultiple :: Int -> Int -> Bool

isCommonDivisor :: Int -> (Int, Int) -> Bool

isCommonMult :: Int -> (Int, Int) -> Bool

swap :: (a, b) -> (b, a)

-}

--4)

{-
a) first (x,y) = x
first :: (a, b) -> a

b) second (x,y) = y
second :: (x, y) -> y

c) const x y = x
const :: a -> b -> a

d) compose f g = (\x -> f (g x))
compose :: (b -> c) -> (a -> b) -> a -> (a -> c)

e) apply f x = f x
apply :: (a -> b) -> a -> b 

f ) subst f g x = f x (g x)
subst :: (a -> b -> c) -> (a -> b) -> a -> c 

g) pairFunc (f1,f2) x y = (f1 (f2 x), f2 (f1 y))
pairFunc :: (b -> a , a -> b) -> a -> b -> (a, b)

-}

--5)
--La comprobación de tipificación se realiza durante la compilación, y no durante la ejecución

--6)

{-
a) if (seven ’a’ < 1) then 5 else power4 2
	¿ ’a’ ?

	ret: 16

b) if False then True
	error falta el else

c) a := 4
  ¿Que es esto?

d) (1 < x && x < 100) || x == True || x == False
	error: x no puede ser Int y Bool

e) False == (1 < 3)
	True

f ) (1 < x < 2)
	error, deberia ser: 1 < x && x < 2

-}

--8)

data ColorPrimario = Rojo | Amarillo | Azul

data ColorSecundario = Naranja | Verde | Morado

mezclar :: ColorPrimario -> ColorPrimario -> ColorSecundario
mezclar Rojo Amarillo = Naranja
mezclar Amarillo Rojo = Naranja
mezclar Rojo Azul     = Morado
mezclar Azul Rojo     = Morado
mezclar Amarillo Azul = Verde
mezclar Azul Amarillo = Verde

data Punto = P2 Int Int | P3 Int Int Int

modulo :: Punto -> Int
modulo (P2 x y)   = sqrt(sqr x + sqr y)
modulo (P3 x y z) = sqrt(sqr x + sqr y + sqr z)

distancia :: Punto -> Punto -> Int
distancia (P2 x1 y1) (P2 x2 y2) = sqrt(sqr(x2 - x1)+ sqr(y2 - y1))
distancia (P3 x1 y1 z1) (P3 x2 y2 z2) = sqrt(sqr(x2 - x1) + sqr(y2 - y1) + sqr(z2 - z1))

xcoord :: Punto -> Int
xcoord (P2 x y) = x
xcoord (P3 x y z) = x

ycoord :: Punto -> Int
ycoord (P2 x y) = y
ycoord (P3 x y z) = y

suma :: Punto -> Punto -> Punto
suma (P2 x1 y1) (P2 x2 y2) = P2 (x1 + x2) (y1 + y2)
suma (P3 x1 y1 z1) (P3 x2 z2 y2) = P3 (x1 + x2) (y1 + y2) (z1 + z2)

data Figura = Circulo Int | Rectangulo Int Int

area :: Figura -> Int
area Circulo r     = pi * sqr r
area Rectagulo b a = b * a

perimetro :: Figura -> Int
perimetro Circulo r      = 2 * pi * r
perimetro Rectangulo b a = 2*b + 2*a

--mover

--data Figura3D = Cubo | Cilindro | Esfera

type Nombre = String
type Edad = Int
type DNI = String
type Address = String
type Phone = String
type Waddress = String
type Wphone = String

data Persona = Person Nombre Edad DNI Address Phone | WPerson Nombre Edad DNI Address Phone Waddress Wphone

--9)

--a) a
--error "bottom"

--b) Int -> a
f :: Int -> a
f x = x + f x

--c) a -> b
g :: a -> b
g x = error "error"

--d) Dar una expresion de tipo c -> c, distinta de la identidad.
h :: c -> c
h x = error "error"

--11)

smaller (x,y,z) | x <= y && x <= z = x
								| y <= x && y <= z = y
								| z <= x && z <= y = z

\(x, y, z) -> if( x <= y && x <= z )then x
							 else if( y <= x && y <= z )then y
							 	else if ( z <= x && z <= y )then z


second x = \x -> x

\ x y -> x

andThen True y = y
andThen False y = False

\b y -> if b then y else False


--12)

iff = \x -> \y -> if x then not y else y

iff' x y = if x then not y else y

alpha = \x -> \x -> x

alpha' x y = y


------------------------------------------------------------------------------------------------
--PRACTICA 3 -
------------------------------------------------------------------------------------------------

--1)

--Bool -> Bool -> Bool

\b1 -> \b2 -> b1 && b2

\b1 -> \b2 -> b1 || b2

--a -> b -> a

\x -> \y -> x 

--(a->b)->(a->c)->a->(b,c)

\f -> \g -> \x -> (fx, gx)

--Int -> Int -> Int

\x -> \y -> x+y

\x -> \y -> x-y

--(Int -> Int) -> Int -> Int

\f -> x -> fx

\f -> x -> fx + 1

--(a -> b -> c) -> b -> a -> c

\f -> y x -> (fx)y

--2)

fUno :: ((Int,Int)->Int)->Int->Int->Int
fUno f x y = f (x,y)

--fUno Es una funcion de alto orden y no esta currificada 


fDos :: (Char->Char->Bool)->(Char->Char)->(Int->Char)->Char->Int->Bool
fDos f1 f2 f3 a b = f1 (f2 a) (f3 b)

--fDos Es una funcion de alto orden y esta currificada

fTres :: (Char,Char,Char) -> Bool
fTres (c1,c2,c3) = (c1==c2) && (c2==c3)

--fTres No es una funcion de alto orden y no esta currificada

--¿Que sucede con la funcion anterior si definimos type Pers=(Char,Char,Char) y fTres :: Pers -> Bool?
--Es lo mismo


--5)

--twice f x = f (f x)
\f x -> f(fx)

--flip f x y = f y x
\f x y -> f y x

--inc = (+1)
\x -> x+1




------------------------------------------------------------------------------------------------
--PRACTICA 4 -
------------------------------------------------------------------------------------------------

--1)
{-
Pattern: expresión especial
Sólo con constructores y variables sin repetir
Argumento en el lado izquierdo de una ecuación

a) (x, y) -----------> Valido

b) (1, 2) -----------> Invalido

c) (n+1) ------------> Invalido

d) (10) -------------> Invalido

e) (’a’,(’a’,b)) ----> Invalido

f ) (a,(a,b)) -------> Invalido

g) (x,(x,y)) --------> Invalido

h) ([]:[4]) ---------> Invalido

i) (x:y:[]) ---------> Invalido

j) [xs] -------------> Valido?

k) ([]:[]) ----------> Valido?
-}

--2)
--suma los elementos de una lista de numeros.
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

--devue ve True si algun elemento de una lista es True.
any :: [Bool] -> Bool
any []     = False
any (x:xs) = x || any xs

--devuelve True si todos los elementos de una lista son True.
all :: [Bool] -> Bool
all []     = True
all (x:xs) = x && all xs

--dada una lista de caracteres, devuelve la lista de sus codigos.
--codes :: [Char] -> [?]

--dada una lista de numeros, devuelve los restos de su division por un numero.
remainders :: [Int] -> Int -> [Int]
remainders _ 0      = error
remainders [] _     = []
remainders (x:xs) n = (x/n):(remainders xs n) 

--dada una lista de numeros, devuelve la lista de sus cuadrados.
squares :: [Int] -> [Int]
squares []     = []
squares (x:xs) = (x*x):(squares xs)

--dada una lista de listas, devuelve la lista de sus longitudes.
lengths :: [[Int]] -> [Int]
lengths []     = []
lengths (xs:xss) = (len xs):(lengths xss)

--dada una lista de pares ordenados de numeros, devuelve la lista de aquellos cuya primer componente es menor que el triple de la segunda.
order :: [(Int, Int)] -> [(Int, Int)]
order []     = []
order ((x, y):xs) | x < 3*y = (x, y):(order xs)
									| otherwise = order xs

--dada una lista de numeros, devuelve la lista con aquellos que son pares.
pairs :: [Int] -> [Int]
pairs []     = []
pairs (x:xs) | mod x 2 == 0 = x:(pairs xs)
             | otherwise = pairs xs

--dada una lista de caracteres, devuelve la lista de los que son letras.
--chars

--dada una lista de listas xss y un numero n, devuelve la lista de aquellas listas de xss que tienen longitud mayor que n.
moreThan :: [[]] -> Int -> [[]]
moreThan [] _       = []
moreThan (xs:xss) n | len xs > n = xs:(moreThan xss)
										| otherwise = moreThan xss


--3)

{-
a) [[]] ++ xs = xs ---------> V   

b) [[]] ++ [xs] = [[],xs] --> F

c) [[]] ++ xs = [xs] -------> V

d) []:xs = xs --------------> V

e) [[]] ++ [xs] = [xs] -----> V

f ) [[]] ++ xs = []:xs -----> V

g) [xs] ++ [xs] = [xs,xs] --> F

h) [] ++ xs = []:xs --------> V

i) [[]] ++ xs = [[],xs] ----> F

j) [xs] ++ [] = [xs] -------> V
-}

--4)
{-
ifThenElse_Lam = \x -> x

true_Lam = \x -> \y -> x

false_Lam = \x -> \y -> y

not_Lam = \x ->ifThenElse_Lam x false_Lam true_Lam

or_Lam 
and_Lam 
xor_Lam 
iff_Lam
-}


--6)

type Set a = [a]

union :: Set a -> Set a -> Set a
union [] [] = []
union xs [] = xs
union [] ys = ys
union xs ys = xs ++ ys

--9)

impares :: [[Int]] -> [Int]
impares []       = []
impares (xs:xss) = retImpares xs xss

retImpares :: [Int] -> [[Int]] -> [Int]
retImpares [] _       = []
retImpares (x:xs) yss | impar x && existeEnListas x yss = x:retImpares xs yss
                      | otherwise = retImpares xs yss

impar :: x -> Bool
impar n = mod n 2 != 0

existeEnListas :: Int -> [[Int]] -> Bool
existeEnListas _ []       = False
existeEnListas n (xs:xss) = (existe x xs) && existeEnListas x xss

existe :: Int -> [Int] -> Bool
existe _ []     = False
existe n (x:xs) | n == x = True
                | otherwise = existe n xs



------------------------------------------------------------------------------------------------
--PRACTICA 5 -
------------------------------------------------------------------------------------------------

--1)

nextDiv :: Int -> Int -> Maby Int
nextDiv x y | x >= y = Nothing
						| dividesTo (x+1) y = Just(x+1)
						| otherwise         = nextDiv (x+1) y

sumDivs :: Int -> Int
sumaDivs x = x + sumaRec x (x-1)

sumaRec :: Int -> Int -> Int
sumaRec _ 0 = 0
sumaRec x y | dividesTo y x = y + sumaRec x (y-1)
            | otherwise     = sumaRec x (y-1)

power :: Int -> Nat -> Int
power _ 0 = 1
power x 1 = x
power x y = x * power x (y-1)

prime :: Int -> Bool
prime 0 = False
prime 1 = False
prime n = nextDiv 1 x == x 

phi :: Int -> Int


--2)

{--

--}

--3)
--Tiene infinitos elementos.
--Todos sus elementos, o bien satisfacen una regla base, o bien satisfacen una regla inductiva.
--Todos sus elementos son finitos.
--El orden basado en ''es parte de'' es bien fundado (o sea, toda cadena descendente es finita).


--5)

{-
pairs . squares = squares . pairs

Por induccion en la estructura de las listas

P(xs) = pairs . squares = squares . pairs

EI: P([]) ^ (P(xs) => P(x:xs))

Caso base (P([])):

			pairs . squares [] = squares . pairs []

(.)->	pairs(squares [])  = squares(pairs [])

sq1-> pairs []           = squares(pairs [])

p 1-> []                 = squares []

sq1-> []                 = []

Paso inductivo HI: P(xs)
							 TI: P(x:xs)

			pairs . squares (x:xs) = squares . pairs (x:xs)

(.)-> pairs(squares (x:xs))  = squares(pairs (x:xs))

sq2-> pairs((x*x):(squares xs)) = squares(pairs (x:xs))

p2 -> pairs((x*x):(squares xs)) = squares( 2 casos posibles)

Caso 1 (x es par):

			pairs((x*x):(squares xs)) = squares(x:(pairs xs))

p2 -> (x*x):(pairs (squares xs)) = squares(x:(pairs xs))

sq2-> (x*x):(pairs (squares xs)) = (x*x):(squares (pairs xs))

(.)-> (x*x):(pairs . squares xs) = (x*x):(squares . pairs xs)

++--> [x*x] ++ (pairs . squares xs) = [x*x] ++ (squares . pairs xs)

HI--> [x*x] ++ (squares . pairs xs) = [x*x] ++ (squares . pairs xs)

			TRUE

Caso 2 (x es impar):

			pairs((x*x):(squares xs)) = squares(pairs xs)

p2--> pairs(squares xs) = squares(pairs xs)

(.)-> pairs . squares xs = squares . pairs xs

HI -> TRUE 			

-}

-----------

{-
((‘mod‘ n) . sum) (remainders n xs) = (sum xs) ‘mod‘ n, para todo n > 0



-}



------------------------------------------------------------------------------------------------
--PRACTICA 6 -
------------------------------------------------------------------------------------------------


--1)

data TipTree a = Tip a | Join (TipTree a) (TipTree a)

heightTip :: TipTree a -> Int
heightTip (Tip x)      = 0
heightTip (Join t1 t2) = 1 + (max (heightTip t1)(heightTip t2))

leaves :: TipTree a -> Int
leaves (Tip x)      = 1
leaves (Join t1 t2) = leaves t1 + leaves t2

nodes :: TipTree a -> Int
nodes (Tip x)      = 0
nodes (Join t1 t2) = 1 + nodes t1 + nodes t2

walkover :: TipTree a -> [a]
walkover (Tip x)      = [x]
walkover (Join t1 t2) = walkover t1 ++ walkover t2

mirrorTip :: TipTree a -> TipTree a 
mirrorTip (Tip x)      = Tip x
mirrorTip (Join t1 t2) = Join t2 t1

mapTip :: (a -> b) -> TipTree a -> TipTree b
mapTip f (Tip x)      = Tip (f x)
mapTip f (Join t1 t2) = Join (mapTip f t1) (mapTip f t2)


--3)

data Var = X Int
data Lt = V Var | Ap Lt Lt | Abs Var Lt


freeVars :: Lt -> [Var]
freeVars (V var)       = [var]
freeVars (Ap lt1 lt2)  = freeVars lt1 ++ freeVars lt2
freeVars (Abs var lt) = [var] ++ freeVars lt


------------------------------------------------------------------------------------------------
--PRACTICA 7 -
------------------------------------------------------------------------------------------------

--1)

--suma los elementos de una lista de numeros.
sum' :: [Int] -> Int
sum' = foldr 0 (+)

--devue ve True si algun elemento de una lista es True.
any' :: [Bool] -> Bool
any' = foldr False (\x rs -> x || rs)

--devuelve True si todos los elementos de una lista son True.
all' :: [Bool] -> Bool
all' = foldr True (\x rs -> x && rs)

--dada una lista de caracteres, devuelve la lista de sus codigos.
--codes :: [Char] -> [?]

--dada una lista de numeros, devuelve los restos de su division por un numero.
remainders' :: [Int] -> Int -> [Int]
remainders' l n = map [] (\x rs -> (x/n):rs) n

--dada una lista de numeros, devuelve la lista de sus cuadrados.
squares' :: [Int] -> [Int
squares' = map [] (\x rs -> (x*x):rs)

--dada una lista de listas, devuelve la lista de sus longitudes.
lengths' :: [[Int]] -> [Int]
lengths' = foldr [] (\x rs -> (len x):rs)

--dada una lista de pares ordenados de numeros, devuelve la lista de aquellos cuya primer componente es menor que el triple de la segunda.
order' :: [(Int, Int)] -> [(Int, Int)]
order' = filter [] (\(x, y) rs -> if x < 3*y then (x, y):rs else rs)

--dada una lista de numeros, devuelve la lista con aquellos que son pares.
pairs' :: [Int] -> [Int
pairs' = filter [] (\x rs -> if mod x 2 == 0 then x:rs else rs)

--dada una lista de caracteres, devuelve la lista de los que son letras.
--chars

--dada una lista de listas xss y un numero n, devuelve la lista de aquellas listas de xss que tienen longitud mayor que n.
moreThan' :: [[]] -> Int -> [[]]
moreThan' l n = filter [] (\xs rs -> if len xs > n then xs:rs else rs)


--2)

pal :: String -> bool
pal s = s == reverse s

hs :: [String] -> Int
hs = foldr [] (\x rs -> if empiezaConH x then 1 + rs else rs)

avgLength :: [String] -> Int
avgLength = foldr [] (\x rs -> (length x + rs)/(1+rs))

--5)

takewhile :: [a] -> f -> [a]
takewhile l f = foldr [] (\x rs -> if fx then x:rs else [])

dropwhile :: [a] -> f -> [a]
dropwhile l f = foldr [] (\x rs -> if fx then rs else x:rs)

--7)

{-

a) map f (xs ++ ys) = map f xs ++ map f ys

Por induccion en la estructura de las listas

P(xs): map f (xs ++ ys) = map f xs ++ map f ys

EI: P([]) ^ (P(xs) => P(x:xs))

Caso Base: P([])

        map f ([] ++ ys) = map f [] ++ map f ys
++ ---> map f ys         = map f [] ++ map f ys
map 1-> map f ys         = [] ++ map f ys
++ ---> map f ys         = map f ys

Paso Inductivo: HI(P xs)
                TI(P x:xs)

        map f (x:xs ++ ys) = map f x:xs ++ map f ys
++ ---> map f (x:(xs++ys)) = map f x:xs ++ map f ys
map --> fx : map f (xs++ys)= fx : map f xs ++ map f ys
HI ---> fx : map f xs ++ map f ys = fx : map f xs ++ map f ys

                TRUE

------------------------------------------------------------------
b) map f . concat = concat . map (map f)

------------------------------------------------------------------
c) filter p (xs ++ ys) = filter p xs ++ filter p ys

Por induccion en la estructura de las listas

P(xs): filter p (xs ++ ys) = filter p xs ++ filter p ys

EI: P([]) ^ (P(xs) => P(x:xs))

Caso Base: P([])

        filter p ([] ++ ys) = filter p [] ++ filter p ys
++ ---> filter p ys         = filter p [] ++ filter p ys
filte-> filter p ys         = [] ++ filter p ys
++ ---> filter p ys         = filter p ys

Paso Inductivo: HI(P xs)
                TI(P x:xs)

        filter p (x:xs ++ ys) = filter p x:xs ++ filter p ys
++ ---> filter p (x: xs++ys)  = filter p x:xs ++ filter p ys
fil --> 2 CASOS (px == TRUE y px == FALSE)

px == TRUE

        x : filter p (xs++ys) = x : filter p xs ++ filter p ys
HI ---> x : filter p xs ++ filter p ys = x : filter p xs ++ filter p ys
                
                TRUE

px == FALSE

        filter p (xs++ys) = filter p xs ++ filter p ys
HI ---> filter p xs ++ filter p ys = filter p xs ++ filter p ys

                TRUE

------------------------------------------------------------------
d) map (map f) . map (x:) = map ((f x):) . map (map f)

------------------------------------------------------------------
e) concat . map concat = concat . concat

-}

------------------------------------------------------------------------------------------------
--PRACTICA 8 -
------------------------------------------------------------------------------------------------


--1)

data TipTree a = Tip a | Join (TipTree a) (TipTree a)

foldTip :: (a -> b) -> (b -> b -> b) -> TipTree a -> b
foldTip f g (Tip x)      = f x
foldTip f g (Join t1 t2) = g (foldTip f g t1) (foldTip f g t2)


heightTip :: TipTree a -> Int
heightTip = foldTip (\x -> 0) (\r1 r2 -> 1 + max r1 r2)

leaves :: TipTree a -> Int
leaves = foldTip (\x -> 1) (+)

nodes :: TipTree a -> Int
nodes = foldTip (\x -> 0) (\ r1 r2 -> 1 + r1 + r2)

walkover :: TipTree a -> [a]
walkover = foldTip (\x -> [x]) (++)

mirrorTip :: TipTree a -> TipTree a 
mirrorTip = foldTip (Tip) (\r1 r2 -> Join r2 r1)

mapTip :: (a -> b) -> TipTree a -> TipTree b
mapTip f = foldTip (\x -> fx) (\r1 r2 -> Join r1 r2)

--2)

data BinTree a = Empty | Bin a (BinTree a) (BinTree a)

--a)
nodesBin :: BinTree a -> Int
nodesBin (Empty)       = 0
nodesBin (Bin x b1 b2) = 1 + nodesBin b1 + nodesBin b2

heightBin :: BinTree a -> Int
heightBin (Empty)       = 0
heightBin (Bin x b1 b2) = 1 + max heightBin b1 heightBin b2

--b)
mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin _ (Empty)       = Empty
mapBin f (Bin x b1 b2) = Bin fx (mapBin f b1) (mapBin f b2)

mirrorBin :: BinTree a -> BinTree a
mirrorBin (Empty)       = Empty
mirrorBin (Bin x b1 b2) = Bin x (mirrorBin b2) (mirrorBin b1)

--c)

foldBin :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldBin f1 cb (Empty)       = cb
foldBin f1 cb (Bin x b1 b2) = f1 x (foldBin f1 cb b1) (foldBin f1 cb b2)

--d)

heightBin' = foldBin (\_ r1 r2 -> 1 + max r1 r2) 0

mapBin' f = foldBin (\x r1 r2 -> Bin fx r1 r2) Empty

mirrorBin' = foldBin (\x r1 r2 -> Bin x r2 r1) Empty


--3)

data GenTree a = Gen a [GenTree a]

--a)

foldGen :: (a -> [b] -> b) -> GenTree a -> b
foldGen f (Gen a [])     = f a [] 
--foldGen f (Gen a (x:xs)) = f a (foldGen f x):(foldGen f xs)


--5)

data GenExp a = Leaf a | Un (GenExp a) | BinG (GenExp a) (GenExp a)

foldGenExp :: (a -> b) -> (b -> b) -> (b -> b -> b) -> GenExp a -> b
foldGenExp f1 f2 f3 (Leaf x)     = f1 x
foldGenExp f1 f2 f3 (Un g1)      = f2 (foldGenExp f1 f2 f3 g1)
foldGenExp f1 f2 f3 (BinG g1 g2) = f3 (foldGenExp f1 f2 f3 g1)(foldGenExp f1 f2 f3 g2)


data NExp = Num Int | Sum NExp NExp | Sub NExp NExp | Neg NExp

foldNExp :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> NExp -> b
foldNExp f1 f2 f3 f4 (Num n)     = f1 n
foldNExp f1 f2 f3 f4 (Sum n1 n2) = f2 (foldNExp f1 f2 f3 f4 n1)(foldNExp f1 f2 f3 f4 n2)
foldNExp f1 f2 f3 f4 (Sub n1 n2) = f3 (foldNExp f1 f2 f3 f4 n1)(foldNExp f1 f2 f3 f4 n2)
foldNExp f1 f2 f3 f4 (Neg n1)    = f4 (foldNExp f1 f2 f3 f4 n1)

data Either a b = Left a | Right b

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f1 _ (Left x)  = f1 x
foldEither _ f2 (Right y) = f2 y

data Nat = Zero | Succ Nat

foldNat :: b -> (b -> b) -> Nat -> b
foldNat cb f (Zero)   = cb
foldNat cb f (Succ n) = f (foldNat cb f n)


















