module Practica04 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

--IMPLEMENTACION PARTE 1
--Ejercicio 1
conflict :: Estado -> Bool
conflict (_, []) = False
conflict (_, x:xs) =
    if null x then True
    else conflict ([], xs)

--Ejercicio 2
success :: Estado -> Bool
success (_, xs) = null xs

--Ejercicio 3
unit :: Estado -> Estado
unit (i, []) = (i, [])
unit (i, x:xs) =
    if esUnitaria x then
        let l = obtenerLiteral x
            nombre = obtenerNombre l
        in if tieneInterpretacion nombre i then
            unit (i, xs)
        else
            let nuevaI = darValor x ++ i
            in acumularClausula ([], xs) (nuevaI, [])
    else
        let (i2, xs2) = unit (i, xs)
        in (i2, x:xs2)

--Ejercicio 4
elim :: Estado -> Estado
elim (i, []) = (i, [])
elim (i, x:xs) =
    if clausulaSatisfecha x i then
        elim (i, xs)
    else
        let (i2, xs2) = elim (i, xs)
        in (i2, x:xs2)

--Ejercicio 5
red :: Estado -> Estado
red (i, []) = (i, [])
red (i, x:xs) =
    let nuevaX = reducirClausula x i
        (i2, xs2) = red (i, xs)
    in (i2, nuevaX:xs2)


--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep l (i, xs) =
    let nombre = obtenerNombre l
    in ( ((nombre, True):i, xs)
       , ((nombre, False):i, xs) )

--IMPLEMENTACION PARTE 2


--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral xs = maxLiteral (concat xs)

--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll xs =
    let arbol = construirArbolDPLL ([], xs)
    in buscarSolucion arbol

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 = undefined

--Auxilixares
esUnitaria :: Clausula -> Bool
esUnitaria [x] = True
esUnitaria xs = False

obtenerNombre :: Literal -> String
obtenerNombre (Var x) = x
obtenerNombre (Not (Var x)) = x

tieneInterpretacion :: String -> Interpretacion -> Bool
tieneInterpretacion _ [] = False
tieneInterpretacion x ((y,b):ys) = if x == y
                            then True
                            else tieneInterpretacion x ys

obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral xs = Var "foo"

--Funcion auxiliar que da un estado a una clausula unitaria
darValor :: Clausula -> Interpretacion
darValor [Var p] = [(p,True)]
darValor [Not (Var p)] = [(p,False)]

--Funcion auxiliar para acumular las clausulas que no son unitarias
acumularClausula :: Estado -> Estado -> Estado
acumularClausula (_,xs) (l2, ys) = (l2, xs ++ ys)

--Funcion auxiliar que verifica si una asignación del modelo satisface un literal
satisfaceLiteral :: Literal -> (String, Bool) -> Bool
satisfaceLiteral (Var x) (y, True) = x == y
satisfaceLiteral (Not (Var x)) (y, False) = x == y
satisfaceLiteral _ _ = False

--Funcion auxiliar que verifica si una disyuncion es verdadera
clausulaSatisfecha :: Clausula -> Interpretacion -> Bool
clausulaSatisfecha [] _ = False
clausulaSatisfecha (x:xs) i =
    if any (satisfaceLiteral x) i then True
    else clausulaSatisfecha xs i

--Funcion auxiliar que verifica si una literal es falsa en una interpretacion
literalFalsa :: Literal -> (String, Bool) -> Bool
literalFalsa (Var x) (y, False) = x == y
literalFalsa (Not (Var x)) (y, True) = x == y
literalFalsa _ _ = False

--Funcion auxiliar que elimina de una clausula las literales que son falsas
reducirClausula :: Clausula -> Interpretacion -> Clausula
reducirClausula [] _ = []
reducirClausula (x:xs) i =
    if any (literalFalsa x) i then
        reducirClausula xs i
    else
        x : reducirClausula xs i


--Funcion auxiliar que cuenta las veces que aparece una literal en una lista
contar :: Literal -> [Literal] -> Int
contar _ [] = 0
contar x (y:ys) =
    if x == y then 1 + contar x ys
    else contar x ys


--Funcion auxiliar que regresa la literal que se repite mas veces en una lista
maxLiteral :: [Literal] -> Literal
maxLiteral [x] = x
maxLiteral (x:xs) =
    let m = maxLiteral xs
    in if contar x (x:xs) >= contar m (x:xs) then x else m

--Funcion para devolver el segundo elemento de una tupla
segundoElemento :: (a, b) -> b
segundoElemento (_, y) = y

--Funcion para construir el arbol dpll desde un estado dado
construirArbolDPLL :: Estado -> ArbolDPLL
construirArbolDPLL estado
    | conflict estado = Node estado Void
    | success estado = Node estado Void
    | (segundoElemento propuesto) /= (segundoElemento estado) = Node estado (construirArbolDPLL propuesto)
    | otherwise = Branch estado (construirArbolDPLL izq) (construirArbolDPLL der)
    where
        propuesto = red (elim (unit (estado))) --Tratamos de aplicar unit, elim y/o red. Si hay cambios en las clausulas, se aplico alguna de las reglas.
        literal = heuristicsLiteral (segundoElemento estado) --Sacamos una literal
        (izq,der) = sep literal estado --Aplicamos separacion


--Funcion para buscar una solucion recorriendo el arbol
buscarSolucion :: ArbolDPLL -> Interpretacion
buscarSolucion Void = []

buscarSolucion (Node (i, cs) hijo) =
    if success (i, cs) then i
    else buscarSolucion hijo

buscarSolucion (Branch (i, cs) izq der) =
    let solIzq = buscarSolucion izq
    in if null solIzq then buscarSolucion der else solIzq