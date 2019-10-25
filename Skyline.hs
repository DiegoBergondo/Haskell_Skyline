module Skyline where

-- Cabecera del programa Skyline.hs

type Edificio = (Int,Int,Int)
type Coordenada = (Int,Int)
type Skyline = [Coordenada]

-- Función edificioAskyline, lo único que hace es recibir los tres valores de un edificio individual
-- y devuelve su linea de horizonte.
edificioAskyline :: Edificio -> Skyline
edificioAskyline(x,y,z) = [(x,z),(y,0)]

-- Función divide, que recibe una lista y la divide a la mitad para devolver la tupla de listas
-- de edificios ya divididas, si es impar habrá como máximo una diferencia de uno entre ambas.
-- El modo de conseguirlo es dividir la longitud de la lista entre dos con `div` ya que siempre devuelve un
-- valor entero y con ese valor se separa la primera parte de la lista, de la segunda.
divide :: [a] -> ([a],[a])
divide xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

-- Función combina, a la que se le pasan dos lineas de horizonte y devuelve una linea de horizonte calculada
-- desde las otras dos, para esto se utilizará una función auxiliar (combinaAux) y se valorarán los 6 casos
-- posibles, actualizando el valor en caso de ser necesario.
combina :: Skyline -> Skyline -> Skyline
combina xs ys = combinaAux (xs,0)(ys,0) 0
 where
 combinaAux ([],_) ([],_) val_max = []
 combinaAux (xs,_) ([],_) val_max = xs
 combinaAux ([],_) (ys,_) val_max = ys
 combinaAux ((x, xh):xs, xh_ant)((y, yh):ys, yh_ant) val_max 
	|x == y && ((max xh yh) == val_max) = combinaAux (xs, xh) (ys, yh) val_max
	|x == y && ((max xh yh) /= val_max) = (x, max xh yh):combinaAux (xs,xh) (ys,yh) (max xh yh)
	|x > y && ((max xh_ant yh)==val_max)= combinaAux((x,xh):xs,xh_ant) (ys, yh) val_max
	|x > y && ((max xh_ant yh)/=val_max)= (y, max xh_ant yh):combinaAux((x,xh):xs, xh_ant)(ys,yh) (max xh_ant yh)
	|x < y && ((max xh yh_ant)==val_max)= combinaAux(xs,xh) ((y,yh):ys, yh_ant) val_max
	|x < y && ((max xh yh_ant)/=val_max)= (x, max xh yh_ant):combinaAux(xs,xh)((y,yh):ys, yh_ant) (max xh yh_ant)
	|otherwise = [(0,0)] 
	-- Realmente el otherwise no sería necesario ya que se valoran los 6 casos posible, pero se añade por seguridad.
	
-- Función resuelveSkyline, función principal que recibe la lista de edificios y devuelve el skyline final, para
-- ello se realizará recursivamente la combinación de la primera y segunda parte de cada una de las divisiónes de 
-- las listas de dos edificios hasta que quede el skyline final que será lo que se devuelva. 	
resuelveSkyline :: [Edificio] -> Skyline
resuelveSkyline [(a,b,c)] = edificioAskyline (a,b,c)
resuelveSkyline d = combina (resuelveSkyline(fst(divide(d))))(resuelveSkyline(snd(divide(d))))