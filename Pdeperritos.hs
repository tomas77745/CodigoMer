import Text.Show.Functions ()

data Perrito = UnPerrito {
    raza :: String,
    jugueteFavorito :: [String],
    tiempo :: Int,
    energia :: Int
} deriving Show

mapRaza :: (String -> String) -> Perrito -> Perrito
mapRaza fn unPerrito = unPerrito {raza= fn.raza $ unPerrito} 

mapjugueteFavorito :: ([String]->[String]) -> Perrito -> Perrito
mapjugueteFavorito fn unPerrito = unPerrito {jugueteFavorito= fn.jugueteFavorito $ unPerrito}

mapTiempo :: (Int -> Int) -> Perrito -> Perrito
mapTiempo fn unPerrito = unPerrito {tiempo= fn.tiempo $ unPerrito}

mapEnergia :: (Int -> Int) -> Perrito -> Perrito
mapEnergia fn unPerrito = unPerrito {energia= fn.energia $ unPerrito}

type Guarderia = (String, [Actividades])
type Actividades = (Ejercicio, Int)
type Ejercicio = (Perrito -> Perrito)

jugar :: Ejercicio
jugar unPerrito 
|energia unPerrito - 10 < 0 = modificarEnergia 0 unPerrito
|otherwise = mapEnergia (substract 10) unPerrito

ladrar :: Ejercicio
ladrar unPerrito ladridos = mapEnergia (ladridos/2) unPerrito

regalar :: Ejercicio 
regalar unJuguete unPerrito = mapjugueteFavorito (++ [unJuguete]) unPerrito

diaDeSpa :: Ejercicio
diaDeSpa tiempoEnElSpa unPerrito
| tiempoEnElSpa > 50 || extravagante unPerrito = modificarEnergia 100. regalar "peine de goma" $ unPerrito
|otherwise = id

modificarEnergia :: Int -> Perrito -> Perrito
modificarEnergia unNumero unPerrito = unPerrito {energia= unNumero}

extravagante :: Perrito -> Boll
extravagante = raza unPerrito == "dalmata" || raza unPerrito == "pomerania"

diaDeCampo :: Ejercicio
diaDeCampo = mapjugueteFavorito tail unPerrito

zara :: Perrito
zara = UnPerrito "dalmata" ["pelota", "mantita"] 90 80

guarderiaPdePerritos :: Guarderia 
guarderiaPdePerritos = ("PdePerritos",[(jugar, 30), (ladrar 18, 20), (regalar "pelota", 0), (diaDeSpa, 120), (diaDeCampo,720)])

--parte B

puedeEstarEnUnaGuarderia :: Perrito -> Guarderia -> Bool
puedeEstarEnUnaGuarderia unPerrito unaGuarderia = tiempo unPerrito > tiempoDeUnaGuarderia unaGuarderia

tiempoDeUnaGuarderia :: Guarderia -> Int
tiempoDeUnaGuarderia (_,[(_,tiempoMinimo)]) = sum.map $ tiempoMinimo

esPerroResponsable :: Perrito -> Bool
esPerroResponsable = (>3).(length jugueteFavorito).diaDeCampo

realizaRutina :: Guarderia -> Perrito -> Perrito
realizaRutina unaGuarderia unPerrito
|puedeEstarEnUnaGuarderia unPerrito unaGuarderia = (\unasActividades -> unasActividades unPerrito)
|otherwise = id

perrosCansados :: Guarderia -> [Perrito] -> [Perrito]
perrosCansados unaGuarderia unosPerritos = map.filter (energia unPerrito <5).realizaRutina $ unosPerritos

--parte C 

perritoPi :: Perrito
perritoPi = UnPerrito "Labrador" ["soguita"..] 314 159

{-
1. Si, es posible saber si Pi es extravagante o no, ya que es un parametro comparable
> extravagante perritoPi
2. 
> elem "huesito" (jugueteFavorito perritoPi)
Es posible encontrar el elemento huesito solo si esta antes de que la lista rompa por Overflow, 
debido al Lazy Evaluation que utiliza haskell por lo que no necesita recorrer la lista completa
> elem "pelota" (jugueteFavorito.guarderiaDePerritos $ perritoPi)
En este caso, al agregarse el juguete "pelota" al final de la lista lo que va a ocurrir es que
no va a llegar nunca al final, debido a los infinitos elementos agregados antes
> elem ("soguita" 31112) perritoPi
Por la manera en la que esta modelada la lista de juguetesFavoritos ([String]), no es posible buscar ese elemento
debido a que las listas son Homogeneas y no podria buscar un elemento que sea (String, Int)
3. Si, es posible que Pi realize una rutina debido a que la unica condicion para que pueda realizarla 
o no es el tiempo que permanecera en la guarderia que tiene como tipo de dato Int, el cual es comparable
> realizaRutina guarderiaPdePerritos perritoPi
4.Si le regalamos un hueso a Pi, agregaria el hueso al final de la lista pero a la hora de mostrarlo 
en la consola no terminaria nunca, ya que primero mostraria las infinitas soguitas
> regalar "hueso" perritoPi
-}