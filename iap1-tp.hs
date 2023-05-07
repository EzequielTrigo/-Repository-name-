-- Completar con los datos del grupo
--
-- Nombre de Grupo: 404_Name_Not_Found
-- Integrante 1: Ezequiel Trigo , ezequiel0trigo@gmail.com, 603/22
-- Integrante 2: Joaquin Piaggio, piaggiojoaquinpiaggio@gmail.com, 700/22
-- Integrante 3: Tomas Agustin Andujar Ferreira, tomas.andujar02@gmail.com, 823/22
-- Integrante 4: Francisco Nistal, franistal3@gmail.com, 905/22

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

--1)
-- esta funcion devuelve una lista de todos los usuarios que estan en la red social.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red | usuarios red == [] = []
                      | otherwise = proyectarNombres (usuarios red)

proyectarNombres :: [Usuario] -> [[Char]]
proyectarNombres us = quitarRepetidos (proyectarNombresAUX us)

proyectarNombresAUX :: [Usuario] -> [[Char]]
proyectarNombresAUX [x]    = [nombreDeUsuario(x)]
proyectarNombresAUX (x:xs) = (nombreDeUsuario (x)) : proyectarNombresAUX xs

quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [x] = [x]
quitarRepetidos (x:xs) | hayIgualesDe x xs == True = quitarRepetidos xs
                       | otherwise = x : quitarRepetidos xs 

hayIgualesDe :: (Eq t) => t -> [t] -> Bool
hayIgualesDe x  [] = False
hayIgualesDe x (y:ys) |x == y    = True
                      |otherwise = hayIgualesDe x ys


--2)
-- esta funcion devuelve la lista de amigos del usuario que se introduce
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeAUX (relaciones red) (idDeUsuario u)

amigosDeAUX :: [Relacion] -> Integer -> [Usuario]
amigosDeAUX [] _ = []
amigosDeAUX (x:xs) u | pertenece x u == True = (usuarioDelAmigo x u) : amigosDeAUX xs u
                     | otherwise = amigosDeAUX xs u

usuarioDelAmigo :: (Usuario,Usuario) -> Integer -> Usuario
usuarioDelAmigo (a,b) u |idDeUsuario a == u = b
                        |otherwise          = a

pertenece :: (Usuario,Usuario) -> Integer -> Bool
pertenece (a,b) n |idDeUsuario a == n = True
                  |idDeUsuario b == n = True
                  |otherwise          = False


--3)
-- esta funcion ...
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u) 

longitud :: [Usuario] -> Int
longitud [] = 0
longitud (x:xs)| xs == []  = 1
               | otherwise = 1 + longitud xs


--4)
-- esta funcion ...
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAUX red (usuarios red) (head(usuarios red)) 

usuarioConMasAmigosAUX :: RedSocial -> [Usuario] -> Usuario -> Usuario
usuarioConMasAmigosAUX _ [] n = n
usuarioConMasAmigosAUX r (x:xs) n | (cantidadDeAmigos r x) > (cantidadDeAmigos r n) = usuarioConMasAmigosAUX r xs x
                                  | otherwise                                       = usuarioConMasAmigosAUX r xs n


--5)
-- esta funcion ...
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | cantidadDeAmigos red (usuarioConMasAmigos red) > 1000000 = True 
                      | otherwise                                                = False


--6)
-- esta funcion ...
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,r,p) u = publicacionesDeAUX p u 

publicacionesDeAUX :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAUX [] u = []
publicacionesDeAUX (x:xs) u| autor x == u = x : publicacionesDeAUX xs u
                           | otherwise = publicacionesDeAUX xs u

autor :: Publicacion -> Usuario
autor (a,p,l) = a


--7)
-- esta funcion ...
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = undefined


--8)
-- esta funcion ...
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = undefined


--9)
-- esta funcion ...
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = undefined


--10)
-- esta funcion ...
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = undefined