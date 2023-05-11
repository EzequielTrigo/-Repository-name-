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
-- esta funcion devuelve la lista de amigos del usuario que se introduce.
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
-- esta funcion devuelve la cantidad de amigos que tiene el usuario de entrada.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u) 

longitud :: [Usuario] -> Int
longitud [] = 0
longitud (x:xs)| xs == []  = 1
               | otherwise = 1 + longitud xs


--4)
-- esta funcion devuelve el usuario con la mayor cantidad de amigos en la red social.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAUX red (usuarios red) (head(usuarios red)) 

usuarioConMasAmigosAUX :: RedSocial -> [Usuario] -> Usuario -> Usuario
usuarioConMasAmigosAUX _ [] n = n
usuarioConMasAmigosAUX r (x:xs) n | (cantidadDeAmigos r x) > (cantidadDeAmigos r n) = usuarioConMasAmigosAUX r xs x
                                  | otherwise                                       = usuarioConMasAmigosAUX r xs n


--5)
-- esta funcion devuelve true si exite por lo menos un usuario con mas de un millon de amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | cantidadDeAmigos red (usuarioConMasAmigos red) > 1000000 = True 
                      | otherwise                                                = False


--6)
-- esta funcion devuelve una lista con las publicaciones hechas en la red social por el usuario de entrada.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicacionesDeAUX (publicaciones red) u 

publicacionesDeAUX :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAUX [] u = []
publicacionesDeAUX (x:xs) u| usuarioDePublicacion x == u = x : publicacionesDeAUX xs u
                           | otherwise = publicacionesDeAUX xs u


--7)
-- esta funcion devuelve una lista de las publicaciones que tienen le gustan al usuario de entrada.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_ , _ ,[]) u = []
publicacionesQueLeGustanA (us,rel,(p:ps)) u2 | leGustaLaPublicacion p u2 == True = p : publicacionesQueLeGustanA (us,rel,ps) u2
                                             | otherwise = publicacionesQueLeGustanA (us,rel,ps) u2

leGustaLaPublicacion :: Publicacion -> Usuario -> Bool
leGustaLaPublicacion (aut,pub,[]) u2 = False
leGustaLaPublicacion (aut,pub,(u:us)) u2 | u == u2 = True
                                         | otherwise = leGustaLaPublicacion (aut,pub,us) u2


--8)
-- esta funcion devuelve true si a los dos usuarios de entrada le gustan las mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = lesGustanLasMismasPublicacionesAUX (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

lesGustanLasMismasPublicacionesAUX :: [Publicacion] -> [Publicacion] -> Bool
lesGustanLasMismasPublicacionesAUX [] [] = True
lesGustanLasMismasPublicacionesAUX [] _ = False
lesGustanLasMismasPublicacionesAUX _ [] = False
lesGustanLasMismasPublicacionesAUX (x:xs) (y:ys) | x /= y = False
                                                 | otherwise = lesGustanLasMismasPublicacionesAUX xs ys


--9)
-- esta función devuelve true si a un usuario de la red social, le gusta todas las publicaciones del usuario de entrada.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = tieneUnSeguidorFielAUX (publicacionesDelUsuario (publicaciones red) u) u

tieneUnSeguidorFielAUX :: [Publicacion] -> Usuario -> Bool
tieneUnSeguidorFielAUX [] u = False
tieneUnSeguidorFielAUX (x:xs) u = busquedaDelUsuarioFiel (likesDePublicacion x) xs xs

busquedaDelUsuarioFiel :: [Usuario] -> [Publicacion] -> [Publicacion] -> Bool
busquedaDelUsuarioFiel [] _ _ = False
busquedaDelUsuarioFiel _ [] _ = True
busquedaDelUsuarioFiel (x:xs) (y:ys) (publis) | leGustaLaPublicacion y x == True = busquedaDelUsuarioFiel (x:xs) (ys) (publis)
                                              | otherwise = busquedaDelUsuarioFiel xs (publis) (publis)

publicacionesDelUsuario ::  [Publicacion] -> Usuario -> [Publicacion]
publicacionesDelUsuario [] u = []
publicacionesDelUsuario (x:xs) u | usuarioDePublicacion x == u = x : publicacionesDelUsuario xs u
                                 | otherwise = publicacionesDelUsuario xs u


--10)
-- esta funcion devuelve true si el segundo usuario de entrada, es amigo inderectamente (o directamente), del primer usuario de entrada.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 | u2EsAmigo (amigosDe red u1 ) u2 == True = True
                                  | otherwise = cadena red [u1] u2 [] 

u2EsAmigo :: [Usuario] -> Usuario -> Bool
u2EsAmigo [] _ = False
u2EsAmigo (x:xs) u2 | x == u2 = True
                    | otherwise = u2EsAmigo xs u2 

-- comentar 
cadena :: RedSocial -> [Usuario] -> Usuario -> [Usuario] -> Bool
cadena red [] _ _ = False
cadena red (x:xs) u2 revisados | u2EsAmigo (amigosDe2 red (x:xs)) u2 = True
                               | otherwise = cadena red (quitarRevisados(amigosDe2 red (x:xs)) revisados) u2 (revisados ++ (x:xs))

quitarRevisados :: [Usuario] -> [Usuario] -> [Usuario]
quitarRevisados _ [] = []
quitarRevisados us (x:xs) | perteneceARevisados x us == True = quitarRevisados (quitarTodos x us) xs
                          | otherwise = quitarRevisados us xs

amigosDe2 :: RedSocial -> [Usuario] -> [Usuario]
amigosDe2 red (x:xs) = quitarRepetidos (amigosDe2Aux red (x:xs) )

amigosDe2Aux :: RedSocial -> [Usuario] -> [Usuario]
amigosDe2Aux red [] = []
amigosDe2Aux red (x:xs) = (amigosDe red x) ++ (amigosDe2Aux red xs)

quitarTodos :: Usuario -> [Usuario] -> [Usuario]
quitarTodos y []=[]
quitarTodos y (x:xs)| y == x = quitarTodos y xs
                    | otherwise = x : quitarTodos y xs

perteneceARevisados :: Usuario -> [Usuario] -> Bool
perteneceARevisados y [] = False
perteneceARevisados y (x:xs) | y==x = True
                             |otherwise = perteneceARevisados y xs