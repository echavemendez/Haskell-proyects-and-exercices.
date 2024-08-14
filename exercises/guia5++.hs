
type (Punto2D) = (Float, Float)

prodInt :: Punto2D -> Punto2D -> Float
prodInt (a,b) (c,d) = (a*c) + (b*d)

type Año = Int
type EsBisiesto = Int

bisiesto :: Año -> Bool
bisiesto año | mod año 100 == 0 && not (mod año 400 == 0) = False
             | mod año 4 == 0 = True
             | otherwise = False


type Coordenada3d = (Float, Float, Float)

distanciaManhattan :: Coordenada3d ->Coordenada3d ->Float

distanciaManhattan (a,b,c) (a2,b2,c2) | 0 > (a-a2) + (b-b2) + (c-c2) = -((a-a2) + (b-b2) + (c-c2))
                                      | 0 < (a-a2) + (b-b2) + (c-c2) = (a-a2) + (b-b2) + (c-c2)

{-Problema telefonico-}

type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

enLosContactos :: Nombre ->ContactosTel ->Bool
enLosContactos _ [] = False
enLosContactos nuevoContacto ((nombre,telefono):xs) | nombre == nuevoContacto = True
                                                    | otherwise = enLosContactos nuevoContacto xs
                                           
eliminarContacto :: Nombre ->ContactosTel ->ContactosTel
eliminarContacto quieroEliminar ((nombre,telefono):xs) | quieroEliminar == nombre = xs
                                                       | otherwise = eliminarContacto quieroEliminar xs

agregarContacto :: Contacto ->ContactosTel ->ContactosTel
agregarContacto (nuevoContacto,nuevoTel) ys | not (enLosContactos nuevoContacto ys) = (nuevoContacto,nuevoTel):ys
                                            | otherwise = actualizarNumero (nuevoContacto,nuevoTel) ys

actualizarNumero :: Contacto ->ContactosTel ->ContactosTel
actualizarNumero (nuevoContacto,nuevoTel) [] = [(nuevoContacto,nuevoTel)]
actualizarNumero (nuevoContacto,nuevoTel) ((nombre,telefono):ys) | nuevoContacto == nombre = (nuevoContacto,nuevoTel):ys 
                                                                 | otherwise = actualizarNumero (nuevoContacto,nuevoTel) ys


{-Lockers de la facultad-}
type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
data Disponibilidad = Libre | Ocupado deriving (Eq, Show)

existeElLocker :: Identificacion ->MapaDeLockers ->Bool
existeElLocker numeroId ((idLocker1,estadoLocker1):xs) | numeroId == idLocker1 = True
                                                       | otherwise = existeElLocker numeroId xs

ubicacionDelLocker :: Identificacion ->MapaDeLockers ->Ubicacion
ubicacionDelLocker numeroId ((idLocker1,(disponibilidad,ubicacion)):xs) | numeroId == idLocker1 = ubicacion
                                                                        | otherwise = ubicacionDelLocker numeroId xs

estaDisponibleElLocker :: Identificacion ->MapaDeLockers ->Bool
estaDisponibleElLocker numeroId ((idLocker1,(disponibilidad,ubicacion)):xs) | numeroId == idLocker1 && disponibilidad == Libre = True
                                                                            | numeroId == idLocker1 && disponibilidad == Ocupado = False    
                                                                            | otherwise = estaDisponibleElLocker numeroId xs

ocuparLocker :: Identificacion ->MapaDeLockers ->MapaDeLockers
ocuparLocker numeroId ((idLocker1,(disponibilidad,ubicacion)):xs) | numeroId == idLocker1 = (idLocker1,(Ocupado, ubicacion)):xs
                                                                  | otherwise = (idLocker1,(disponibilidad,ubicacion)):ocuparLocker numeroId xs





