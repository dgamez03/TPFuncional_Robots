-- ESTRUCTURA BASE
-- Modelar robots que pueden modificar a otros robots mediante programas.

-- Cada robot tiene:
-- - Un nombre
-- - Un nivel de experiencia
-- - Una cantidad de energía
-- - Una lista de programas (cada uno transforma un robot en otro robot)

-- Representación de un Programa
type Programa = Robot -> Robot

-- Representación de un Robot
data Robot = Robot {
    nombre :: String,
    nivelExperiencia :: Int,
    energia :: Int,
    programas :: [Programa]
}

-- Mostrar información básica de un robot
mostrarRobot :: Robot -> String
mostrarRobot (Robot nombre _ energia programas) =
  "Robot: " ++ show nombre ++ 
  " / Energía: " ++ show energia ++ 
  " / Cantidad de programas: " ++ show (length programas)

-- PUNTO 1 - PROGRAMAS BÁSICOS

-- Aumenta la energía en una cantidad específica
recargaBateria :: Int -> Programa
recargaBateria cantidad robot = robot { energia = energia robot + cantidad }

-- Quita 10 puntos de energía si tiene más de 10, si no, la divide a la mitad
descargaElectrica :: Programa
descargaElectrica robot
    | energia robot > 10 = robot { energia = energia robot - 10 }
    | otherwise = robot { energia = energia robot `div` 2 }

-- Elimina los primeros N programas del robot
olvidarProgramas :: Int -> Programa
olvidarProgramas n robot = robot { programas = drop n (programas robot) }

-- Si no tiene programas, lanza error
-- El autoataque no puede llamarse así mismo
-- El robot se ataca a sí mismo con su primer programa

autoAtaque :: Programa
autoAtaque robot
    | null (programas robot) = error "El robot no tiene programas para autoatacarse."
    | head (programas robot) == autoAtaque = error "autoAtaque no puede ejecutarse a sí mismo"
    | otherwise = (head (programas robot)) robot


-- PUNTO 2 - FUNCIONES AUXILIARES SOBRE ROBOTS

-- Calcular el poder del robot 
poder :: Robot -> Int

--Calcula la fuerza de un robot sumando su energía más el producto de su nivel de experiencia por la cantidad de programas que tiene.

dano :: Robot -> Programa -> Int

--Calcula cuánta energía se pierde o gana al aplicar un programa a un robot. La ganancia se indica con un número negativo. La función retorna 0 si no hay cambio.

diferenciaDePoder :: Robot -> Robot -> Int

-- PUNTO 3 - CONSULTAS SOBRE ACADEMIAS

-- type Academia = [Robot]

-- PUNTO 4 - FUNCIÓN GENERAL DE COMPARACIÓN

-- valorMaximo :: (...)


-- PUNTO 5 - ESTRATEGIAS DE COMBATE

-- mejorProgramaContra :: Robot -> Robot -> Programa
-- mejorOponente :: Robot -> Academia -> Robot
-- noPuedeDerrotarle :: Robot -> Robot -> Bool


-- PUNTO 6 - ROBOTS DE EJEMPLO Y PRUEBAS

-- robot1 = (...)
-- robotSinProgramas = (...)
-- Qué pasa si un robot no tiene programas?
