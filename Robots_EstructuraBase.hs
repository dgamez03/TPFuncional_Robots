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

-- Calcula el poder del robot
poder :: Robot -> Int
poder (Robot _ experiencia energia programas) = energia + (experiencia * length programas)

-- Calcula cuánta energía se pierde o gana al aplicar un programa a un robot
-- La ganancia se indica con un número negativo
daño :: Robot -> Programa -> Int
daño robot programa = energia robot - energia (programa robot)

-- Diferencia absoluta en poder entre dos robots
diferenciaDePoder :: Robot -> Robot -> Int
diferenciaDePoder robot1 robot2 = abs (poder robot1 - poder robot2)

-- PUNTO 3 - CONSULTAS SOBRE ACADEMIAS

type Academia = [Robot]

-- Consulta 1: ¿Existe algún robot llamado "Atlas" sin programas?
existeAtlasSinProgramas :: Academia -> Bool
existeAtlasSinProgramas = any (\robot -> nombre robot == "Atlas" && length (programas robot) == 0)

-- Consulta 2: ¿Todos los robots viejos son obstinados?
todosLosViejosSonObstinados :: Academia -> Bool
todosLosViejosSonObstinados = all esObstinadoSiViejo
  where
    esObstinadoSiViejo robot
      | nivelExperiencia robot > 16 = length (programas robot) > 3 * nivelExperiencia robot
      | otherwise                   = True


-- PUNTO 4 - FUNCIÓN GENERAL DE COMPARACIÓN

valorMaximo :: Ord b => (a -> b) -> [a] -> a
valorMaximo func [valor] = valor
valorMaximo func (x : siguiente : xs)
      | func x >= func siguiente = valorMaximo func (x : xs)
      | otherwise = valorMaximo func (siguiente : xs)


-- PUNTO 5 - ESTRATEGIAS DE COMBATE

-- mejorProgramaContra :: Robot -> Robot -> Programa
-- mejorOponente :: Robot -> Academia -> Robot
-- noPuedeDerrotarle :: Robot -> Robot -> Bool


-- PUNTO 6 - ROBOTS DE EJEMPLO Y PRUEBAS

-- robot1 = (...)
-- robotSinProgramas = (...)
-- Qué pasa si un robot no tiene programas?
