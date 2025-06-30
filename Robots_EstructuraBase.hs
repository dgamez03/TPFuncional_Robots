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
  " Robot: " ++ show nombre ++ 
  " Energia: " ++ show energia ++ 
  " Cantidad de programas: " ++ show (length programas)

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
--El proposito de valorMaximo es buscar el "mejor" elemento según el criterio definido por func.

valorMaximo :: Ord b => (a -> b) -> [a] -> a
valorMaximo func [valor] = valor
valorMaximo func (x : siguiente : xs)
      | func x >= func siguiente = valorMaximo func (x : xs)
      | otherwise = valorMaximo func (siguiente : xs)

-- PUNTO 5 - ESTRATEGIAS DE COMBATE

-- mejorProgramaContra :: Robot -> Robot -> Programa
-- Elige el programa del segundo robot que cause mayor reducción de energía al primero.

mejorProgramaContra :: Robot -> Robot -> Programa
mejorProgramaContra victima atacante =
  foldl1 (\programa1 programa2 -> if daño victima programa1 > daño victima programa2 then programa1 else programa2) (programas atacante)

-- mejorOponente :: Robot -> Academia -> Robot
-- Encuentra el robot con la mayor diferencia de poder respecto al robot recibido.

mejorOponente :: Robot -> Academia -> Robot
mejorOponente robot academia = 
  foldl1 (\robot1 robot2 -> if diferenciaDePoder robot robot1 > diferenciaDePoder robot robot2 then robot1 else robot2) academia

-- noPuedeDerrotarle :: Robot -> Robot -> Bool
-- Tras aplicar todos los programas que conoce al segundo robot, la energía del primero quede igual que antes
noPuedeDerrotarle :: Robot -> Robot -> Bool
noPuedeDerrotarle atacante _ =
  energia atacante == energia (foldl (\robot programa -> programa robot) atacante (programas atacante))

-- PUNTO 6 - ROBOTS DE EJEMPLO Y PRUEBAS

-- Robot con programas variados
robot1 :: Robot
robot1 = Robot {
    nombre = "EVA 02",
    nivelExperiencia = 5,
    energia = 80,
    programas = [recargaBateria 15, descargaElectrica]
}

-- Robot sin programas
robotSinProgramas :: Robot
robotSinProgramas = Robot {
    nombre = "EVA 01",
    nivelExperiencia = 3,
    energia = 40,
    programas = []
}

-- Otro robot para probar estrategias
robot2 :: Robot
robot2 = Robot {
    nombre = "EVA 03",
    nivelExperiencia = 8,
    energia = 100,
    programas = [descargaElectrica, olvidarProgramas 1]
}

-- Aplicar el primer programa de robot1 a robot2
robot2Modificado :: Robot
robot2Modificado = (head (programas robot1)) robot2

-- Simular interacción de un robot con otro, aplicando todos sus programas
simularInteraccion :: Robot -> Robot -> Robot
simularInteraccion atacante objetivo = foldl (\r prog -> prog r) objetivo (programas atacante)

-- Probar simulación
ejemploCombate :: Robot
ejemploCombate = simularInteraccion robot1 robot2

-- Mostrar resultados para prueba manual (en GHCi)
-- mostrarRobot robot1
-- mostrarRobot robot2
-- mostrarRobot ejemploCombate

