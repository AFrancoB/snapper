-- haskell compiler online
-- https://www.tutorialspoint.com/compile_haskell_online.php

main =  do
    putStrLn "minor Scale: "
    putStrLn $ show $ contornoToScale [0.001,0.3,0.4, 0.7, 0.5, 0.9, 1] [0,2,3,5,7,8,10,12]
    putStrLn "major Scale: "
    putStrLn $ show $ contornoToScale [0.001,0.3,0.4, 0.7, 0.5, 0.9, 1] [0,2,4,5,7,9,11,12]
    return ()

-- [0.0,3.6,10.8,8.4,12.0]
-- contornos deben de expresarse de 0 a 1. 0 siendo la raiz y 1 la raiz "una octava arriba"

contornoToScale:: [Rational] -> [Rational] -> [Double] 
contornoToScale contornoMelodico scale =
    let contornoScaled = map (\x -> x * (last scale)) contornoMelodico
        snapToNewScale = map (\x -> snapFunction scale x) contornoScaled
    in map (\x -> realToFrac x :: Double) snapToNewScale



snapFunction:: [Rational] -> Rational -> Rational
snapFunction grid target =
    let ceilFloor =  snapToGrid grid target
        ceil = snd ceilFloor
        floor = fst ceilFloor
    in if (target - floor) < (ceil - target) then floor else ceil -- this should be a more nuanced process, perhaps with weights givent to each note

-- se debe de determinar q es una nota en este contexto y los arreglos de notas deben de determinarse q tipo de informacion contienen con un datatype
-- type Note = (Rational,Rational)

snapToGrid:: [Rational] -> Rational -> (Rational,Rational)
snapToGrid grid target 
    | target == (last grid) = (last grid, last grid)
    | target == 0 = (0,0)
    | otherwise =
    let first = last $ filter (\x -> x < target) grid
        second = head $ filter (\x -> x > target) grid 
    in (first,second)
