main = putStrLn $ show $ snapToGrid [0,2,4,5,7,9,11,12] 8


-- snapFunction:: [Rational] -> Rational -> Rational
-- snapFunction grid target =
--     let ceilFloor =  snapToGrid [0,2,4,5,7,9,11,12] target
--         ceil = last ceilFloor
--         floor = last $ init ceilFloor
--     in if (target - floor) < (ceil - target) then floor else ceil


type Note = (Rational,Rational)

snapToGrid:: [Rational] -> Rational -> (Rational,Rational)
snapToGrid grid target = 
    let first = last $ filter (\x -> x < target) grid
        second = head $ filter (\x -> x > target) grid 
    in (first,second)
