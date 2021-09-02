lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5
