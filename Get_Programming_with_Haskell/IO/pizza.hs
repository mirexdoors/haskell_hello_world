-- вычисление плозади окружности
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size/2)^2

-- для представления пиццы используем пару (размер, стоимость)
type Pizza = (Double, Double)

--вычисление стомости квадратного сантиметра
costPerCm :: Pizza -> Double
costPerCm (size, cost) = cost / areaGivenDiameter size

--сравнение двух пицц лдля получения более дешевой
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costPerCm p1 < costPerCm p2
                      then p1
                      else p2

-- сообщаем пользователю результат
describePizza :: Pizza -> String
describePizza (size, cost) = "Пицца размера " ++ show size ++ " дешевле по цене " ++ show costSqCm ++ " за квадратный сантиметр"
  where costSqCm = costPerCm (size, cost)

main :: IO ()
main = do
  putStrLn "Введите размер первой пиццы:"
  size1 <- getLine
  putStrLn "Введите стоимость первой пиццы"
  cost1 <- getLine
  putStrLn "Введите размер второй пиццы:"
  size2 <- getLine
  putStrLn "Введите стоимость второй пиццы:"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)
