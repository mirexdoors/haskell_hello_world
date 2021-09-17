{-# LANGUAGE FlexibleContexts #-}
{-
Нам необходимо написать программу, которая заменит метки их номером (в виде «[1]», «[2]», ...), а ссылки — текстом «см. [1]». Из нашего образца должно получиться:

Введение[1]. Как известно, чушь бывает разная: зеленая (см. [2]) и красная (см. [3]).
...
Как мы уже отмечали (см. [1]), чушь бывает разная, и именно поэтому жизненно важно ...
...
Чушь красная[2].
Не следует путать с чушью зеленой (см. [3]).
...
Чушь зеленая[3].
Радикально отличается от чуши красной (см. [2]).
...
-}
module Main where
import Data.List
import Text.Regex.Posix
import Control.Arrow

main = do
   txt <- readFile "sample.txt"
   let numbering = collectLabels txt
   let result = replaceLabels numbering txt
   writeFile "sample.out.txt" result


collectLabels txt = let matches = (txt =~ "@label\\{([^}]*)\\}" :: [[String]] ) in map (!!1) matches

replaceLabels labelList = lines >>> map words >>> map (map process) >>> map unwords >>> unlines
   where process w | isInfixOf "@label{" w = replace ""     w
                   | isInfixOf "@ref{"   w = replace "see " w
                   | otherwise             = w

         -- Эта функция заменяет ссылку/метку в слове `w' на ее [номер], предваряя [номер] строкой `prepend'
         replace prepend w =
           -- Слово "Введение@label{aaaa}." будет разбито на части "Введение", "aaa" и ".",
           -- из которых мы потом соберем результат.
           let [[_, prefix, label, suffix]] = ( w =~ "(.*)@.*\\{([^}]*)\\}(.*)" :: [[String]] )
               in prefix ++ prepend ++ "[" ++ idx label ++ "]" ++ suffix

         -- Возвращает позицию `l' в списке `labelList' (нумерация начинается с 1).
         idx l = case elemIndex l labelList of
                      Just x  -> show (x+1)
                      Nothing -> error $ "Label "++l++" is not defined"
