main :: IO()
main = do
    print $ controller "" == ""
    print $ controller ".........." == "0000000000"
    print $ controller "P...." -- == "12345"
    print $ controller "P.P.." -- == "12222"
    print $ controller "..P...O..." -- == "0012343210"
    print $ controller "P......P......" -- == "12345554321000"
    print $ controller "P.P.P...." -- == "122234555"
    print $ controller ".....P.P........P...." -- == "000001222222222234555" 
    print $ controller ".........." -- == "0000000000"
    print $ controller "P.." -- == "123"
    print $ controller "P...." -- == "12345"
    print $ controller "P......P......" -- == "12345554321000"
    print $ controller "P.P.." -- == "12222"
    print $ controller "P.P.P...." -- == "122234555"
    print $ controller ".....P.P........P...." -- == "000001222222222234555"
    print $ controller ".....P......P.P..P...." -- == "0000012345554333321000"
    print $ controller "P.O...." -- == "1210000"
    print $ controller "P......P.O...." -- == "12345554345555"
    print $ controller "P..OP..P.." -- == "1232222100"
    print $ controller "P......P..OP..P..." -- == "123455543233334555"
    print $ controller "..P...O....." -- == "001234321000"



-- имаме хелпър със дигитална стойност на състоянието която се обновява при извикване на помощната ф-ция.
-- имаме "коефициент" който се променя при срещането на буква Р или О и който варира между -1, 0, 1
-- имаме гардове за '.', 'р' и 'о', 5 секунди за пълното движение на вратата

controller :: String -> String
controller events = helper events 0 0
  where
    helper [] _ _ = []
    helper (x:xs) position step
      | x == 'P' && position == 0 = show (position + 1) ++ helper xs (min 5 (position + 1)) 1
      | x == 'P' && position == 5 = show (position + step) ++ helper xs position (-1)
      | x == 'P' && position > 0 = show (position + step) ++ helper xs position 0
      | x == 'O' = show position ++ helper xs (5 - position) (-1)
      | otherwise = show position ++ helper xs (position + step) step
