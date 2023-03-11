--Q1

countNumbersRecursive [] = 0     
countNumbersRecursive (x:xs) = if((elem x (['a'..'z'] ++ ['A'..'Z'])) == True) then 0 + (countNumbersRecursive xs)
                               else if (x == ' ') then 0 + (countNumbersRecursive xs)
                               else 1 + (countNumbersRecursive xs)
                               
                               
countNumbersListComp list = length[a| a<-list, not(elem a (['a'..'z'] ++ ['A'..'Z'])), a /= ' ']
 
-------------------------------------------------------------------------------
--Q2

sumEquation 0 = 6
sumEquation j = (((j*j) + 6) / ((2*j) + 1)) + sumEquation (j-1)

-------------------------------------------------------------------------------
--Q1

mapQuestion xs = map f xs where f x = x * 2 + 3


lambdaQuestion xs = foldr (\x y -> x + y) 1 xs


---------------------------------------------------------------------------------
--Q2

setUnion list [] = list
setUnion list (y:ys) = if((elem y list) == True) then setUnion list ys
                       else setUnion list ys ++ [y]
                       

setIntersection list [] = []
setIntersection list (y:ys) = if((elem y list) /= True) then setIntersection list ys
                              else setIntersection list ys ++ [y]
                              
                              
setDifference [] list = []
setDifference (y:ys) list = if((elem y list) == True) then setDifference ys list
                            else [y] ++ setDifference ys list 



setRest list list1 = setDifference (list ++ list1) (setIntersection list list1)










