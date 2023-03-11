funca 1 = [1]
funca x = funca (x-1) ++ [x]

-------------------

funcb x 0 = 1
funcb x y =x * (funcb x (y-1))

----------------------

funcc 5 = 1-5
funcc 10 = 10 / (10 ** 2)
funcc x = x

------------------------

funcd [] = 0
funcd (x:xs) = (if (elem x "aeıou" == True) then 1 else 0)  + funcd xs

--------------------------

letternum letter = if letter == 'A' then 4
                   else if letter == 'B' then 3
                   else if letter == 'C' then 2
                   else if letter == 'D' then 1
                   else 0

calccredit [] [] = 0
calccredit (x:xs) (y:ys) = (((letternum x) * y) + calccredit xs ys)
funce list1 list2 = (calccredit list1 list2) / sum(list2)

-----------------------------



