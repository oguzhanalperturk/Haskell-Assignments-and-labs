isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if (x > y) then False
                    else isSorted (y:xs)
                    
swap [x,y] = [y,x]

bubble [x,y] = if x > y then swap [x,y] else [x,y]
bubble (x:y:xs) = if x > y then [y] ++ (bubble (x:xs))
                  else [x] ++ (bubble (y:xs))
                  
bubbleSort x = if (isSorted x) then x
               else bubbleSort (bubble x)