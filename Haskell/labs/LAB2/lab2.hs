
fonk1 [] = 0
fonk1 (a:[]) = 0
fonk1 mylist = sum mylist - ((head mylist) + (last mylist)) 

body list = drop 1 (init list) 

fonk3 list1 list2 = max (maximum list1) (maximum list2)

fonk4 list1 list2 = (maximum(list1),maximum(list2))
 


                  
fonk5 item list = if((elem item list) == True) then print(list)
                  else print(list ++ [item])
           
           
fonk6 (a,b,c) = c

fonk7 (a,b) (c,d) = sqrt ((c-a)**2 + (d-b)**2)



fonk8 cls reg over = if cls == 1 then reg*10
                     else if cls == 2 || cls == 3 then (reg+over*1.5)*7
                     else reg*5 + over*(5*2)
                     
                     
                    
--salary hour c overtime = if c==1 then hour*10
                        -- else if c==2 || c== 3 then (hour+overtime*1.5)*7
                        -- else hour*5 + overtime*(5*2)