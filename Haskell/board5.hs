-- I read and accept the submission rules and the extra rules. This is my own work that is done by myself only.
 
--Part 1  
   
--Calculating parents in the given list 

findparent [] [] op = []
findparent [] (x:y:xs) op = []
findparent beforelist [] op = []
findparent beforelist (x:y:xs) op = if(x /= 0 || y /= 0) then if (op == '+') then if ((x + y) == (head beforelist) || (head beforelist) == 0) then [x+y] ++ (findparent (tail beforelist) xs op)  
                                                                                  else [(head beforelist)] ++ (findparent (tail beforelist) xs op)                             
                                                              else if(op == '*') then if ((x*y) == (head beforelist) || (head beforelist) == 0) then [x*y] ++ (findparent (tail beforelist) xs op)
                                                                                      else [(head beforelist)] ++ (findparent (tail beforelist) xs op)  
                                                                                                                          
                                                              else [(head beforelist)] ++ (findparent (tail beforelist) xs op) 
                                    else [head beforelist] ++ (findparent (tail beforelist) xs op)


verticecalc [] lastoflist op = [lastoflist]
verticecalc remainlist lastoflist op = (verticecalc (init remainlist) (findparent (last remainlist) lastoflist op) op) ++ [lastoflist]  

listwithoutzeros list op = verticecalc (init list) (last list) op




--Deleting Children of Error Nodes which is (Summation or multiplication of children nodes don't equal to the parent node)

arrangezeros [] [] op = []
arrangezeros (x:xs) (y:z:ys) op = if (op == '+') then if ((y+z) /= x) then [0,0] ++ (arrangezeros xs ys op)  
                                                      else [y,z] ++ (arrangezeros xs ys op)
                                  else  if ((y*z) /= x) then [0,0] ++ (arrangezeros xs ys op)  
                                        else [y,z] ++ (arrangezeros xs ys op)                        
                                                           
arrzerosrecursively [] op = []
arrzerosrecursively [x] op = []
arrzerosrecursively (x:y:xs) op = [arrangezeros x y op] ++ (arrzerosrecursively ([arrangezeros x y op]++xs) op) 


-- Getting the calculated list and it is ready to place in binary tree

calculatedlist list op = [head (listwithoutzeros list op)]  ++ (arrzerosrecursively (verticecalc (init list) (last list) op) op)


-- defining the tree data type and insert operation recursively

data Tree a = EmptyTree | NotFullBinary | Leaf a | Node a (Tree a) (Tree a) deriving (Show,Eq,Ord)




insert [0,0] (Node a left right) = Leaf a
insert [0,0] (Leaf a) = if (a == 0) then (Node a (Leaf 0) (Leaf 0))
                        else Leaf a
insert [x,y] (Leaf a) = (Node a (Leaf x) (Leaf y))
insert list (Node a left right) = (Node a (insert (take (div (length list) 2) list) left) (insert (drop (div (length list) 2) list) right)) 




inserter [[x]] = Leaf x
inserter [x,y] = Node (head x) (Leaf (head y)) (Leaf (last y))
inserter list = insert (last list) (inserter (init list))


--by doing size check, I am controlling if given list satisfies binary tree features or not

binarytreecontrol [x] counter = counter
binarytreecontrol [] counter = counter
binarytreecontrol (x:y:xs) counter =  if (((length x)*2) == (length y)) then binarytreecontrol xs counter 
                                      else binarytreecontrol xs (counter + 1)

--numcontrol : controls numbers' match with each other

--numcontrol (x:xs) (y:z:ys) op = if (op == '+') then if (x /= 0 && y /= 0 && x == y + z) then 0
                           --     else  




positivetree list op = if ((binarytreecontrol list 0) == 0) then if ((head (head (list))) /= 0) then (Leaf (head (head list)))
                                                                 else inserter (calculatedlist list op)
                       else NotFullBinary



----------------------------------------------------------------------

--Part 2

generateheight (NotFullBinary) = 0
generateheight (Leaf a) = 1
generateheight (Node a left right) =  (max (generateheight (left)) (generateheight(right)))+1


-----------------------------------------------------------------------

--Part 3

levelweight (NotFullBinary) x = 0
                
levelweight (Node a left right) x = if(x == 1) then a 
                                    else 0 + (levelweight left (x-1)) + (levelweight right (x-1))                               
 
levelweight (Leaf a) x = if(x == 1) then a
                         else 0

------------------------------------------------------------------------

--Part 4

-- First I am finding total number of nodes because after I traverse the tree
-- I can detect that if searched node exist in the tree or not.

countnodes (Leaf a) = 1
countnodes (Node a left right) = 1 + countnodes left + countnodes right 

findnode (Leaf a) num = 1

findnode (Node a left right) num = if (a == num) then 0
                                   else 1 + findnode left num + findnode right num


data Info a = StoppedEarly | NodeNotFound | Found a deriving (Show,Eq,Ord)
 

-- İf traversed number of nodes are less than total one that means the searched nodes exist in the tree 
-- 1->Exist  0-> Not Exist

isExist (Leaf a) num = if ((countnodes (Leaf a)) > (findnode (Leaf a) num)) then 1
                       else 0

isExist (Node a left right) num = if ((countnodes (Node a left right)) > (findnode (Node a left right) num)) then 1
                                  else 0

findtree (Leaf a) num = if (a == num) then (Leaf a)
                        else EmptyTree

findtree (Node a left right) num = if (a == num) then (Node a left right)
                                   else if ((isExist left num) == 1) then (findtree left num)
                                        else if ((isExist right num) == 1) then (findtree right num)
                                        else EmptyTree



traversetree (Node a left right) direction = if ((length direction) > 0) then if ((head direction) == 'L') then (traversetree left (tail direction)) 
                                                                              else (traversetree right (tail direction))
                                             else Found a



traversetree (Leaf a) direction = if ((length direction) > 0) then StoppedEarly
                                  else Found a



manualtraversal (Leaf a) num direction = if (a /= num) then NodeNotFound
                                         else if ((length direction) > 0) then StoppedEarly 
                                              else Found a

manualtraversal (Node a left right) num direction = if ((findtree (Node a left right) num) == EmptyTree) then NodeNotFound
                                                    else if ((generateheight (findtree (Node a left right) num)) <= (length direction)) then StoppedEarly
                                                         else traversetree (findtree (Node a left right) num) direction                





















    