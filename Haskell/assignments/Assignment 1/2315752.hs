-- I read and accept the submission rules and the extra rules. This is my own work that is done by myself only.
--Oğuzhan Alpertürk 2315752
 
--Part 1  
   
--Calculating parents in the given list 
-- ex -> *Main> calculatedlist [[0],[0,0],[0,2,4,5],[1,3,0,0,0,0,0,0]] '+'
---------[[15],[6,9],[4,2,4,5],[1,3,0,0,0,0,0,0]]


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


--Error node : (Summation or multiplication of error nodes doesn't equal to the parent node)
-- Some lists have error nodes. I deleted the error nodes and replaced them with zeros
--Ex : *Main> calculatedlist [[0],[0,2],[1,3,4,5]] '+'
-------------[[6],[4,2],[1,3,0,0]]
 

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


-- defining the tree data type, insert function and the function that does insert operation recursively

data Tree a = EmptyTree | NotFullBinary | Leaf a | Node a (Tree a) (Tree a) deriving (Show,Eq,Ord)


insert [0,0] (Leaf a) = if (a == 0) then (Node a (Leaf 0) (Leaf 0))
                        else Leaf a
                        
insert [x,y] (Leaf a) = (Node a (Leaf x) (Leaf y))
insert list (Leaf a) = Leaf a
insert list (Node a left right) = (Node a (insert (take (div (length list) 2) list) left) (insert (drop (div (length list) 2) list) right)) 


inserter [[x]] = Leaf x
inserter list = insert (last list) (inserter (init list))



-- The given list may not be a binary tree.
-- By doing size check, I am controlling if given list satisfies binary tree conditions or not

binarytreecontrol [x] counter = counter
binarytreecontrol [] counter = counter
binarytreecontrol (x:xs) counter =  if (((length x)*2) == (length (head xs))) then binarytreecontrol xs counter 
                                      else binarytreecontrol xs (counter + 1)



positivetree list op = if ((binarytreecontrol list 0) == 0) then inserter (calculatedlist list op)
                       else NotFullBinary



----------------------------------------------------------------------

--Part 2

generateheight (NotFullBinary) = 0
generateheight (Leaf a) = 1
generateheight (Node a left right) =  (max (generateheight (left)) (generateheight(right))) + 1


-----------------------------------------------------------------------

--Part 3

levelweight (NotFullBinary) x = 0
                
levelweight (Node a left right) x = if(x == 1) then a 
                                    else 0 + (levelweight left (x-1)) + (levelweight right (x-1))                               
 
levelweight (Leaf a) x = if(x == 1) then a
                         else 0

------------------------------------------------------------------------

--Part 4

{-- First I am finding total number of nodes because after I traverse the tree
 for finding the desired node, I can compare the number of nodes that I visited with total one.
 if total > visited then the node exists in the tree
 visited == total then searched node doesn't exist or it is leaf node  --}


countnodes (Leaf a) = 1
countnodes (Node a left right) = 1 + countnodes left + countnodes right 

findnode (Leaf a) num = 1

findnode (Node a left right) num = if (a == num) then 0
                                   else 1 + findnode left num + findnode right num


data Info a = StoppedEarly | NodeNotFound | Found a deriving (Show,Eq,Ord)
 
 
-- 1->Exist  0-> Not Exist
isExist (Leaf a) num = if ((countnodes (Leaf a)) > (findnode (Leaf a) num)) then 1
                       else 0

isExist (Node a left right) num = if ((countnodes (Node a left right)) > (findnode (Node a left right) num)) then 1
                                  else 0


-- I get the subtree that includes desired node as root node. 
--So that, I can go left or right from the desired node.

findtree (Leaf a) num = if (a == num) then (Leaf a)
                        else EmptyTree

findtree (Node a left right) num = if (a == num) then (Node a left right)
                                   else if ((isExist left num) == 1) then (findtree left num)
                                        else if ((isExist right num) == 1) then (findtree right num)
                                        else EmptyTree



-- After I get the subtree, I can traverse on it.

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






