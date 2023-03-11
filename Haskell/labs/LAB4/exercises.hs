--Q1

helper char 0 = []
helper char rep = [char] ++ helper char (rep-1)
repli [] num = []
repli list num = (helper (head list) num) ++ (repli (tail list) num) 

---------------------------------------------------------------------------

--Q2

compress [] = []
compress [a] = [a]
compress list = if((head list) == (head (tail list))) then compress (tail list)
                else [head list] ++ compress (tail list) 
                
-----------------------------------------------------------------------------

--Q3

--example q p = (\x y -> ([a | a<-x,a<'o',a/='a'],[ b | b<-y,b>=2])) q p

--(\x y ->(x+y)) 2 4 
--6

--(\x y -> ([a | a<-x, even a ],[b | b<-y, odd b])) [1,2,3,4,5,6,7,8] [11,12,13,14,15,16,17,18]





----------------------------------------------------------
--Q4

data ThreeDShapes = Cube Float | Cylinder Float Float deriving (Show, Eq, Ord) 


volume (Cube x) = x*x*x

volume (Cylinder r h) = (3.14) * (r*r) * h

surfaceArea (Cube x) = 6*(x*x)

surfaceArea (Cylinder r h) = (2*(3.14)*r*h) + (2*(3.14)*(r*r))

-------------------------------------------------------------------
--Q5

data ThreeDShapes a = Cube a | Cylinder a a deriving (Show, Eq, Ord, Read) 


volume (Cube x) = x*x*x

--volume (Cylinder r h) = (3.14) * (r*r) * h

surfaceArea (Cube x) = 6*(x*x)

--surfaceArea (Cylinder r h) = (2*(3.14)*r*h) + (2*(3.14)*(r*r))



-------------------------------------------------------------------
--Q6

data Tree = EmptyTree | Node Integer Tree Tree deriving (Show, Eq, Ord)

insertElement x EmptyTree = Node x EmptyTree EmptyTree -- BASE CASE
insertElement x (Node a left right) = if x == a -- DO NOTHING
then (Node x left right)
 else if x < a -- INSERT TO LEFT 
 then (Node a (insertElement x left) right)
 else -- INSERT TO RIGHT
 Node a left (insertElement x right)

inserter [x] = insertElement x EmptyTree  
inserter (x:xs) = insertElement (x) (inserter xs)  

-------------------------------------------------------------------
--Q7

minOf (Node int left right) = if(left /= EmptyTree) then (minOf left)
                              else int 

------------------------------------------------------------------------
--Q8

isEmpty EmptyTree = True
isEmpty _ = False

-------------------------------------------------------------------------
--Q9

searchElement num (Node int left right) = if(num == int) then True
                                          else if(num > int) then searchElement num right
                                          else searchElement num left
                                          
searchElement num EmptyTree = False

--------------------------------------------------------------------------

























