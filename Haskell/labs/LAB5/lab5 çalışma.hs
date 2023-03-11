--Curried Functions

--max 4 5
--5

--(max 4) 5
--5

myexample x y z = x+y+z
--12

--(((myexample 3) 4) 5) şeklinde yapıyor fonksiyon 

test = (5 /) 
-- test 3
--1.6666666666666667
-- test 5
--1.0
-- test 19
--0.2631578947368421
-- test 10
--0.5

test1 = (/5)

-- test1 10
--2.0



lastdigit a = rem a 10

-- lastdigit 15
--5

lastdigit1 = (`rem` 10)

-- lastdigit1 15
--5

--------------------------------------------------------
--Where 

mywherefunction x = x + secondfunction 2 3
                        where secondfunction y z = (y+z)/2
                        
--mywherefunction 2
--4.5
--secondfunction 2 3

-- error:
 --Variable not in scope: secondfunction :: Integer -> Integer -> t
 
 --------------------------------------------------------------
 
 --let .... in

myletinfunction x = let secondfunction y z = (y+z)/2
                    in x + secondfunction 2 3
                    
-- myletinfunction 2
--4.5

--------------------------------------------------------------
--Map function

-- :t map
--map :: (a -> b) -> [a] -> [b]

square a = a * a

-- map square [1,2,3]
--[1,4,9]

-- :t even
--even :: Integral a => a -> Bool

-- map even [1,2,3]
--[False,True,False]

---------------------------------------------------

--Filter

-- :t filter
--filter :: (a -> Bool) -> [a] -> [a]

-- filter even [1,2,3,4,5,6]
--[2,4,6]


----------------------------------------------

-- zipWith

-- :t zipWith
--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]


-- zipWith (+) [1,2,3] [2,3,4]
--[3,5,7]

-----------------------------------------------------

--foldr and foldl

-- :t foldr
--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b


power x y = x**y

-- :t power
--power :: Floating a => a -> a -> a

-- foldr power 1 [2,3]
--8.0

-- :t foldl
--foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- foldl power 1 [2,3]
--1.0




