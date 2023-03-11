--1-) Using strong typing and write a function to compute the length of a list.

lengthy::[a]->Int
lengthy [] =0 
lengthy (x:xs) = 1 + lengthy xs

--2-) Use strong typing to search for given element in a list.

searchy::(Eq a) => [a]->a->String
searchy [] l = "not in list"
searchy (x:xs) l= if (x==l) then "In the list" else searchy xs l

--3-) Use strong typing and write a function to compute Fibonacci Numbers

fibo::Int->Int
fibo 1 = 1
fibo 2 = 1
fibo n = fibo (n-1) + fibo (n-2)

--4-) Use type definitions to write a function which will give the area of a circle, recatangle and square

data Shapes= Rectangle Float Float |Circle Float | Square Float deriving Show
areas (Rectangle a b) = a*b
areas (Square a) = a*a
areas (Circle a) = (3.14)*a*a

--5-) Higher order functions

-- let add x y = x+y
-- add 4 5
--9
-- let inc = add 1
-- inc 6
--7
-- map inc [1,2,3,4,5,6]
--[2,3,4,5,6,7]



----------------------------------

--  filter even [1,2,3,4,5,6]
--  [2,4,6]

-------------------------------

--WEEK 4 PART 4 VİDEOSUNDA DK 14-15 GİBİ ÇOK 
--GÜZEL BİR EGZERSİZ VAR BAK ONA SINAVDAN ÖNCE


----------------------------------------
--Lambda example

fun1::Int->Int->Int
fun1 a b= (\x y->x+y) a b * (\x y ->x-y) a b

-----------------------------------------------------------