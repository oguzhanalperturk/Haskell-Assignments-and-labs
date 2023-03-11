--LİSTS
listfunction [] = "I am an empty list"
listfunction [x] = "I have one element:"++[x]
listfunction (x:xs) = x : " is my first element and others are: " ++ xs

example (x:y:ys) = x:" " ++ [y] ++ " " ++ ys

ex a@(x:y:ys) = a

--RECURSİON

factorial 0 = 1
factorial x = factorial (x-1) * x

--RANGES
--[1..5] ---> [1,2,3,4,5]
--['a'..'z'] --> "abcd...yz"
--take 5 [1..] --> [1,2,3,4,5]  (lazy evaluation)
--take 3 (repeat 5) --> [5,5,5]  repeat works with single element

--replicate 4 6 --> [6,6,6,6]

-- cycle [1,2,3] cycle works with lists Same as repeat.

--drop 2 [1,2,3,4,5,6] --> [3,4,5,6]

--drop 2 (cycle [2,3,4]) --> [4,2,3,4,2,3,4,2,3,4,2,3...] 
--2 ve 3 ü droplayıp 4ten başlıyor ama yine sonsuza kadar gidiyor


--let newlist = [1,2,3,4,5,6,7]
--[a|a<-newlist, even a]
--[2,4,6]

--[a|a<-newlist, even a, a/=4]
--[2,6] even numbers except 4

myexample [] = []
myexample (x:xs) = if (even x) && (x/=4) then x:(myexample xs)
                   else myexample xs


-- :type 'a'
--Typeını söylüyor
-- :type = :t






