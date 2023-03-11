

--Bazen bölümlerde problem olabiliyor '/'bunu kulanırsan float çıkar div kullanırsan integer division yapar ve int çıkar
-- fonksiyonlar bazen bu yüzden çalışmayabiliyor. DİKKAT!!

-- defining data types

data LabFour = Hard | Easy | Normal deriving (Show)

data Springdate = March Int | April Int | May Int deriving (Show, Eq, Ord)

data Schoolmember = Student [Char] [Char] Int | Teacher [Char] [Char] [Char] | TA [Char] [Char] deriving (Show, Eq, Ord)


--Type Classes


--(show fonksiyonunu bir şeyi stinge çevirirken kullanabilirsin)

-- Eq does Equality check 
--(Not Equal Sign = '/=')

-- Ord > < operasyonlarını gerçekleştirmeye yarar

--ex : *Main> March 25 > April 1
--False

data Decision = Yes | No | Maybe deriving (Show, Eq, Ord, Read)
--               0     1     2
--So, No > Yes

------------------------------------
--Read (Stringden istenilen type'a çeviriyor)

--read "200" :: Int
--read "200" :: float



----------------------------------------
--Disjont Unions

askQuestion x = if x == "Are you taking cng 242 ?" then Yes
                 else if x == "Do you fail CNG 242 ? " then No
                 else Maybe 


react Yes = "Nice!"
react No = "Why?"
react Maybe = "To What?" 


accept Yes = "Ok!"
accept _ = "You should accept!"

                
                
toString (Student name surname id) = "I am a student in METU NCC, my name is " ++ name ++ ", my surname is " ++ surname ++ ", and my student ID is " ++ (show id)
toString (Teacher name surname department) = "My name is " ++ name ++ " " ++ surname ++ ". I am the instructor of CNG 242 in " ++ department ++ " department"
toString (TA name message) = "You have a message from your TA, " ++ name ++ ": " ++ message

--Outputs:(
-- toString (Student "John" "Smith" 321343)
--"I am a student in METU NCC, my name is John, my surname is Smith, and my student ID is 321343"

-- toString (TA "Zekican" "Hey there")
--"You have a message from your TA, Zekican: Hey there")

---------------------------------------------

--Polymorphic data types : 


--data Distance = Kilometers Float | Miles Float deriving Show 
--(Bu şekilde kullansaydık çıktılar direkt float typeında olurdu.)

-- Ama biz girdi hangi type ise çıktı da o type olmasını istiyorsak şu şekilde yazarız : 

data Distance a = Kilometers a | Miles a deriving Show

-------------------------------------------------------------------------
--Recursive Data Types

data Tree = EmptyTree | Node Integer Tree Tree deriving (Show, Eq, Ord)

-- x = 2

insertElement x EmptyTree = Node x EmptyTree EmptyTree 
insertElement x (Node y left right) = if x < y then (Node y (insertElement x left) right)   
                                      else (Node y left (insertElement x right))
                                      
                                      
-------------------------------------------------------
--Lambda Abstractions

--(\x ->x*x) 3
--9


--(\x y ->(x+y)) 2 4 
--6


--Prelude> example q p = (\x y -> ([a | a<-x,a<'o',a/='a'],[ b | b<-y,b>=2])) q p
--Prelude> example "congrats" [(-2),2,1,(-3),4,(-4),2,0]
--("cng",[2,4,2])


--(\x y -> ([a | a<-x, even a ],[b | b<-y, odd b])) [1,2,3,4,5,6,7,8] [11,12,13,14,15,16,17,18]


-- THE END


data ThreeDShapes = Sphere Float deriving (Show, Eq, Ord)

volume (Sphere r) = (3.14) * (r*r*r) * (4/3)
area (Sphere r) = (3.14) * (4) * (r*r)


isEqual x y = if x==y then "They are equal"
                      else "They are not equal"
                      
isGreater x y = if x > y then "Greater"
                else "Not"
                
-------------------------------------------

--lambda first second  = first + second 

--(\first second -> first + second) 3 5

---------------------------------------------

sort [] = []
sort (x:xs) = (sort [a|a <- xs,a<x]) ++ [x] ++ (sort [a | a <- xs, a>=x]) 

















