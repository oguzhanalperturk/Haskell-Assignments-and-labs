-- I read and accept the submission rules and the extra rules. This is my own work that is done by myself only.
--Oğuzhan Alpertürk 2315752


module Qfour (lastncompare, capitalizeFirstLast, verifier) where
import Data.List (words)
import Data.Char (toLower,toUpper,isUpper,isLower,isNumber)


--Q1

makestrlower [] = []
makestrlower str = [toLower (head str)] ++ makestrlower (tail str)


takelastn str num outputstr = if ((length str) >= num ) then if (num == 0) then makestrlower outputstr 
                                                             else (takelastn (init str) (num-1) ([last str] ++ outputstr))
                              else makestrlower str


lastncompare str1 str2 num = (takelastn str1 num []) == (takelastn str2 num [])    

------------------------------------------------------------------------------------

--Q2


capFirstLastofWord originalstr [] strlen = []
capFirstLastofWord originalstr str strlen = if ((length originalstr) == strlen) then [toUpper (head str)] ++ capFirstLastofWord originalstr (tail str) (strlen-1)
                                            else if (strlen == 1) then [toUpper (head str)] ++ capFirstLastofWord originalstr (tail str) (strlen-1)
                                            else [toLower (head str)] ++ capFirstLastofWord originalstr (tail str) (strlen-1)


recursivelycapFirstLastofWord [x] = capFirstLastofWord x x (length x)
recursivelycapFirstLastofWord list = (capFirstLastofWord (head list) (head list) (length (head list))) ++ " " ++ (recursivelycapFirstLastofWord (tail list))

capitalizeFirstLast [] = []
capitalizeFirstLast str = recursivelycapFirstLastofWord (words str)

-------------------------------------------------------------------------------------

--Q3

counter [] lcasenum ucasenum numbernum = isSatisfy lcasenum ucasenum numbernum
counter password lcasenum ucasenum numbernum = if (isLower (head password) == True) then counter (tail password) (lcasenum+1) ucasenum numbernum
                                               else if (isUpper (head password) == True) then counter (tail password) lcasenum (ucasenum+1) numbernum
                                               else if (isNumber (head password) == True) then counter (tail password) lcasenum ucasenum (numbernum+1)
                                               else counter (tail password) lcasenum ucasenum numbernum
                                               
isSatisfy lcasenum ucasenum numbernum = if (lcasenum >=2 && ucasenum >= 2 && numbernum >= 1) then True
                                        else False
                                             
verifier password = if ((length password) > 10 || (length password) < 7) then False
                    else counter password 0 0 0


---------------------------------------------------------------------------------------

--Q4








