-- I read and accept the submission rules and this is my own work that is done by myself only

--Q1
helperf num1 op num2 = if (op == 'x') then product [(min num1 num2) .. (max num1 num2)]
                       else if (op == '.') then sum [(min num1 num2) .. (max num1 num2)]
                       else 0
                       
calculate num1 op num2 = if (op == '+' || op == 's') then (num1 + num2) 
                          else if (op == '-' || op == 'r') then (num1 - num2)
                          else if (op == '*' || op == 'm') then (num1 * num2)
                          else if (op == '/' || op == 'd') then (num1 / num2) 
                          else if (op == 'x' || op == '.') then helperf num1 op num2
                          else 0
-----------------------------------------------------
--Q2
evaluate [] op = if(op == "+" || op == "s" || op == "r" || op == "-") then 0
                 else 1
evaluate [a] op = a
evaluate list op = if(op == "+" || op == "s") then (head list) + (evaluate (tail list) "+") 
                       else if(op == "-" || op == "r") then (head list) - (evaluate (tail list) "-")
                       else if(op == "*" || op == "m") then (head list) * (evaluate (tail list) "*")
                       else if(op == "/" || op == "d") then (head list) / (evaluate (tail list) "/") 
                       else if (op == "x") then calculate (head list) 'x' (evaluate (tail list) op)
                       else if (op == ".") then calculate (head list) '.' (evaluate (tail list) op)
                       else 0

-----------------------------------------------------
--Q3

polynomial [] x op = 0
polynomial [a] x op = a               
polynomial list x op = if (op == "+" || op == "s") then ((head list) * (x ** (fromIntegral(length list) - 1)) + (polynomial (tail list) x op))
                       else if (op == "-" || op == "r") then ((head list) * (x ** (fromIntegral(length list) - 1)) - (polynomial (tail list) x op))
                       else if (op == "*" || op == "m") then ((head list) * (x ** (fromIntegral(length list) - 1)) * (polynomial (tail list) x op))
                       else if (op == "/" || op == "d") then ((head list) * (x ** (fromIntegral(length list) - 1)) / (polynomial (tail list) x op))
                       else if (op == "x") then calculate ((head list) * (x ** (fromIntegral(length list) - 1))) 'x' (polynomial (tail list) x op)
                       else if (op == ".") then calculate ((head list) * (x ** (fromIntegral(length list) - 1))) '.' (polynomial (tail list) x op)
                       else 0
                       
-----------------------------------------------------
--(Bonus)

bonus [a] x [b] = a
bonus [a] x [] = a
bonus [] x op = 0     
bonus list x [] = 0         
bonus list x op = if (head op == '+' || head op == 's') then ((head list) * (x ** (fromIntegral(length list) - 1)) + (bonus (tail list) x (tail op)))
                  else if (head op == '-' || head op == 'r') then ((head list) * (x ** (fromIntegral(length list) - 1)) - (bonus (tail list) x (tail op)))
                  else if (head op == '*' || head op == 'm') then ((head list) * (x ** (fromIntegral(length list) - 1)) * (bonus (tail list) x (tail op)))
                  else if (head op == '/' || head op == 'd') then ((head list) * (x ** (fromIntegral(length list) - 1)) / (bonus (tail list) x (tail op)))
                  else if (head op == 'x') then calculate ((head list) * (x ** (fromIntegral(length list) - 1))) 'x' (bonus (tail list) x (tail op))
                  else if (head op == '.') then calculate ((head list) * (x ** (fromIntegral(length list) - 1))) '.' (bonus (tail list) x (tail op))
                  else 0
                  
-----------------------------------------------


