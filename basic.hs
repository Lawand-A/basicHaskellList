--Doing some operation on lists with haskell

--Find the maximum element in a List 
--e.g. maxi [1,2,-1,100,3,4] -> 100
maxi :: [Int] -> Int
maxi [] = 0
maxi (x:xs) = foldl (\x acc -> if (x > acc) then x else acc) x (xs)


--Find the minimum element in a List 
--e.g. mini [1,2,-1,100,3,4] -> -1
mini :: [Int] -> Int
mini [] = 0
mini (x:xs) = foldl (\x acc -> if (x < acc) then x else acc) x (xs)


--Calculates the length of a list of integers 
--e.g. len [1,2,3,4,100] -> 5
len :: [Int] -> Int
len [] = 0
len (x:xs) = 1 + len (xs)


--Calculates the length of a string 
--e.g. lenStr "abcfd" -> 5
lenStr :: [char] -> Int
lenStr [] = 0
lenStr (x:xs) = 1 + lenStr (xs)


--calculates the sum of a list 
--e.g. sumLst [1,2,3,4] -> 1+2+3+4 = 10
sumLst:: [Int] -> Int
sumLst [] = 0
sumLst (x:xs) = x + sumLst xs


--calculates the alternating sum of a list  
--e.g. altSum [1,2,3,4,4,6,-2,-3] -> 1-2+3-4+4-6+(-2)-(-3) = -3
altSum :: [Int] -> Int
altSum [] = 0
altSum (x:xs) = x + (-1 * altSum xs)


--Removes an element in a list
--e.g. removeElt [1,2,3,2,2,3,4] 2 -> [1,3,3,4]
removeElt :: (Eq a) =>  [a] -> a -> [a]
removeElt [] a = []
removeElt list a = filter (\x -> (x /= a)) list


--get the last element in a list 
--getLast [1,2,3,30,40,22] -> [22]
getLast :: (Eq a) =>  [a] -> [a]
getLast [] = []
getLast (x:xs) = if (xs == []) then [x] else getLast (xs)


--compares if two lists are equals
--e.g. compareList [1,2,3] [1,2,3] -> True
compareList :: (Eq a) =>  [a] -> [a] -> Bool
compareList [] [] = True
compareList x [] = False
compareList [] y = False
compareList (x:xs) (y:ys) = (x == y) && compareList (xs) (ys)


--prepend an element in a List 
-- prependElt 1 [2,3,4] -> [1,2,3,4]
prependElt :: a -> [a] -> [a]
prependElt a lst = a:lst


--appends an element at the end of a list
--e.g. lastAppend [1,2,3] 4 -> [1,2,3,4]
lastAppend [] a = [a]
lastAppend (x:xs) a = x:lastAppend xs a


--appends two lists together
--appendLists [1,2,3] [4,5,6] -> [1,2,3,4,5,6]
appendLists :: [a] -> [a] -> [a]
appendLists [] lst' = lst'
appendLists lst [] = lst
appendLists (x:xs) lst =  x:(appendLists (xs) lst) 


--reverse a list order
-- e.g. reverseList [1,2,3,4] -> [4,3,2,1]
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]


--Makes out of all odd numbers a even number by adding 1 to the odd numbers  
--e.g. makeAllEven [1,2,3,4,5] -> [2,2,4,4,6]
makeAllEven list = map (\x -> if (mod x 2 /= 0) then x+1 else x) list


--Checks if an element is in a list
--e.g. contain [1,2,3,4,40] 4 -> True
contain :: (Eq a) =>  [a] -> a -> Bool
contain [] a = False
contain (x:xs) a = if (a == x) then (True) else (if (contain (xs) a) then True else False)
     

--checks if an element already exists in a list then keep just the last one and remove all duplicates
--the contain function is the function above this one
--e.g. removeDup [1,2,3,4,3,3,3,3] -> [1,2,4,3]
removeDup :: (Eq a) =>  [a] -> [a]
removeDup [] = []
removeDup (x:xs) = if (contain xs x) then (removeDup xs) else (x:(removeDup xs))


--remove all negative numbers
--e.g. removeNeg [1,2,-4,3,-6,4] -> [1,2,3,4]
removeNeg :: [Int] -> [Int]
removeNeg list = filter (\x -> (x > 0)) list


--makes all negative numbers positive
--e.g. posNeg [1,2,-3,4,-5,6] -> [1,2,3,4,5,6]
posNeg list = map (\x -> if (x < 0) then (x * (-1)) else x) list
