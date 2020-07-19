-- Author: Hannah Moore
-- Date: 03/04/2020
-- Description: This program includes the a factorial function using pattern matching,
-- 				a factorial fucntion using guards, palindrome function, index function, quicksort function,
-- 				split function, merge function, mergesort (driver) function and mergesort (recursive) function.

main = do
	putStr "5! (pattern matching) = "
	print (factorial1 5)
	putStr "5! (guards) = "
	print (factorial2 5)
	putStr "Is 'hannah' a palindrome? "
	print (palindrome "hannah")
	putStr "Is 'car' a palindrome? "
	print (palindrome "car")
	putStr "Index of 'a' in 'now is the time for all good men': "
	print(index 'a' "now is the time for all good men")
	putStr "Index of 'z' in 'now is the time for all good men': "
	print(index 'z' "now is the time for all good men")
	putStr "Index of 15 in [-12, 98, 76, 8, -25, 70, -24, 69, 15, 0, 10, 33, -14]: "
	print(index 15 [-12, 98, 76, 8, -25, 70, -24, 69, 15, 0, 10, 33, -14])
	putStr "Quicksort: "
	print(quicksort [-12, 98, 76, 8, -25, 70, -24, 69, 15, 0, 10, 33, -14, -8, -3, -9, 33, 49, 86, 75, 92, 100, 29, 88, -54, 38, 78, 75, 45, -27])
	putStr "Split list:"
	print(split [8,70,86,92,100,2,56,63,94,99,102])
	putStr "Merge: "
	print(merge [8, 70, 86, 92, 100] [2, 56, 63, 94, 99, 102])
	putStr "Mergesort: "
	print(mergesort [-12, 98, 76, 8, -25, 70, -24, 69, 15, 0, 10, 33, -14, -8, -3, -9, 33, 49, 86, 75, 92, 100, 29, 88, -54, 38, 78, 75, 45, -27])

-- Purpose: Computes the factorial of Integer n (n!) using pattern matching
-- Parameters: Takes in an Integer
-- Returns: Returns an Integer value of n!
factorial1 :: (Integral a) => a -> a
factorial1 0 = 1
factorial1 n = n*factorial1(n-1)

-- Purpose: Computes the factorial of Integer n (n!) using guards
-- Parameters: Takes in an Integer
-- Returns: Returns an Integer. (value of n!)
factorial2 :: (Integral a) => a -> a
factorial2 a
	| a > 0 = a*factorial2(a-1)
	| a == 0 = 1
	| otherwise = -1

-- Purpose: Checks if the input is equivalent to its reserved self. 
-- Parameters: Takes in a list
-- Returns: Returns a Bool. (True if the parameter is a palindrome and False otherwise)
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome (x:xs) -- x equals the first element, xs equals everything else
	| x /= last xs = False
	| x == last xs = palindrome(init xs)

-- Purpose: Finds the index at which the parameter exists.
-- Parameters: Takes in an atom and a list of the same type.
-- Returns: Returns an Integer. (Positive integer indicates the index where the atom waas found. Negative integer indicates atom was not found.)
index :: (Eq a) => a -> [a] -> Int
index a [] = (-1000000)
index a (x:xs)
	| a == x = 0
	| otherwise = 1+(index a xs)

-- Purpose: Sorts an unsorted list using quicksort method.
-- Parameters: Takes in a list
-- Returns: Returns sorted list
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallSort = quicksort [a | a <- xs, a <= x]  
        bigSort = quicksort [a | a <- xs, a > x]  
    in  smallSort ++ [x] ++ bigSort

-- Purpose: Splits a list into two approximately equal-sized halves.
-- Parameters: Takes in a list
-- Returns: Returns two lists.
split :: [a] -> ([a],[a])
split myList = splitAt (((length myList) + 1) `div` 2) myList

-- Purpose: Combines two sorted list into one sorted list.
-- Parameters: Takes in two sorted lists of the same type
-- Returns: Returns a merged, sorted list.
merge :: (Ord a) => [a] -> [a] -> [a]
merge as [] = as 
merge [] bs = bs
merge (a:as) (b:bs)
        | a < b = a:merge as (b:bs)
        | otherwise = b:merge bs (a:as)

-- Purpose: Sorts a list by merging the two halves recursively.
-- Parameters: Takes in two lists of the same type
-- Returns: Returns a merged, sorted list.
mergesort' :: (Ord a) => [a] -> [a] -> [a]
mergesort' as [] = as 
mergesort' [] bs = bs
mergesort' as bs =  
        merge (mergesort' ax ay) (mergesort' bx by)
        where (ax,ay) = split(as)
              (bx,by) = split(bs)

-- Purpose: Calls recursive mergesort on a split list.
-- Parameters: Takes in a list of any type
-- Returns: Returns a one sorted list
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort a =
        mergesort' halfone halftwo
        where (halfone, halftwo) = split(a) -- set splits return values to halfone and halftwo
