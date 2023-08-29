quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

main = do
    print ("Input 10 Integers")

    print ("Int 1: ")
    temp <- getLine
    let int1 = read temp :: Int

    print ("Int 2: ")
    temp <- getLine
    let int2 = read temp :: Int

    print ("Int 3: ")
    temp <- getLine
    let int3 = read temp :: Int

    print ("Int 4: ")
    temp <- getLine
    let int4 = read temp :: Int

    print ("Int 5: ")
    temp <- getLine
    let int5 = read temp :: Int

    print ("Int 6: ")
    temp <- getLine
    let int6 = read temp :: Int

    print ("Int 7: ")
    temp <- getLine
    let int7 = read temp :: Int

    print ("Int 8: ")
    temp <- getLine
    let int8 = read temp :: Int

    print ("Int 9: ")
    temp <- getLine
    let int9 = read temp :: Int

    print ("Int 10: ")
    temp <- getLine
    let int10 = read temp :: Int

    let input = [int1, int2, int3, int4, int5, int6, int7, int8, int9, int10]

    print ("The sorted list is:")
    print (quicksort input)