module MergeSort where


--required imports


--required types


--required functions

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists [] secondRem = secondRem
mergeSortedLists firstRem [] = firstRem
mergeSortedLists (x:xs) (y:ys) | x <= y = x : mergeSortedLists xs (y:ys)
                               | otherwise = y : mergeSortedLists (x:xs) ys

exampleMsInp = [3,6,2,3,8]

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort whole = mergeSortedLists sortedFirstHalf sortedSecondHalf
  where 
    wholeLength = length whole
    halfPos = wholeLength `div` 2
    (firstHalf, secondHalf) = splitAt halfPos whole
    sortedFirstHalf = mergeSort firstHalf
    sortedSecondHalf = mergeSort secondHalf
