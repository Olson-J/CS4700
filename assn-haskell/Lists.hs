module Lists where

  {- inputs: none
    outputs: infinite list 
    purpose: create an infinite/unbounded list of whole numbers
      starting at 1
    -}
  countingNumbers :: [Integer]
  countingNumbers = iterate (+1) 1


  {- inputs: base number (integer)
    outputs: infinite array of integers
    purpose: create an infinite array that contains multiples
      of the provided base number
    -}
  multiplesOfNumbers :: Integer -> [Integer]
  multiplesOfNumbers m = iterate (+m) m


  {- inputs: none
    outputs: infinite array of integers
    purpose: create an infinite array of woodall numbers, 
      which are defined with W[n] = n * 2^n -1
    -}
  woodallNumbers :: [Integer]
  fnct :: Integer -> Integer
  fnct x = x * 2^x - 1
  woodallNumbers = map (fnct) countingNumbers   -- apply function to every element


  {- inputs: none
    outputs: infinite array of integers
    purpose: create an infinite array of padovan numbers, 
      which are defined by P(0) = P(1) = P(2) =1 and 
      P(n) = P(n-2) + P(n-3)
    -}
  padovanNumbers :: [Integer]
  padovanNumbers = 1 : 1 : 1 : next padovanNumbers    -- specify first 3 values, then start calculations
    where
        next (x:y:z:rest) = (x + y) : next (y : z : rest)


  {- inputs: operator, two lists of integers
    outputs: list of integers
    purpose: traverse the given lists and apply the operator to
      the corresponding elements, adding the element that 'wins'
      to the output list and leaving the 'losing' element in the 
      list to be reassessed in the next recursive call 
    -}
  order :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  order _ [] ay = ay    -- base cases if one list is empty
  order _ ax [] = ax
  order op (x:ax) (y:ay) 
    | x `op` y = x : order op ax (y:ay)     -- add first element of x and start over with the rest of ax and all of ay
    | otherwise = y : order op (x:ax) ay       -- add first element of y and start over with the rest of ay and all of ax
  

  {- inputs: list of integers
    outputs: list of lists containing integer pairs
    purpose: divide the original list of integer into a list
      of lists, each of which contain a pair of integers. If 
      the original list does not have an even number of elements
      the last sublist will have only one element
    -}
  pairUp :: [Integer] -> [[Integer]]
  pairUp [] = []
  pairUp nums 
    | length nums >= 2 = take 2 nums : pairUp (drop 2 nums)
    | length nums == 1 = take 1 nums : pairUp (drop 1 nums)


  {- inputs: list of integers
    outputs: list of tuples
    purpose: create a list of tuples that contain an integer
      and the number of times it was found in a row in the original 
      input list
    -}
  runLengthEncoding :: Eq a => [a] -> [(a, Int)]    -- use Eq to let us use == on the elements
  runLengthEncoding [] = []
  runLengthEncoding (a:aa) = runLength a 1 aa   -- helper function to count number of occurences in a row
    where
      runLength num ct [] = [(num, ct)]
      runLength num ct (b:ab)
        | num == b = runLength num (ct+1) ab    -- add to the occurence count until a different int is found
        | otherwise = (num, ct) : runLength b 1 ab


  {- inputs: list of operations, list of integer pairs
    outputs: list of integers
    purpose: traverse the lists, applying an operator to the 
      corresponding integer pair and adding the result to the 
      output list. If there are more pairs than operators, loop
      back around to the start of the operator list
    -}
  listPairApply :: [a -> a -> a] -> [[a]] -> [a]
  listPairApply [] _ = []       -- base case to make sure neither array is empty
  -- apply operators to the number pairs, loop the operator array if the pair list is longer
  listPairApply opList numPairs = zipWith (\op numPairs -> loopOps op numPairs) (cycle opList) numPairs
    where
      loopOps op numPairs
        | length numPairs == 1 = head numPairs  -- if only one number in pair, return the number
        | length numPairs == 2 = op (head numPairs) (numPairs !! 1)   -- if two numbers in pair, apply operator


  {- inputs: list of functions
    outputs: integer
    purpose: build a function to compose the functions in the given
      list, assuming each takes one argument. Essentially 'nests' the
      functions.
    -}
  composeList :: [a -> a] -> (a -> a)
  composeList [] = id   -- return identity function if empty, returns unchanged input
  composeList (funct:functList) = funct . composeList functList -- compose the current function with the result(s) of the list recursively
