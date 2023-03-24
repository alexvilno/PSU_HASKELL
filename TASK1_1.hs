factorial :: Integer -> Integer
factorial x = if x == 0
    then 1 
    else x * factorial(x - 1)

withRepeats :: (Integer, Integer) -> Integer
withRepeats (n,k) = factorial (n + k - 1) `div` (factorial (k) * factorial (n - 1))

test :: Bool 
test = 
    let x1 = (16, 2) 
        x2 = (10, 14)
        x3 = (322, 111)
        x4 = (900, 99)
        x5 = (1, 1)
        x6 = (2, 0)
    in withRepeats x1 == 136 &&
        withRepeats x2 == 817190 &&
        withRepeats x3 == 3567758874928703917474752067915918930616206659321915014222576373359082839961120729229500009894750264391200 &&
        withRepeats x4 == 5752298371739200922225091093875880565879540603049849709575388848785979909437018193596484175837630621109857730804814972774557857059195049900 &&
        withRepeats x5 == 1 &&
        withRepeats x6 == 1

main = print (test)
