
solveQEq (a, b, c)
    | a == 0 = ( (-c) / b ) : []
    | d > 0 = (-b + sqrt d) / (2 * a) : (-b - sqrt d) / (2 * a) : []
    | d < 0 = []
    | d == 0 = (-b) / (2 * a) : []
        where d = b ** 2 - 4 * a * c

test :: Bool
test = 
    let x1 = (1, 2, 3) 
        x2 = (1, 4, 3)
        x3 = (0, 5, 1)
        x4 = (1, 4, 4)
    in solveQEq x1 == [] &&
        solveQEq x2 == [-1.0, -3.0] &&
        solveQEq x3 == [-0.2] &&
        solveQEq x4 == [-2.0]

main = print (test)