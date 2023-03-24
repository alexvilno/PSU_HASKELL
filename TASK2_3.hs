import Data.List
import Data.Function


list = [(1,2), (2,1), (30, 1), (20, 3)]

mostCommon = head.maximumBy(compare `on` length).group.sort

maxVacations list = mostCommon (map snd list)

test =
    let x1 = [(1,2), (2,1), (30, 1), (20, 3)]
        x2 = [(26, 1), (1, 2), (2, 2), (3, 3)]
        x3 = [(1, 12), (2, 12)]
    in maxVacations x1 == 1 &&
        maxVacations x2 == 2 &&
        maxVacations x3 == 12

main = print(test)