findFloor :: [Char] -> Int
findFloor string = findFloorHelper 0 string

findFloorHelper :: Int -> [Char] -> Int
findFloorHelper curFloor []		= curFloor
findFloorHelper curFloor ('(':xs)	= findFloorHelper (curFloor + 1) xs
findFloorHelper curFloor (')':xs)	= findFloorHelper (curFloor - 1) xs
findFloorHelper _ _ 			= error "what the shit is this"
