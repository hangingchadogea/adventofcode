import Data.Char (ord)
import Data.Maybe (fromJust)
import Debug.Trace (trace)


main = putStrLn $ head $ filter isAllowed $ successorPasswords "hxbxwxba"

intToChar :: Int -> Char
intToChar value
  | value >= 0 && value <=25 = ['a'..'z'] !! value
  | otherwise = error ("Fuck off with your " ++ show value)

charToInt :: Char -> Int
charToInt char
  | char >= 'a' && char <= 'z' = ((ord char) - 97)
  | otherwise = error ("Fuck off with your " ++ show char)

intToPassInner :: [Char] -> Int -> Int -> [Char]
intToPassInner curString 8 0 = curString
-- Pad with leading 'a's until it's 8 characters long.
intToPassInner curString charsSoFar 0 = intToPassInner ('a':curString) (charsSoFar + 1) 0
intToPassInner curString charsSoFar value = intToPassInner newString (charsSoFar + 1) quotient
  where (quotient, remainder) = quotRem value 26
        newChar = intToChar remainder
	newString = newChar:curString

intToPass :: Int -> [Char]
intToPass value = intToPassInner [] 0 value

passToInt :: [Char] -> Int
passToInt string = passToIntInner string 0

passToIntInner :: [Char] -> Int -> Int
passToIntInner [] curValue = curValue
passToIntInner (x:xs) curValue = passToIntInner xs ((26 * curValue) + (charToInt x))

successorPasswords :: [Char] -> [[Char]]
successorPasswords password = map intToPass [(passToInt password)..]

data StraightState = StraightFinished | StraightProgress Int deriving (Eq, Show)
data ValidationState =  ValidationState
      { remainder      		:: [Char]
      , charsSoFar     		:: Int
      , pairsMade     		:: [Char]
      , lastChar           	:: Maybe Char
      , straight 		:: StraightState
      } deriving Show

data Validation = Okay | FailsRule Int
  deriving (Eq, Show)

isAllowed :: [Char] -> Bool
isAllowed password = validatePassword password == Okay

validatePassword :: [Char] -> Validation
validatePassword string  = validatePasswordInner ValidationState {
    remainder = string
  , charsSoFar = 0
  , pairsMade = []
  , lastChar = Nothing
  , straight = StraightProgress 0
  }

validatePasswordInner :: ValidationState -> Validation
validatePasswordInner oldState
  -- | trace (show oldState) False = undefined
  | (charsSoFar oldState) >= 6 && (length $ pairsMade oldState) < 1 = FailsRule 3
  | (not isStraightFinished) && (((charsSoFar oldState) - straightProgress) > 5) = FailsRule 1
  | null (remainder oldState) && (length $ pairsMade oldState) < 2 = FailsRule 3
  | null (remainder oldState) = Okay
  | x == 'i' || x == 'l' || x == 'o' = FailsRule 2
  -- The first character...
  | (charsSoFar oldState) == 0 = validatePasswordInner newState {
        straight = StraightProgress 1
    }
  -- If we have made a pair...
  | x == fromJust (lastChar oldState) && notElem x (pairsMade oldState) = validatePasswordInner newState {
      pairsMade = x:(pairsMade oldState)
      }
  -- If we have advanced a straight but not completed it...
  | (not isStraightFinished) && straightProgress < 2 && straightContinues = validatePasswordInner newState {
        straight = StraightProgress (straightProgress + 1)
	}
  -- If we have finished the straight...
  | (not isStraightFinished) && straightProgress >= 2 && straightContinues = validatePasswordInner newState {
        straight = StraightFinished
	}
  | otherwise = validatePasswordInner newState
  where (x:xs) = remainder oldState
        isStraightFinished = (straight oldState) == StraightFinished
        StraightProgress straightProgress = straight oldState
	straightContinues = consecutiveChars (fromJust $ lastChar oldState) x
	nextStraight = if isStraightFinished then StraightFinished else (StraightProgress 1)
        newState = oldState {
            remainder = xs
          , charsSoFar = (charsSoFar oldState) + 1
          , lastChar = Just x
	  , straight = nextStraight
	}

consecutiveChars :: Char -> Char -> Bool
consecutiveChars c1 c2 = ord(c2) == ord(c1) + 1
