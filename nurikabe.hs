import Graphics.UI.Gtk
import Control.Exception
import Data.Array.IO
import Control.Monad.State

type Square = Either Bool Int
type Grid = [[Square]] -- a square in a Nurikabe grid is either a clue or an empty cell which can be shaded or unshaded

data GridButton =  GridButton {button :: Button, --buttons will carry their coordinates around
                               gref :: (Int, Int)}
                               
main :: IO ()
main = do
       filename <- (!! 0) <$> initGUI -- get filename as command line argument
       grid <- gridFromString <$> readFile filename
       trackingArray <- newListArray ((0, 0), (height grid, width grid)) $ concat grid
                        :: IO (IOArray (Int, Int) Square)
       putStrLn "Placeholder"
       
gridFromString :: String -> Grid
-- parse a grid encoded as a list of lists of Ints. 0 denotes empty square
gridFromString lists = assert (allEqual $ map length readLists) $ --grid must be rectangular
                       map (map intToSquare) readLists --don't allow negative integers
                       where readLists = read lists :: [[Int]]
                       
allEqual :: Eq a => [a] -> Bool --test whether all elements of a list are equal
allEqual [] = True
allEqual [x] = True
allEqual (x:xs) = allEqual xs && (x == head xs)

intToSquare :: Int -> Either Bool Int
intToSquare n = assert (n >= 0) $
                if n == 0 then Left False else Right n
                
height :: Grid -> Int
height = length

width :: Grid -> Int
width = length . (!! 0)