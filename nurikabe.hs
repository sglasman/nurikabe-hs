import Graphics.UI.Gtk hiding (get)
import Control.Exception
import Data.Array.IO
import Control.Monad.State
import System.Environment

type Square = Either Bool Int
type Grid = [[Square]] -- a square in a Nurikabe grid is either a clue or an empty cell which can be shaded or unshaded

data GridButton =  GridButton {button :: Button, --buttons will carry their coordinates around
                               gref :: (Int, Int)}
                               
main :: IO ()
main = do
       filename <- (!! 0) <$> getArgs -- get filename as command line argument
       grid <- gridFromString <$> readFile filename -- initialize the grid
       void $ runStateT stateGUI grid
       
stateGUI :: StateT Grid IO () -- within this function we keep track of the state of the grid
stateGUI = do
           --GUI placeholder
           liftIO $ do initGUI
                       window <- windowNew
                       onDestroy window mainQuit
                       widgetShowAll window
                       mainGUI
           grid <- get
           return ()
       
gridFromString :: String -> Grid
-- parse a grid encoded as a list of lists of Ints. 0 denotes empty square
gridFromString lists = assert (allEqual $ map length readLists) $ -- grid must be rectangular
                       map (map intToSquare) readLists -- don't allow negative integers
                       where readLists = read lists :: [[Int]]
                       
buttonsFromGrid :: Grid -> [IO GridButton]
buttonsFromGrid _ = []

buttonFromSquare :: Square -> (Int, Int) -> IO Button
buttonFromSquare _ _ = buttonNew
                     
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