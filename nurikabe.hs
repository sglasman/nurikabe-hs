import Graphics.UI.Gtk
import Control.Exception
import Data.Array.IO
import Control.Monad.State
import System.Environment

type Square = Either Bool Int
type Grid = [[Square]] -- a square in a Nurikabe grid is either a clue or an empty cell which can be shaded or unshaded
type Coord = (Int, Int)

data GridButton =  GridButton {button :: Button, --buttons will carry their coordinates around
                               gref :: Coord}
                               
main :: IO ()
main = do
       filename <- (!! 0) <$> getArgs
       grid <- gridFromString <$> readFile filename
       trackingArray <- newListArray ((0, 0), (height grid, width grid)) $ concat grid
                        :: IO (IOArray Coord Square)
       initGUI
       window <- windowNew
       set window [windowTitle := "Nurikabe", containerBorderWidth := 18,
                   windowDefaultWidth := 36 * (width grid + 1), windowDefaultHeight := 36 * (height grid + 1)]
       table <- tableNew (height grid) (width grid) True
       onDestroy window mainQuit
       widgetShowAll window
       mainGUI
       
createElement :: (Coord, Square) -> Table -> IOArray Coord Square -> IO () -- create either a button or a label
createElement (coord, Left _) table trackingArray = do -- create a button
                                                    button <- buttonNew
                                                    on button buttonActivated $ buttonClick coord trackingArray
                                                    tableAttachAt coord table button

createElement (coord, Right n) table _ = do -- create a label
                                         label <- labelNew . Just $ show n
                                         tableAttachAt coord table label
                                                      
tableAttachAt :: WidgetClass a => Coord -> Table -> a -> IO ()
tableAttachAt (i, j) table widget = tableAttachDefaults table
                                                        widget
                                                        (j - 1)
                                                        j
                                                        (i - 1)
                                                        i

buttonClick :: Coord -> IOArray Coord Square -> IO ()
buttonClick _ _ = putStrLn ""
                                                        
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

coordArray :: Int -> Int -> [[Coord]]
coordArray height width = map (\y -> [(y, x) | x <- [1..width]]) [1..height]