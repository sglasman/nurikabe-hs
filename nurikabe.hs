import Graphics.UI.Gtk
import Control.Exception
import Data.Array.IO
import Control.Monad.State
import System.Environment

data ShadeState = Unshaded | Neutral | Shaded
type Square = Either ShadeState Clue -- a square in a Nurikabe grid is either a clue or an empty cell which can be marked shaded, unshaded or neutral
type Clue = Int
type Grid = [[Square]] 
type Coord = (Int, Int)
             
colorWhite :: Color
colorWhite = Color 65535 65535 65535

colorBlack :: Color
colorBlack = Color 0 0 0
             
main :: IO ()
main = do
       filename <- (!! 0) <$> getArgs
       grid <- gridFromString <$> readFile filename
       trackingArray <- newListArray ((1, 1), (height grid, width grid)) $ concat grid :: IO (IOArray Coord Square) -- populate tracking array
       let zippedGrid = zipWith zip (coordArray (height grid) (width grid)) grid -- zippedGrid :: [[(Coord, Square)]]
       initGUI
       window <- windowNew
       set window [windowTitle := "Nurikabe", containerBorderWidth := 18,
                   windowDefaultWidth := 36 * (width grid + 1), windowDefaultHeight := 36 * (height grid + 1)]
       table <- tableNew (height grid) (width grid) True
       containerAdd window table
       sequence_ . (map $ createElement table trackingArray) . concat $ zippedGrid
       onDestroy window mainQuit
       widgetShowAll window
       mainGUI
       
createElement :: Table -> IOArray Coord Square -> (Coord, Square) -> IO () -- create either a button or a label
createElement table trackingArray (coord, Left _) = do -- create a button
                                                    button <- buttonNew
                                                    on button buttonActivated $ buttonClick button coord trackingArray
                                                    tableAttachAt coord table button

createElement table _ (coord, Right n) = do -- create a label
                                         label <- labelNew . Just $ show n
                                         tableAttachAt coord table label
                                                      
tableAttachAt :: WidgetClass a => Coord -> Table -> a -> IO ()
tableAttachAt (i, j) table widget = tableAttachDefaults table
                                                        widget
                                                        (j - 1)
                                                        j
                                                        (i - 1)
                                                        i

buttonClick :: Button -> Coord -> IOArray Coord Square -> IO ()
buttonClick button coord trackingArray = do 
                                         square <- readArray trackingArray coord
                                         case square of  -- cycle through shading states
                                              Left Neutral -> do
                                                              writeArray trackingArray coord $ Left Shaded
                                                              buttonChangeColor button colorBlack
                                              Left Shaded -> do
                                                             writeArray trackingArray coord $ Left Unshaded
                                                             buttonChangeColor button colorWhite
                                                             buttonSetLabel "."
                                              Left Unshaded -> do
                                                               writeArray trackingArray coord $ Left Neutral
                                                               buttonSetLabel ""
                                                            
buttonChangeColor :: Button -> Color -> IO ()
buttonChangeColor button color = do widgetModifyBg button StateNormal color 
                                    widgetModifyBg button StatePrelight color
                                    
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
                if n == 0 then Left Neutral else Right n
                
height :: Grid -> Int
height = length

width :: Grid -> Int
width = length . (!! 0)

coordArray :: Int -> Int -> [[Coord]]
coordArray height width = map (\y -> [(y, x) | x <- [1..width]]) [1..height]