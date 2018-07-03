import Graphics.UI.Gtk
import Control.Exception
import Data.Array.IO
import Control.Monad.State
import System.Environment
import qualified Data.Set.Monad as Set

data ShadeState = Unshaded | Neutral | Shaded deriving (Eq, Show)
type Square = Either ShadeState Clue -- a square in a Nurikabe grid is either a clue or an empty cell which can be marked shaded, unshaded or neutral
type Clue = Int
type Grid = [[Square]] 
type Coord = (Int, Int)
             
colorWhite :: Color
colorWhite = Color 65535 65535 65535

colorBlack :: Color
colorBlack = Color 0 0 0

colorRed :: Color
colorRed = Color 65535 0 0

colorGreen :: Color
colorGreen = Color 0 65535 0

-- 0. The main function
             
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
       table <- tableNew (height grid + 1) (width grid) True
       containerAdd window table
       sequence_ . (map $ createElement table trackingArray window) . concat $ zippedGrid
       submitButton <- buttonNewWithLabel "SUBMIT"
       tableAttachDefaults table submitButton 0 (width grid) (height grid) (height grid + 1)
       on submitButton buttonActivated $ do
                                         result <- check <$> extractGrid trackingArray ((height grid), (width grid)) -- let's pull the grid out, since I wrote the checking functions for a Grid.
                                         widgetModifyBg window StateNormal (if result then colorGreen else colorRed)
       onDestroy window mainQuit
       widgetShowAll window
       mainGUI
       
-- 1. The solution checker

check :: Grid -> Bool -- Do we have a valid Nurikabe solution?
check grid = twoByTwoCheck grid && cluesSatisfied grid && isConnected grid
             
twoByTwoCheck :: Grid -> Bool
twoByTwoCheck grid = not . elem False . -- for each row:
                     map (not . elem True) . -- make sure there are no 2x2 shaded areas. Could be done marginally more efficiently by checking for vertical 2x1s then horizontal 1x2s thereof, but oh well
                     map (map $ isAtTwoByTwo grid) $
                     coordArray (height grid - 1) (width grid - 1)
                     
isAtTwoByTwo :: Grid -> Coord -> Bool -- check whether we're at the top left corner of a 2x2 shaded block
isAtTwoByTwo grid (i, j) = (gridLookup grid (i, j) == (Just $ Left Shaded)) &&
                           (gridLookup grid (i + 1, j) == (Just $ Left Shaded)) &&
                           (gridLookup grid (i + 1, j + 1) == (Just $ Left Shaded)) &&
                           (gridLookup grid (i, j + 1) == (Just $ Left Shaded))
                           
cluesSatisfied :: Grid -> Bool
cluesSatisfied grid = not . elem False .
                      map (isClueSatisfied grid) $ cluesInGrid grid
                      
cluesInGrid :: Grid -> [(Coord, Clue)] -- find all the clues in the grid
cluesInGrid grid = filter ((/= 0) . snd) .
                   map (\coord -> (coord, either (\_ -> 0) id $ maybe (Right 0) id (gridLookup grid coord))) . -- look up a clue from the grid; if we're out of bounds, or if it's not a clue, we get 0. no clues should be 0
                   concat $ coordArray (height grid) (width grid)
                   
isClueSatisfied :: Grid -> (Coord, Clue) -> Bool
isClueSatisfied grid (coord, n) = n == (length $ maxSpread grid [Left Unshaded, Left Neutral] coord)

maxSpread :: Grid -> [Square] -> Coord -> [Coord] -- Find the largest region of shaded/unshaded squares containing a given square. The second argument is the list of square types we can expand into.
maxSpread admissibles grid coord = snd . head . dropWhile ((> 0) . fst) $ iterate (spreadOnce admissibles grid) (1, [coord])

spreadOnce :: Grid -> [Square] -> (Int, [Coord]) -> (Int, [Coord])
-- When given a set S of coordinates, we try to expand S to neighboring unshaded/shaded squares in orthogonal directions. 
-- The first element of the output tuple is the amount of new territory acquired; the second element is the expanded set of coords.
spreadOnce grid admissibles (_, s) = (length s' - length s, s') where s' = Set.toList . Set.fromList $ (s >>= (spreadOneSquare grid admissibles ))

spreadOneSquare :: Grid -> [Square] -> Coord -> [Coord]
spreadOneSquare grid admissibles (i, j) = (i,j): (filter (\neighbor -> elem (gridLookup grid neighbor) (map Just admissibles)) $
                                    [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)])
                                                
gridLookup :: Grid -> Coord -> Maybe Square
gridLookup grid (i,j) = if (1 <= i) && (i <= height grid) && (1 <= j) && (j <= width grid) 
                        then Just $ (grid !! (i - 1)) !! (j - 1) -- indexing starting at 1 makes sense for puzzle grids!
                        else Nothing
                        
findShaded :: Grid -> [Coord]
findShaded grid = filter (\coord -> gridLookup grid coord == (Just $ Left Shaded))
                         (concat $ coordArray (height grid) (width grid))
                         
isConnected :: Grid -> Bool -- Check whether the shaded area is connected by checking that the size of the largest shaded area containing the first shaded cell is equal to the number of shaded cells.
isConnected grid = let shadeds = findShaded grid
                   in (shadeds == []) ||
                      ((length . maxSpread grid [Left Shaded] $ head shadeds) == length shadeds)
             
-- 2. Functions governing graphical appearance
       
createElement :: Table -> IOArray Coord Square -> Window -> (Coord, Square) -> IO () -- create either a button or a label
createElement table trackingArray window (coord, Left _) = do -- create a button
                                                           button <- buttonNew
                                                           on button buttonActivated $ do
                                                                                       widgetModifyBg window StateNormal colorWhite -- if the window is red or green after submitting, change it back
                                                                                       buttonClick button coord trackingArray
                                                           tableAttachAt coord table button
createElement table _ window (coord, Right n) = do -- create a label
                                                label <- labelNew . Just $ show n
                                                tableAttachAt coord table label
                                                      
tableAttachAt :: WidgetClass a => Coord -> Table -> a -> IO ()
tableAttachAt (i, j) table widget = tableAttachDefaults table
                                                        widget
                                                        (j - 1)
                                                        j
                                                        (i - 1)
                                                        i

buttonChangeColor :: Button -> Color -> IO ()
buttonChangeColor button color = do widgetModifyBg button StateNormal color 
                                    widgetModifyBg button StatePrelight color
                                    
-- 3. Functions governing interaction

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
                                                             buttonSetLabel button "x"
                                              Left Unshaded -> do
                                                               writeArray trackingArray coord $ Left Neutral
                                                               buttonSetLabel button ""                                    

-- 4. Assorted auxiliary functions
                                    
gridFromString :: String -> Grid
-- parse a grid encoded as a list of lists of Ints. 0 denotes empty square
gridFromString lists = assert (allEqual $ map length readLists) $ --grid must be rectangular
                       map (map intToSquare) readLists --don't allow negative integers
                       where readLists = read lists :: [[Int]]
                       
allEqual :: Eq a => [a] -> Bool --test whether all elements of a list are equal
allEqual [] = True
allEqual [x] = True
allEqual (x:xs) = allEqual xs && (x == head xs)

intToSquare :: Int -> Square
intToSquare n = assert (n >= 0) $
                if n == 0 then Left Neutral else Right n
                
height :: Grid -> Int
height = length

width :: Grid -> Int
width = length . (!! 0)

coordArray :: Int -> Int -> [[Coord]]
coordArray height width = map (\y -> [(y, x) | x <- [1..width]]) [1..height]

extractGrid :: IOArray Coord Square -> (Int, Int) -> IO Grid
extractGrid trackingArray (h, w) = mapM (mapM (
                                   \coord -> readArray trackingArray coord)) $ 
                                   coordArray h w 