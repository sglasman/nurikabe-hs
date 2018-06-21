import Graphics.UI.Gtk
import Control.Monad
import Data.Array.MArray
import Data.Array.IO

main :: IO ()
main = do
  s <- (liftM (!!0)) initGUI
  let n = read s :: Int
  window  <- windowNew
  hbox    <- hBoxNew True 10
  checkedArray <- newArray (1, n) False :: IO (IOArray Int Bool)
  set window [windowDefaultWidth := 100*n, windowDefaultHeight := 100,
              containerBorderWidth := 10, containerChild := hbox]
  let buttonActions = map (\i -> do
                             button <- buttonNew
                             label <- labelNew . Just $ show i
                             set button [containerChild := label]
                             onClicked button $ tog button checkedArray i
                             boxPackStart hbox button PackGrow 0)
                          [1..n]
  sequence_ $ buttonActions
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
  
fontString :: Integer -> String
fontString i = "Arial Plain " ++ (show $ 10 * i)

tog :: Button -> IOArray Int Bool -> Int -> IO ()
tog button checkedArray i = do 
                         val <- readArray checkedArray i
                         putStrLn $ show val
                         if val == False then do
                                                widgetModifyBg button StateNormal (Color 0 0 0) 
                                                widgetModifyBg button StatePrelight (Color 0 0 0)
                                                writeArray checkedArray i True
                                         else do
                                                widgetModifyBg button StateNormal (Color 65535 65535 65535) 
                                                widgetModifyBg button StatePrelight (Color 65535 65535 65535)
                                                writeArray checkedArray i False
                                                