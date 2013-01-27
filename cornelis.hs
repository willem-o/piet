module Main where

import Control.Arrow
import Control.Monad (when, guard)
import Codec.Picture

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V

import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Exit

data DirectionPointer = DLeft | DRight | DUp | DDown
  deriving (Show, Eq)
data CodelChooser = CLeft | CRight
  deriving (Show, Eq)
data Colour = Light Hue | Normal Hue | Dark Hue | Black | White
  deriving (Show, Eq)
data Hue = Red | Yellow | Green | Cyan | Blue | Magenta
  deriving (Show, Eq)
type ColourMap = V.Vector (V.Vector Colour)
type CodelSize = Int
type Position = (Int, Int) -- (X, Y)

data ProgramState = ProgramState {
  _directionPointer :: DirectionPointer,
  _codelChooser :: CodelChooser,
  _currentPosition :: Position
  }
                    
data ProgramConfig = ProgramConfig {
  _codelSize :: CodelSize
  }

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(a, "")] -> Just a
  _ -> Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a, b, c) = f a b c

pixelToColour :: PixelRGB8 -> Colour
pixelToColour (PixelRGB8 r g b) = case (r, g, b) of
  (255, 192, 192) -> Light Red
  (255, 0, 0) -> Normal Red
  (192, 0, 0) -> Dark Red
  (255, 255, 192) -> Light Yellow
  (255, 255, 0) -> Normal Yellow
  (192, 192, 0) -> Dark Yellow
  (192, 255, 192) -> Light Green
  (0, 255, 0) -> Normal Green
  (0, 192, 0) -> Dark Green
  (192, 255, 255) -> Light Cyan
  (0, 255, 255) -> Normal Cyan
  (0, 192, 192) -> Dark Cyan
  (192, 192, 255) -> Light Blue
  (0, 0, 255) -> Normal Blue
  (0, 0, 192) -> Dark Blue
  (255, 192, 255) -> Light Magenta
  (255, 0, 255) -> Normal Magenta
  (192, 0, 192) -> Dark Magenta
  (0, 0, 0) -> Black
  _ -> White
   
imageToColourMap :: Image PixelRGB8 -> CodelSize -> ColourMap -- V.Vector Colour
imageToColourMap img cs = to2D $ V.fromList $ map (pixelToColour . pixAt) coords
  where coords =
          [(x, y) | x <- [0..w], y <- [0..h],
                    x + y `mod` cs == 0]
        w = imageWidth img
        h = imageHeight img
        pixAt = uncurry (pixelAt img)
        to2D v =
          let (head, tail) = V.splitAt w v
          in V.cons head (to2D tail)

-- moveToEdge :: State Pos Pos
main = do
  args <- getArgs
  case args of
    ["--help"] -> help
    [fp, "-cs", n] -> something fp n
    _ -> repl
    
  where help = putStrLn $ unlines ["", "Help", "----", "hoi"]
        
        repl = putStrLn "repl"
        
        something fp n = do
          let n' = readMaybe n :: Maybe Int
          when (isNothing n') $ do
            putStrLn $ "Error while parsing " ++ n ++ " as an int."
            exitFailure
          e <- doesFileExist fp
          if (not e)
             then putStrLn $ "Can't find \"" ++ fp ++ "\"."
             else do
               img <- readImage fp
               case img of
                 Left e -> putStrLn "kon het bestand niet laden"
                 Right a -> case a of
                   ImageRGB8 i -> do
                     putStrLn "colourMap:"
                     print $ imageToColourMap i 16
                   ImageY8 i -> putStrLn "Y8"
                   ImageYF i -> putStrLn "YF"
                   ImageYA8 i -> putStrLn "YA8"
                   ImageRGBA8 i -> putStrLn "RGBA8"
                   ImageRGBF i -> putStrLn "RGBF"
                   ImageYCbCr8 i ->putStrLn "YCbCr8"