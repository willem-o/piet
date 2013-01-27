{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad (when)
import Control.Monad.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.RWST
import Codec.Picture

import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import Debug.Trace
import Data.Vector ((!))
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
  deriving (Show, Eq, Enum)
data ColourMap = ColourMap {
  _matrix :: V.Vector (V.Vector Colour),
  _mapWidth :: Int,
  _mapHeight :: Int
  } deriving (Show, Eq)
type CodelSize = Int
type Position = (Int, Int) -- (X, Y)
type Block = [Position]

data ProgramState = ProgramState {
  _directionPointer :: DirectionPointer,
  _codelChooser :: CodelChooser,
  _currentPosition :: Position
  } deriving (Show, Eq)
                    
data ProgramConfig = ProgramConfig {
  _codelSize :: CodelSize
  } deriving (Show, Eq)

data Error = Parse String | LoadFile String | FindFile String

data Instruction r = Push Int r
                   | Pop r  
                   | Add r  
                   | Subtract r
                   | Multiply r  
                   | Divide r 
                   | Mod r
                   | Not r
                   | Greater r  
                   | Pointer r  
                   | Switch (Int -> r)
                   | Duplicate r  
                   | Roll r  
                   | InNum r  
                   | InChar r  
                   | OutNum r  
                   | OutChar r 
                    deriving Functor

type Program = Free Instruction ()

newtype Cornelis w m a = Cornelis { runCornelis :: RWST ProgramConfig w ProgramState m a }

instance Show Error where
  show (Parse m) = "Error while parsing: " ++ m
  show (LoadFile m) = "Error while loading file: " ++ m
  show (FindFile m) = "Can't find file: " ++ m
  
makeLenses ''ColourMap
makeLenses ''ProgramState
makeLenses ''ProgramConfig

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(a, "")] -> Just a
  _ -> Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a, b, c) = f a b c

throw = left
io = lift

onMap :: ColourMap -> Position -> Bool
onMap m (i, j) =
  let w = _mapWidth m
      h = _mapHeight m
  in (0 <= i && i < w) && (0 <= j && j < h)
     
(&!) :: ColourMap -> Position -> Maybe Colour
m &! c@(i, j) =
  if onMap m c
    then Just $ _matrix m ! j ! i
    else Nothing
  
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
   
imageToColourMap :: Image PixelRGB8 -> CodelSize -> ColourMap
imageToColourMap img cs = ColourMap matrix w' (V.length matrix)
  where matrix = to2D . V.fromList $ map (pixelToColour . pixAt) coords
        coords = [(x, y) | y <- [0..pred h], x <- [0..pred w], cs |^ x, cs |^ y]
        w = imageWidth img
        h = imageHeight img
        w' = w `div` cs
        a |^ b = b `mod` a == 0 -- a divides b
        pixAt = uncurry (pixelAt img)
        
        -- Converts a 1D array to a 2D array in row order.
        to2D v = let (head, tail) = V.splitAt w' v
                 in if V.length tail < w'
                      then V.singleton head
                      else V.cons head (to2D tail)

-- Finds all the codels which are in the same colour block as c.
discoverBlock :: ColourMap -> Position -> Block
discoverBlock m c =
  let discover visited c'@(x, y) =
        if onMap m c'
          then c':concatMap (discover (S.insert c' visited)) neighbours
          else []
        where neighbours = filter p [up, down, left, right]
              p = and . sequence [(== colour) . (m &!), onMap m, (`S.notMember` visited)]
              up = (x, pred y)
              down = (x, succ y)
              left = (pred x, y)
              right = (succ x, y)
              colour = m &! c'
  in L.nub $ discover S.empty c
                                       
-- How many steps do we need to take from a to b? 'hueSteps a b' is the answer.
hueSteps a b = (fromEnum b - fromEnum a + 6) `mod` 6

lightnessSteps :: Colour -> Colour -> Maybe Int
lightnessSteps a b = case (a, b) of
  (Light _, Light _) -> Just 0
  (Normal _, Normal _) -> Just 0
  (Dark _, Dark _) -> Just 0
  (Light _, Normal _) -> Just 1
  (Normal _, Dark _) -> Just 1
  (Dark _, Light _) -> Just 1
  (Light _, Dark _) -> Just 2
  (Normal _, Light _) -> Just 2
  (Dark _, Normal _) -> Just 2
  (_, _) -> Nothing

main = do
  args <- getArgs
  case args of
    ["--help"] -> help
    [fp, "-cs", n] -> do
      v <- runEitherT $ something fp n
      case v of
        Left e -> print e >> exitFailure
        Right a -> undefined
    _ -> repl
    
  where help = putStrLn $ unlines ["", "Help", "----", "..."]
        
        repl = putStrLn "repl"
        
        something fp n = do
          let n' = readMaybe n
          when (isNothing n') $ throw $ Parse $ n ++ " as an int."
          let cs = fromJust n'
          e <- io $ doesFileExist fp
          when (not e) $ throw $ FindFile fp
          img <- io $ readImage fp
          case img of
            Left e -> throw $ LoadFile fp
            Right a -> case a of
              ImageRGB8 i -> do
                let w = imageWidth i
                    h = imageHeight i
                io $ do
                  putStrLn $ "w: " ++ show w ++ ", h: " ++ show h
                  print [(x, y) | x <- [0..w], y <- [0..h], x `mod` cs == 0, y `mod` cs == 0]
                  print $ (\(PixelRGB8 r g b) -> (r,g,b)) $ pixelAt i 0 0
                  putStrLn "colourMap:"
                  let m = imageToColourMap i cs
                  print m
                  putStrLn $ (if onMap m (0, 0) then "" else "not ") ++ "on map"
                  print $ discoverBlock m (0, 0)
              ImageY8 i -> io $ putStrLn "Y8"
              ImageYF i -> io $ putStrLn "YF"
              ImageYA8 i -> io $ putStrLn "YA8"
              ImageRGBA8 i -> io $ putStrLn "RGBA8"
              ImageRGBF i -> io $ putStrLn "RGBF"
              ImageYCbCr8 i -> io $ putStrLn "YCbCr8"
              
testMap = ColourMap {
  _matrix = V.fromList [V.fromList [Normal Blue,Normal Blue,Normal Blue,Dark Blue,Dark Blue,Dark Blue,Black,Normal Green,Normal Green,Black],V.fromList [Normal Blue,Normal Blue,Normal Blue,Dark Blue,Dark Blue,Normal Green,Normal Green,Normal Green,Normal Green,Normal Green],V.fromList [Normal Blue,Normal Blue,Normal Blue,Dark Blue,Dark Blue,Dark Blue,Light Blue,Dark Magenta,Dark Cyan,Normal Green],V.fromList [Normal Red,Normal Red,Normal Red,Normal Red,Black,Normal Red,Normal Red,Normal Cyan,Normal Red,Normal Green],V.fromList [Normal Red,Normal Red,Normal Red,Normal Red,Normal Green,Dark Cyan,Normal Cyan,Normal Cyan,Normal Cyan,Normal Green],V.fromList [Normal Red,Normal Red,Normal Red,Normal Red,Normal Green,Normal Red,Normal Red,Normal Cyan,Dark Green,Normal Green],V.fromList [Normal Yellow,Normal Yellow,Black,Black,Normal Green,Black,Black,Normal Cyan,Dark Green,Dark Green],V.fromList [Normal Yellow,Normal Yellow,Black,Normal Green,Normal Green,Normal Green,Black,Normal Cyan,Normal Cyan,Light Green],V.fromList [Black,Black,Black,Black,Black,Black,Black,Black,Normal Cyan,Dark Cyan],V.fromList [Normal Yellow,Normal Yellow,Black,Normal Yellow,Normal Yellow,Normal Yellow,Normal Yellow,Black,Normal Cyan,Dark Blue]],
  _mapWidth = 10,
  _mapHeight = 10
  }