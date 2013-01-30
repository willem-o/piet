{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Piet.Types where

import Control.Lens
import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import qualified Data.Vector as V

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
    _currentPosition :: Position,
    _stack :: [Int],
    _collisionCount :: Int
  } deriving (Show, Eq)
                    
data ProgramConfig = ProgramConfig {
    codelSize :: CodelSize,
    colourMap :: ColourMap
  } deriving (Show, Eq)

makeLenses ''ColourMap
makeLenses ''ProgramState
makeLenses ''ProgramConfig

data ProgramError = ParseInt String | LoadFile String
                  | FindFile String | NotImplemented String

data Instruction r = Push Int r  | Pop r     | Add r    | Subtract r
                   | Multiply r  | Divide r  | Mod r    | Not r
                   | Greater r   | Pointer r | Switch r | Duplicate r
                   | Roll r      | InNum r   | InChar r | OutNum r
                   | OutChar r   | Nop r      deriving (Functor)

type Program = Free Instruction ()

type Piet = ReaderT ProgramConfig (StateT ProgramState IO)

initialState = ProgramState {
    _directionPointer = DRight,
    _codelChooser = CLeft,
    _currentPosition = (0, 0),
    _stack = [],
    _collisionCount = 0
  }

runPiet :: ProgramConfig -> ProgramState -> Piet a -> IO (a, ProgramState)
runPiet conf s c = runStateT (runReaderT c conf) s

push n = liftF (Push n ())
pop = liftF (Pop ())
add = liftF (Add ())
subtract' = liftF (Subtract ())
multiply = liftF (Multiply ())
divide = liftF (Divide ())
mod' = liftF (Mod ())
not' = liftF (Not ())
greater = liftF (Greater ())
pointer = liftF (Pointer ())
switch = liftF (Switch ())
duplicate = liftF (Duplicate ())
roll = liftF (Roll ())
inNum = liftF (InNum ())
inChar = liftF (InChar ())
outNum = liftF (OutNum ())
outChar = liftF (OutChar ())
nop = liftF (Nop ())

instance Show ProgramError where
  show (ParseInt m) = "Error while parsing: " ++ m
  show (LoadFile m) = "Error while loading file: " ++ m
  show (FindFile m) = "Can't find file: " ++ m
  show (NotImplemented m) = m ++ " hasn't been implemented yet."