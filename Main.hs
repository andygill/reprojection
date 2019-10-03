{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Put everything in one file for now

module Main where


import Codec.Picture
import Codec.Picture.Types
import Data.Maybe
import Data.Semigroup ((<>))
import Linear.Quaternion
import Linear.Euler
import Options.Applicative

type Distance = Double -- in meters

type Radian = Double

type Longitude = Radian
type Latitude = Radian

data Projection
  = EquiRectangular Longitude Latitude
  | RectiLinear Longitude Latitude
  deriving (Eq, Ord, Show)

data Crop = LeftCrop | RightCrop | TopCrop | BottomCrop
  deriving (Eq, Ord, Show)

data Options = Options
  { inFile  :: String       --  
  , crop    :: Maybe Crop   -- 
  , inProj  :: Projection   -- How to view the input
  , rot     :: Euler Radian -- Rotate the spherical world
  , outProj :: Projection   -- How to project the output
  , size    :: (Int,Int)    -- size of output image, in pixels
  , outFile :: String 		
  } deriving (Show)


parsePair :: (Read a,Read b) => ReadM (a,b)
parsePair = maybeReader $ \ str -> listToMaybe
  [ (a,b)
  | (a,',':rest) <- reads str
  , (b,"") <- reads rest
  ]

parseTriple :: (Read a,Read b,Read c) => ReadM (a,b,c)
parseTriple = maybeReader $ \ str -> listToMaybe
  [ (a,b,c)
  | (a,',':rest) <- reads str
  , (b,',':rest') <- reads rest
  , (c,"") <- reads rest'
  ]

parseCrop :: ReadM Crop
parseCrop = maybeReader $ \ str -> case str of
  "left"   -> pure LeftCrop
  "right"  -> pure RightCrop
  "top"    -> pure TopCrop
  "bottom" -> pure BottomCrop
  _ -> Nothing

projectionParser :: Parser Projection
projectionParser = 
  option (equiRectangular <$> parsePair)
         (long "equirect" <> metavar "longitude,latitude" <>
 	  help "equilrectangular projection") <|>
  option (rectiLinear <$> parsePair)
         (long "rectilinear" <> metavar "longitude,latitude" <>
 	  help "rectilinear (gnomonic) projection")
   where
     equiRectangular (a,b) = EquiRectangular (a*pi/180) (b*pi/180)
     rectiLinear (a,b) = RectiLinear (a*pi/180) (b*pi/180)

rotationParser :: Parser (Euler Radian)
rotationParser = 
  option (euler <$> parseTriple)
         (long "rot" <> metavar "X,Y,Z" <> value (euler (0,0,0)) <>
 	  help "X, Y and Z rotations in intrinsic (Tait-Bryan) ordering")
   where
     euler (a,b,c) = Euler XYZ (a*pi/180) (b*pi/180) (c*pi/180)

optionsParser :: Parser Options
optionsParser = do
  inFile <- argument str (metavar "INPUT")
  crop <- option (Just <$> parseCrop)
    (long "crop" <> value Nothing  <>
     metavar "left|right|top|bottom" <>
     help "crop the input image")
  inProj <- projectionParser
  rot <- rotationParser
  outProj <- projectionParser
  size <- option (parsePair :: ReadM (Int,Int))
    (long "size" <> value (256,256) <> metavar "width,height" <>
     help "width and height of output in pixels")
  outFile <- argument str (metavar "OUTPUT")
  pure Options {..}

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "resampling of images between two projections")
main = do
   options <- execParser opts
   print options
