module Main where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Lib

-- Artifact version
newtype Version = Version T.Text deriving (Show)

-- Individual artifact
data Artifact = Artifact { artifactId :: T.Text
                         , organizationId :: T.Text
                         , version :: Version
                         } deriving (Show)

makeArtifactFromComponents :: [T.Text] -> Maybe Artifact
makeArtifactFromComponents components
  | length components < 3 = Nothing
  | otherwise =
    let
      parts = init components
      version = last parts
      orgAndArtifact = init parts
      artifactId = last orgAndArtifact
      organizationId = T.intercalate (T.pack ".") $ init parts
    in
      Just $ Artifact artifactId organizationId $ Version version

-- Makes an artifact from a T.Text path.
makeArtifact :: T.Text -> Maybe Artifact
makeArtifact path =
  makeArtifactFromComponents $ T.splitOn (T.pack "/") path

parseArtifacts :: T.Text -> [Artifact]
parseArtifacts input =
  let
    lines = T.splitOn (T.pack "\n") input
  in
    mapMaybe (\line ->
            makeArtifact $ last $ T.splitOn (T.pack " ") line) lines

main :: IO ()
main = do
  input <- T.getContents
  let artifacts = parseArtifacts input
  T.putStrLn $ T.pack $ show artifacts
