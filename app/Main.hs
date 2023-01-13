module Main (main) where

import System.Random
import Data.Maybe (isNothing, fromJust)
import Control.Monad (when)
import System.Exit (exitFailure)

data Weapon = Rock | Paper | Scissors deriving (Enum, Show)

beats :: Weapon -> Weapon -> String
beats Rock Paper = "Lose"
beats Rock Scissors = "Win"
beats Paper Rock = "Win"
beats Paper Scissors = "Lose"
beats _ _ = "Draw"

main :: IO ()
main = do
        print "Choose hand (Rock, Paper, Scissors)"
        gen <- newStdGen
        let enemyHand = fst $ randomWeapon gen
        userIn <- getLine
        let playerIn = stringIn userIn
        let playerHand = fromJust playerIn
        when (isNothing playerIn) $ do 
                print "Hand unrecognised!"
                exitFailure
        print enemyHand
        print $ beats playerHand enemyHand

stringIn :: String -> Maybe Weapon
stringIn "Rock" = Just Rock
stringIn "Paper" = Just Paper
stringIn "Scissors" = Just Scissors
stringIn _ = Nothing 

randomWeapon :: RandomGen g => g -> (Weapon, g)
randomWeapon g = case randomR (0,2) g of
                   (r, g') -> (toEnum r, g')
