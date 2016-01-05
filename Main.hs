module Main where

import System.Process (readProcess)
import System.Environment (getArgs)
import Data.List (elemIndex)
import Control.Monad (liftM)

import Diagrams.Prelude hiding (Direction)
import Diagrams.Animation
import Diagrams.Backend.Cairo.CmdLine


readFilename :: IO (Maybe String)
readFilename = do
    args <- getArgs
    case args of
        [] -> return Nothing
        a -> return $ Just (head a)


orientations :: [Orientation]
orientations = [Inner, Outer, Clock, AntiClock]

orientationCombinations :: [(Orientation, Orientation)]
orientationCombinations = [(o1, o2) | o1 <- orientations, o2 <- orientations]


ruleFromPythonFile :: FilePath -> IO Rule
ruleFromPythonFile filename = do
    processResults <- mapM (\(o1, o2) ->
            readProcess "python3" [filename, show o1, show o2] "")
        orientationCombinations
    return $ Rule $ \o1 o2 ->
        let maybeIndex = elemIndex (o1, o2) orientationCombinations
        in  case maybeIndex of
                Nothing -> None
                Just n -> read $ processResults !! n


ruleFromTextFile :: FilePath -> IO Rule
ruleFromTextFile filename = do
    text <- liftM (map words . lines) $ readFile filename
    return . Rule $ (\o p ->
        let h = [n | [l, m, n] <- text, read l == o, read m == p]
        in  if null h then None else read $ head h)



main :: IO ()
main = do
    putStrLn "Stuhlkreis"
    -- maybeFilename <- readFilename
    -- case maybeFilename of
    --     Nothing -> print "Provide a filename!"
    --     Just filename -> do
    let stuhlkreis = [Inner, Outer, Inner, Clock, Clock, Clock, AntiClock, Outer]
    -- let rule = Rule $ \o p -> if o == p then ToLeft else None
    -- rule <- ruleFromPythonFile "tmp.py"
    rule <- ruleFromPythonFile "tmp.py" -- filename
    -- mainWith $ flip toAnimation 10
    --          $ map stuhlkreisDiagram
    --                (applyOften' 7 (applyRule rule) stuhlkreis)
    mainWith $ stuhlkreisDiagramWithTime
             $ (applyOften' 7 (applyRule rule) stuhlkreis)


-- orientationColor :: Orientation -> Color
orientationColor Inner = yellow
orientationColor Outer = blue
orientationColor Clock = red
orientationColor AntiClock = green

stuhlkreisDiagram :: Stuhlkreis -> Diagram B
stuhlkreisDiagram [l] = circle 1 # fc (orientationColor l)
stuhlkreisDiagram (l:ll) = (circle 1 # fc (orientationColor l)) ||| stuhlkreisDiagram ll


stuhlkreisDiagramWithTime :: [Stuhlkreis] -> Diagram B
stuhlkreisDiagramWithTime = foldl1 (===) . map stuhlkreisDiagram


toAnimation :: [a] -> Int -> [(a, Int)]
toAnimation ll dt =
    let number = length ll
    in  zip ll $ repeat dt



data Direction = ToLeft | ToRight | None deriving (Show, Read)
data Orientation = Inner | Outer | Clock | AntiClock deriving (Eq, Show, Read)
type Stuhlkreis = [Orientation]
data Rule = Rule (Orientation -> Orientation -> Direction)


applyRuleAt :: Int -> Rule -> Stuhlkreis -> Orientation
applyRuleAt n (Rule r) circle =
    let number = length circle
        leftNeighbor = circle !! (if n == 0 then number - 1 else n - 1)
        rightNeighbor = circle !! (if n == number - 1 then 0 else n + 1)
        direction = r leftNeighbor rightNeighbor
    in  applyDirection (circle !! n) direction

applyRuleAt' :: Int -> Rule -> Stuhlkreis -> Stuhlkreis
applyRuleAt' n r circle =
    let applied = applyRuleAt n r circle
        number = length circle
    in  if n == 0 then applied : tail circle
            else if n == number - 1 then init circle ++ [applied]
                else take (n - 1) circle ++ [applied] ++ drop n circle


applyRule :: Rule -> Stuhlkreis -> Stuhlkreis
applyRule r s = map (\i -> applyRuleAt i r s) [0..length s - 1]


applyOften :: Int -> (a -> a) -> a -> a
applyOften 0 _ a = a
applyOften n f a = applyOften (n - 1) f (f a)


applyOften' :: Int -> (a -> a) -> a -> [a]
applyOften' n f a = applyOften'' n [] f a
    where applyOften'' :: Int -> [a] -> (a -> a) -> a -> [a]
          applyOften'' 0 acc _ a = a:acc
          applyOften'' n acc f a =
            let r = f a
            in  applyOften'' (n - 1) (a:acc) f r


applyDirection :: Orientation -> Direction -> Orientation
applyDirection Inner ToLeft = AntiClock
applyDirection Inner ToRight = Clock
applyDirection Outer ToLeft = Clock
applyDirection Outer ToRight = AntiClock
applyDirection Clock ToLeft = Inner
applyDirection Clock ToRight = Outer
applyDirection AntiClock ToLeft = Outer
applyDirection AntiClock ToRight = Inner
applyDirection o None = o
