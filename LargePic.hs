module LargePic where

import Diagrams.Backend.Cairo.CmdLine

import Logic


main :: IO ()
main = do
    putStrLn "Stuhlkreis"
    stuhlkreis <- randomOrientation 30
    rule <- ruleFromPythonFile "tmp.py"
    mainWith $ stuhlkreisDiagramWithTime
             $ (applyOften' 30 (applyRule rule) stuhlkreis)

