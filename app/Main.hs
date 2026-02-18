module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension)
import System.Directory (doesFileExist)
import AST
import Parser
import Eval
import Control.Monad.Writer

import Turtle

ejemplosDir :: FilePath
ejemplosDir = "Ejemplos"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [name] -> runExample name
        _ -> do
            putStrLn "Uso: stack run NOMBRE"
            putStrLn "El archivo debe estar dentro de la carpeta Ejemplos/"
            exitFailure

runExample :: String -> IO ()
runExample name = do
    let fileName =
            if takeExtension name == ".lis"
                then name
                else name ++ ".lis"

    let fullPath = ejemplosDir </> fileName

    exists <- doesFileExist fullPath

    if not exists
        then do
            putStrLn $ "Error: no existe el archivo " ++ fullPath
            exitFailure
        else do
            putStrLn $ "Cargando: " ++ fullPath
            input <- readFile fullPath
            let ast = parseString input

            let (result,trace) = runWriter (evalM ast)

            putStrLn "\n=== Cadena Final ==="
            case result of
                LSys name ax _ ang st it -> do
                    putStrLn name
                    putStrLn ax
                    putStrLn ("\nAngle: " ++ show ang)
                    putStrLn ("Step: " ++ show st)
                    putStrLn ("Iterations: " ++ show it)

                    putStrLn "\n=== Traza Final ==="
                    mapM_ putStrLn trace

                    putStrLn "\nAbriendo ventana gráfica..."
                    animateTrace (realToFrac st) (realToFrac ang) trace

                _ -> print result
