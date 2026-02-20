module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension)
import System.Directory (doesFileExist)
import AST
import Parser
import Eval
import Control.Monad.Writer
import Control.Monad(when)
import Turtle

ejemplosDir :: FilePath
ejemplosDir = "Ejemplos"

main :: IO ()
main = do
    args <- getArgs
    let (flags, names) = partitionArgs args
    case names of
        [name] -> runExample name flags
        _ -> do
            putStrLn (show args)
            putStrLn "Uso: stack run -- [FLAGS] NOMBRE "
            putStrLn "El archivo debe estar dentro de la carpeta Ejemplos/"
            putStrLn "las flags son: "
            putStrLn "trz: muestra la traza"
            putStrLn "ext: muestra el sistema resultante"
            putStrLn "grf: realiza el gráfico estilo turtle(solo toma F como paso)"
            exitFailure

-- Separar flags y argumentos
partitionArgs :: [String] -> ([String], [String])
partitionArgs = foldr f ([], [])
  where
    f arg (fs, ns)
      | arg `elem` ["-trz", "-ext","-grf"] = (arg:fs, ns)
      | otherwise                  = (fs, arg:ns)

runExample :: String -> [String] -> IO ()
runExample name flags = do
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

            
            case result of
                LSys name ax _ ang st it ->
                    do
                    (when (elem "-ext" flags) 
                        (do
                            putStrLn "\n=== LSistem Final ==="
                            putStrLn name
                            putStrLn ax
                            putStrLn ("\nAngle: " ++ show ang)
                            putStrLn ("Step: " ++ show st)
                            putStrLn ("Iterations: " ++ show it)))
                    (when (elem "-trz" flags)
                        (do
                            putStrLn "\n=== Traza Final ==="
                            mapM_ putStrLn trace))
                    
                    (when (elem "-grf" flags)
                        (do        
                            putStrLn "\nAbriendo ventana gráfica..."
                            animateTrace (realToFrac st) (realToFrac ang) trace))
                    (when (True)
                        (putStrLn "finalizado"))


                _ -> print result
