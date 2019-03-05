module Interactive where

import Prog
import Term
import Parser
import Evaluation
import PrettyPrinting
import System.FilePath.Windows

type ProgName = String
type LastFileLoadCommand = String
type MarkForClose = Bool

header :: String
header = unlines
  [ "------------------------------------------",
    "| Welcome to Smol Haskell!               |",
    "| Type \":help\" for help.                 |",
    "------------------------------------------" ]
  
helpMessage :: String
helpMessage = unlines 
  [ "Commands available from the prompt:                        ",
    "  <expression>       Evaluates the specified expression.   ",
    "  :h[elp]            Shows this help message.              ",
    "  :l[oad] <file>     Loads the specified file.             ",
    "  :l[oad]            Unloads the currently loaded file.    ",
    "  :r[eload]          Reloads the lastly loaded file.       ",
    "  :s[et] <strategy>  Sets the specified evaluation strategy",
    "                     where <strategy> is one of 'lo', 'li',",
    "                     'ro', 'ri', 'po', or 'pi'.            ",
    "  :q[uit]            Exits the interactive environment.    " ]

startLoop :: IO ()
startLoop = do 
  putStr header
  inputLoop (Prog [], "", "", loStrategy, False)
  putStrLn "cya fam"

inputLoop :: (Prog, ProgName, LastFileLoadCommand, Strategy, MarkForClose) -> IO ()
inputLoop (_, _, _, _, True)  = return ()
inputLoop (p, n, l, s, False) = do 
  putStr (n ++ ">")
  input <- getLine
  output <- processInput p n l s input
  inputLoop output

processInput :: Prog -> ProgName -> LastFileLoadCommand -> Strategy -> String ->
                  IO (Prog, ProgName, LastFileLoadCommand, Strategy, MarkForClose)
processInput p n l s input 
  | input == ""                         = do return (p, n, l, s, False)
  | input == ":h" || input == ":help"   = do putStr helpMessage
                                             return (p, n, l, s, False)
  | input == ":q" || input == ":quit"   = do return (p, n, l, s, True)
  | input == ":d" || input == ":debug"   = do putStrLn ("Prog: " ++ (show p) ++ 
                                                        "\nProgName: " ++ (show n) ++ 
                                                        "\nLastFileLoadCommand: " ++ (show l))
                                              return (p, n, l, s, False)
  | input == ":r" || input == ":reload" = if l == ""
                                          then do putStrLn "You haven't laoded a file yet!"
                                                  return (p, n, l, s, False)
                                          else do out <- loadFile p n l s l
                                                  return out
  | (words input)!!0 == ":l" ||
    (words input)!!0 == ":load"         = if length (words input) == 1
                                          then return (Prog [], "", l, s, False)
                                          else do out <- loadFile p n l s input
                                                  return out
  | length (words input) == 2 && 
    ((words input)!!0 == ":s" ||
     (words input)!!0 == ":set")        = case (words input)!!1 of 
                                            "lo" -> return (p, n, l, loStrategy, False)
                                            "li" -> return (p, n, l, liStrategy, False)
                                            "ro" -> return (p, n, l, roStrategy, False)
                                            "ri" -> return (p, n, l, riStrategy, False)
                                            "po" -> return (p, n, l, poStrategy, False)
                                            "pi" -> return (p, n, l, piStrategy, False)
                                            _    -> do putStrLn "I don't know that evaluation strategy :/"
                                                       return (p, n, l, s, False)
  | otherwise                           = case (parse::String -> Either String Term) input of 
                                            Left m  -> do putStrLn m
                                                          return (p, n, l, s, False)
                                            Right t -> do putStrLn $ pretty $ evaluateWith s p t
                                                          return (p, n, l, s, False)
  where
    loadFile :: Prog -> ProgName -> LastFileLoadCommand -> Strategy -> String -> IO (Prog, ProgName, LastFileLoadCommand, Strategy, MarkForClose)
    loadFile p' n' l' s' loadInput = do parseOut <- (parseFile::FilePath -> IO (Either String Prog)) ((words loadInput)!!1)
                                        case parseOut of 
                                          Left m  -> do putStrLn m
                                                        return (p', n', l', s', False)
                                          Right p'' -> do putStrLn $ "Loaded " ++ takeBaseName ((words loadInput)!!1) ++ "!"
                                                          return (p'', takeBaseName ((words loadInput)!!1), loadInput, s', False)