module Interactive(startLoop) where

import Prog
import Term
import Parser
import Evaluation
import PrettyPrinting()
import System.FilePath.Windows
import PrettyPrinting
import System.IO
import System.Console.ANSI

type ProgName = String
type LastFileLoadCommand = String
type MarkForClose = Bool

header :: String
header = unlines
  [ "                      _   _               _        _ _  ",
    "   Welcome to        | | | |             | |      | | | ",
    "  ___ _ __ ___   ___ | | | |__   __ _ ___| | _____| | | ",
    " / __| '_ ` _ \\ / _ \\| | | '_ \\ / _` / __| |/ / _ \\ | | ",
    " \\__ \\ | | | | | (_) | | | | | | (_| \\__ \\   <  __/ | | ",
    " |___/_| |_| |_|\\___/|_| |_| |_|\\__,_|___/_|\\_\\___|_|_| ",
    "                                                        ",
    "                                     Type :h for help   " ]
  
helpMessage :: String
helpMessage = unlines 
  [ "   ____                                          _        ",
    "  / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___     ",
    " | |   / _ \\| '_ ` _ \\| '_ ` _ \\ / _` | '_ \\ / _` / __|    ",
    " | |__| (_) | | | | | | | | | | | (_| | | | | (_| \\__ \\    ",
    "  \\____\\___/|_| |_| |_|_| |_| |_|\\__,_|_| |_|\\__,_|___/    ",
    "                                                           ",
    "  <expression>       Evaluates the specified expression.   ",
    "  :h[elp]            Shows this help message.              ",
    "  :l[oad] <file>     Loads the specified file.             ",
    "  :l[oad]            Unloads the currently loaded file.    ",
    "  :r[eload]          Reloads the lastly loaded file.       ",
    "  :s[et] <strategy>  Sets the specified evaluation strategy",
    "                     where <strategy> is one of 'lo', 'li',",
    "                     'ro', 'ri', 'po', or 'pi'.            ",
    "  :q[uit]            Exits the interactive environment.    " ]
    
quitMessage :: String
quitMessage = unlines 
  [ "                                        ",
    "     ____                ____           ",
    "    / __ )__  _____     / __ )__  _____ ",
    "   / __  / / / / _ \\   / __  / / / / _ \\",
    "  / /_/ / /_/ /  __/  / /_/ / /_/ /  __/",
    " /_____/\\__, /\\___/  /_____/\\__, /\\___/ ",
    "       /____/              /____/       ", 
    "                                        " ]

startLoop :: IO ()
startLoop = do 
  clearScreen 
  setSGR [SetBlinkSpeed NoBlink]
  setSGR [SetColor Foreground Vivid Red]
  setTitle "Smol Haskell"
  putStr header
  inputLoop (Prog [], "", "", loStrategy, False)
  setSGR [SetColor Foreground Vivid Red]
  putStr quitMessage
  setTitle "Ordinary Console Title"
  setSGR [Reset]

inputLoop :: (Prog, ProgName, LastFileLoadCommand, Strategy, MarkForClose) -> IO ()
inputLoop (_, _, _, _, True)  = return ()
inputLoop (p, n, l, s, False) = do 
  setSGR [SetColor Foreground Vivid White]
  putStr (n ++ "> ")
  hFlush stdout
  setSGR [SetColor Foreground Vivid Magenta]
  input <- getLine
  setSGR [SetColor Foreground Vivid Yellow]
  output <- processInput p n l s input
  inputLoop output

processInput :: Prog -> ProgName -> LastFileLoadCommand -> Strategy -> String ->
                  IO (Prog, ProgName, LastFileLoadCommand, Strategy, MarkForClose)
processInput p n l s input 
  | input == ""        = do return (p, n, l, s, False)
  | input == ":h" || 
    input == ":help"   = do putStr helpMessage
                            return (p, n, l, s, False)
  | input == ":q" || 
    input == ":quit"   = do return (p, n, l, s, True)
  | input == ":d" || 
    input == ":debug"  = do setSGR [SetColor Foreground Vivid Blue]
                            setSGR [SetColor Background  Vivid White]
                            putStrLn ("Prog: " ++ (show p) ++ 
                                      "\nProgName: " ++ (show n) ++ 
                                      "\nLastFileLoadCommand: " ++ (show l))
                            setSGR [SetColor Background Dull Black]
                            return (p, n, l, s, False)
  | input == ":r" || 
    input == ":reload" = if l == ""
                         then do setSGR [SetColor Foreground Vivid Red]
                                 putStrLn "You haven't loaded a file yet!"
                                 return (p, n, l, s, False)
                         else do out <- loadFile p n l s l
                                 return out
  | input == ":l" || 
    input == ":load"   = do setTitle "Smol Haskell"
                            return (Prog [], "", l, s, False)
  | (words input)!!0 == ":l" ||
    (words input)!!0 == ":load"            
                       = do out <- loadFile p n l s input
                            return out
  | length (words input) == 2 && 
    ((words input)!!0 == ":s" ||
     (words input)!!0 == ":set")
                       = case (words input)!!1 of 
                           "lo" -> return (p, n, l, loStrategy, False)
                           "li" -> return (p, n, l, liStrategy, False)
                           "ro" -> return (p, n, l, roStrategy, False)
                           "ri" -> return (p, n, l, riStrategy, False)
                           "po" -> return (p, n, l, poStrategy, False)
                           "pi" -> return (p, n, l, piStrategy, False)
                           _    -> 
                             do setSGR [SetColor Foreground Vivid Red]
                                putStrLn "I don't know that evaluation strategy :/"
                                return (p, n, l, s, False)
  | otherwise          = case (parse::String -> Either String Term) input of 
                           Left m  -> do setSGR [SetColor Foreground Vivid Red]
                                         putStrLn m
                                         return (p, n, l, s, False)
                           Right t -> do setSGR [SetColor Foreground Vivid Green]
                                         putStrLn $ pretty $ evaluateWith s p t
                                         return (p, n, l, s, False)
  where
    loadFile :: Prog -> ProgName -> LastFileLoadCommand -> Strategy -> String -> 
      IO (Prog, ProgName, LastFileLoadCommand, Strategy, MarkForClose)
    loadFile p' n' l' s' input = 
      do loadInput <- if elem '.' input 
                      then return (input) 
                      else return (input ++ ".smolhs")
         parseOut <- (parseFile::FilePath -> 
           IO (Either String Prog)) ((words loadInput)!!1)
         case parseOut of 
           Left m  -> do setSGR [SetColor Foreground Vivid Red]
                         putStrLn m
                         return (p', n', l', s', False)
           Right pr -> do setSGR [SetColor Foreground Vivid Green]
                          putStrLn $ "Loaded " ++ takeBaseName 
                            ((words loadInput)!!1) ++ "!"
                          setTitle ("Smol Haskell - " ++ takeBaseName 
                            ((words loadInput)!!1))
                          return (pr, takeBaseName ((words loadInput)!!1), 
                            loadInput, s', False)
