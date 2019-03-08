module Interactive(startLoop) where

import Data.List.Split
import System.Console.ANSI
import System.FilePath.Windows
import System.IO

import Parser
import Prog
import Term

import Evaluation
import PrettyPrinting
import Reduction

type ProgName = String
type LastFileLoadCommand = String
type MarkForClose = Bool

-- Three big String literals incomming!
-- Just here for the code to stay readable and this messages to be editable.
header :: String
header = unlines
  [ "                      _   _               _        _ _  "
  , "   Welcome to        | | | |             | |      | | | "
  , "  ___ _ __ ___   ___ | | | |__   __ _ ___| | _____| | | "
  , " / __| '_ ` _ \\ / _ \\| | | '_ \\ / _` / __| |/ / _ \\ | | "
  , " \\__ \\ | | | | | (_) | | | | | | (_| \\__ \\   <  __/ | | "
  , " |___/_| |_| |_|\\___/|_| |_| |_|\\__,_|___/_|\\_\\___|_|_| "
  , "                                                        "
  , "                                     Type :h for help   "
  ]
  
helpMessage :: String
helpMessage = unlines 
  [ "   ____                                          _        "
  , "  / ___|___  _ __ ___  _ __ ___   __ _ _ __   __| |___     "
  , " | |   / _ \\| '_ ` _ \\| '_ ` _ \\ / _` | '_ \\ / _` / __|    "
  , " | |__| (_) | | | | | | | | | | | (_| | | | | (_| \\__ \\    "
  , "  \\____\\___/|_| |_| |_|_| |_| |_|\\__,_|_| |_|\\__,_|___/    "
  , "                                                           "
  , "  <expression>       Evaluates the specified expression.   "
  , "  :h[elp]            Shows this help message.              "
  , "  :l[oad] <file>     Loads the specified file.             "
  , "  :l[oad]            Unloads the currently loaded file.    "
  , "  :r[eload]          Reloads the lastly loaded file.       "
  , "  :s[et] <strategy>  Sets the specified evaluation strategy"
  , "                     where <strategy> is one of 'lo', 'li',"
  , "                     'ro', 'ri', 'po', or 'pi'.            "
  , "  :q[uit]            Exits the interactive environment.    "
  ]
    
quitMessage :: String
quitMessage = unlines 
  [ "                                        "
  , "     ____                ____           "
  , "    / __ )__  _____     / __ )__  _____ "
  , "   / __  / / / / _ \\   / __  / / / / _ \\"
  , "  / /_/ / /_/ /  __/  / /_/ / /_/ /  __/"
  , " /_____/\\__, /\\___/  /_____/\\__, /\\___/ "
  , "       /____/              /____/       "
  , "                                        "
  ]

-- actual main function
startLoop :: IO ()
startLoop = do 
  clearScreen 
  setSGR [SetBlinkSpeed NoBlink]
  setSGR [SetColor Foreground Vivid Magenta]
  setSGR [SetColor Background Dull Black]
  setTitle "Smol Haskell"
  putStr header
  inputLoop (Prog [], "", "", loStrategy, False)
  setSGR [SetColor Foreground Vivid Magenta]
  putStr quitMessage
  setTitle "Ordinary Console Title"
  setSGR [Reset]

-- The loop function that does some colour arrangements and calls itself
-- to get more than one input.
inputLoop :: (Prog, ProgName, LastFileLoadCommand, Strategy, MarkForClose)
          -> IO ()
-- if we did something to cause the program to perform a regular exit
inputLoop (_, _, _, _, True)  = return ()
-- if we did something that makes the program do something
inputLoop (p, n, l, s, False) = do
  setSGR [SetColor Foreground Dull Cyan]     -- make it fancy
  putStr (n ++ "> ")                         -- prompt the user to input stuff
  hFlush stdout                              -- make the prompt appear
  setSGR [SetColor Foreground Vivid Cyan]    -- make it fancy
  input <- getLine                           -- get the actual input
  setSGR [SetColor Foreground Vivid Magenta] -- make it fancy
  output <- processInput p n l s input       -- do what we are told to by input
  inputLoop output                           -- repeat

-- parse input and forward to processing functions
processInput :: Prog -> ProgName -> LastFileLoadCommand -> Strategy -> String
             -> IO ( Prog
                   , ProgName
                   , LastFileLoadCommand
                   , Strategy
                   , MarkForClose
                   )
processInput p n l s input 
  -- told do do nothing by entering nothing. Seems legit.
  | input == ""        = do return (p, n, l, s, False)
  -- output possibilities provided by smolhs Interpreter
  -- (not the ones provided by Program)
  | input == ":h" || 
    input == ":help"   = do putStr helpMessage
                            return (p, n, l, s, False)
  -- exit
  | input == ":q" || 
    input == ":quit"   = do return (p, n, l, s, True)
  -- output some status info (of course fancy)
  | input == ":d" || 
    input == ":debug"  = do setSGR [SetColor Foreground Vivid Blue]
                            setSGR [SetColor Background  Vivid White]
                            putStrLn ("Prog:\n" ++ (pretty p) ++ 
                                      "\nProgName: " ++ (show n) ++ 
                                      "\nLastFileLoadCommand: " ++ (show l))
                            setSGR [SetColor Background Dull Black]
                            return (p, n, l, s, False)
  -- show evaluation steps whilst evaluating evaluatable Term that user gave us
  | (words input)!!0 == ":d"
                       = do setSGR [SetColor Foreground Vivid Blue]
                            setSGR [SetColor Background  Vivid White]
                            -- remove leading ":d "
                            input' <- return (drop 3 input)
                            _ <- case
                                   (parse::String -> Either String Term) input'
                                   of Left m  -> do
                                        setSGR [SetColor Foreground Vivid Red]
                                        putStrLn m -- Thats the error message
                                        return (p, n, l, s, False)
                                      Right t -> do
                                        debugEval s p t
                                        return (p, n, l, s, False)
                            setSGR [SetColor Background Dull Black]
                            return (p, n, l, s, False)
  -- reload the file, something could have changed
  | input == ":r" || 
    input == ":reload" = if l == "" -- somehow no file is loaded! Unbelievable!
                         then do setSGR [SetColor Foreground Vivid Red]
                                 putStrLn "You haven't loaded a file yet!"
                                 return (p, n, l, s, False)
                         else do out <- loadFile p n l s l
                                 return out
  -- unload program (why would you, it's kinda useless then)
  | input == ":l" || 
    input == ":load"   = do setTitle "Smol Haskell"
                            return (Prog [], "", l, s, False)
  -- load some program so some actually useful stuff can happen
  -- it's the program you type here, not some file, not in this case
  | ((words input)!!0 == ":l" ||
    (words input)!!0 == ":load") &&
    (words input)!!1 == "-d"
                       = case
                           -- cut of the command and parse the remaining input
                           (parse::String -> Either String Prog)
                           (drop (4 + length ((words input)!!0))
                           (unlines (splitOn ", " input)))
                           of Left m  -> do 
                                setSGR [SetColor Foreground Vivid Red]
                                putStrLn m  -- not a valid program dude
                                return (p, n, l, s, False)
                              Right p' -> do
                                setSGR [SetColor Foreground Vivid Green]
                                putStrLn "Loaded program!"
                                setTitle "Smol Haskell - Unnamed Program"
                                return (p', n, l, s, False)
  -- load some file so some actually useful stuff can happen
  | (words input)!!0 == ":l" ||
    (words input)!!0 == ":load"            
                       -- cut off command and 
                       -- pass it to the function that does the real doing
                       = do out <- loadFile p n l s 
                              (drop (1 + length ((words input)!!0)) input)
                            return out
  -- choose your strategy
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
                           _    -> do
                             setSGR [SetColor Foreground Vivid Red]
                             putStrLn "I don't know that evaluation strategy :/"
                             return (p, n, l, s, False)
  -- well, here is the stuff that let's users use their useful program
  | otherwise          = case
                           (parse::String -> Either String Term) input
                           of Left m  -> do
                                setSGR [SetColor Foreground Vivid Red]
                                putStrLn m  -- your program can't handle the input
                                return (p, n, l, s, False)
                              Right t -> do
                                setSGR [SetColor Foreground Vivid Green]
                                -- pass it to the real evaluating function thing
                                putStrLn $ pretty $ evaluateWith s p t
                                return (p, n, l, s, False)
  where
    -- you can load some files with this
    loadFile :: Prog -> ProgName -> LastFileLoadCommand -> Strategy -> String
             -> IO ( Prog
                   , ProgName
                   , LastFileLoadCommand
                   , Strategy
                   , MarkForClose
                   )
    loadFile p' n' l' s' input' = 
      do loadInput <- if elem '.' input' 
                        then return (input') 
                        -- let's add our file ending if none is present
                        else return (input' ++ ".smolhs")
         parseOut <- (parseFile::FilePath -> 
           IO (Either String Prog)) loadInput -- the loading and parsing
         case parseOut of 
           Left m  -> do setSGR [SetColor Foreground Vivid Red]
                         putStrLn m -- tried to parse invalid program
                         return (p', n', l', s', False)
           Right pr -> do setSGR [SetColor Foreground Vivid Green]
                          putStrLn $ "Loaded " ++ takeBaseName
                            loadInput ++ "!"  -- tell the user the success
                          setTitle ("Smol Haskell - " ++ takeBaseName 
                            loadInput)
                          return (pr, takeBaseName loadInput, 
                            loadInput, s', False)

    -- evaluates stuff with outputting the steps
    debugEval :: Strategy -> Prog -> Term -> IO ()
    debugEval s' p' t = if (isNormalForm p' t)
                      then do
                        putStrLn $ "Normal Form: " ++ (pretty t)
                        return ()
                      else 
                        case (reduceWith s' p' t) of
                          Nothing -> return ()
                          Just a  -> do
                            putStrLn ("Reduction on pos: " ++ (show (s' p' t)))
                            putStrLn $ pretty $ a
                            debugEval s' p' a
