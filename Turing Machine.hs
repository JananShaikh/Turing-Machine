import Data.List

data State = Q0 | Q1 | Q2 | Q3 | Q4 | Halt deriving (Eq, Show)
data Symbol = Blank | Zero | One deriving (Eq, Show)
type Tape = ([Symbol], State,  Int) -- The tape is a tuple that has the actual tape, the current state and the position of the tape head

-- The transition function
transition :: Tape -> Tape 
transition (ls, state,pos) = case (state, currentSymbol) of 
    (Q0, One) -> (replaceSymbol Blank pos ls, Q1, pos + 1)  -- +1 represents moving right on the tape 
    (Q0, _)   -> (ls, Halt,  pos)
    (Q1, Zero) -> (replaceSymbol Zero pos ls, Q1, pos + 1)
    (Q1, One) -> (replaceSymbol One pos ls, Q1, pos + 1)
    (Q1, Blank) -> (replaceSymbol Blank pos ls, Q2, pos - 1)  -- -1 represents moving left on the tape
    (Q2, Zero) -> (replaceSymbol One pos ls, Q4, pos - 1)
    (Q2, One) -> (replaceSymbol Blank pos ls, Q3, pos - 1)
    (Q3, Zero) -> (replaceSymbol Zero pos ls, Q3,  pos - 1)
    (Q3, One) -> (replaceSymbol One pos ls, Q3, pos - 1)
    (Q3, Blank) -> (replaceSymbol Blank pos ls, Q0, pos + 1)
    (_, _) -> (ls, Halt,  pos)
  where
    currentSymbol = ls !! pos

-- Replacing symbols where the tape position is at
replaceSymbol :: Symbol -> Int -> [Symbol] -> [Symbol]
replaceSymbol symbol pos list = take pos list ++ [symbol] ++ drop (pos + 1) list

-- Execute the Turing machine step by step (Visualisation/Testing purposes)
executeStepByStep :: Tape -> IO ()
executeStepByStep tape@(ls, state, pos)
    | state == Halt = do
        putStrLn $ "Halted. Result: " ++ show (ls) ++ " = " ++ show(length(takeWhile (== One) (dropWhile (== Blank) ls)))
    | otherwise = do
        putStrLn $ "State: " ++ show state
        let (leftPart, currentSymbol) =(ls, ls !! pos)
        putStrLn $ "Tape: " ++ map symbolToChar leftPart ++ " " ++ show (currentSymbolToChar currentSymbol)  
        putStrLn "   |"
        putStrLn "   V"
        executeStepByStep (transition tape)

-- Functions for conversions for printing
symbolToChar :: Symbol -> Char
symbolToChar Blank = 'B'
symbolToChar Zero = '0'
symbolToChar One = '1'

currentSymbolToChar :: Symbol -> Char
currentSymbolToChar Blank = 'B'
currentSymbolToChar symbol = symbolToChar symbol

-- Execute the Turing machine with input and output
runTuringMachine :: [Symbol] -> IO ()
runTuringMachine input = do
    putStrLn "Starting Turing Machine:"
    executeStepByStep (input, Q0, 0)

-- Natural Number to Unary conversion
toUnary :: Int -> [Symbol]
toUnary 0 = []
toUnary n = One : toUnary (n - 1)

-- Main function to demonstrate the Turing machine
main :: IO ()
main = do
    putStrLn "Enter the first number:"
    inputA <- getLine
    putStrLn "Enter the second number:"
    inputB <- getLine
    let a = read inputA :: Int
        b = read inputB :: Int
        maxInputLength = 32
        inputTape = toUnary a ++ [Zero] ++ toUnary b ++ [Blank]
    if a > b && length (toUnary a)  <= maxInputLength && length (toUnary b)  <= maxInputLength -- Restricting inputs for validity
        then do
            runTuringMachine inputTape
            putStrLn $ "For input tape: " ++ show (inputTape) ++ " = " ++ show (a) ++ "-" ++ show (b)
        else putStrLn "Invalid input: a must be greater than b, and the unary number inputs for the tape must be <= 32" 

