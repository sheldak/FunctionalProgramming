import System.Environment
import System.IO.Error
import Control.Exception

riskyAction :: IO ()
riskyAction = do 
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn contents

exHdlr :: IOError -> IO ()
exHdlr ex
        | isDoesNotExistError ex = putStrLn "The file doesn't exist!"
        | isUserError ex         = putStrLn "Incorrect number of arguments!" 
        | otherwise              = ioError ex

-- main :: IO ()
-- main = riskyAction `catch` exHdlr

getDiffWordsNum :: [String] -> Int
getDiffWordsNum ws = length $ diffWords [] ws
                    where diffWords ds [] = ds
                          diffWords ds (w:ws) | w `elem` ds = diffWords ds ws
                                              | otherwise   = diffWords (w:ds) ws

countingAction :: IO ()
countingAction = do
    (fileName:_) <- getArgs
    contents <- readFile fileName

    let linesNum = length . lines $ contents
    putStrLn ("Number of lines: " ++ show linesNum)

    let wordsNum = length . words $ contents
    putStrLn ("Number of words: " ++ show wordsNum)

    let charsNum = length contents
    putStrLn ("Number of characters: " ++ show charsNum)

    let diffWordsNum = getDiffWordsNum . words $ contents
    putStrLn ("Number of unique words: " ++ show diffWordsNum)

    let longLinesNum = length . (filter (\x -> length x > 80)) . lines $ contents
    putStrLn ("Number of long lines: " ++ show longLinesNum)

-- main :: IO ()
-- main = countingAction `catch` exHdlr


wordsInFile :: IO ()
wordsInFile = do
    (fileName:word:_) <- getArgs
    contents <- readFile fileName

    let numberOfOccurences = length . (filter (\w -> w == word)) . words $ contents
    putStrLn ("Number of word occurences: " ++ show numberOfOccurences)

main :: IO ()
main = wordsInFile `catch` exHdlr