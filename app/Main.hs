import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
import Data.List.Split (splitOn)

main = do  
    putStrLn "What would you like to do? add, remove or view?" 
    a <- getLine
    let (ac:ar) = splitOn ": " a
    let arg = intercalate " " ar
    case ac of
        "add" -> addWithOr arg
        "remove" -> remove "./todo.txt"
        "view" -> view "./todo.txt"
        _ -> print "Uh oh"

addWithOr :: String -> IO()
addWithOr arg = do
    case arg of
        "" -> addE
        otherwise -> add arg

removeWithOr :: String -> IO()
removeWithOr arg = do
    case arg of
        "" -> removeE
        otherwise -> rem arg

add :: String -> IO ()  
add todoItem = do
    appendFile "./todo.txt" (todoItem ++ "\n")
    putStrLn "Done"

addE :: IO ()  
addE = do
    putStrLn "What would you like to add?"
    todoItem <- getLine
    appendFile "./todo.txt" (todoItem ++ "\n")
    putStrLn "Done"
  
view :: String -> IO ()  
view fileName = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  

-- Pass args though and if = "" then ask for number to delete 
remove :: String -> IO ()  
remove fileName = do  
    putStrLn "What number would you like to remove?"
    numberString <- getLine
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  
    putStrLn "Done"