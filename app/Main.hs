import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
   
main = do  
    putStrLn "What would you like to do? add, remove or view?" 
    action <- getLine
    case action of
        "add" -> add
        "remove" -> remove "./todo.txt"
        "view" -> view "./todo.txt"
        _ -> print "Uh oh"
  
add :: IO ()  
add = do
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