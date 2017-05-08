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
        "remove" -> removeWithOr arg
        "view" -> view "./todo.txt"
        _ -> print "Unrecognized command"

addWithOr :: String -> IO()
addWithOr arg = do
    case arg of
        "" -> addE
        otherwise -> add arg

removeWithOr :: String -> IO()
removeWithOr arg = do
    case arg of
        "" -> removeE
        otherwise -> remove arg

add :: String -> IO ()  
add todoItem = do
    appendFile "./todo.txt" (todoItem ++ "\n")
    putStrLn "Done"
    main

addE :: IO ()  
addE = do
    putStrLn "What would you like to add?"
    todoItem <- getLine
    appendFile "./todo.txt" (todoItem ++ "\n")
    putStrLn "Done"
    main
  
view :: String -> IO ()  
view fileName = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  
    main

remove :: String -> IO ()  
remove numberString = do  
    let fileName = "./todo.txt"
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
    main

removeE :: IO ()  
removeE = do  
    let fileName = "./todo.txt"
    putStrLn "What number would you like to remove?"
    numberString <- getLine
    handle <- openFile "./todo.txt" ReadMode  
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
    main