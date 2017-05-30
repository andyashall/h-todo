import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
import Data.List.Split (splitOn)

main = do  
    putStrLn "What would you like to do? add, complete, view or delete?" 
    a <- getLine
    let (ac:ar) = splitOn " " a
    let arg = intercalate " " ar
    case ac of
        "add" -> addWithOr arg
        "delete" -> delWithOr arg
        "complete" -> completeWithOr arg
        "view" -> view "./todo.txt"
        "a" -> addWithOr arg
        "d" -> delWithOr arg
        "c" -> completeWithOr arg
        "v" -> view "./todo.txt"
        _ -> print "Unrecognized command"

addWithOr :: String -> IO()
addWithOr arg = do
    case arg of
        "" -> addE
        otherwise -> add arg

delWithOr :: String -> IO()
delWithOr arg = do
    case arg of
        "" -> delE
        otherwise -> del arg

completeWithOr :: String -> IO()
completeWithOr arg = do
    case arg of
        "" -> completeE
        otherwise -> complete arg

add :: String -> IO ()  
add todoItem = do
    appendFile "./todo.txt" (todoItem ++ "\n")
    putStrLn "Done"
    main

addC :: String -> IO ()  
addC todoItem = do
    appendFile "./complete.txt" (todoItem ++ "\n")

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

complete :: String -> IO ()  
complete numberString = do  
    let fileName = "./todo.txt"
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    addC (todoTasks !! number)
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  
    putStrLn "Done"
    main

completeE :: IO ()  
completeE = do  
    let fileName = "./todo.txt"
    putStrLn "What number would you like to complete?"
    numberString <- getLine
    handle <- openFile "./todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks 
    addC (todoTasks !! number) 
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  
    putStrLn "Done"
    main

del :: String -> IO ()  
del numberString = do  
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

delE :: IO ()  
delE = do  
    let fileName = "./todo.txt"
    putStrLn "What number would you like to delete?"
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