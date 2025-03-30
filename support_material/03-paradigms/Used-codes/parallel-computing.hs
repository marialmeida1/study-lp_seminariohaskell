-- Example 1: Using `par` and `pseq` for parallelism
import Control.Parallel

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

exampleParPseq :: IO ()
exampleParPseq = do
    let x = fib 35 `par` fib 36 `pseq` (fib 35 + fib 36)
    print x

-- Example 2: Using `Strategies` for parallelism
import Control.Parallel.Strategies

exampleStrategies :: IO ()
exampleStrategies = do
    let numbers = [1..1000000]
    let squares = map (^2) numbers `using` parList rseq
    print (sum squares)

-- Example 3: Concurrency with `forkIO`
import Control.Concurrent

exampleForkIO :: IO ()
exampleForkIO = do
    forkIO $ putStrLn "Hello from thread 1"
    forkIO $ putStrLn "Hello from thread 2"
    threadDelay 1000000 -- Wait for threads to finish

-- Example 4: Asynchronous programming with `async`
import Control.Concurrent.Async

exampleAsync :: IO ()
exampleAsync = do
    result <- concurrently (return (fib 35)) (return (fib 36))
    print result

-- Example 5: Software Transactional Memory (STM)
import Control.Concurrent.STM

exampleSTM :: IO ()
exampleSTM = do
    counter <- atomically $ newTVar 0
    atomically $ modifyTVar counter (+1)
    value <- atomically $ readTVar counter
    print value

-- Main function to run all examples
main :: IO ()
main = do
    putStrLn "Example 1: par and pseq"
    exampleParPseq

    putStrLn "\nExample 2: Strategies"
    exampleStrategies

    putStrLn "\nExample 3: forkIO"
    exampleForkIO

    putStrLn "\nExample 4: async"
    exampleAsync

    putStrLn "\nExample 5: STM"
    exampleSTM
