import System.IO

main :: IO ()
main = do h <- openFile "test.txt" ReadMode 
          xs <- getlines h
          --sequence_ $ map putStrLn $ process $  words $ lines xs
          sequence_ $ map putStrLn $ lines $ concat xs

getlines :: Handle -> IO [String]
getlines h = hGetContents h >>= return . lines

process :: [String] -> String
process xss = if (head xss == "--" && xss !! 1 == "#") then concat (drop 2 xss)  else "Not a comment"



