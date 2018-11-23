readData :: FilePath -> IO (PGF, String)
readData file = do
    gr <- readPGF file
    content <- readFile "input.txt"
    return (gr, content)

processData :: PGF -> String -> String
processData gr content = undefined

outputData :: String -> IO ()
outputData contents = writeFile "output.txt" contents

main :: IO ()
main = do
    file:_ <- getArgs
    (gr, content) <- readData file
    outputData $ processData gr content
    putStrLn "bye"
