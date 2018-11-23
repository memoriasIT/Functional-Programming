process :: [String] -> String
process xss = if (head xss == "--" && xss !! 1 == "#") then concat (drop 2 xss)  else "Not comment" 

main = do 
    putStrLn "Insert a string to convert: "
    -- Input string
    line <- getLine
    putStrLn (process (words line))


------------------------------------------------------------------------
-- 9. [    ]                                                          --
