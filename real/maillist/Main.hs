module Main where
import Control.Exception (catch, IOException)
 
type Line = String
 
type Entry = [Line]
 
type FileName = String
 
type UserInput = [FileName]
maxLineLength = 35 :: Int
main
  = do putStr
         "\n\nWelcome to the LaTex Mailing List Generator.\n(Please type Cntrl-D at file prompt to exit.)\n"
       s <- getContents
       mainLoop (lines s)
 
mainLoop :: UserInput -> IO ()
mainLoop fns
  = putStr "\nFile to be converted: " >>
      case fns of
          [] -> putStr "\nGoodbye!\n"
          (fn : fns') -> catch (readFile fn >>= process (fn ++ ".tex") fns')
                           (\ err ->
                              let _ = err :: IOException in
                                putStr ("\nCan't read " ++ fn ++ "; try again.\n") >>
                                  mainLoop fns')
 
process :: FileName -> UserInput -> String -> IO ()
process (!out) fns (!rawText)
  = writeFile out
      "% Latex Mailing List.\n\n\\input{labels.sty}\n\n\\begin{document}\n\n"
      >> loop (paras (lines rawText))
  where loop []
          = appendFile out "\n\\end{document}\n" >>
              putStr ("\nConversion completed; file " ++ out ++ " written.\n")
              >> mainLoop fns
        loop ps = writePage out ps loop
 
paras :: [Line] -> [Entry]
paras [] = []
paras lns = p : paras (dropWhile blankLine lns')
  where ((!p), lns') = break blankLine lns
        blankLine = all (\ c -> c == ' ' || c == '\t')
 
writePage :: FileName -> [Entry] -> ([Entry] -> IO ()) -> IO ()
writePage out (!ps) cont
  = appendFile out "\\lpage\n" >> writeBlock out ps long 9 >>=
      \ (!ps) ->
        writeBlock out ps long 9 >>=
          \ ps ->
            writeBlock out ps long 9 >>=
              \ (!ps) -> writeBlock out ps short 3 >>= cont
long = "{\\lblock{\n"
short = "{\\sblock{\n"
 
writeBlock :: FileName -> [Entry] -> String -> Int -> IO [Entry]
writeBlock out (!ps) kind size = appendFile out kind >> loop ps 1
  where loop (!((!e) : es)) n
          = writeEntry out e >>
              (if n == size then appendFile out "\n}}\n" >> return es else
                 appendFile out "\n}{\n" >> loop es (n + 1))
        loop (![]) (!n) = loop (take (size - n + 1) (repeat [])) n
 
writeEntry :: FileName -> Entry -> IO ()
writeEntry out entry = loop entry 1
  where loop (![]) (!n)
          = if n < 5 then loop (take (5 - n) (repeat "")) n else return ()
        loop (ln : lns) n
          = if n > 4 then
              putStr "\nThis entry was truncated to 4 lines:\n" >> print entry >>
                putStr "\n"
                >> return ()
              else
              appendFile out ln >> appendFile out "\\\\ " >>
                (if length ln > maxLineLength then
                   putStr "\nThis line may be too long:\n" >> putStr ln >>
                     putStr "\nCheck LaTex output to be sure.\n"
                     >> loop lns (n + 1)
                   else loop lns (n + 1))