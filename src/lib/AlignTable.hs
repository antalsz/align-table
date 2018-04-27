{-# LANGUAGE RecordWildCards #-}
module AlignTable where

import Control.Applicative hiding (optional, (<|>), many)
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char
import Text.Parsec hiding (sepBy, endBy)
import qualified Text.Parsec as P
import qualified Data.ByteString.UTF8 as BU
import System.Console.CmdArgs.Explicit
import System.IO
import System.Environment
import System.Exit

extendWith :: a -> Int -> [a] -> [a]
extendWith y n []     = replicate n y
extendWith y n (x:xs) = x : extendWith y (n-1) xs

matchLengths :: a -> [[a]] -> [[a]]
matchLengths y xs = map (extendWith y (maximum $ map length xs)) xs

matchColumns :: a -> [[[a]]] -> [[[a]]]
matchColumns x = transpose . map (matchLengths x) . transpose . matchLengths []

-- Allows blank fields
tabbedTable :: String -> [[String]]
tabbedTable = map (splitOn "\t") . lines

-- Doesn't allow blank fields
spacedTable :: String -> [[String]]
spacedTable = map words . lines

-- More general than just CSV; any single-character separator can be used,
-- except for '\n', '\r', and '"'.
csvParser :: Char -> Parsec String () [[String]]
csvParser sep = let plainChar = noneOf $ sep : "\n\r\""
                    escQuote  = string "\"\"" >> return '"'
                    eol       = char '\n' >> optional (char '\r')
                    unquoted  = many plainChar
                    quoted    = between (char '"') (char '"')
                              . many $ noneOf "\"" <|> try escQuote
                    row       = (quoted <|> unquoted) `P.sepBy` char sep
                    table     = do r  <- row
                                   rs <-  try (optional eol >> eof >> return [])
                                      <|> (eol >> table)
                                   return $ r:rs
                in table

-- Treats a separator of '\n', '\r', or '"' as ','.  Falls back to dumb-CSV
-- which just looks for comma-separated (or whatever-separated) fields, no
-- quoting, if it can't parse what it's given.
csvTable :: Char -> String -> [[String]]
csvTable sep str
  | sep `elem` "\r\n\"" = csvTable ',' str
  | otherwise            = case parse (csvParser sep) "" str of
                            Left  _     -> map (splitOn [sep]) $ lines str
                            Right table -> table

-- Try to figure out if we should use tabbedTable, spacedTable, or csvTable
guessTable :: String -> [[String]]
guessTable str = let line = head $ lines str
                 in case dropWhile (/= '\t') line of
                      (_:c:_) | isSpace c -> spacedTable str
                              | otherwise -> tabbedTable str
                      [] -> let commaSplit = splitOn "," line
                                wordSplit  = words line
                            in if or [ null wordSplit
                                     , "\"\"" `isInfixOf` line
                                     , length commaSplit > length wordSplit ]
                                 then csvTable ',' str
                                 else spacedTable str

data TableType = Guess
               | Tabbed
               | Spaced
               | CSV Char
               | SepBy String
               | SepByAny String
               deriving (Eq, Read, Show)

data AlignTableArgs = AlignTableArgs { tableType :: TableType
                                     , fill      :: Char
                                     , separator :: String
                                     , input     :: (Maybe FilePath) }
                    | AlignTableHelp
                    | AlignTableVersion
                    | AlignTableExtraFiles
                    deriving (Eq, Read, Show)

args :: String -> Mode AlignTableArgs
args progName =
  let setType tt ata@AlignTableArgs{} = ata{tableType = tt}
      setType _  ata                  = ata
      
      setCSV [sep] ata@AlignTableArgs{}
        = Right ata{tableType = CSV sep}
      setCSV _     ata@AlignTableArgs{}
        = Left $ progName ++ ": The CSV separator must be exactly one "
                          ++ "character long."
      setCSV _      ata
        = Right ata
      
      setType' tt str = Right . setType (tt str)
      
      setFill [fill] ata@AlignTableArgs{}
        = Right ata{fill = fill}
      setFill _      ata@AlignTableArgs{}
        = Left $ progName ++ ": The fill must be exactly one character long."
      setFill _      ata
        = Right ata
      
      setSeparator sep ata@AlignTableArgs{} = Right ata{separator = sep}
      setSeparator _   ata                  = Right ata
      
      -- Because of the formatting, we can't use Left "..." for the error case
      -- here; the string gets surrounded by "Unhandled argument, ...: <arg>".
      -- This isn't true when a switch fails to parse.
      setFile input ata@AlignTableArgs{input = Nothing}
        = Right ata{input = Just input}
      setFile _     ata@AlignTableArgs{input = Just _}
        = Right AlignTableExtraFiles
      setFile _ ata
        = Right ata
  in mode progName
          (AlignTableArgs { tableType = Guess
                          , fill      = ' '
                          , separator = " "
                          , input     = Nothing })
          ""
          (flagArg setFile "FILE")
          [ flagNone     ["g", "guess"]   (setType Guess)
                     $  "Attempt to guess the format of the input table "
                     ++ "as either tab-separated, whitespace-separated, "
                     ++ "or CSV.  This is the default way to select the format "
                     ++ "for input tables."
          , flagNone     ["t", "tab"]     (setType Tabbed)
                     "The input table is tab-separated."
          , flagNone     ["s", "space"]   (setType Spaced)
                     $  "The input table is whitespace-separated.  This format "
                     ++ "does not permit empty columns."
          , flagOpt  "," ["c", "csv"]     setCSV              "SEP"
                     $  "The input table is CSV-formatted.  If a "
                     ++ "single-character separator is provided, then that "
                     ++ "character is used as a field separator instead of a "
                     ++ "comma.  (Newlines, carriage returns, and double "
                     ++ "quotes cannot be used.)  If the input is not valid "
                     ++ "CSV, then the flag defaults to being treated as "
                     ++ "`-s SEP`."
          , flagReq      ["d", "delim"]   (setType' SepBy)    "SEP"
                     $  "The input table has its fields delimited by the "
                     ++ "string SEP.  (For instance, `-t` is the same as "
                     ++ "`-s \"\\t\"`.)"
          , flagReq      ["a", "any"]     (setType' SepByAny) "OPTS"
                     $  "The input table has its fields separated by any "
                     ++ "character in OPTS.  There are no empty columns.  "
                     ++ "(For instance, `-s` is roughly the same as "
                     ++ "`-a \" \\t\"`.)"
          
          , flagReq      ["f", "fill"]    setFill      "CHAR"
                     $  "When printing the table, pad the columns with the "
                     ++ "single character CHAR.  By default, this is a space."
          , flagReq      ["o", "out"]     setSeparator "SEP"
                     $  "When printing the table, separate the columns with "
                     ++ "the string SEP.  By default, this is a single space."
          
          , flagNone     ["h", "help"]    (const AlignTableHelp)
                     "Display this help message."
          , flagNone     ["v", "version"] (const AlignTableVersion)
                     "Print the version information." ]

-- Print to stdout and exit with success
stop :: String -> IO a
stop str = putStrLn str >> exitSuccess

-- Print to stderr and exit with failure
-- die :: String -> IO a
-- die str = hPutStrLn stderr str >> exitFailure

main :: IO ()
main =
  do progName   <- getProgName
     let atArgs =  args progName
     ata        <- processArgs atArgs
     case ata of
       AlignTableExtraFiles
         -> die $ progName ++ ": Too many files."
       AlignTableVersion
         -> stop "Plain-text table column aligner, version 1.0."
       AlignTableHelp
         -> do putStrLn $  "Aligns the columns of plain-text tables for "
                        ++ "easier reading."
               putStrLn ""
               putStr "Usage: "
               putStr . show $ helpText [] HelpFormatAll atArgs
               exitSuccess
       AlignTableArgs{..}
         -> do let fn = case tableType of
                          Guess         -> guessTable
                          Tabbed        -> tabbedTable
                          Spaced        -> spacedTable
                          CSV      sep  -> csvTable sep
                          SepBy    sep  -> map (splitOn sep)           . lines
                          SepByAny opts -> map (wordsBy (`elem` opts)) . lines
               str <- case input of
                        Nothing   -> getContents
                        Just "-"  -> getContents
                        Just file -> readFile file
               putStr . unlines
                      . map (intercalate separator)
                      . matchColumns fill
                      $ fn str
