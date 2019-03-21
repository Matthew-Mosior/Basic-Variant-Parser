{-=BasicVepParser (BVP): A Haskell-based solution=-}
{-=to VEP (ensembl-vep ouput) file parsing.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in=-} 
{-=a *.vep file and parse it accordingly.=-}

{-Imports-}

import Data.List as DL
import Data.List.Split as DLS
import Data.Ord as DO
import Data.Tuple as DT
import System.Console.GetOpt as SCG
import System.Directory as SD
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO
import System.IO.Temp as SIOT
import System.Process as SP

{---------} 

{-Custom CML Option Datatype.-}

data Flag
    = Verbose               -- -v
    | Version               -- -V -?
    | OutputFile String     -- -o 
    | Help                  -- --help
    deriving (Eq,Ord,Show)

{-----------------------------}

{-Option Description function relating to datatype above.-}

--options -> This function will
--describe flags.
options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"]             (NoArg Verbose)                "Output on stderr.",
      Option ['V','?'] ["version"]             (NoArg Version)                "Show version number.",
      Option ['o']     ["outputfile"]          (ReqArg OutputFile "OUTFILE")  "The output file.", 
      Option []        ["help"]                (NoArg Help)                   "Print this help message."
    ]

{---------------------------------------------------------}

{-Custom bool functions for Flag Datatype.-}

--isOutputFile -> This function will
--test for OutputFile flag.
isOutputFile :: Flag -> Bool
isOutputFile (OutputFile _) = True
isOutputFile _              = False

{------------------------------------------}

{-Custom extraction functions for Flag Datatype.-}

--extractOutputFile -> This function will
--extract the string associated with 
--OutputFile.
extractOutputFile :: Flag -> String
extractOutputFile (OutputFile x) = x

{------------------------------------------------}

{-Function to correctly parse the flags.-}

--compilerOpts -> This function will
--parse incoming command line arguments.
compilerOpts :: [String] -> IO ([Flag],String)
compilerOpts argv =
    case getOpt Permute options argv of
        (args,file,[]) ->
            if DL.elem Help args
                then do hPutStrLn stderr (SCG.usageInfo header options)
                        SX.exitWith SX.ExitSuccess
                else if DL.elem Version args
                    then do hPutStrLn stderr (version ++ SCG.usageInfo header options)
                            SX.exitWith SX.ExitSuccess
                    else if DL.length file > 1
                        then do hPutStrLn stderr (flerror ++ github ++ SCG.usageInfo header options)
                                SX.exitWith (SX.ExitFailure 1)
                        else return (DL.nub args, DL.concat file)
        (_,_,errors) -> do
            hPutStrLn stderr (DL.concat errors ++ SCG.usageInfo header options)
            SX.exitWith (SX.ExitFailure 1)
        where
            header      = "Usage: bvf [-vV?o] [file]"
            version     = "Basic VEP Parser (BVP), Version 1.0.\n"
            github      = "Please see https://github.com/Matthew-Mosior/Basic-Variant-Filter/wiki for more information.\n"
            flerror     = "Incorrect number of input files:  Please provide one input file.\n" 

{----------------------------------------}

{-General Utility Functions.-}

--lineFeed -> This function will
--read the file in and split on
--whitespace, returning a list
--of lists.
lineFeed :: String -> [[String]]
lineFeed [] = []
lineFeed xs = DL.map DL.words (DL.lines xs)

--mapNotLast -> This function will
--work like the traditional map 
--function in Data.List, but not
--map to the last element of a list.
mapNotLast :: (a -> a) -> [a] -> [a]
mapNotLast fn []     = []
mapNotLast fn [x]    = [x]
mapNotLast fn (x:xs) = fn x : mapNotLast fn xs

--onlyDataBool -> This function will
--return True for only lines of the 
--file that contain tab-delimited 
--information.
onlyDataBool :: [String] -> Bool
onlyDataBool xs = not (DL.head xs == "##")

--onlyPoundSignBool -> This function will
--return True for only lines of the 
--file that contains the initial 
--header lines.
onlyPoundSignBool :: [String] -> Bool
onlyPoundSignBool xs = DL.head xs == "##"

--onlyDataGrabber -> This function will 
--grab only lines of the file that 
--contain tab-delimited information.
onlyDataGrabber :: [[String]] -> [[String]]
onlyDataGrabber [] = []
onlyDataGrabber xs = DL.filter (onlyDataBool) xs

--onlyPoundSignGrabber -> This function will
--grab only lines of file that contain
--the initial header lines.
onlyPoundSignGrabber :: [[String]] -> [[String]]
onlyPoundSignGrabber [] = []
onlyPoundSignGrabber xs = DL.filter (onlyPoundSignBool) xs

--orderList -> This function will
--order a nested list.
orderList :: [[[String]]] -> [Int] -> [[[(String,Int)]]]
orderList [] [] = []
orderList _  [] = []
orderList [] _  = []
orderList (x:xs) (y:ys) = [DL.map (DL.map (\z -> (z,y))) x] ++ (orderList xs ys)

--tuplifyTwo -> This function will
--turn a list of two elements into
--a two-tuple.
tuplifyTwo :: [a] -> (a,a)
tuplifyTwo [x,y] = (x,y)

--singleunnest -> This function will
--unnest a list.
singleunnest :: [a] -> a
singleunnest [a] = a

--columnMatcher -> This function will
--take data and match it to the 
--appropriate column.
columnMatcher :: [[((String,Int),(String,Int))]] -> [[String]] -> [[[(String,(String,Int))]]]
columnMatcher [] []     = []
columnMatcher [] _      = []
columnMatcher _ []      = []
columnMatcher (x:xs) ys = [smallColumnMatcher x ys] ++ (columnMatcher xs ys)
    where
        --Nested function defintion.--
        smallColumnMatcher :: [((String,Int),(String,Int))] -> [[String]] -> [[(String,(String,Int))]]
        smallColumnMatcher [] []     = []
        smallColumnMatcher [] _      = []
        smallColumnMatcher _ []      = []
        smallColumnMatcher (x:xs) ys = [[((singleunnest (singleunnest (DL.filter (\y -> (fst (fst x)) == (singleunnest y)) ys))),snd x)]] ++ (smallColumnMatcher xs ys)
        ------------------------------

--missingColumnAdder -> This function will
--add back missing values from result of
--the sorted columnMatcher function.
missingColumnAdder :: [[(String,(String,Int))]] -> [String] -> [[(String,(String,Int))]]
missingColumnAdder [] [] = []
missingColumnAdder _  [] = []
missingColumnAdder [] _  = []
missingColumnAdder (x:xs) ys = [smallMissingColumnAdder x ys] ++ (missingColumnAdder xs ys)
    where
        --Nested function definitions.--
        --smallMissingColumnAdder
        smallMissingColumnAdder :: [(String,(String,Int))] -> [String] -> [(String,(String,Int))]
        smallMissingColumnAdder [] []     = []
        smallMissingColumnAdder _  []     = []
        smallMissingColumnAdder [] _      = []
        smallMissingColumnAdder xs ys     = (DL.map (\z -> (z,("N/A",snd (snd (DL.head xs))))) (ys DL.\\ (DL.map (fst) xs))) ++ xs
        -------------------------------- 

--mergeLists -> This function will
--merge lists from two different nested
--lists.
mergeLists :: [[String]] -> [[String]] -> [[String]]
mergeLists [] []         = []
mergeLists [] _          = []
mergeLists _  []         = []
mergeLists (x:xs) (y:ys) = [x ++ y] ++ (mergeLists xs ys)

{----------------------------}

{-Printing functions.-}

--tempFileCreation -> This function will
--print the file to stdout using
--readProcess of the unix tool cat.
catFile :: [[String]] -> IO ()
catFile [] = return ()
catFile xs = do
    --Open a temporary file.
    (tempfile,temph) <- SIOT.openTempFile "." "temp.txt"
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Add intercalatedxs to temp.txt.
    hPutStrLn temph intercalatedxs
    --Close the temporary file's handle.
    hClose temph
    --Print out the contents of tempfile to the screen using cat unix tool.
    (_,_,_,ph) <- SP.createProcess (SP.proc "cat" [tempfile])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitSuccess   -> do SP.readProcess "rm" [tempfile] []
                               return ()
        SX.ExitFailure _ -> do error "Could not cat file."
                               SP.readProcess "rm" [tempfile] []
                               return ()

--printFile -> This function will
--print the file to either stdout
--or to a output file based on
--command-lines options provided.
printFile :: [Flag] -> [[String]] -> IO ()
printFile [] [] = return ()
printFile [] _  = return ()
printFile _  [] = return ()
printFile opts xs = do
    --Grab just "OUTFILE".
    let outfile = DL.head (DL.filter (isOutputFile) opts)
    --Extract the string from FilterFields.
    let outfilestring = extractOutputFile outfile
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Write the output to the user-specified filename.
    SIO.writeFile outfilestring intercalatedxs

{---------------------}

{-BVP Specific Function.-}

--processArgsAndFiles -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFiles :: ([Flag],String) -> IO ()
processArgsAndFiles ([],[]) = return ()
processArgsAndFiles (options,inputfile) = do
    --Read in the file.
    readinputfile <- SIO.readFile inputfile
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed readinputfile
    --Grab only the data portion of processedfile.
    let onlydata = onlyDataGrabber processedfile
    --Grab all of onlydata except for the last column.
    let allbutextra = DL.map (DL.init) onlydata
    --Grab only the header portion of processedfile.
    let onlyheaders = onlyPoundSignGrabber processedfile
    --Grab only the last field "Extra" from onlydata.
    let onlydatalastfield = DL.map (DL.last) onlydata
    --Split the subfields of onlydatalastfield by semicolon delimiter.
    let splitlastfield = DL.map (DLS.splitOn ";") onlydatalastfield
    --Split each subfield of the subfields of splitlastfield by equal-sign delimiter.
    let splitkeyvalue = DL.map (DL.map (DLS.splitOn "=")) splitlastfield
    --Order the sublists of splitkeyvalue so that they can be reordered later.
    let splitkeyvaluenumbered = orderList splitkeyvalue [0..(DL.length splitkeyvalue -1)] 
    --Grab only the keys of the key-value pair from splitkeyvalue.
    let keysonly = DL.map (DL.map (DL.head)) splitkeyvalue
    --Grab all keys from keysonly.
    let allkeys = DL.nub (DL.sort (DL.concat keysonly)) 
    --Nested each of the keys within all keys into a sublist.
    let allkeysnested = DL.map (\x -> [x]) allkeys
    --Grab everything but the head of splitkeyvaluenumbered.
    let splitkeyvaluesanshead = DL.tail splitkeyvaluenumbered
    --Turn list of two items into tuple of splitkeyaluesanshead.
    let keyvaluetuple = DL.map (DL.map (tuplifyTwo)) splitkeyvaluesanshead
    --Match the columns of keyvaluetuple and allkeysnested.
    let matchedcolumns = columnMatcher keyvaluetuple allkeysnested
    --Concatenate the sublists within matchedcolumns
    let concatmatchedcolumns = DL.map (DL.concat) matchedcolumns
    --Add the missing column values back to each sublist of concatmatchedcolumns.
    let columnsaddedback = missingColumnAdder concatmatchedcolumns allkeys
    --Sort each list of columnsaddedback.
    let sortedmatchedcolumns = DL.map (DL.sortOn (fst)) columnsaddedback
    --Grab only the values of the columns.
    let finalmatchedcolumns = DL.map (DL.map (\(_,(y,_)) -> y)) sortedmatchedcolumns
    --Add all keys back onto finalmatchedcolumns.
    let addcolumnheaders = allkeys : finalmatchedcolumns
    --Merge each sublist of allbutextra and addcolumnheaders.
    let finalmergedlists = mergeLists allbutextra addcolumnheaders
    --mapNotLast tabs in finalmergedlists.
    let finalmergedliststabbed = DL.map (mapNotLast (++ "\t")) finalmergedlists
    --mapNotLast spaces in onlyheaders.
    let onlyheadersspaces = DL.map (mapNotLast (++ " ")) onlyheaders
    --Add the initial "##" VEP header section back onto finalmergedlists.
    let finaloutput = onlyheadersspaces ++ finalmergedliststabbed
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFile) options) > 0
        then printFile options finaloutput
        else catFile finaloutput

--processArgsAndContents -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContents :: ([Flag],String) -> IO ()
processArgsAndContents ([],[]) = return ()
processArgsAndContents (options,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    --Grab only the data portion of processedfile.
    let onlydata = onlyDataGrabber processedfile
    --Grab all of onlydata except for the last column.
    let allbutextra = DL.map (DL.init) onlydata
    --Grab only the header portion of processedfile.
    let onlyheaders = onlyPoundSignGrabber processedfile
    --Grab only the last field "Extra" from onlydata.
    let onlydatalastfield = DL.map (DL.last) onlydata
    --Split the subfields of onlydatalastfield by semicolon delimiter.
    let splitlastfield = DL.map (DLS.splitOn ";") onlydatalastfield
    --Split each subfield of the subfields of splitlastfield by equal-sign delimiter.
    let splitkeyvalue = DL.map (DL.map (DLS.splitOn "=")) splitlastfield
    --Order the sublists of splitkeyvalue so that they can be reordered later.
    let splitkeyvaluenumbered = orderList splitkeyvalue [0..(DL.length splitkeyvalue -1)]
    --Grab only the keys of the key-value pair from splitkeyvalue.
    let keysonly = DL.map (DL.map (DL.head)) splitkeyvalue
    --Grab all keys from keysonly.
    let allkeys = DL.nub (DL.sort (DL.concat keysonly))
    --Nested each of the keys within all keys into a sublist.
    let allkeysnested = DL.map (\x -> [x]) allkeys
    --Grab everything but the head of splitkeyvaluenumbered.
    let splitkeyvaluesanshead = DL.tail splitkeyvaluenumbered
    --Turn list of two items into tuple of splitkeyaluesanshead.
    let keyvaluetuple = DL.map (DL.map (tuplifyTwo)) splitkeyvaluesanshead
    --Match the columns of keyvaluetuple and allkeysnested.
    let matchedcolumns = columnMatcher keyvaluetuple allkeysnested
    --Concatenate the sublists within matchedcolumns
    let concatmatchedcolumns = DL.map (DL.concat) matchedcolumns
    --Add the missing column values back to each sublist of concatmatchedcolumns.
    let columnsaddedback = missingColumnAdder concatmatchedcolumns allkeys
    --Sort each list of columnsaddedback.
    let sortedmatchedcolumns = DL.map (DL.sortOn (fst)) columnsaddedback
    --Grab only the values of the columns.
    let finalmatchedcolumns = DL.map (DL.map (\(_,(y,_)) -> y)) sortedmatchedcolumns
    --Add all keys back onto finalmatchedcolumns.
    let addcolumnheaders = allkeys : finalmatchedcolumns
    --Merge each sublist of allbutextra and addcolumnheaders.
    let finalmergedlists = mergeLists allbutextra addcolumnheaders
    --mapNotLast tabs in finalmergedlists.
    let finalmergedliststabbed = DL.map (mapNotLast (++ "\t")) finalmergedlists
    --mapNotLast spaces in onlyheaders.
    let onlyheadersspaces = DL.map (mapNotLast (++ " ")) onlyheaders
    --Add the initial "##" VEP header section back onto finalmergedlists.
    let finaloutput = onlyheadersspaces ++ finalmergedliststabbed
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFile) options) > 0
        then printFile options finaloutput
        else catFile finaloutput       

{-------------------------}

{-Main function.-}

main :: IO ()
main = do
--Get command line arguments.
    (args,files) <- SE.getArgs >>= compilerOpts
    --See if files is null.
    if null files
        then do --Get stdin.
                contents <- SIO.getContents
                --Run args and contents through processArgsandContents.
                processArgsAndContents (args,contents)
        else do --Run args and files through processArgsandFiles.
                processArgsAndFiles (args,files)

{----------------}
