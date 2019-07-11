{-=BasicVariantParser (BVP): A Haskell-based solution=-}
{-=to ensembl-vep ouput file parsing.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 2.0=-}
{-=Synopsis:  This Haskell Script will take in=-} 
{-=a .vcf or .vep file and parse it accordingly.=-}

{-Imports-}

import Data.List as DL
import Data.List.Extra as DLE
import Data.List.Split as DLS
import Data.Maybe as DM
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
    | InFormat String       -- -I
    | OutFormat String      -- -O
    | OutputFile String     -- -o 
    | GzipIn                -- -g
    | GzipOut               -- -G
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
      Option ['I']     ["InFormat"]            (ReqArg InFormat "IN")         "The format of the input file.",
      Option ['O']     ["OutFormat"]           (ReqArg OutFormat "OUT")       "The format of the output file.",
      Option ['o']     ["OutputFile"]          (ReqArg OutputFile "OUTFILE")  "The output file.", 
      Option ['g']     ["GzipIn"]              (NoArg GzipIn)                 "Gzipped input file?",
      Option ['G']     ["GzipOut"]             (NoArg GzipOut)                "Gzipped output file?",
      Option []        ["help"]                (NoArg Help)                   "Print this help message."
    ]

{---------------------------------------------------------}

{-Custom bool functions for Flag Datatype.-}

--isOutputFile -> This function will
--test for OutputFile flag.
isOutputFile :: Flag -> Bool
isOutputFile (OutputFile _) = True
isOutputFile _              = False

--isInFormat -> This function will
--test for InFormat flag.
isInFormat :: Flag -> Bool
isInFormat (InFormat _) = True
isInFormat _            = False

--IsOutFormat -> This function will
--test for OutFormat flag.
isOutFormat :: Flag -> Bool
isOutFormat (OutFormat _) = True
isOutFormat _             = False

--isGzipOut -> This function will
--test for the GzipOut flag.
isGzipOut :: Flag -> Bool
isGzipOut GzipOut = True
isGzipOut _       = False

{------------------------------------------}

{-Custom extraction functions for Flag Datatype.-}

--extractOutputFile -> This function will
--extract the string associated with 
--OutputFile.
extractOutputFile :: Flag -> String
extractOutputFile (OutputFile x) = x

--extractInFormat -> This function will
--extract the string associated with
--InFormat.
extractInFormat :: Flag -> String
extractInFormat (InFormat x) = x

--extractOutFormat -> This function will
--extract the string associated with
--OutFormat.
extractOutFormat :: Flag -> String
extractOutFormat (OutFormat x) = x

{------------------------------------------------}

{-compilerOpts-related functions-}

--checkInFormat -> This function will
--check the format of IN string.
checkInFormat :: String -> Bool
checkInFormat [] = False
checkInFormat xs = if xs == "vcf" || xs == "vep"
                   || xs == "tvcf" || xs == "tvep"
                       then True
                       else False

--checkOutFormat -> This function will
--check the format of OUT string.
checkOutFormat :: String -> Bool
checkOutFormat [] = False
checkOutFormat xs = if xs == "tvcf" || xs == "tvep"
                    || xs == "vcf"  || xs == "vep"
                        then True
                        else False

--checkInOutFormats -> This function will
--check the formats of the IN and OUT string.
checkInOutFormats :: String -> String -> Bool
checkInOutFormats [] [] = False
checkInOutFormats [] _  = False
checkInOutFormats _  [] = False
checkInOutFormats xs ys = if (xs == "vep" && ys == "tvep")
                          || (xs == "tvep" && ys == "vep") 
                          || (xs == "vcf" && ys == "tvcf")
                          || (xs == "tvcf" && ys == "vcf")
                              then True 
                              else False

{--------------------}

{-Function to correctly parse the flags.-}

--compilerOpts -> This function will
--parse incoming command line arguments.
compilerOpts :: [String] -> IO ([Flag],String)
compilerOpts argv =
    case getOpt Permute options argv of
        (args,file,[]) ->
            if DL.elem Help args
                then do hPutStrLn stderr (greeting ++ SCG.usageInfo header options)
                        SX.exitWith SX.ExitSuccess
                else if DL.elem Version args
                    then do hPutStrLn stderr (version ++ SCG.usageInfo header options)
                            SX.exitWith SX.ExitSuccess
                    else if (DL.length (DL.filter (isInFormat) args) < 1)
                        then do hPutStrLn stderr (inerror ++ formats ++ SCG.usageInfo header options)
                                SX.exitWith (SX.ExitFailure 1)
                        else if (DL.length (DL.filter (isInFormat) args) > 0) &&
                                (not (checkInFormat (extractInFormat (DL.head (DL.filter (isInFormat) args)))))
                            then do hPutStrLn stderr (inferror ++ formats ++ SCG.usageInfo header options)
                                    SX.exitWith (SX.ExitFailure 1)
                            else if (DL.length (DL.filter (isOutFormat) args) < 1)
                                then do hPutStrLn stderr (outerror ++ formats ++ SCG.usageInfo header options)
                                        SX.exitWith (SX.ExitFailure 1)
                                else if (DL.length (DL.filter (isOutFormat) args) > 0) &&
                                        (not (checkOutFormat (extractOutFormat (DL.head (DL.filter (isOutFormat) args)))))
                                    then do hPutStrLn stderr (outferror ++ formats ++ SCG.usageInfo header options)
                                            SX.exitWith (SX.ExitFailure 1)
                                    else if (DL.length (DL.filter (isInFormat) args) < 1) &&
                                            (DL.length (DL.filter (isOutFormat) args) < 1)
                                        then do hPutStrLn stderr (inerror ++ outerror ++ SCG.usageInfo header options) 
                                                SX.exitWith (SX.ExitFailure 1)
                                        else if (DL.length (DL.filter (isInFormat) args) > 0) &&
                                                (DL.length (DL.filter (isOutFormat) args) > 0) &&
                                                (not (checkInOutFormats (extractInFormat (DL.head (DL.filter (isInFormat) args))) 
                                                                        (extractOutFormat (DL.head (DL.filter (isOutFormat) args)))))
                                            then do hPutStrLn stderr (inoutmismatch ++ inoutmappings ++ SCG.usageInfo header options)
                                                    SX.exitWith (SX.ExitFailure 1)
                                            else if (DL.length (DL.filter (isGzipOut) args) > 0) &&
                                                    (DL.length (DL.filter (isOutputFile) args) < 1) 
                                                then do hPutStrLn stderr (gziperror ++ SCG.usageInfo header options)
                                                        SX.exitWith (ExitFailure 1)
                                                else if DL.length file > 1
                                                    then do hPutStrLn stderr (flerror ++ greeting ++ github ++ SCG.usageInfo header options)
                                                            SX.exitWith (SX.ExitFailure 1)
                                                    else return (DL.nub args, DL.concat file)
        (_,_,errors) -> do
            hPutStrLn stderr (DL.concat errors ++ SCG.usageInfo header options)
            SX.exitWith (SX.ExitFailure 1)
        where
            greeting       = "Basic Variant Parser, Copyright (c) 2019 Matthew Mosior.\n"
            header         = "Usage: bvp [-vV?IoOgG] [file]"
            version        = "Basic Variant Parser (BVP), Version 2.0.\n"
            github         = "Please see https://github.com/Matthew-Mosior/Basic-Variant-Parser for more information.\n"
            flerror        = "Incorrect number of input files:  Please provide one input file.\n" 
            inerror        = "Please provide an input format (-I).\n"
            outerror       = "Please provide an output format (-O).\n"
            inferror       = "Input format not recognized.\n" 
            outferror      = "Output format not recognized.\n"
            gziperror      = "OutputFile argument (-o) necessary to use GzipOut argument (-G).\n"
            formats        = "Accepted formats are vcf, vep, tvcf and tvep.\n"
            inoutmismatch  = "Please provide an appropriate input/output mapping.\n"
            inoutmappings  = "Appropriate mappings are: vep <-> tvep and vcf <-> tvcf.\n"

{----------------------------------------}

{-Vep or vcf pipeline function.-}

--vepOrVcfPipeline -> This function will
--help decide which pipeline the script
--will follow.
vepOrVcfPipeline :: [Flag] -> (String,String)
vepOrVcfPipeline []      = ([],[])
vepOrVcfPipeline options = if instring == "vep" && outstring == "tvep"
                               then ("vep","tvep")
                               else if instring == "tvep" && outstring == "vep"
                                   then ("tvep","vep")
                                   else if instring == "vcf" && outstring == "tvcf"
                                       then ("vcf","tvcf")
                                       else ("tvcf","vcf")
    where
        --Local definitions.--
        instring  = extractInFormat (DL.head (DL.filter (isInFormat) options))
        outstring = extractOutFormat (DL.head (DL.filter (isOutFormat) options))   
        ----------------------  

{-------------------------------}

{-General Utility Functions (VEP).-}

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

--onlyDataVepBool -> This function will
--return True for only lines of the 
--file that contain tab-delimited 
--information.
onlyDataVepBool :: [String] -> Bool
onlyDataVepBool xs = not (DL.head xs == "##")

--onlyPoundSignBool -> This function will
--return True for only lines of the 
--file that contains the initial 
--header lines.
onlyPoundSignBool :: [String] -> Bool
onlyPoundSignBool xs = DL.head xs == "##"

--onlyDataVepGrabber -> This function will 
--grab only lines of the file that 
--contain tab-delimited information.
onlyDataVepGrabber :: [[String]] -> [[String]]
onlyDataVepGrabber [] = []
onlyDataVepGrabber xs = DL.filter (onlyDataVepBool) xs

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

{-General utility functions (VCF).-}

--onlyInfoBool -> This function will
--return True for only lines that 
--contain the "##INFO" fields.
onlyInfoBool :: [String] -> Bool
onlyInfoBool xs = DL.isInfixOf "##INFO" (DL.head xs) 

--onlyInfoGrabber -> This function will
--grab only lines of file that contain
--the initial header lines.
onlyInfoGrabber :: [[String]] -> [[String]]
onlyInfoGrabber [] = []
onlyInfoGrabber xs = DL.filter (onlyInfoBool) xs

--onlyMetadataBool -> This function will
--return true for only lines that 
--metadata.
onlyMetadataBool :: String -> Bool
onlyMetadataBool xs = DL.isPrefixOf "##" xs

--onlyMetadataGrabber -> This function will
--grab only lines of the file that contain
--all header lines.
onlyMetadataGrabber :: [String] -> [String]
onlyMetadataGrabber [] = []
onlyMetadataGrabber xs = DL.filter (onlyMetadataBool) xs

--onlyDataVcfBool -> This function will
--return True for only lines of the
--file that contain tab-delimited
--information.
onlyDataVcfBool :: [String] -> Bool
onlyDataVcfBool xs = not (DL.isInfixOf "##" (DL.head xs))

--onlyDataVcfGrabber -> This function will
--grab only lines of the file that
--contain tab-delimited information.
onlyDataVcfGrabber :: [[String]] -> [[String]]
onlyDataVcfGrabber [] = []
onlyDataVcfGrabber xs = DL.filter (onlyDataVcfBool) xs

--infixFilter -> This function will
--filter out only elements that contain "=".
infixFilter :: [[[String]]] -> [[[String]]]
infixFilter []     = []
infixFilter (x:xs) = [smallInfixFilter x] ++ (infixFilter xs)
    where
        --Nested function definitions.--
        --smallInfixFilter
        smallInfixFilter :: [[String]] -> [[String]]
        smallInfixFilter []     = []
        smallInfixFilter (x:xs) = [smallestInfixFilter x] ++ (smallInfixFilter xs)
        --smallestInfixFilter 
        smallestInfixFilter :: [String] -> [String]
        smallestInfixFilter [] = []
        smallestInfixFilter (x:xs) = if (not (DL.isInfixOf "CSQ" x)) &&
                                        (DL.isInfixOf "=" x)
                                         then [x] ++ (smallestInfixFilter xs)
                                         else smallestInfixFilter xs
        --------------------------------

--notCsqFieldsAdder -> This function will
--annotate non-CSQ fields.
notCsqFieldsAdder :: [[[String]]] -> [[[(String,String)]]]
notCsqFieldsAdder []     = []
notCsqFieldsAdder (x:xs) = [(smallNotCsqFieldsAdder x)] ++ (notCsqFieldsAdder xs)
    where
        --Nested Function Definitions.--
        --smallNotCsqFieldsAdder
        smallNotCsqFieldsAdder :: [[String]] -> [[(String,String)]]
        smallNotCsqFieldsAdder []     = []
        smallNotCsqFieldsAdder (x:xs) = [smallerNotCsqFieldsAdder x] ++ (smallNotCsqFieldsAdder xs)
        --smallerNotCsqFieldsAdder 
        smallerNotCsqFieldsAdder :: [String] -> [(String,String)]
        smallerNotCsqFieldsAdder [] = []
        smallerNotCsqFieldsAdder xs = [(DL.head xs,DL.last xs)]
        --------------------------------

--notCsqFieldsAddedBack -> This function will
--add back fields not present in each sublist.
notCsqFieldsAddedBack :: [[[(String,String)]]] -> [String] -> [[[(String,String)]]]
notCsqFieldsAddedBack [] []     = []
notCsqFieldsAddedBack _  []     = []
notCsqFieldsAddedBack [] _      = []
notCsqFieldsAddedBack (x:xs) ys = [smallNotCsqFieldsAddedBack x ys] ++ (notCsqFieldsAddedBack xs ys) 
    where
        --Nested Function Definitions.--
        --smallNotCsqFieldsAddedBack
        smallNotCsqFieldsAddedBack :: [[(String,String)]] -> [String] -> [[(String,String)]]
        smallNotCsqFieldsAddedBack [] []     = []
        smallNotCsqFieldsAddedBack _  []     = []
        smallNotCsqFieldsAddedBack [] _      = []
        smallNotCsqFieldsAddedBack xs ys = [DL.zip (ys DL.\\ (DL.map (fst) (DL.concat xs))) (DL.replicate (DL.length (ys DL.\\ (DL.map (fst) (DL.concat xs)))) "N/A")] ++ xs

--csqFieldsAdder -> This function will
--annotate the CSQ fields.
csqFieldsAdder :: [[[[String]]]] -> [String] -> [[[[(String,String)]]]]
csqFieldsAdder []   []   = []
csqFieldsAdder _    []   = []
csqFieldsAdder []    _   = []
csqFieldsAdder (x:xs) ys = [smallCsqFieldsAdder x ys] ++ (csqFieldsAdder xs ys)
    where
        --Nested Function Definitions.--
        --smallCsqFieldsAdder
        smallCsqFieldsAdder :: [[[String]]] -> [String] -> [[[(String,String)]]]
        smallCsqFieldsAdder [] []     = []
        smallCsqFieldsAdder _  []     = []
        smallCsqFieldsAdder [] _      = []
        smallCsqFieldsAdder (x:xs) ys = [smallerCsqFieldsAdder x ys] ++ (smallCsqFieldsAdder xs ys) 
        --smallerCsqFieldsAdder
        smallerCsqFieldsAdder :: [[String]] -> [String] -> [[(String,String)]]
        smallerCsqFieldsAdder [] []     = []
        smallerCsqFieldsAdder [] _      = []
        smallerCsqFieldsAdder _  []     = []
        smallerCsqFieldsAdder (x:xs) ys = [smallestCsqFieldsAdder x ys] ++ (smallerCsqFieldsAdder xs ys)
        --smallestCsqFieldsAdder 
        smallestCsqFieldsAdder :: [String] -> [String] -> [(String,String)]
        smallestCsqFieldsAdder [] []         = []
        smallestCsqFieldsAdder [] _          = []
        smallestCsqFieldsAdder _  []         = []
        smallestCsqFieldsAdder (x:xs) (y:ys) = [(y,x)] ++ (smallestCsqFieldsAdder xs ys)  
        --------------------------------

--headerFieldsAdder -> This function will
--annotate the header fields.
headerFieldsAdder :: [[String]] -> [String] -> [[(String,String)]]
headerFieldsAdder [] []     = []
headerFieldsAdder _  []     = []
headerFieldsAdder [] _      = []
headerFieldsAdder (x:xs) ys = [smallHeaderFieldsAdder x ys] ++ (headerFieldsAdder xs ys)
    where
        --Nested Function Definitions.--
        smallHeaderFieldsAdder :: [String] -> [String] -> [(String,String)]
        smallHeaderFieldsAdder [] []         = []
        smallHeaderFieldsAdder [] _          = []
        smallHeaderFieldsAdder _  []         = []
        smallHeaderFieldsAdder (x:xs) (y:ys) = [(y,x)] ++ (smallHeaderFieldsAdder xs ys)
        -------------------------------- 

--insertSubfields -> This function will
--insert subfields.
insertSubfields :: [[String]] -> [String] -> [[String]]
insertSubfields []    _  = []
insertSubfields _     [] = []
insertSubfields [x,y] z  = [x,z,y]

--dataReplicator -> This function will
--replicate the data field based on the
--amount of data associated with the "CSQ"
--field.
--dataReplicator :: [[String]] -> [Int] -> [[String]]
dataReplicator :: [[a]] -> [Int] -> [[a]]
dataReplicator [] _ = []
dataReplicator _ [] = []
dataReplicator (x:xs) (y:ys) = (DL.replicate y x) ++ (dataReplicator xs ys)

--dataCombinator This function will
--combine various data fields.
dataCombinator :: [[(String,String)]] -> [[(String,String)]] -> [[(String,String)]] -> [[(String,String)]]
dataCombinator []     []     []     = []
dataCombinator (a:as) (b:bs) (c:cs) = [a ++ b ++ c] ++ (dataCombinator as bs cs)

--nonCsqFieldNotApplicableAdder -> This function will
--add N/As for elements not seen in header list.
nonCsqFieldNotApplicableAdder :: [[[[String]]]] -> [String] -> [[[[String]]]]
nonCsqFieldNotApplicableAdder []     [] = []
nonCsqFieldNotApplicableAdder []     _  = []
nonCsqFieldNotApplicableAdder _      [] = []
nonCsqFieldNotApplicableAdder (x:xs) ys = [smallNonCsqField x ys] ++ (nonCsqFieldNotApplicableAdder xs ys)
    where
        --Nested function definitions.--
        --smallNonCsqField
        smallNonCsqField :: [[[String]]] -> [String] -> [[[String]]]
        smallNonCsqField [] []     = []
        smallNonCsqField _  []     = []
        smallNonCsqField [] _      = []
        smallNonCsqField xs ys = [DL.map (\z -> [z,"N/A"]) (ys DL.\\ (concat (map (map (head)) xs)))] ++ xs
        --------------------------------

--notApplicableAdder -> This function will
--add N/A in-place of each null string.
notApplicableAdder :: [[String]] -> [[String]]
notApplicableAdder []     = []
notApplicableAdder (x:xs) = [smallNotApplicableAdder x] ++ (notApplicableAdder xs)
    where
        --Nested function definition.--
        smallNotApplicableAdder :: [String] -> [String] 
        smallNotApplicableAdder []     = []
        smallNotApplicableAdder (x:xs) = if DL.null x
                                             then ["N/A"] ++ (smallNotApplicableAdder xs)
                                             else [x] ++ (smallNotApplicableAdder xs)
        -------------------------------

--combineInfoFields -> This function will
--combine the finalized INFO field.
combineInfoFields :: [[(String,String)]] -> [[(String,String)]] -> [[[(String,String)]]]
combineInfoFields [] []         = []
combineInfoFields (x:xs) (y:ys) = [[take 3 x] ++ [y] ++ [drop 3 x]] ++ (combineInfoFields xs ys)

{----------------------------------}

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

--noGzipPrintFile -> This function will
--print the file, not gzipped.
noGzipPrintFile :: [Flag] -> [[String]] -> IO ()
noGzipPrintFile [] [] = return ()
noGzipPrintFile [] _  = return ()
noGzipPrintFile _  [] = return ()
noGzipPrintFile opts xs = do
    --Grab just "OUTFILE".
    let outfile = DL.head (DL.filter (isOutputFile) opts)
    --Extract the string from FilterFields.
    let outfilestring = extractOutputFile outfile
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Write the output to the user-specified filename.
    SIO.writeFile outfilestring intercalatedxs
               
--gzipPrintFile -> This function will
--will print the file, but gzipped.
gzipPrintFile :: [Flag] -> [[String]] -> IO String
gzipPrintFile [] [] = return []
gzipPrintFile [] _  = return []
gzipPrintFile _  [] = return []
gzipPrintFile opts xs = do
    --Grab just "OUTFILE".
    let outfile = DL.head (DL.filter (isOutputFile) opts)
    --Extract the string from FilterFields.
    let outfilestring = extractOutputFile outfile
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Write the output to the user-specified filename.
    SIO.writeFile outfilestring intercalatedxs
    --Gzip outfile.
    SP.readProcess "gzip" [outfilestring] []

 
{---------------------}

{-BVP Specific Functions.-}

--processArgsAndFilesVepTvep -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesVepTvep :: ([Flag],String) -> IO ()
processArgsAndFilesVepTvep ([],[]) = return ()
processArgsAndFilesVepTvep (options,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if DL.elem GzipIn options
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)               
                --Apply lineFeed function to inputfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab only the data portion of processedfile.
                let gonlydata = onlyDataVepGrabber gprocessedfile
                --Grab all of onlydata except for the last column.
                let gallbutextra = DL.map (DL.init) gonlydata
                --Grab only the header portion of processedfile.
                let gonlyheaders = onlyPoundSignGrabber gprocessedfile
                --Grab only the last field "Extra" from onlydata.
                let gonlydatalastfield = DL.map (DL.last) gonlydata
                --Split the subfields of onlydatalastfield by semicolon delimiter.
                let gsplitlastfield = DL.map (DLS.splitOn ";") gonlydatalastfield
                --Split each subfield of the subfields of splitlastfield by equal-sign delimiter.
                let gsplitkeyvalue = DL.map (DL.map (DLS.splitOn "=")) gsplitlastfield
                --Order the sublists of splitkeyvalue so that they can be reordered later.
                let gsplitkeyvaluenumbered = orderList gsplitkeyvalue [0..(DL.length gsplitkeyvalue -1)]
                --Grab only the keys of the key-value pair from splitkeyvalue.
                let gkeysonly = DL.map (DL.map (DL.head)) gsplitkeyvalue
                --Grab all keys from keysonly.
                let gallkeys = DL.nub (DL.sort (DL.concat gkeysonly))
                --Nested each of the keys within all keys into a sublist.
                let gallkeysnested = DL.map (\x -> [x]) gallkeys
                --Grab everything but the head of splitkeyvaluenumbered.
                let gsplitkeyvaluesanshead = DL.tail gsplitkeyvaluenumbered
                --Turn list of two items into tuple of splitkeyaluesanshead.
                let gkeyvaluetuple = DL.map (DL.map (tuplifyTwo)) gsplitkeyvaluesanshead
                --Match the columns of keyvaluetuple and allkeysnested.
                let gmatchedcolumns = columnMatcher gkeyvaluetuple gallkeysnested
                --Concatenate the sublists within matchedcolumns
                let gconcatmatchedcolumns = DL.map (DL.concat) gmatchedcolumns
                --Add the missing column values back to each sublist of concatmatchedcolumns.
                let gcolumnsaddedback = missingColumnAdder gconcatmatchedcolumns gallkeys
                --Sort each list of columnsaddedback.
                let gsortedmatchedcolumns = DL.map (DL.sortOn (fst)) gcolumnsaddedback
                --Grab only the values of the columns.
                let gfinalmatchedcolumns = DL.map (DL.map (\(_,(y,_)) -> y)) gsortedmatchedcolumns
                --Add all keys back onto finalmatchedcolumns.
                let gaddcolumnheaders = gallkeys : gfinalmatchedcolumns
                --Merge each sublist of allbutextra and addcolumnheaders.
                let gfinalmergedlists = mergeLists gallbutextra gaddcolumnheaders
                --mapNotLast tabs in finalmergedlists.
                let gfinalmergedliststabbed = DL.map (mapNotLast (++ "\t")) gfinalmergedlists
                --mapNotLast spaces in onlyheaders.
                let gonlyheadersspaces = DL.map (mapNotLast (++ " ")) gonlyheaders
                --Add the initial "##" VEP header section back onto finalmergedlists.
                let gfinaloutput = gonlyheadersspaces ++ gfinalmergedliststabbed
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options gfinaloutput
                            return ()
                        else noGzipPrintFile options gfinaloutput
                else catFile gfinaloutput

        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Grab only the data portion of processedfile.
                let onlydata = onlyDataVepGrabber processedfile
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
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options finaloutput
                            return ()
                        else noGzipPrintFile options finaloutput
                else catFile finaloutput

--processArgsAndContentsVepTvep -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsVepTvep :: ([Flag],String) -> IO ()
processArgsAndContentsVepTvep ([],[]) = return ()
processArgsAndContentsVepTvep (options,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    --Grab only the data portion of processedfile.
    let onlydata = onlyDataVepGrabber processedfile
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
        --Check to see if outfile is to be gzipped.
        then if DL.elem GzipOut options
            then do
                _ <- gzipPrintFile options finaloutput
                return ()
            else noGzipPrintFile options finaloutput
    else catFile finaloutput

--processArgsAndFilesTvepVep -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesTvepVep :: ([Flag],String) -> IO ()
processArgsAndFilesTvepVep ([],[]) = return ()
processArgsAndFilesTvepVep (options,inputfile) = do
    --Read in the file.
    readinputfile <- SIO.readFile inputfile
    print "tvepvepfiles"

--processArgsAndContentsTvepVep -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsTvepVep :: ([Flag],String) -> IO ()
processArgsAndContentsTvepVep ([],[]) = return ()
processArgsAndContentsTvepVep (options,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    print "tvepvepcontents"

--processArgsAndFilesVcfTvcf -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesVcfTvcf :: ([Flag],String) -> IO ()
processArgsAndFilesVcfTvcf ([],[]) = return ()
processArgsAndFilesVcfTvcf (options,inputfile) = do 
    --Check to see if inputfile is gzip compressed.
    if DL.elem GzipIn options
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lineFeed function to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Apply lines to gunzippedfile.
                let gprocessedfilenew = DL.lines gunzippedfile
                --Grab only the INFO portion of processedfile.
                let gonlyinfo = onlyInfoGrabber gprocessedfile
                --Grab only the data lines in gprocessedfile.
                let gdataonly = onlyDataVcfGrabber gprocessedfile
                --Grab all metadata lines from gprocessedfile.
                let gallmetadata = onlyMetadataGrabber gprocessedfilenew
                --Grab only the INFO Field of gdataonly.
                let ginfodataonly = DL.map (\x -> [x DL.!! 7]) (DL.tail gdataonly) 
                --Grab all head fields except for the INFO field of gdataonly.
                let gheadinfodataonly = DL.map (DL.take 7) (DL.tail gdataonly)
                --Grab all tail fields except for the INFO field of gdataonly.
                let gtailinfodataonly = DL.map (DL.drop 8) (DL.tail gdataonly)
                --Split the subfields of gdataonly by semicolon delimiter.
                let gsplitsemicolon = DL.map (DL.map (DLS.splitOn ";")) ginfodataonly
                --Grab only data with equals from gsplitsemicolon.
                let gonlyequalssplitsemicolon = infixFilter gsplitsemicolon
                --Turn gonlyequalssplitsemicolon from lists into tuples.
                let gsplitonlyequals = DL.map (DL.map (DL.map (\x -> DLS.splitOneOf "=" x))) gonlyequalssplitsemicolon
                --Split each subfield of the subfields of gsplitsemicolon by equal-sign delimiter.
                let gsplitequals = DL.map (DL.map (DL.map (DLS.splitOn "="))) gsplitsemicolon
                --Grab all elements except for the for the list with "CSQ" as the first (head) element.
                let gnotcsqsplitequals = DL.concat (DL.map (DL.map (DL.filter (\x -> DL.head x /= "CSQ"))) gsplitequals)
                --Grab the index of the list with "CSQ" as the first (head) element.
                let gcsqsplitequalsindex = DL.map (DL.map (DM.fromJust)) (DL.map (DL.map (DL.elemIndex "CSQ")) (DL.map (DL.map (DL.map (DL.head))) gsplitequals))
                --Grab only the list with "CSQ" as the first (head) element.
                let gcsqsplitequals = DL.map (DL.map (DL.filter (\x -> DL.head x == "CSQ"))) gsplitequals
                --Grab only the last element of gheadsplitequals
                let glastcsq = DL.map (DL.last) gcsqsplitequals
                --Split each subfield of the sublists of gsplitequals by comma delimiter.
                let gsplitcomma = DL.map (DL.map (DL.map (DLS.splitOn ","))) glastcsq
                --Grab the last field of gsplitcomma.
                let glastsplitcomma = DL.map (DL.map (DL.last)) gsplitcomma
                --Split each item in glastsplitcomma by | delimiter.
                let gsplitlastsplitcomma = DL.map (DL.map (DL.map (DLS.splitOn "|"))) glastsplitcomma
                --Grab just the headers for the fields of gonlyinfo.
                let gheadersonlyinfo = DL.nub (DL.map (DL.head) (DL.concat (DL.concat gsplitequals)))
                --Grab just the CSQ header from gonlyinfo.
                let gcsqheadersonlyinfo = DL.concat (DL.concat (DL.filter (DL.any (\x -> DL.isInfixOf "##INFO=<ID=CSQ" x)) gonlyinfo))
                --Grab the subfields from gcsqheadersonlyinfo.
                let gsubfieldscsqheadersonlyinfo = DLS.splitOn "|" (DL.filter (\x -> x /= '"' && x /= '>') (DL.last (DLS.splitOn ":" gcsqheadersonlyinfo)))
                --Remove "CSQ" from gheadersonlyinfo.
                let gfinalheadersonlyinfo =  DLS.splitWhen (\x -> x == "CSQ") gheadersonlyinfo
                --Insert gsubfieldscsqheadersonlyinfo into gfinalheadersonlyinfo.
                let gfinalheaders = DL.concat (insertSubfields gfinalheadersonlyinfo gsubfieldscsqheadersonlyinfo)
                --Grab data header from gdataonly.
                let gdataheader = DL.concat (DL.filter (\x -> DL.head x == "#CHROM") gdataonly)
                --Remove "INFO" from gdataheader.
                let gfinaldataheader = DLS.splitWhen (\x -> x == "INFO") gdataheader
                --Insert finalheaders into finaldataheader.
                let gtruefinalheader = DL.concat (insertSubfields gfinaldataheader gfinalheaders)
                --Remove CSQ from gtruefinalheader.
                let gactualtruefinalheader = DLS.splitWhen (\x -> x == "CSQ") gtruefinalheader
                --Relicate gheadinfodataonly the correct number of times.
                let gheadreplicateddata = dataReplicator gheadinfodataonly (DL.concat (DL.map (DL.map (DL.length)) gsplitlastsplitcomma))
                --Replicate gtailinfodataonly the correct number of times.
                let gtailreplicateddata = dataReplicator gtailinfodataonly (DL.concat (DL.map (DL.map (DL.length)) gsplitlastsplitcomma)) 
                --Concatenate gsplitlastsplitcomma.
                let gconcatsplitlastsplitcomma = (DL.concat (DL.concat gsplitlastsplitcomma))
                let gannotatedcsqheader = csqFieldsAdder gsplitlastsplitcomma gsubfieldscsqheadersonlyinfo
                let gannotatedcsqheaderfinal = headerFieldsAdder gconcatsplitlastsplitcomma gsubfieldscsqheadersonlyinfo
                let gconcatannotatedcsqheader = DL.concat (DL.concat (gannotatedcsqheader))
                let gannotatedheadfields = headerFieldsAdder gheadreplicateddata (DL.head gfinaldataheader)
                let gannotatedtailfields = headerFieldsAdder gtailreplicateddata (DL.last gfinaldataheader)
                let gannotatednotcsqfields = notCsqFieldsAdder gnotcsqsplitequals
                let gannotatednotcsqfieldsaddedback = notCsqFieldsAddedBack gannotatednotcsqfields (DL.concat gfinalheadersonlyinfo)
                let greplicatedannotatednotcsqfinal = dataReplicator (DL.map (DL.concat) gannotatednotcsqfieldsaddedback) (DL.concat (DL.map (DL.map (DL.length)) gsplitlastsplitcomma))
                let gfinalizedinfofield = combineInfoFields greplicatedannotatednotcsqfinal gannotatedcsqheaderfinal
                let gfinalinfofield = DL.map (DL.concat) gfinalizedinfofield
                let gfinalizeddata = dataCombinator gannotatedheadfields gfinalinfofield gannotatedtailfields
                let gsortedfinalizeddata = DL.map (DL.sortBy (\(a,_) (b,_) -> compare a b)) gfinalizeddata
                let gtailfinalizeddata = DL.map (DL.map (\(a,b) -> b)) gsortedfinalizeddata
                let gsortedactualtruefinalheader = DL.map (DL.sort) gactualtruefinalheader
                let gfinalfinaldata = [mapNotLast (++ "\n") gallmetadata] ++ (DL.map (mapNotLast (++ "\t")) gsortedactualtruefinalheader)
                                                   ++ (DL.map (mapNotLast (++ "\t")) (notApplicableAdder gtailfinalizeddata))
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options gfinalfinaldata
                            return ()
                        else noGzipPrintFile options gfinalfinaldata
                else catFile gfinalfinaldata
               
        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Apply lines to inputfile.
                let processedfilenew = DL.lines readinputfile
                --Grab only the data portion of processedfile.
                let onlyinfo = onlyInfoGrabber processedfile
                --Grab only the data lines in processedfile.
                let dataonly = onlyDataVcfGrabber processedfile
                --Grab all metadata lines from processedfile.
                let allmetadata = onlyMetadataGrabber processedfilenew
                --Grab only the INFO field of dataonly.
                let infodataonly = DL.map (\x -> [x DL.!! 7]) (DL.tail dataonly)
                --Grab all head fields except for the INFO field of dataonly.
                let headinfodataonly = DL.map (DL.take 7) (DL.tail dataonly)
                --Grab all tail fields except for the INFO field of dataonly.
                let tailinfodataonly = DL.map (DL.drop 8) (DL.tail dataonly)
                --Split the subfields of gdataonly by semicolon delimiter.
                let splitsemicolon = DL.map (DL.map (DLS.splitOn ";")) infodataonly
                --Grab only data with equals from splitsemicolon.
                let onlyequalssplitsemicolon = infixFilter splitsemicolon 
                --Turn onlyequalssplitsemicolon from lists into tuples.
                let splitonlyequals = DL.map (DL.map (DL.map (\x -> DLS.splitOneOf "=" x))) onlyequalssplitsemicolon
                --Split each subfield of the subfields of splitsemicolon by equal-sign delimiter.
                let splitequals = DL.map (DL.map (DL.map (DLS.splitOn "="))) splitsemicolon
                --Grab only the list with "CSQ" as the first (head) element.
                let csqsplitequals = DL.map (DL.map (DL.filter (\x -> DL.head x == "CSQ"))) splitequals
                --Grab all elements except for the for the list with "CSQ" as the first (head) element.
                let notcsqsplitequals = DL.concat (DL.map (DL.map (DL.filter (\x -> DL.head x /= "CSQ"))) splitequals)
                --Grab the index of the list with "CSQ" as the first (head) element.
                let csqsplitequalsindex = DL.map (DL.map (DM.fromJust)) (DL.map (DL.map (DL.elemIndex "CSQ")) (DL.map (DL.map (DL.map (DL.head))) splitequals))
                --Grab only the last element of csqsplitequals.
                let lastcsq = DL.map (DL.last) csqsplitequals
                --Split each subfield of the sublists of splitequals by comma delimiter.
                let splitcomma = DL.map (DL.map (DL.map (DLS.splitOn ","))) lastcsq
                --Grab the last field of splitcomma.
                let lastsplitcomma = DL.map (DL.map (DL.last)) splitcomma 
                --Split each item in lastsplitcomma by | delimiter.
                let splitlastsplitcomma = DL.map (DL.map (DL.map (DLS.splitOn "|"))) lastsplitcomma
                --Grab just the headers for the fields of onlyinfo.
                let headersonlyinfo = DL.nub (DL.map (DL.head) (DL.concat (DL.concat splitequals)))
                --Grab just the CSQ header from onlyinfo.
                let csqheadersonlyinfo = DL.concat (DL.concat (DL.filter (DL.any (\x -> DL.isInfixOf "##INFO=<ID=CSQ" x)) onlyinfo))
                --Grab the subfields from csqheadersonlyinfo.
                let subfieldscsqheadersonlyinfo = DLS.splitOn "|" (DL.filter (\x -> x /= '"' && x /= '>') (DL.last (DLS.splitOn ":" csqheadersonlyinfo)))
                --Remove "CSQ" from headersonlyinfo.
                let finalheadersonlyinfo =  DLS.splitWhen (\x -> x == "CSQ") headersonlyinfo
                --Insert subfieldscsqheadersonlyinfo into finalheadersonlyinfo.
                let finalheaders = DL.concat (insertSubfields finalheadersonlyinfo subfieldscsqheadersonlyinfo)
                --Grab data header from dataonly.
                let dataheader = DL.concat (DL.filter (\x -> DL.head x == "#CHROM") dataonly)
                --Remove "INFO" from dataheader.
                let finaldataheader = DLS.splitWhen (\x -> x == "INFO") dataheader
                --Insert finalheaders into finaldataheader.
                let truefinalheader = DL.concat (insertSubfields finaldataheader finalheaders) 
                --Remove CSQ from truefinalheader.
                let actualtruefinalheader = DLS.splitWhen (\x -> x == "CSQ") truefinalheader
                --Relicate headinfodataonly the correct number of times.
                let headreplicateddata = dataReplicator headinfodataonly (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
                --Replicate tailinfodataonly the correct number of times.
                let tailreplicateddata = dataReplicator tailinfodataonly (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
                --Concatenate splitlastsplitcomma.
                let concatsplitlastsplitcomma = (DL.concat (DL.concat splitlastsplitcomma))
                let annotatedcsqheader = csqFieldsAdder splitlastsplitcomma subfieldscsqheadersonlyinfo
                let annotatedcsqheaderfinal = headerFieldsAdder concatsplitlastsplitcomma subfieldscsqheadersonlyinfo
                let concatannotatedcsqheader = DL.concat (DL.concat (annotatedcsqheader))
                let annotatedheadfields = headerFieldsAdder headreplicateddata (DL.head finaldataheader)
                let annotatedtailfields = headerFieldsAdder tailreplicateddata (DL.last finaldataheader)
                let annotatednotcsqfields = notCsqFieldsAdder notcsqsplitequals
                let annotatednotcsqfieldsaddedback = notCsqFieldsAddedBack annotatednotcsqfields (DL.concat finalheadersonlyinfo) 
                let replicatedannotatednotcsqfinal = dataReplicator (DL.map (DL.concat) annotatednotcsqfieldsaddedback) (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
                let finalizedinfofield = combineInfoFields replicatedannotatednotcsqfinal annotatedcsqheaderfinal
                let finalinfofield = DL.map (DL.concat) finalizedinfofield
                let finalizeddata = dataCombinator annotatedheadfields finalinfofield annotatedtailfields
                let sortedfinalizeddata = DL.map (DL.sortBy (\(a,_) (b,_) -> compare a b)) finalizeddata
                let tailfinalizeddata = DL.map (DL.map (\(a,b) -> b)) sortedfinalizeddata
                let sortedactualtruefinalheader = DL.map (DL.sort) actualtruefinalheader
                let finalfinaldata = [mapNotLast (++ "\n") allmetadata] ++ (DL.map (mapNotLast (++ "\t")) sortedactualtruefinalheader)
                                                  ++ (DL.map (mapNotLast (++ "\t")) (notApplicableAdder tailfinalizeddata))
                --Print the file to stdout (cat) or to a file.
                if DL.length (DL.filter (isOutputFile) options) > 0
                    --Check to see if outfile is to be gzipped.
                    then if DL.elem GzipOut options
                        then do
                            _ <- gzipPrintFile options finalfinaldata
                            return ()
                       else noGzipPrintFile options finalfinaldata
                else catFile finalfinaldata
 
--processArgsAndContentsVcfTvcf -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsVcfTvcf :: ([Flag],String) -> IO ()
processArgsAndContentsVcfTvcf ([],[]) = return ()
processArgsAndContentsVcfTvcf (options,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Apply lines to inputfile.
    let processedfilenew = DL.lines content
    --Grab only the data portion of processedfile.
    let onlyinfo = onlyInfoGrabber processedfile
    --Grab only the data lines in processedfile.
    let dataonly = onlyDataVcfGrabber processedfile
    --Grab all metadata lines from processedfile.
    let allmetadata = onlyMetadataGrabber processedfilenew
    --Grab only the INFO field of dataonly.
    let infodataonly = DL.map (\x -> [x DL.!! 7]) (DL.tail dataonly)
    --Grab all head fields except for the INFO field of dataonly.
    let headinfodataonly = DL.map (DL.take 7) (DL.tail dataonly)
    --Grab all tail fields except for the INFO field of dataonly.
    let tailinfodataonly = DL.map (DL.drop 8) (DL.tail dataonly)
    --Split the subfields of gdataonly by semicolon delimiter.
    let splitsemicolon = DL.map (DL.map (DLS.splitOn ";")) infodataonly
    --Grab only data with equals from splitsemicolon.
    let onlyequalssplitsemicolon = infixFilter splitsemicolon
    --Turn onlyequalssplitsemicolon from lists into tuples.
    let splitonlyequals = DL.map (DL.map (DL.map (\x -> DLS.splitOneOf "=" x))) onlyequalssplitsemicolon
    --Split each subfield of the subfields of splitsemicolon by equal-sign delimiter.
    let splitequals = DL.map (DL.map (DL.map (DLS.splitOn "="))) splitsemicolon
    --Grab only the list with "CSQ" as the first (head) element.
    let csqsplitequals = DL.map (DL.map (DL.filter (\x -> DL.head x == "CSQ"))) splitequals
    --Grab all elements except for the for the list with "CSQ" as the first (head) element.
    let notcsqsplitequals = DL.concat (DL.map (DL.map (DL.filter (\x -> DL.head x /= "CSQ"))) splitequals)
    --Grab the index of the list with "CSQ" as the first (head) element.
    let csqsplitequalsindex = DL.map (DL.map (DM.fromJust)) (DL.map (DL.map (DL.elemIndex "CSQ")) (DL.map (DL.map (DL.map (DL.head))) splitequals))
    --Grab only the last element of csqsplitequals.
    let lastcsq = DL.map (DL.last) csqsplitequals
    --Split each subfield of the sublists of splitequals by comma delimiter.
    let splitcomma = DL.map (DL.map (DL.map (DLS.splitOn ","))) lastcsq
    --Grab the last field of splitcomma.
    let lastsplitcomma = DL.map (DL.map (DL.last)) splitcomma
    --Split each item in lastsplitcomma by | delimiter.
    let splitlastsplitcomma = DL.map (DL.map (DL.map (DLS.splitOn "|"))) lastsplitcomma
    --Grab just the headers for the fields of onlyinfo.
    let headersonlyinfo = DL.nub (DL.map (DL.head) (DL.concat (DL.concat splitequals)))
    --Grab just the CSQ header from onlyinfo.
    let csqheadersonlyinfo = DL.concat (DL.concat (DL.filter (DL.any (\x -> DL.isInfixOf "##INFO=<ID=CSQ" x)) onlyinfo))
    --Grab the subfields from csqheadersonlyinfo.
    let subfieldscsqheadersonlyinfo = DLS.splitOn "|" (DL.filter (\x -> x /= '"' && x /= '>') (DL.last (DLS.splitOn ":" csqheadersonlyinfo)))
    --Remove "CSQ" from headersonlyinfo.
    let finalheadersonlyinfo =  DLS.splitWhen (\x -> x == "CSQ") headersonlyinfo
    --Insert subfieldscsqheadersonlyinfo into finalheadersonlyinfo.
    let finalheaders = DL.concat (insertSubfields finalheadersonlyinfo subfieldscsqheadersonlyinfo)
    --Grab data header from dataonly.
    let dataheader = DL.concat (DL.filter (\x -> DL.head x == "#CHROM") dataonly)
    --Remove "INFO" from dataheader.
    let finaldataheader = DLS.splitWhen (\x -> x == "INFO") dataheader
    --Insert finalheaders into finaldataheader.
    let truefinalheader = DL.concat (insertSubfields finaldataheader finalheaders)
    --Remove CSQ from truefinalheader.
    let actualtruefinalheader = DLS.splitWhen (\x -> x == "CSQ") truefinalheader
    --Relicate headinfodataonly the correct number of times.
    let headreplicateddata = dataReplicator headinfodataonly (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
    --Replicate tailinfodataonly the correct number of times.
    let tailreplicateddata = dataReplicator tailinfodataonly (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
    --Concatenate splitlastsplitcomma.
    let concatsplitlastsplitcomma = (DL.concat (DL.concat splitlastsplitcomma))
    let annotatedcsqheader = csqFieldsAdder splitlastsplitcomma subfieldscsqheadersonlyinfo
    let annotatedcsqheaderfinal = headerFieldsAdder concatsplitlastsplitcomma subfieldscsqheadersonlyinfo
    let concatannotatedcsqheader = DL.concat (DL.concat (annotatedcsqheader))
    let annotatedheadfields = headerFieldsAdder headreplicateddata (DL.head finaldataheader)
    let annotatedtailfields = headerFieldsAdder tailreplicateddata (DL.last finaldataheader)
    let annotatednotcsqfields = notCsqFieldsAdder notcsqsplitequals
    let annotatednotcsqfieldsaddedback = notCsqFieldsAddedBack annotatednotcsqfields (DL.concat finalheadersonlyinfo)
    let replicatedannotatednotcsqfinal = dataReplicator (DL.map (DL.concat) annotatednotcsqfieldsaddedback) (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
    let finalizedinfofield = combineInfoFields replicatedannotatednotcsqfinal annotatedcsqheaderfinal
    let finalinfofield = DL.map (DL.concat) finalizedinfofield
    let finalizeddata = dataCombinator annotatedheadfields finalinfofield annotatedtailfields
    let sortedfinalizeddata = DL.map (DL.sortBy (\(a,_) (b,_) -> compare a b)) finalizeddata
    let tailfinalizeddata = DL.map (DL.map (\(a,b) -> b)) sortedfinalizeddata
    let sortedactualtruefinalheader = DL.map (DL.sort) actualtruefinalheader
    let finalfinaldata = [mapNotLast (++ "\n") allmetadata] ++ (DL.map (mapNotLast (++ "\t")) sortedactualtruefinalheader)
                                      ++ (DL.map (mapNotLast (++ "\t")) (notApplicableAdder tailfinalizeddata))
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFile) options) > 0
        --Check to see if outfile is to be gzipped.
        then if DL.elem GzipOut options
            then do
                _ <- gzipPrintFile options finalfinaldata
                return ()
            else noGzipPrintFile options finalfinaldata
    else catFile finalfinaldata

--processArgsAndFilesTvcfVcf -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesTvcfVcf :: ([Flag],String) -> IO ()
processArgsAndFilesTvcfVcf ([],[]) = return ()
processArgsAndFilesTvcfVcf (options,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if DL.elem GzipIn options
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lineFeed function to inputfile.
                let gprocessedfile = lineFeed gunzippedfile
                print gprocessedfile
        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                print processedfile

--processArgsAndContentsTvcfVcf -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsTvcfVcf :: ([Flag],String) -> IO ()
processArgsAndContentsTvcfVcf ([],[]) = return ()
processArgsAndContentsTvcfVcf (options,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    --Grab only the data portion of processedfile.
    print "tvcfvcfcontents"

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
                --Vep or vcf parsing pipeline?
                if (fst (vepOrVcfPipeline args) == "vep" && snd (vepOrVcfPipeline args) == "tvep") 
                    then do --Run args and contents through processArgsAndContentsVepTvep.
                            processArgsAndContentsVepTvep (args,contents)
                    else if (fst (vepOrVcfPipeline args) == "tvep" && snd (vepOrVcfPipeline args) == "vep")
                        then do --Run args and contents through processArgsAndContentsTvepVep.
                                processArgsAndContentsTvepVep (args,contents)
                        else if (fst (vepOrVcfPipeline args) == "vcf" && snd (vepOrVcfPipeline args) == "tvcf")
                            then do --Run args and contents through processArgsAndContentsVcfTvcf.
                                    processArgsAndContentsVcfTvcf (args,contents)
                            else do --Run args and contents through processArgsAndContentsTvcfVcf.
                                    processArgsAndContentsTvcfVcf (args,contents) 

        else do --Vep or vcf parsing pipeline?
                if (fst (vepOrVcfPipeline args) == "vep" && snd (vepOrVcfPipeline args) == "tvep")
                    then do --Run args and contents through processArgsAndFilesVepTvep.
                            processArgsAndFilesVepTvep (args,files)
                    else if (fst (vepOrVcfPipeline args) == "tvep" && snd (vepOrVcfPipeline args) == "vep")
                        then do --Run args and contents through processArgsAndFilesTvepVep.
                                processArgsAndFilesTvepVep (args,files)
                        else if (fst (vepOrVcfPipeline args) == "vcf" && snd (vepOrVcfPipeline args) == "tvcf")
                            then do --Run args and contents through processArgsAndFilesVcfTvcf.
                                    processArgsAndFilesVcfTvcf (args,files)
                            else do --Run args and contents through processArgsAndFilesTvcfVcf.
                                    processArgsAndFilesTvcfVcf (args,files)

{----------------}
