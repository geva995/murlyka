module MakeContents(
  handleMakefileDescr
) where

import qualified Data.List        as Lists
import          System.Directory
import          Split
import          System.FilePath
import          Parser
import qualified Scaner           as SC

data Info1=Info1{proj_name    :: String,
                main_f       :: String,
                compiler_name:: String,
                compiler_flgs:: String,
                linker_flgs  :: String,
                linker_name  :: String,
                src_dir      :: String,
                src_exts     :: [String],
                build_d      :: String,
                incl_dirs    :: [String],
                makefile_n   :: String }
  deriving (Eq,Ord,Show)

convertInfo::Info->Either String Info1
convertInfo i = Right i1
  where
    i1=Info1{proj_name     = name . project $ i,
             main_f        = main_file . project $ i,
             compiler_name = compiler i,
             compiler_flgs = compiler_flags i,
             linker_name   = linker i,
             src_dir       = source_dir i,
             src_exts      = Lists.words . source_exts $ i,
             linker_flgs   = linker_flags i,
             build_d       = build_dir i,
             incl_dirs     = filter (not . null) . splitBy ';' $ include_dirs i,
             makefile_n    = makefile_name i }

correctInfo1::Info1->Either String Info1
correctInfo1 = 
  Right               . correctMakefileName  . correctSourceExts    . 
  correctLinkerFlags  . correctLinkerName    . correctCompilerFlags . 
  correctCompilerName . correctPNandMainfile

correctPNandMainfile::Info1->Info1
correctPNandMainfile i 
  |(null pn)         && (null mf)         = i{proj_name = "main", main_f="main.cpp"}
  |(null pn)         && (not . null $ mf) = i{proj_name = dropExtension  mf}
  |(not . null $ pn) && (null mf)         = i{main_f = pn ++ ".cpp"}
  |otherwise                             = i 
  where
    pn = proj_name i
    mf = main_f    i 

correctCompilerName::Info1->Info1
correctCompilerName i =
  if null cn then i{compiler_name = "g++"} else i
  where
    cn = compiler_name i

correctCompilerFlags::Info1->Info1
correctCompilerFlags i =
  if null cf then i{compiler_flgs = "-O3 -Wall -std=c++14"} else i
  where
    cf=compiler_flgs i

correctLinkerName::Info1->Info1
correctLinkerName i =
  if null ln then i{linker_name = compiler_name i} else i
  where
    ln=linker_name i

correctLinkerFlags::Info1->Info1
correctLinkerFlags i =
  if null lf then i{linker_flgs = "-s"} else i
  where
    lf=linker_flgs i

correctSourceExts::Info1->Info1
correctSourceExts i =
  if null se then i{src_exts=["cpp"]} else i
  where
    se=src_exts i

correctMakefileName::Info1->Info1
correctMakefileName i =
  if null . makefile_n $ i then i{makefile_n = "Makefile"} else i

txt2Info1::String->Either String Info1
txt2Info1 t = do
  lexStream <- SC.scaner t
  i         <- parser lexStream
  i1        <- convertInfo i
  correctInfo1 i1

usualContents::FilePath->IO [FilePath]
usualContents p = do
  contents<-getDirectoryContents p
  return $ filter (\n -> (n/=".") && (n/="..")) contents

sourceFiles::FilePath->[String]->IO [FilePath]
sourceFiles p exts = do 
  files <- usualContents p
  return $ filter (\n -> admissibleName n exts) files

admissibleExt :: String -> [String] -> Bool
admissibleExt [] _     = False
admissibleExt e  exts  = (tail e) `elem` exts

admissibleName :: FilePath -> [String] -> Bool
admissibleName n exts = admissibleExt (takeExtension n) exts

makefileText :: Info1 -> IO String
makefileText i = do
  o <-  obj i
  lo <- linkobj i
  return $ Lists.intercalate "\n\n" [
    cppLinkerCompilerBin i ++ vpath i ++ o ++ lo, 
    phony,                         
    target_all, 
    clean i,
    rules_for_srcs i,
    bin_sec i]

cppLinkerCompilerBin::Info1->String
cppLinkerCompilerBin i =
  "LINKER      = " ++ linker_name   i ++ "\n" ++
  "LINKERFLAGS = " ++ linker_flgs   i ++ "\n" ++
  "CXX         = " ++ compiler_name i ++ "\n" ++
  "CXXFLAGS    = " ++ compiler_flgs i ++ "\n" ++
  "BIN         = " ++ proj_name     i

phony::String
phony=".PHONY: all all-before all-after clean clean-custom"

target_all::String
target_all = "all: all-before $(BIN) all-after"

clean::Info1->String
clean i = 
  "clean: clean-custom \n\t" ++
  "rm -f " ++ prefix ++ "*.o\n\t" ++
  "rm -f " ++ prefix ++ "$(BIN)"
  where
    b      = build_d i
    prefix = if not . null $ b then "./" ++ b ++ "/"  else "./"

includes::Info1->String
includes i =
  Lists.intercalate " " . map (\x-> "-I\""++x++ "\"") $ incl
  where 
    incl = incl_dirs i

src_vpath::Info1->String
src_vpath i = 
  if null srcs then
    ""
  else
    Lists.intercalate "\n" . map (\e-> "vpath %." ++ e ++ " " ++ srcs ) $ exts
  where 
    srcs = src_dir  i 
    exts = src_exts i

objects_vpath::Info1->String
objects_vpath i =
  if null b then
    ""
  else
    "vpath %.o " ++ b
  where 
    b = build_d i

vpath::Info1->String
vpath i = if null v then "" else "\n" ++ v
  where 
    v = Lists.intercalate "\n" . filter (\s -> not . null $ s) $ [
      src_vpath i, objects_vpath i]

rules_for_srcs::Info1->String
rules_for_srcs i = 
  Lists.intercalate "\n\n" . map (
    \e-> "." ++ e ++ ".o:\n\t" ++ "$(CXX) -c $< -o $@ $(CXXFLAGS) " ++ incl ++
         (if not . null $ b then "\n\t" ++ "mv $@ ./" ++ b else "") 
  ) $ exts
  where 
    exts = src_exts i
    incl = includes i
    b    = build_d  i

bin_sec::Info1->String
bin_sec i =
  "$(BIN):$(OBJ)\n\t"++
  "$(LINKER) -o $(BIN) $(LINKOBJ) $(LINKERFLAGS)\n\t"++
  if null b then "" else ("mv $(BIN) ./"++ b)
  where
    b=build_d i

obj'' :: Info1 -> IO [String]
obj'' i = do
  sdir   <- srcdir i
  fnames <- sourceFiles sdir . src_exts $ i
  return . map (\f->replaceExtension f "o") $ mf:(Lists.delete mf fnames)
  where
    mf     = main_f i

obj' :: Info1 -> IO String
obj' i = do
  o'' <- obj'' i
  return . Lists.intercalate " " $ o''

obj :: Info1 -> IO String
obj i = do
  o' <- obj' i
  return $ "\nOBJ         = " ++ o'

linkobj' :: Info1 -> IO String
linkobj' i = do
  o'' <- obj'' i
  return . Lists.intercalate " " . map (prefix ++) $ o''
  where
    b      = build_d i
    prefix = if not . null $ b then b ++ "/"  else ""

linkobj :: Info1 ->  IO String
linkobj i = do 
  lo' <- linkobj' i
  return $ "\nLINKOBJ     = " ++ lo'

srcdir :: Info1-> IO String
srcdir i =
  if null sd then getCurrentDirectory else return sd
  where
    sd = src_dir i

writeMakefile :: Info1->IO()
writeMakefile i = do
  mft <- makefileText i
  writeFile mkf mft
  where
    mkf = makefile_n   i

handleMakefileDescr :: FilePath -> IO ()
handleMakefileDescr n = do
  mkfDescr <- readFile n
  either putStrLn writeMakefile . txt2Info1 $ mkfDescr