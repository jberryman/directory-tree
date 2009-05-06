module DirectoryTree (readDirectory, readDirectoryWith,
                      writeDirectory, writeDirectoryWith,
                      build, openDirectory, writeJustDirs, 
                      zipPaths, free,
                      -- utilities:
                      files, filesWithPaths,
                      anyFailed, failures, failedMap,
                      -- Types:
                      DirTree (..), AnchoredDirTree (..), FileName)
    where

{- 

NOTES:
    a simple interface to directories. DirTree's free variable can hold
    file paths, strings to be written to files in a directory tree, 
    file-handles, byte-strings read with 'readFile',etc.

    the AnchoredDirTree type is simply a wrapper that allows us to store,
    and return the base file path of the directory.

    we don't provide many special functions for only working within the 
    current directory. it should be easy enough to create a DirTree
    "Anchored" at the current directory like so:
         anchorAtCurrent dTree = "." :/: dTree

IDEAS:
    an informal comonad instance might might make sense: for example, cobind 
    to convert Failed constructors to Files or Dirs, or cojoin to allow us to 
    use the traversable/foldable functions over an entire File/Failed 
    constructor.

TODO:
    - tree combining functions
    - ...
-}


import System.Directory
import System.FilePath
import System.IO
import Control.Exception (handle, Exception)

import Data.Function (on)
import Data.List (sort, (\\)) --partition
import Control.Monad (liftM, filterM, liftM2, ap)

import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F




-- the String in Dir is always the directory name, never a full path. We
-- provide functions to convert a DirTree String (where String is a filename)
-- to a DirTree FilePath, where the File constructor holds a full path to a
-- file (relative or absolute) rather than a bare file name.
data DirTree a = Dir { name     :: FileName,
                       contents :: [DirTree a]  } --files + directories
               | File { name :: FileName,
                        file :: a }
               | Failed { name :: FileName,
                          err  :: Exception }
                 deriving (Show, Eq)


instance (Ord a)=> Ord (DirTree a) where
    compare = compare `on` name


-- a simple wrapper to hold a base directory name, which can be either 
-- an absolute or relative path. This lets us give the DirTree a context,
-- while still letting us store only directory and file NAMES (not full paths)
-- in the DirTree.
data AnchoredDirTree a = FilePath :/ DirTree a
                     deriving (Show, Ord, Eq)

-- an element in a FilePath:
type FileName = String


instance Functor DirTree where
    fmap = T.fmapDefault 

instance F.Foldable DirTree where
    foldMap = T.foldMapDefault

instance T.Traversable DirTree where
    traverse f (Dir n cs)   = Dir n <$> T.traverse (T.traverse f) cs
    traverse f (File n a)   = File n <$> f a
    traverse f (Failed n e) = pure (Failed n e)




   
    ----------------------------
    --[ HIGH LEVEL FUNCTIONS ]--
    ----------------------------


-- build an AnchoredDirTree, given the path to a directory, opening the files
-- using readFile.
readDirectory :: FilePath -> IO (AnchoredDirTree String)
readDirectory = readDirectoryWith readFile

-- same as readDirectory but allows us to, for example, use ByteString.readFile
-- to return a tree of ByteStrings.
readDirectoryWith :: (FilePath -> IO a) -> FilePath -> IO (AnchoredDirTree a)
readDirectoryWith f p = do (b:/t) <- build p
                           t'     <- T.mapM f t
                           return $ b:/t'
                        

-- write a DirTree of strings to disk. clobbers files of the same name. doesn't
-- affect files in the directories (if any already exist) with different names:
writeDirectory :: AnchoredDirTree String -> IO ()
writeDirectory = writeDirectoryWith writeFile

-- see: readDirectoryWith
writeDirectoryWith :: (FilePath -> a -> IO ()) -> AnchoredDirTree a -> IO ()
writeDirectoryWith f t = do writeJustDirs t
                            F.mapM_ (uncurry f) (zipPaths t)



-- a simple application of readDirectoryWith openFile:
openDirectory :: FilePath -> IOMode -> IO (AnchoredDirTree Handle)
openDirectory p m = readDirectoryWith (flip openFile m) p


-- strips away base directory wrapper:
free :: AnchoredDirTree a -> DirTree a
free (_:/t) = t


    -----------------------------
    --[ LOWER LEVEL FUNCTIONS ]--
    -----------------------------


-- builds a DirTree from the contents of the directory passed to it, saving the
-- base directory in the Anchored* wrapper. Errors are caught in the tree in 
-- the Failed constructor. The 'file' fields initially contain full paths
-- to the files they are abstracting.
build :: FilePath -> IO (AnchoredDirTree FilePath)
build p = do let base = baseDir p
             tree <- build' p
             return (base :/ tree)
                     
-- HELPER: not exported:
build' :: FilePath -> IO (DirTree FilePath)
build' p = 
    handle (return . Failed n) $ 
           do isFile <- doesFileExist p    
              if isFile                         
                  -- store full path to the file in 'file' field:
                 then return (File n p)              
                  -- else is directory, build a Dir from contents:
                 else do cs <- getDirsFiles p
                         Dir n <$> T.mapM (build' . combine p) cs
      -- the directory to build, located under "base":
     where n = topDir p





                     
                                
    -----------------
    --[ UTILITIES ]--
    -----------------


---- CHECKING ----

anyFailed :: DirTree a -> Bool
anyFailed = not . null . failures

-- ???
--validate :: DirTree a -> Bool



---- FLATTENING ----

-- flatten Dir to a list of contents of Files. synonym for Data.Foldable.toList.
files :: DirTree a -> [a]
files = F.toList

-- collapses directory structure, returning File contents tupled with its 
-- associated FilePath 
filesWithPaths :: AnchoredDirTree a -> [(FilePath,a)]
filesWithPaths = F.toList . zipPaths

-- returns a list of 'Failed' constructors only:
failures :: DirTree a -> [DirTree a]
failures (Dir _ cs) = concatMap failures cs
failures (File _ _) = []
failures f          = [f]



---- HANDLING FAILURES ----

-- maps a function to convert Failed DirTrees to Files or Dirs
failedMap :: (FileName -> Exception -> DirTree a) -> DirTree a -> DirTree a
failedMap f (Dir n cs)   = Dir n $map (failedMap f) cs
failedMap f (Failed n e) = f n e
failedMap _ fle          = fle




    ---------------
    --[ HELPERS ]--
    ---------------


---- PATH CONVERSIONS ----



-- tuple up the complete filename with the File contents, by building up the 
-- path, trie-style, from the root. The filepath will be relative to the current
-- directory.
-- This allows us to, for example, mapM_ 'uncurry writeFile' over a DirTree of 
-- strings. 
zipPaths :: AnchoredDirTree a -> DirTree (FilePath, a)
zipPaths (b :/ t) = zipP b t
    where zipP p (File n a)   = File n (p</>n , a)
          zipP p (Dir n cs)   = Dir n $ map (zipP $ p</>n) cs
          zipP p (Failed n e) = Failed n e


-- extracting pathnames and base names:
topDir = last . splitDirectories 

baseDir = joinPath . init . splitDirectories



---- IO HELPERS: ----


-- writes the directory structure (not files) of a DirTree to the anchored 
-- directory. can be preparation for writing files:
writeJustDirs :: AnchoredDirTree a -> IO ()
writeJustDirs (b:/t) = write' b t
    where write' b' (Dir n cs) = do let bas = b' </> n
                                    createDirectoryIfMissing True bas
                                    mapM_ (write' bas) cs
          write' _ _           = return ()


----- the let expression is an annoying hack, because dropFileName "." == ""
----- and getDirectoryContents fails epically on ""
-- prepares the directory contents list. we sort so that we can be sure of 
-- a consistent fold/traversal order on the same directory:
getDirsFiles cs = do let cs' = if null cs then "." else cs 
                     dfs <- getDirectoryContents cs'
                     return $ sort $ dfs \\ [".",".."]
