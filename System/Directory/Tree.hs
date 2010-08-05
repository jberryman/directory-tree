
--------------------------------------------------------------------
-- |
-- Module    : System.Directory.Tree
-- Copyright : (c) Brandon Simmons
-- License   : BSD3
--
-- Maintainer:  Brandon Simmons <brandon.m.simmons@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- Provides a simple data structure mirroring a directory tree on the 
-- filesystem, as well as useful functions for reading and writing 
-- file and directory structures in the IO monad. 
-- 
-- Errors are caught in a special constructor in the DirTree type.
-- 
-- Defined instances of Functor, Traversable and Foldable allow for
-- easily operating on a directory of files. For example, you could use
-- Foldable.foldr to create a hash of the entire contents of a directory.
-- 
-- The AnchoredDirTree type is a simple wrapper for DirTree to keep track 
-- of a base directory context for the DirTree. 
--
-- Please send me any requests, bugs, or other feedback on this module!
--
--------------------------------------------------------------------

module System.Directory.Tree (
         
       -- * Data types for representing directory trees
         DirTree (..)
       , AnchoredDirTree (..)
       , FileName
 
       -- * High level IO functions
       , readDirectory
       , readDirectoryWith
       , writeDirectory
       , writeDirectoryWith                            
                                                                        
       -- * Lower level functions
       , zipPaths
       , build
       , openDirectory
       , writeJustDirs                 
                                                                        
       -- * Utility functions
       -- ** Handling failure
       , successful
       , anyFailed
       , failed
       , failures
       , failedMap
       -- ** Misc.
       , free                          
    ) where

{- 
TODO:
    - add whatever needed to make an efficient 'du' simple
        - look at using 'withFile' ?
        - strictness ? what does this do when called on a big 
          directory tree and we only use the top level ?

    - add some tests
    - tree combining functions
    - tree searching based on file names
    - look into comonad abstraction
-}


import System.Directory
import System.FilePath
import System.IO
import Control.Exception (handle, IOException)
import System.IO.Error(ioeGetErrorType,isDoesNotExistErrorType)

import Data.Ord (comparing)
import Data.List (sort, (\\))

import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F




-- | the String in the name field is always a file name, never a full path.
-- The free type variable is used in the File constructor and can hold Handles,
-- Strings representing a file's contents or anything else you can think of.
-- We catch any IO errors in the Failed constructor. an Exception can be 
-- converted to a String with 'show'.
data DirTree a = Dir { name     :: FileName,
                       contents :: [DirTree a]  } 
               | File { name :: FileName,
                        file :: a }
               | Failed { name :: FileName,
                          err  :: IOException }
                 deriving (Show, Eq)


instance (Ord a)=> Ord (DirTree a) where
    compare = comparing name


-- | a simple wrapper to hold a base directory name, which can be either 
-- an absolute or relative path. This lets us give the DirTree a context,
-- while still letting us store only directory and file NAMES (not full paths)
-- in the DirTree. (uses an infix constructor; don't be scared)
data AnchoredDirTree a = FilePath :/ DirTree a
                     deriving (Show, Ord, Eq)

-- | an element in a FilePath:
type FileName = String


instance Functor DirTree where
    fmap = T.fmapDefault 

instance F.Foldable DirTree where
    foldMap = T.foldMapDefault

instance T.Traversable DirTree where
    traverse f (Dir n cs)   = Dir n <$> T.traverse (T.traverse f) cs
    traverse f (File n a)   = File n <$> f a
    traverse _ (Failed n e) = pure (Failed n e)




   
    ----------------------------
    --[ HIGH LEVEL FUNCTIONS ]--
    ----------------------------


-- | build an AnchoredDirTree, given the path to a directory, opening the files
-- using readFile. 
-- Uses `readDirectoryWith` internally and has the effect of traversing the
-- entire directory structure, so is not suitable for running on large directory
-- trees (suggestions or patches welcomed):
readDirectory :: FilePath -> IO (AnchoredDirTree String)
readDirectory = readDirectoryWith readFile

-- | same as readDirectory but allows us to, for example, use 
-- ByteString.readFile to return a tree of ByteStrings.
readDirectoryWith :: (FilePath -> IO a) -> FilePath -> IO (AnchoredDirTree a)
readDirectoryWith f p = do (b:/t) <- build p
                           t'     <- T.mapM f t
                           return $ b:/t'
                        

-- | write a DirTree of strings to disk. clobbers files of the same name. 
-- doesn't affect files in the directories (if any already exist) with 
-- different names:
writeDirectory :: AnchoredDirTree String -> IO ()
writeDirectory = writeDirectoryWith writeFile

-- | writes the directory structure to disc, then uses the provided function to 
-- write the contents of Files to disc. 
writeDirectoryWith :: (FilePath -> a -> IO ()) -> AnchoredDirTree a -> IO ()
writeDirectoryWith f t = do writeJustDirs t
                            F.mapM_ (uncurry f) (zipPaths t)





    -----------------------------
    --[ LOWER LEVEL FUNCTIONS ]--
    -----------------------------


-- | a simple application of readDirectoryWith openFile:
openDirectory :: FilePath -> IOMode -> IO (AnchoredDirTree Handle)
openDirectory p m = readDirectoryWith (flip openFile m) p



-- | builds a DirTree from the contents of the directory passed to it, saving 
-- the base directory in the Anchored* wrapper. Errors are caught in the tree in
-- the Failed constructor. The 'file' fields initially are populated with full 
-- paths to the files they are abstracting.
build :: FilePath -> IO (AnchoredDirTree FilePath)
build p = do let base = baseDir p
             tree <- build' p
              -- we make sure the directory tree is free of non-existent
              -- file errors, which are artifacts of the "non-atomic"
              -- nature of traversing a system firectory tree.
             let treeClean = removeNonexistent tree
             return (base :/ treeClean)
                     
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




---- HANDLING FAILURES ----

-- | True if any Failed constructors in the tree
anyFailed :: DirTree a -> Bool
anyFailed = not . successful

-- | True if there are no Failed constructors in the tree
successful :: DirTree a -> Bool
successful = null . failures


-- | returns true if argument is a `Failed` constructor:
failed (Failed _ _) = True
failed _            = False


-- | returns a list of 'Failed' constructors only:
failures :: DirTree a -> [DirTree a]
failures (Dir _ cs) = concatMap failures cs
failures (File _ _) = []
failures f          = [f]


-- | maps a function to convert Failed DirTrees to Files or Dirs
failedMap :: (FileName -> IOException -> DirTree a) -> DirTree a -> DirTree a
failedMap f (Dir n cs)   = Dir n $map (failedMap f) cs
failedMap f (Failed n e) = f n e
failedMap _ fle          = fle



---- OTHER ----

-- | strips away base directory wrapper:
free :: AnchoredDirTree a -> DirTree a
free (_:/t) = t



    ---------------
    --[ HELPERS ]--
    ---------------


---- PATH CONVERSIONS ----



-- | tuple up the complete filename with the File contents, by building up the 
-- path, trie-style, from the root. The filepath will be relative to the current
-- directory.
-- This allows us to, for example, mapM_ 'uncurry writeFile' over a DirTree of 
-- strings. 
zipPaths :: AnchoredDirTree a -> DirTree (FilePath, a)
zipPaths (b :/ t) = zipP b t
    where zipP p (File n a)   = File n (p</>n , a)
          zipP p (Dir n cs)   = Dir n $ map (zipP $ p</>n) cs
          zipP _ (Failed n e) = Failed n e


-- extracting pathnames and base names:
topDir, baseDir :: FilePath -> FilePath
topDir = last . splitDirectories 
baseDir = joinPath . init . splitDirectories



---- IO HELPERS: ----


-- | writes the directory structure (not files) of a DirTree to the anchored 
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
getDirsFiles :: String -> IO [FilePath]
getDirsFiles cs = do let cs' = if null cs then "." else cs 
                     dfs <- getDirectoryContents cs'
                     return $ sort $ dfs \\ [".",".."]



-- DoesNotExist errors not present at the topmost level could happen if a
-- named file or directory is deleted after being listed by 
-- getDirectoryContents but before we can get it into memory. 
--    So we filter those errors out because the user should not see errors 
-- raised by the internal implementation of this module:
--     This leaves the error if it exists in the top (user-supplied) level:
removeNonexistent (Dir n c) = 
    Dir n $ map removeNonexistent $ filter isOkConstructor c
        
     where isOkConstructor c = not (failed c) || isOkError c
           isOkError = not . isDoesNotExistErrorType . ioeGetErrorType . err
removeNonexistent f = f
