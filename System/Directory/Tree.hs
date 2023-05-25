{-# LANGUAGE CPP               #-}
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
-- filesystem, as well as useful functions for reading and writing file
-- and directory structures in the IO monad.
--
-- Errors are caught in a special constructor in the DirTree type.
--
--   Defined instances of Functor, Traversable and Foldable allow for
-- easily operating on a directory of files. For example, you could use
-- Foldable.foldr to create a hash of the entire contents of a directory.
--
--   The functions `readDirectoryWithL` and `buildL` allow for doing
-- directory-traversing IO lazily as required by the execution of pure
-- code. This allows you to treat large directories the same way as you
-- would a lazy infinite list.
--
--   The AnchoredDirTree type is a simple wrapper for DirTree to keep
-- track of a base directory context for the DirTree.
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
       , readDirectoryWithL
       , writeDirectory
       , writeDirectoryWith

       -- * Lower level functions
       , build
       , buildL
       , openDirectory
       , writeJustDirs
       -- ** Manipulating FilePaths
       , zipPaths
       , free

       -- * Utility functions
       -- ** Shape comparison and equality
       , equalShape
       , comparingShape
       -- ** Handling failure
       , successful
       , anyFailed
       , failed
       , failures
       , failedMap
       -- ** Tree Manipulations
       , flattenDir
       , showTree
       , sortDir
       , sortDirShape
       , filterDir
       -- *** Low-level
       , transformDir
       -- ** Navigation
       , dropTo
       -- ** Operators
       , (</$>)

       -- * Lenses
       {- | These are compatible with the "lens" library
       -}
       , _contents, _err, _file, _name
       , _anchor, _dirTree
    ) where




{-
TODO:
   NEXT:
    - performance improvements, we want lazy dir functions to run in constant
       space if possible.
    - v1.0.0 will have a completely stable API, i.e. no added/modified functions

   NEXT MAYBE:
    - tree combining functions
    - more tree searching based on file names
    - look into comonad abstraction

    THE FUTURE!:
        -`par` annotations for multithreaded directory traversal(?)

-}
{-
CHANGES:
    0.3.0
        -remove does not exist errors from DirTrees returned by `read*`
          functions
        -add lazy `readDirectoryWithL` function which uses unsafePerformIO
          internally (and safely, we hope) to do DirTree-producing IO as
          needed by consuming function
        -writeDirectory now returns a DirTree to reflect what was written
          successfully to Disk. This lets us inspect for write failures with
          (passed_DirTree == returned_DirTree) and easily inspect failures in
          the returned DirTree
        -added functor instance for the AnchoredDirTree type

    0.9.0:
        -removed `sort` from `getDirsFiles`, move it to the Eq instance
        -Eq instance now only compares name, for directories we sort contents
          (see info re. Ord below) and recursively compare
        -Ord instance now works like this:
           1) compare constructor: Failed < Dir < File
           2) compare `name`
        -added sortDir function

    0.10.0
        -Eq and Ord instances now compare on free "contents" type variable
        -we provide `equalShape` function for comparison of shape and filenames
          of arbitrary trees (ignoring free "contents" variable)
        -provide a comparingShape used in sortDirShape
        -provide a `sortDirShape` function that sorts a tree, taking into
          account the free file "contents" data

    0.11.0
        - added records for AnchoredDirTree: 'anchor', 'dirTree'
        - 'free' deprecated in favor of 'dirTree'
        - added a new function 'dropTo'
        - implemented lenses compatible with "lens" package, maybe even allowing
            zipper usage!
-}

import System.Console.ANSI
import System.Directory
import System.FilePath
import System.IO
import Control.Exception (handle, IOException)
import System.IO.Error(ioeGetErrorType,isDoesNotExistErrorType)

import Data.Ord (comparing)
import Data.List (sort, sortBy, (\\))
import Data.Maybe (catMaybes, fromMaybe)

import qualified Data.Traversable as T
import qualified Data.Foldable as F

 -- exported functions affected: `buildL`, `readDirectoryWithL`
import System.IO.Unsafe(unsafeInterleaveIO)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | the String in the name field is always a file name, never a full path.
-- The free type variable is used in the File constructor and can hold Handles,
-- Strings representing a file's contents or anything else you can think of.
-- We catch any IO errors in the Failed constructor. an Exception can be
-- converted to a String with 'show'.
data DirTree a = Failed { name :: FileName,
                          err  :: IOException     }
               | Dir    { name     :: FileName,
                          contents :: [DirTree a] }
               | File   { name :: FileName,
                          file :: a               }
                 deriving Show


-- | Two DirTrees are equal if they have the same constructor, the same name
-- (and in the case of `Dir`s) their sorted `contents` are equal:
instance (Eq a)=> Eq (DirTree a) where
    (File n a) == (File n' a') = n == n' && a == a'
    (Dir n cs) == (Dir n' cs') =
        n == n' && sortBy comparingConstr cs == sortBy comparingConstr cs'
     -- after comparing above we can hand off to shape equality function:
    d == d' = equalShape d d'


-- | First compare constructors: Failed < Dir < File...
-- Then compare `name`...
-- Then compare free variable parameter of `File` constructors
instance (Ord a,Eq a) => Ord (DirTree a) where
    compare (File n a) (File n' a') =
        case compare n n' of
             EQ -> compare a a'
             el -> el
    compare (Dir n cs) (Dir n' cs') =
        case compare n n' of
             EQ -> comparing sort cs cs'
             el -> el
     -- after comparing above we can hand off to shape ord function:
    compare d d' = comparingShape d d'



-- | a simple wrapper to hold a base directory name, which can be either an
-- absolute or relative path. This lets us give the DirTree a context, while
-- still letting us store only directory and file /names/ (not full paths) in
-- the DirTree. (uses an infix constructor; don't be scared)
data AnchoredDirTree a = (:/) { anchor :: FilePath, dirTree :: DirTree a }
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



-- for convenience:
instance Functor AnchoredDirTree where
    fmap f (b:/d) = b :/ fmap f d


-- given the same fixity as <$>, is that right?
infixl 4 </$>


    ----------------------------
    --[ HIGH LEVEL FUNCTIONS ]--
    ----------------------------


-- | Build an AnchoredDirTree, given the path to a directory, opening the files
-- using readFile.
-- Uses @readDirectoryWith readFile@ internally and has the effect of traversing the
-- entire directory structure. See `readDirectoryWithL` for lazy production
-- of a DirTree structure.
readDirectory :: FilePath -> IO (AnchoredDirTree String)
readDirectory = readDirectoryWith readFile


-- | Build a 'DirTree' rooted at @p@ and using @f@ to fill the 'file' field of 'File' nodes.
--
-- The 'FilePath' arguments to @f@ will be the full path to the current file, and
-- will include the root @p@ as a prefix.
-- For example, the following would return a tree of full 'FilePath's
-- like \"..\/tmp\/foo\" and \"..\/tmp\/bar\/baz\":
--
-- > readDirectoryWith return "../tmp"
--
-- Note though that the 'build' function below already does this.
readDirectoryWith :: (FilePath -> IO a) -> FilePath -> IO (AnchoredDirTree a)
readDirectoryWith f p = buildWith' buildAtOnce' f p


-- | A "lazy" version of `readDirectoryWith` that does IO operations as needed
-- i.e. as the tree is traversed in pure code.
--
-- /NOTE:/ This function uses `unsafeInterleaveIO` under the hood.  This means
-- that:
--
-- * side effects are tied to evaluation order and only run on demand
-- * you might receive exceptions in pure code
readDirectoryWithL :: (FilePath -> IO a) -> FilePath -> IO (AnchoredDirTree a)
readDirectoryWithL f p = buildWith' buildLazilyUnsafe' f p

-- | generate a string that represents tree command-like output for a
-- given DirTree.
-- The first parameter determines whether the tree will be colorized
-- (directories will be shown in blue bold text). Instances of Failed
-- will be removed from the tree before it is displayed.
showTree :: Bool -> DirTree a -> String
showTree colorize tree =
    let treeNoFailed = filterDir notFailed tree
        treeM = showTree' colorize "" True treeNoFailed
    in fromMaybe "" treeM
        where notFailed (Failed _ _) = False
              notFailed _ = True

singleInd :: String
singleInd = "   "

substituteJoiner :: Char -> String -> String
substituteJoiner joiner str =
    let indWidth = length singleInd
    in if length str > 1
        then take (length str - indWidth) str <> (joiner:"──")
        else str

setDirFormat :: String -> String
setDirFormat dirName =
    setSGRCode [SetConsoleIntensity BoldIntensity]
    <> setSGRCode [SetColor Foreground Vivid Blue]
    <> dirName
    <> setSGRCode [Reset]

showTree' :: Bool -> String -> Bool -> DirTree a -> Maybe String
showTree' colorize prelimStr isLast (Dir nm conts) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr
                      <> if colorize then setDirFormat nm else nm
        prelimStr' = prelimStr <> "│  "
        subLinesM = showTree' colorize prelimStr' False <$> (init conts)
        lastPrelimStr = prelimStr <> singleInd
        lastLineM = showTree' colorize lastPrelimStr True (last conts)
        tailLinesM = subLinesM ++ [lastLineM]
        allLines = thisLineStr:(catMaybes tailLinesM)
    in Just (init $ unlines $ allLines)
showTree' _ prelimStr isLast (File nm _) =
    let joiner = if isLast then '└' else '├'
        thisLineStr = substituteJoiner joiner prelimStr <> nm
    in Just thisLineStr
showTree' _ _ _ (Failed _ _) = error "Cannot showTree' for Failed"

-- | write a DirTree of strings to disk. Clobbers files of the same name.
-- Doesn't affect files in the directories (if any already exist) with
-- different names. Returns a new AnchoredDirTree where failures were
-- lifted into a `Failed` constructor:
writeDirectory :: AnchoredDirTree String -> IO (AnchoredDirTree ())
writeDirectory = writeDirectoryWith writeFile


-- | writes the directory structure to disk and uses the provided function to
-- write the contents of `Files` to disk. The return value of the function will
-- become the new `contents` of the returned, where IO errors at each node are
-- replaced with `Failed` constructors. The returned tree can be compared to
-- the passed tree to see what operations, if any, failed:
writeDirectoryWith :: (FilePath -> a -> IO b) -> AnchoredDirTree a -> IO (AnchoredDirTree b)
writeDirectoryWith f (b:/t) = (b:/) <$> write' b t
    where write' b' (File n a) = handleDT n $
              File n <$> f (b'</>n) a
          write' b' (Dir n cs) = handleDT n $
              do let bas = b'</>n
                 createDirectoryIfMissing True bas
                 Dir n <$> mapM (write' bas) cs
          write' _ (Failed n e) = return $ Failed n e






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
build = buildWith' buildAtOnce' return   -- we say 'return' here to get
                                         -- back a  tree  of  FilePaths


-- | identical to `build` but does directory reading IO lazily as needed:
buildL :: FilePath -> IO (AnchoredDirTree FilePath)
buildL = buildWith' buildLazilyUnsafe' return




    -- -- -- helpers: -- -- --


type UserIO a = FilePath -> IO a
type Builder a = UserIO a -> FilePath -> IO (DirTree a)

-- remove non-existent file errors, which are artifacts of the "non-atomic"
-- nature of traversing a system directory tree:
buildWith' :: Builder a -> UserIO a -> FilePath -> IO (AnchoredDirTree a)
buildWith' bf' f p =
    do tree <- bf' f p
       return (baseDir p :/ removeNonexistent tree)



-- IO function passed to our builder and finally executed here:
buildAtOnce' :: Builder a
buildAtOnce' f p = handleDT n $
           do isFile <- doesFileExist p
              if isFile
                 then  File n <$> f p
                 else do cs <- getDirsFiles p
                         Dir n <$> T.mapM (buildAtOnce' f . combine p) cs
     where n = topDir p


unsafeMapM :: (a -> IO b) -> [a] -> IO [b]
unsafeMapM _    []  = return []
unsafeMapM f (x:xs) = unsafeInterleaveIO io
  where
    io = do
        y  <- f x
        ys <- unsafeMapM f xs
        return (y:ys)


-- using unsafeInterleaveIO to get "lazy" traversal:
buildLazilyUnsafe' :: Builder a
buildLazilyUnsafe' f p = handleDT n $
           do isFile <- doesFileExist p
              if isFile
                 then  File n <$> f p
                 else do
                     files <- getDirsFiles p

                     -- HERE IS THE UNSAFE LINE:
                     dirTrees <- unsafeMapM (rec . combine p) files

                     return (Dir n dirTrees)
     where rec = buildLazilyUnsafe' f
           n = topDir p




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
failed :: DirTree a -> Bool
failed (Failed _ _) = True
failed _            = False


-- | returns a list of 'Failed' constructors only:
failures :: DirTree a -> [DirTree a]
failures = filter failed . flattenDir


-- | maps a function to convert Failed DirTrees to Files or Dirs
failedMap :: (FileName -> IOException -> DirTree a) -> DirTree a -> DirTree a
failedMap f = transformDir unFail
    where unFail (Failed n e) = f n e
          unFail c            = c


---- ORDERING AND EQUALITY ----


-- | Recursively sort a directory tree according to the Ord instance
sortDir :: (Ord a)=> DirTree a -> DirTree a
sortDir = sortDirBy compare

-- | Recursively sort a tree as in `sortDir` but ignore the file contents of a
-- File constructor
sortDirShape :: DirTree a -> DirTree a
sortDirShape = sortDirBy comparingShape  where

  -- HELPER:
sortDirBy :: (DirTree a -> DirTree a -> Ordering) -> DirTree a -> DirTree a
sortDirBy cf = transformDir sortD
    where sortD (Dir n cs) = Dir n (sortBy cf cs)
          sortD c          = c


-- | Tests equality of two trees, ignoring their free variable portion. Can be
-- used to check if any files have been added or deleted, for instance.
equalShape :: DirTree a -> DirTree b -> Bool
equalShape d d' = comparingShape d d' == EQ

-- TODO: we should use equalFilePath here, but how to sort properly? with System.Directory.canonicalizePath, before compare?

-- | a compare function that ignores the free "file" type variable:
comparingShape :: DirTree a -> DirTree b -> Ordering
comparingShape (Dir n cs) (Dir n' cs') =
    case compare n n' of
         EQ -> comp (sortCs cs) (sortCs cs')
         el -> el
    where sortCs = sortBy comparingConstr
           -- stolen from [] Ord instance:
          comp []     []     = EQ
          comp []     (_:_)  = LT
          comp (_:_)  []     = GT
          comp (x:xs) (y:ys) = case comparingShape x y of
                                    EQ    -> comp xs ys
                                    other -> other
 -- else simply compare the flat constructors, non-recursively:
comparingShape t t'  = comparingConstr t t'


 -- HELPER: a non-recursive comparison
comparingConstr :: DirTree a -> DirTree a1 -> Ordering
comparingConstr (Failed _ _) (Dir _ _)    = LT
comparingConstr (Failed _ _) (File _ _)   = LT
comparingConstr (File _ _) (Failed _ _)   = GT
comparingConstr (File _ _) (Dir _ _)      = GT
comparingConstr (Dir _ _)    (Failed _ _) = GT
comparingConstr (Dir _ _)    (File _ _)   = LT
 -- else compare on the names of constructors that are the same, without
 -- looking at the contents of Dir constructors:
comparingConstr t t'  = compare (name t) (name t')




---- OTHER ----

{-# DEPRECATED free "Use record 'dirTree'" #-}
-- | DEPRECATED. Use record 'dirTree' instead.
free :: AnchoredDirTree a -> DirTree a
free = dirTree

-- | If the argument is a 'Dir' containing a sub-DirTree matching 'FileName'
-- then return that subtree, appending the 'name' of the old root 'Dir' to the
-- 'anchor' of the AnchoredDirTree wrapper. Otherwise return @Nothing@.
dropTo :: FileName -> AnchoredDirTree a -> Maybe (AnchoredDirTree a)
dropTo n' (p :/ Dir n ds') = search ds'
    where search [] = Nothing
          search (d:ds) | equalFilePath n' (name d) = Just ((p</>n) :/ d)
                        | otherwise = search ds
dropTo _ _ = Nothing


-- | applies the predicate to each constructor in the tree, removing it (and
-- its children, of course) when the predicate returns False. The topmost
-- constructor will always be preserved:
filterDir :: (DirTree a -> Bool) -> DirTree a -> DirTree a
filterDir p = transformDir filterD
    where filterD (Dir n cs) = Dir n $ filter p cs
          filterD c          = c


-- | Flattens a `DirTree` into a (never empty) list of tree constructors. `Dir`
-- constructors will have [] as their `contents`:
flattenDir :: DirTree a -> [ DirTree a ]
flattenDir (Dir n cs) = Dir n [] : concatMap flattenDir cs
flattenDir f          = [f]





-- | Allows for a function on a bare DirTree to be applied to an AnchoredDirTree
-- within a Functor. Very similar to and useful in combination with `<$>`:
(</$>) :: (Functor f) => (DirTree a -> DirTree b) -> f (AnchoredDirTree a) ->
                         f (AnchoredDirTree b)
(</$>) f = fmap (\(b :/ t) -> b :/ f t)


    ---------------
    --[ HELPERS ]--
    ---------------


---- CONSTRUCTOR IDENTIFIERS ----
{-
isFileC :: DirTree a -> Bool
isFileC (File _ _) = True
isFileC _ = False

isDirC :: DirTree a -> Bool
isDirC (Dir _ _) = True
isDirC _ = False
-}


---- PATH CONVERSIONS ----



-- | tuple up the complete file path with the 'file' contents, by building up the
-- path, trie-style, from the root. The filepath will be relative to \"anchored\"
-- directory.
--
-- This allows us to, for example, @mapM_ uncurry writeFile@ over a DirTree of
-- strings, although 'writeDirectory' does a better job of this.
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
-- directory. Returns a structure identical to the supplied tree with errors
-- replaced by `Failed` constructors:
writeJustDirs :: AnchoredDirTree a -> IO (AnchoredDirTree a)
writeJustDirs = writeDirectoryWith (const return)


----- the let expression is an annoying hack, because dropFileName "." == ""
----- and getDirectoryContents fails epically on ""
-- prepares the directory contents list. we sort so that we can be sure of
-- a consistent fold/traversal order on the same directory:
getDirsFiles :: String -> IO [FilePath]
getDirsFiles cs = do let cs' = if null cs then "." else cs
                     dfs <- getDirectoryContents cs'
                     return $ dfs \\ [".",".."]



---- FAILURE HELPERS: ----


-- handles an IO exception by returning a Failed constructor filled with that
-- exception:
handleDT :: FileName -> IO (DirTree a) -> IO (DirTree a)
handleDT n = handle (return . Failed n)


-- DoesNotExist errors not present at the topmost level could happen if a
-- named file or directory is deleted after being listed by
-- getDirectoryContents but before we can get it into memory.
--    So we filter those errors out because the user should not see errors
-- raised by the internal implementation of this module:
--     This leaves the error if it exists in the top (user-supplied) level:
removeNonexistent :: DirTree a -> DirTree a
removeNonexistent = filterDir isOkConstructor
     where isOkConstructor c = not (failed c) || isOkError c
           isOkError = not . isDoesNotExistErrorType . ioeGetErrorType . err


-- | At 'Dir' constructor, apply transformation function to all of directory's
-- contents, then remove the Nothing's and recurse. This always preserves the
-- topomst constructor.
transformDir :: (DirTree a -> DirTree a) -> DirTree a -> DirTree a
transformDir f t = case f t of
                     (Dir n cs) -> Dir n $ map (transformDir f) cs
                     t'         -> t'

-- Lenses, generated with TH from "lens" -----------
-- TODO deprecate these? Pain in the ass to generate, and maybe it's intended
--      for users to generate their own lenses.
_contents ::
            Applicative f =>
            ([DirTree a] -> f [DirTree a]) -> DirTree a -> f (DirTree a)

_err ::
       Applicative f =>
       (IOException -> f IOException) -> DirTree a -> f (DirTree a)

_file ::
        Applicative f =>
        (a -> f a) -> DirTree a -> f (DirTree a)

_name ::
        Functor f =>
        (FileName -> f FileName) -> DirTree a -> f (DirTree a)

_anchor ::
          Functor f =>
          (FilePath -> f FilePath)
          -> AnchoredDirTree a -> f (AnchoredDirTree a)

_dirTree ::
           Functor f =>
           (DirTree t -> f (DirTree a))
           -> AnchoredDirTree t -> f (AnchoredDirTree a)

--makeLensesFor [("name","_name"),("err","_err"),("contents","_contents"),("file","_file")] ''DirTree
_contents _f_a6s2 (Failed _name_a6s3 _err_a6s4)
  = pure (Failed _name_a6s3 _err_a6s4)
_contents _f_a6s5 (Dir _name_a6s6 _contents'_a6s7)
  = ((\ _contents_a6s8 -> Dir _name_a6s6 _contents_a6s8)
     <$> (_f_a6s5 _contents'_a6s7))
_contents _f_a6s9 (File _name_a6sa _file_a6sb)
  = pure (File _name_a6sa _file_a6sb)
_err _f_a6sd (Failed _name_a6se _err'_a6sf)
  = ((\ _err_a6sg -> Failed _name_a6se _err_a6sg)
     <$> (_f_a6sd _err'_a6sf))
_err _f_a6sh (Dir _name_a6si _contents_a6sj)
  = pure (Dir _name_a6si _contents_a6sj)
_err _f_a6sk (File _name_a6sl _file_a6sm)
  = pure (File _name_a6sl _file_a6sm)
_file _f_a6so (Failed _name_a6sp _err_a6sq)
  = pure (Failed _name_a6sp _err_a6sq)
_file _f_a6sr (Dir _name_a6ss _contents_a6st)
  = pure (Dir _name_a6ss _contents_a6st)
_file _f_a6su (File _name_a6sv _file'_a6sw)
  = ((\ _file_a6sx -> File _name_a6sv _file_a6sx)
     <$> (_f_a6su _file'_a6sw))
_name _f_a6sz (Failed _name'_a6sA _err_a6sC)
  = ((\ _name_a6sB -> Failed _name_a6sB _err_a6sC)
     <$> (_f_a6sz _name'_a6sA))
_name _f_a6sD (Dir _name'_a6sE _contents_a6sG)
  = ((\ _name_a6sF -> Dir _name_a6sF _contents_a6sG)
     <$> (_f_a6sD _name'_a6sE))
_name _f_a6sH (File _name'_a6sI _file_a6sK)
  = ((\ _name_a6sJ -> File _name_a6sJ _file_a6sK)
     <$> (_f_a6sH _name'_a6sI))

--makeLensesFor [("anchor","_anchor"),("dirTree","_dirTree")] ''AnchoredDirTree
_anchor _f_a7wT (_anchor'_a7wU :/ _dirTree_a7wW)
  = ((\ _anchor_a7wV -> (:/) _anchor_a7wV _dirTree_a7wW)
     <$> (_f_a7wT _anchor'_a7wU))
_dirTree _f_a7wZ (_anchor_a7x0 :/ _dirTree'_a7x1)
  = ((\ _dirTree_a7x2 -> (:/) _anchor_a7x0 _dirTree_a7x2)
     <$> (_f_a7wZ _dirTree'_a7x1))
