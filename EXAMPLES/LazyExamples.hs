module LazyExamples
    where

import System.Directory.Tree
import qualified Data.Foldable as F
import System.IO
import Control.Monad 



main = du "/etc"



-- Here are a few examples of using the directory-tree package to recreate
-- the basic functionality of some linux command-line tools. This module
-- uses the lazy directory building IO provided by `readDirectoryWithL`:



-- the command `ls <dir>`. Try: 
--     ghci> ls "/"
-- ...IO is done lazily.
ls :: FileName -> IO ()
ls d = do (_ :/ Dir _ c) <- readDirectoryWithL readFile d
          mapM_ (putStrLn . name) c



-- the command `du -bs <dir> 2> /dev/null` gets the total size of all files 
-- under the supplied directory. We use a more compositional style here, where 
-- (<=<) is equivalent to (.) but for monadic functions (a -> m b):
du :: FileName -> IO ()
du = print . F.sum . free <=< readDirectoryWithL (hFileSize <=< readHs) 
    where readHs = flip openFile ReadMode       

