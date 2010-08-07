module LazyExamples
    where

import System.Directory.Tree


-- Here are a few examples of using the directory-tree package to recreate
-- the basic functionality of some linux command-line tools. This module
-- uses the lazy directory building IO provided by `readDirectoryWithL`:



-- the command `ls <dir>`. Try: 
--     ghci> ls "/"
-- ...IO is done lazily.
ls = undefined


-- the command `du -c <dir>` 
du = undefined
