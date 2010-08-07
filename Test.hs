module Test
    where

-- do a quick test for Darcs:

import System.Directory.Tree
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import System.Directory
import System.IO
import System.Cmd
import System.IO.Error(ioeGetErrorType,isPermissionErrorType)




testDir = "/tmp/TESTDIR-LKJHBAE"

main = do
    -- write our testing directory structure to disk. We include Failed 
    -- constructors which should be discarded:
    writeDirectory testTree
    putStrLn "OK"

    -- make file farthest to the right unreadable:
    (_:/Dir _ [_,_,Dir "C" [_,_,File "G" p_unreadable]]) <- build testDir
    setPermissions p_unreadable (Permissions False True True True)
    putStrLn "OK"


    -- read with lazy and standard functions, compare for equality:
    tL <- readDirectoryWithL readFile testDir
    t@(_:/Dir _ [_,_,Dir "C" [_,_,unreadable_constr]]) <-readDirectory testDir
    if  t == tL  then return () else error "lazy read  /=  standard read"
    putStrLn "OK"
    
    -- make sure the unreadable file left the correct error type in a Failed:
    if isPermissionErrorType $ ioeGetErrorType $ err unreadable_constr 
       then return ()
       else error "wrong error type for Failed file read"
    putStrLn "OK"
    
    
    -- run lazy fold, concating file contents. compare for equality:
    tL_again <- readDirectoryWithL readFile testDir
    let tL_concated = F.concat $ free tL_again
    if tL_concated == "abcdef" then return () else error "foldable broke"
    putStrLn "OK"

     -- get a lazy DirTree at root directory with lazy Directory traversal:
    putStrLn "If you can read this before script exits, lazy Dir IO is borkin"
    mapM_ putStr =<< (map name . contents . free) <$>  buildL "/"
    putStrLn "\nOK"

    -- clean up by removing the directory:
    system$ "rm -r " ++ testDir
    putStrLn "SUCCESS"
    

testTree :: AnchoredDirTree String
testTree = "" :/ Dir testDir [dA , dB , dC , Failed "FAAAIIILL" undefined]
    where dA = Dir "A" [dA1 , dA2 , Failed "FAIL" undefined]
          dA1    = Dir "A1" [File "A" "a", File "B" "b"]
          dA2    = Dir "A2" [File "C" "c"]
          dB = Dir "B" [File "D" "d"]
          dC = Dir "C" [File "E" "e", File "F" "f", File "G" "g"]

