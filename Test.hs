module Main
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
    _:/written <- writeDirectory testTree
    putStrLn "OK"


    if (fmap (const ()) (filterDir (not . failed) $free testTree)) == 
                                  filterDir (not . failed) written
       then return ()
       else error "writeDirectory returned a tree that didn't match"
    putStrLn "OK"

    -- make file farthest to the right unreadable:
    (Dir _ [_,_,Dir "C" [_,_,File "G" p_unreadable]]) <- sortDir . free <$> build testDir
    setPermissions p_unreadable (Permissions False True True True)
    putStrLn "OK"


    -- read with lazy and standard functions, compare for equality. Also test that our crazy
    -- operator works correctly inline with <$>:
    tL <- readDirectoryWithL readFile testDir
    t@(_:/Dir _ [_,_,Dir "C" [unreadable_constr,_,_]]) <- sortDir </$> id <$> readDirectory testDir
    if  t == tL  then return () else error "lazy read  /=  standard read"
    putStrLn "OK"
    
    -- make sure the unreadable file left the correct error type in a Failed:
    if isPermissionErrorType $ ioeGetErrorType $ err unreadable_constr 
       then return ()
       else error "wrong error type for Failed file read"
    putStrLn "OK"
    
    
    -- run lazy fold, concating file contents. compare for equality:
    tL_again <- sortDir </$> readDirectoryWithL readFile testDir
    let tL_concated = F.concat $ free tL_again
    if tL_concated == "abcdef" then return () else error "foldable broke"
    putStrLn "OK"

     -- get a lazy DirTree at root directory with lazy Directory traversal:
    putStrLn "If lazy IO is not working, we should be stalled right now."
    mapM_ putStr =<< (map name . contents . free) <$> readDirectoryWithL readFile "/"
    putStrLn "\nOK"


    if Dir "d" [File "b" "b",File "a" "a"] == Dir "d" [File "a" undefined, File "b" undefined]
        then putStrLn "OK"
        else error "Ord/Eq instance seems messed up"

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

