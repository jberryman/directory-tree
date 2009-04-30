module Main
    where

import DirectoryTree
import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- for main2:
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as B 


main = main0

-- simple example of creating a directory by hand and writing to disk: here we 
-- replicate (kind of) running the command "darcs initialize" in the current 
-- directory:
main0 = writeDirectory ("source_dir" :/ darcs_d) 
    where darcs_d = Dir "_darcs" [prist_d, prefs_d, patch_d, inven_f, forma_f]

          prist_d = Dir "pristine.hashed" [hash_f]
          prefs_d = Dir "prefs" [motd_f, bori_f, bina_f]
          patch_d = Dir "patches" []
          inven_f = File "hashed_inventory"  ""
          forma_f = File "format"  "hashed\ndarcs-2\n"
          
          hash_f = File "da39a3ee5..."  ""
          motd_f = File "motd"          ""
          bori_f = File "boring"        "# Boring file regexps:\n..."
          bina_f = File "binaries"      "# Binary file regexps:\n..."


-- here we read directories from different locations on the disk and combine 
-- them into a new directory structure, ignoring the anchored base directory,
-- then simply 'print' the structure to screen:
main1 = do (_:/d1) <- readDirectory "../dir1/"
           (b:/d2) <- readDirectory "/home/me/dir2"
           let readme = File "README"  "nothing to see here"
            
            -- anchor to the parent directory:
           print $  b:/Dir "Combined_Dir_Test" [d1,d2,readme]


-- read two directory structures using readFile from Data.ByteString, and build 
-- up an MD5 hash of all the files in each directory, compare the two hashes 
-- to see if the directories are identical in their files. (note: doesn't take 
-- into account directory name mis-matches)
main2 = do (_:/bsd1) <- readByteStrs "./dir_modified"
           (_:/bsd2) <- readByteStrs "./dir"
           let hash1 = hashDir bsd1
           let hash2 = hashDir bsd2
           print $ if hash1 == hash2
                      then "directories match with hash: " ++ show hash1
                      else show hash1 ++ " doesn't match " ++ show hash2

    where readByteStrs = readDirectoryWith B.readFile
          hashDir = md5Finalize. F.foldl' md5Update md5InitialContext

