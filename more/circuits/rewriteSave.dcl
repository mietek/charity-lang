// This module contains code for saving rewrites to files and reading them from files.

definition module rewriteSave

import StdEnv
import rewriteDefs


////////// SaveRewrite
// Takes a rewrite and a file (which should have its file pointer positioned at the end of the
// file) and returns the new file after writing the rewrite at the file-pointer position.
SaveRewrite :: Rewrite *File -> *File

////////// ReadRewrite
// Takes a file (which should have its file pointer positioned at the beginning of a rewrite),
// and returns:
//   - True if it has reached the end of the file, False otherwise,
//   - True if it read a rewrite from the file, False otherwise (always False if the first argument is True),
//   - the rewrite that it read starting at the file-pointer position, if any,
//   - the new file.
ReadRewrite :: *File -> (Bool, Bool, Rewrite, *File)
