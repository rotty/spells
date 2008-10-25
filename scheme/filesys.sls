#!r6rs
(library (spells filesys)
  (export file-exists?
          create-directory
          create-directory*
          delete-file
          rename-file
          copy-file
          install-file
          
          file-regular?
          file-directory?
          file-symbolic-link?
          file-readable?
          file-writable?
          file-executable?
          file-modification-time
          file-size-in-bytes

          directory-fold*
          directory-fold
          directory-fold-tree

          file-unreachable-error?
          file-unreachable-error-pathname
          file-unreachable-error-operator

          working-directory
          with-working-directory

          call-with-input-file-and-directory

          search-directory-list)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs io simple)
          (spells receive)
          (spells lists)
          (spells pathname)
          (spells time-lib)
          (spells filesys compat)
          (spells include))

  (include-file (spells filesys)))
