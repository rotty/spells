(library (spells filesys compat)
  (export file-exists?
          create-directory
          delete-file
          rename-file

          file-regular?
          file-directory?
          file-symbolic-link?
          file-readable?
          file-writable?
          file-executable?
          file-modification-time
          file-size-in-bytes

          directory-fold*

          working-directory
          with-working-directory

          copy-file)
  (import (rnrs base)))
