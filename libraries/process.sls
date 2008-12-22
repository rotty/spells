#!r6rs

;;@ Process interface.
(library (spells process)
  (export process?
          process-input
          process-output
          process-errors

          spawn-process
          wait-for-process
          close-process-ports

          call-with-process-input
          call-with-process-output
          
          run-process
          run-process/string
          run-process/lines
          run-process/sexps)
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs io ports)
          (rnrs io simple)
          (spells receive)
          (spells lists)
          (spells strings)
          (spells pathname)
          (spells delimited-readers)
          (spells process compat)
          (spells include))
  
  (include-file ((spells scheme) process)))
