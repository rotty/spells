#!r6rs

(library (spells logging)
  (export make-log

          log-entry?
          log-entry-level
          log-entry-level-name
          log-entry-object

          make-log-handler

          default-log-formatter

          configure-logger)
  (import (except (rnrs base) error)
          (rnrs control)
          (rnrs mutable-pairs)
          (rnrs io simple)
          (spells lists)
          (spells time-lib)
          (spells error)
          (spells opt-args)
          (spells record-types)
          (spells include))
  
  (include-file ((spells scheme) logging)))
