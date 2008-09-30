;;@ Miscellaneous procedures providing access to various bits of
;; information regarding the host running the scheme implementation.
(library (spells sysutils)
  (export lookup-environment-variable
          current-process-environment
          extend-process-environment
          find-exec-path
          os-name
          os-node-name
          os-release-name
          os-version-name
          machine-name)
  (import (rnrs base))

  ;; copy this template to sysutils.<implementation>.sls and fill it out
  )
