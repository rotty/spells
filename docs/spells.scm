;; About the document
(name "Spells")
(version "0.0.1")
(description "Portability and Utility Library")
(updated "28 May 2011")
(authors
 ("Andreas Rottmann" . "a.rottmann@gmx.at"))

;; Copying the documentation
(copyright-holder
 "Andreas Rottmann, Taylor Campbell, Richard Kelsey, Jonathan Rees, "
 "and Mike Sperber")

(years 2004 2005 2006 2007 2008 2009 2010 2011)
(permissions
 "Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU General Public License, Version 3 or any
later version published by the Free Software Foundation.")

;; Texinfo info
(texinfo-basename "spells")
(texinfo-category "The Algorithmic Language Scheme")

;; Libraries to document
(libraries
 "Operating System Interfaces"
 ((spells pathname) "Pathname abstraction")
 ((spells filesys) "Interacting with the file system")
 ((spells process) "Launching OS processes")
 ((spells foreign) "Interfacing with C libraries")
 ((spells network) "Simple TCP networking")
 
 "Language Facilities"
 ((spells include) "Include Scheme code from other files")
 ((spells define-values) "Multiple-value " (code "define"))
 ;;((spells delimited-control) "Delimited control operators")
 ((spells match) "Pattern matching")
 ((spells operations) "T-style dynamic dispatch")
 ((spells opt-args) "Define procedures with optional arguments")
 ((spells awk) "SCSH's awk macro")
 ((spells gc) "Interface to the garbage collector")
 ((spells tracing) "A debugging aid")

 "Data Types"
 ((spells algebraic-types) "Algebraic data types")
 ((spells finite-types) "Types with a fixed number of instances")
 ((spells xvector) "Extensible vectors")
 ((spells zipper-tree) "Purely functional trees")
 ((spells alist) "Association list utilities")
 ((spells cells) "Single-value, mutable cells")
 ((spells string-utils) "Procedures operating on strings")

 "Miscellany"
 ((spells logging) "Hierarchical logging facility"))
