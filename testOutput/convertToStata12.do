* Convert .dta files to Stata 12 format
* 
* (Run this from within the directory that contains your files)

set more off

local files : dir "." files "*.dta"

foreach file in `files' {
   use `file', clear
   saveold `file', version(12) replace
} 

clear
