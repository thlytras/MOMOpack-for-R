* Convert .dta files to Stata 12 format
* 
* (Run this from within the directory that contains your files.
* It is needed in order to run compareMOMOoutput.R, because R reads
* dta files only up to version 12).

set more off

local files : dir "." files "*.dta"

foreach file in `files' {
   use `file', clear
   saveold `file', version(12) replace
} 

clear
