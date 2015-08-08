# Script to compare output files from the R and Stata versions of MOMOpack,
#    and verify that they are identical in content.
#
# Author: Theodore Lytras <thlytras@gmail.com>
#
# Usage: Place all R output files and Stata output files in respective directories,
#    as specified below. If using Stata 13 or newer, first run convertToStata12.do 
#    on the Stata files, otherwise R won't be able to read them. 
#    Then run this script and check its output.


# Paths where the output files are - modify as appropriate
R.files.path <- "./R"
Stata.files.path <- "./stata"

# Tolerance for numerical differences
tol <- 10^(-6)


# Do not modify after this point

library(foreign)

compareVector <- function(x1, x2, tol=10^(-5)) {
  suppressWarnings({
  if (class(x1)=="factor") x1 <- as.character(x1)
  if (class(x2)=="factor") x2 <- as.character(x2)
  if (class(x1)=="Date" && class(x2)!="Date") x2 <- as.Date(as.numeric(x2), origin="1960-1-1")
  if (class(x2)=="Date" && class(x1)!="Date") x1 <- as.Date(as.numeric(x1), origin="1960-1-1")
  if (class(x1)=="Integer") x1 <- as.numeric(x2)
  if (class(x2)=="Integer") x2 <- as.numeric(x1)
  if (class(x1)=="character" || class(x2)=="character") {
    pb.na <- is.na(x1) & is.na(x2); x1 <- x1[!pb.na]; x2 <- x2[!pb.na]
    x1n <- as.numeric(x1); x2n <- as.numeric(x2)
    pn.na <- !is.na(x1n) & !is.na(x2n)
    r1 <- all.equal(x1n[pn.na], x2n[pn.na], tolerance=tol)
    r2 <- all.equal(as.character(x1[!pn.na]), as.character(x2[!pn.na]))
    if (class(r1)=="logical" && class(r2)=="logical") {
      return(TRUE)
    } else {
      if (class(r1)=="logical") { r1 <- "Numeric part: identical" } else { r1 <- paste("Numeric part:", r1) }
      if (class(r2)=="logical") { r2 <- "Character part: identical" } else { r2 <- paste("Character part:", r2) }
      return(paste(r1, r2, sep=" | "))
    }
  }
  return(all.equal(x1, x2, tolerance=tol))
  })
}


compareOutputs <- function(x1, x2, tol=10^(-5)) {
  if (ncol(x1) == ncol(x2)) {
    cat("- Files have equal number of columns\n")
  } else {
    cat("- Files have unequal number of columns! Breaking...\n\n")
    return
  }
  # Fix the wrong column name (in the stata version)
  names(x1)[names(x1) == "Automn"] <- "Autumn"
  names(x2)[names(x2) == "Automn"] <- "Autumn"
  if (identical(names(x1), names(x2))) {
    cat("- Files have the same column names\n")
  } else {
    cat("- Files don't have the same column names!\n")
    cat("\tStata file: "); cat(names(x1)); cat("\n")
    cat("\t    R file: "); cat(names(x2)); cat("\n")
    cat("Breaking...\n\n")
    return
  }
  cl1 <- sapply(x1, class)
  cl2 <- sapply(x2, class)
  # Checking amount of missing values
  d <- t(sapply(1:ncol(x1), function(i){
  c1 <- sum(is.na(x1[,i]))
  c2 <- sum(is.na(x2[,i]))
  c(c1,c2)
  }))
  if (identical(d[,1], d[,2])) {
    cat("- All columns contain the same amount of missing values.\n")
  } else {
    cat("- Not all columns contain the same amount of missing values:\n")
    for(j in which(d[,1] != d[,2])) {
      cat("\tColumn '");cat(names(x1)[j]); 
      cat("': stata "); cat(d[j,1]); cat(", R "); cat(d[j,2]); cat("\n")
    }
    cat("Breaking...\n\n")
    return
  }
  # Checking column contents after sorting
  msg1 <- "The two data files"
  d <- sapply(1:ncol(x1), function(i){
    compareVector(x1[,i][order(as.character(x1[,i]))], x2[,i][order(as.character(x2[,i]))], tol=tol)
  })
  if (class(d)=="logical") {
    cat("- After sorting, all columns contain the same values.\n")
  } else {
    cat("- After sorting, the following columns are different:\n")
    for(j in which(d!=TRUE)) {
      cat("\tColumn '");cat(names(x1)[j]); cat("': ")
      cat(d[j]);cat("\n")
    }
    msg1 <- "The other columns in the two data files"
  }
  # Checking column contents without sort
  dd <- sapply(which(d==TRUE), function(i){
    compareVector(x1[,i], x2[,i], tol=tol)
  })
  if (class(dd)=="logical") {
    cat(sprintf("- %s are sorted the same.\n\n", msg1))
  } else {
    cat(sprintf("- %s don't have the same sort order!\n\n", msg1))
  }
}

cat("Comparing .dta files:\n\n")

common.dta.files <- intersect(list.files(R.files.path, pattern="\\.dta$"), 
	    list.files(Stata.files.path, pattern="\\.dta$"))

for (f in common.dta.files) {
  fR <- read.dta(sprintf("./R/%s", f))
  fS <- read.dta(sprintf("./stata/%s", f))

  cat("Comparing output file '");cat(f);cat("':\n")
  compareOutputs(fR, fS, tol)
}

cat("\nComparing .txt files:\n\n")

common.txt.files <- intersect(list.files(R.files.path, pattern="\\.txt$"), 
	    list.files(Stata.files.path, pattern="\\.txt$"))

for (f in common.txt.files) {
  fR <- try(read.table(sprintf("./R/%s", f), header=TRUE, sep="\t"), silent=TRUE)
  if (class(fR)=="try-error" || ncol(fR)==1) fR <- read.csv(sprintf("./R/%s", f), header=TRUE)
  fS <- try(read.table(sprintf("./stata/%s", f), header=TRUE, sep="\t"), silent=TRUE)
  if (class(fS)=="try-error" || ncol(fS)==1) fS <- read.csv(sprintf("./stata/%s", f), header=TRUE)

  cat("Comparing output file '");cat(f);cat("':\n")
  compareOutputs(fR, fS, tol)
}

extra.files.in.R.dir <- list.files(R.files.path)[!(list.files(R.files.path) %in% c(common.dta.files, common.txt.files))]
extra.files.in.Stata.dir <- list.files(Stata.files.path)[!(list.files(Stata.files.path) %in% c(common.dta.files, common.txt.files))]

if (length(c(extra.files.in.R.dir, extra.files.in.Stata.dir))>0) {
  cat("\n")
  if (length(extra.files.in.R.dir)) { 
    cat("NOTE: The following files in the R files dir do not have a match in the Stata files dir:\n")
    cat(extra.files.in.R.dir); cat("\n")
  }
  if (length(extra.files.in.Stata.dir)) { 
    cat("NOTE: The following files in the Stata files dir do not have a match in the R files dir:\n")
    cat(extra.files.in.Stata.dir); cat("\n")
  }
}