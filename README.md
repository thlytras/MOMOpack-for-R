# MOMOpack for R
[![Build Status](https://travis-ci.org/raubreywhite/MOMOpack-for-R.svg?branch=master)](https://travis-ci.org/raubreywhite/MOMOpack-for-R)
[![codecov](https://codecov.io/gh/raubreywhite/MOMOpack-for-R/branch/master/graph/badge.svg)](https://codecov.io/gh/raubreywhite/MOMOpack-for-R)

#
This is a full port of A-MOMOpack (the code used to analyze mortality data for the [EuroMOMO project](www.euromomo.eu)) from Stata to R.

# How to install

Please use my [drat](https://github.com/eddelbuettel/drat) repository to install this package from, as the drat repository only contains package versions that have passed [testing](http://travis-ci.org/raubreywhite/MOMOpack-for-R).

```
install.packages("momo", repos="https://raubreywhite.github.io/drat/")
```

# Basic example

```
SetOpts(
    DoA=as.Date("2013-12-31"),
    DoPR=as.Date("2008-1-1"),
    WStart=1,
    WEnd=52,
    country = "Denmark",
    source = "SSI",
    MFILE = "DoD_DoR.txt",
    HFILE = "holidays.txt",
    INPUTDIR = system.file("testdata",package="momo"),
    WDIR = tempdir(),
    back = 3,
    WWW = 290,
    Ysum = 2013,
    Wsum = 40,
    USEglm2 = TRUE,
    useAUTOMN = TRUE,
    datesISO = FALSE,
    plotGraphs = TRUE,
    MOMOgroups = list(
      "0to4" =  "age >= 0 & age <=4",
      "5to14" = "age >= 5 & age <=14",
      "15to64" = "age >= 15 & age <=64",
      "65P" = "age >= 65 | is.na(age)",
      "Total" = "age >= 0 | is.na(age)"
    ),
    MOMOmodels = c(
      "0to4" = "LINE",
      "5to14" = "LINE",
      "15to64" = "LINE_SIN",
      "65P" = "LINE_SIN",
      "Total" = "LINE_SIN"
    ))

RunMoMo()
```

# Another example using MDATA and HDATA instead of MFILE and HFILE

```
MDATA <- as.data.frame(data.table::fread(file.path(system.file("testdata",package="momo"),"DoD_DoR.txt")))
HDATA <- as.data.frame(data.table::fread(file.path(system.file("testdata",package="momo"),"holidays.txt")))

SetOpts(
    DoA=as.Date("2013-12-31"),
    DoPR=as.Date("2008-1-1"),
    WStart=1,
    WEnd=52,
    country = "Denmark",
    source = "SSI",
    MDATA = MDATA,
    HDATA = HDATA,
    WDIR = tempdir(),
    back = 3,
    WWW = 290,
    Ysum = 2013,
    Wsum = 40,
    USEglm2 = TRUE,
    useAUTOMN = TRUE,
    datesISO = FALSE,
    plotGraphs = TRUE,
    MOMOgroups = list(
      "0to4" =  "age >= 0 & age <=4",
      "5to14" = "age >= 5 & age <=14",
      "15to64" = "age >= 15 & age <=64",
      "65P" = "age >= 65 | is.na(age)",
      "Total" = "age >= 0 | is.na(age)"
    ),
    MOMOmodels = c(
      "0to4" = "LINE",
      "5to14" = "LINE",
      "15to64" = "LINE_SIN",
      "65P" = "LINE_SIN",
      "Total" = "LINE_SIN"
    ))

RunMoMo()
```
