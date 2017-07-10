# MOMOpack for R
[![Build Status](https://travis-ci.org/raubreywhite/MOMOpack-for-R.svg?branch=master)](https://travis-ci.org/raubreywhite/MOMOpack-for-R)
[![codecov](https://codecov.io/gh/raubreywhite/MOMOpack-for-R/branch/master/graph/badge.svg)](https://codecov.io/gh/raubreywhite/MOMOpack-for-R)

#
This is a full port of A-MOMOpack (the code used to analyze mortality data for the [EuroMOMO project](www.euromomo.eu)) from Stata to R.

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
    plotGraphs = TRUE)

RunMoMo()
```
