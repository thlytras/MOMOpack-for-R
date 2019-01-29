## momo 2018.08.24

* There are now more options for extracting data from inside your MoMo run. Previously only `momo::dataExport$toSave` was available, however, now there is also `momo::dataExport$aggr`, `momo::dataExport$aggr_fullDelay`, and `momo::dataExport$aggr_delay`

## momo 2018.08.23

* The new delay correction that Jens developed in December 2017 is now available. This can be chosen by `delayVersion="2017-12"`, or the original delay correction can be chosen by `delayVersion="original"` (default).
    
## momo 2017.12.14

* This is a full port of A-MOMOpack (the code used to analyze mortality data for the [EuroMOMO project](www.euromomo.eu)) from Stata to R.
