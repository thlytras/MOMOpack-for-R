# MOMOpack for R

# Originally MOMOpack V 4.3 for Stata, 
# created by Bernadette Gergonne, SSI-EpiLife for Euro MOMO. 
# Ported into R by Theodore Lytras <thlytras@gmail.com>


# Load all MOMO function files
#
# This file will not be needed if we turn MOMOpack into an R package

source(sprintf("%s/include.R", opts$CODEDIR)) # Some necessary helper functions 
source(sprintf("%s/makeMOMOinput.R", opts$CODEDIR)) # Function to prepare MOMO input frame
source(sprintf("%s/analyzeMOMO.R", opts$CODEDIR)) # Function that runs all the analyses
source(sprintf("%s/writeMOMOoutput.R", opts$CODEDIR)) # Create directories, join ("append") the output, and write to disk

source(sprintf("%s/aggregation.R", opts$CODEDIR))
source(sprintf("%s/delay.R", opts$CODEDIR))
source(sprintf("%s/excess.R", opts$CODEDIR))
source(sprintf("%s/table.R", opts$CODEDIR))
source(sprintf("%s/EUROMOMO.R", opts$CODEDIR))
source(sprintf("%s/Period.R", opts$CODEDIR))

source(sprintf("%s/Control_graphs.R", opts$CODEDIR))
source(sprintf("%s/CUSUM_graphs.R", opts$CODEDIR))
source(sprintf("%s/excess_graphs.R", opts$CODEDIR))
source(sprintf("%s/Fit_graphs.R", opts$CODEDIR))
