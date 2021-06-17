# inflation
This repo contains data and code for working with data from the Consumer Price Index, including relative-importance values for all months back to Dec. 2017, and the code used to calculate it.
There are three files here:
- `cpi_weights.csv` contains the weights for both CPI-U (`wt`) and CPI-W(`w_wt`) for all available item codes by month.
- `cpi_weights.R` contains the code to calculate and extract those weights.
- `cpi_functions.R` contains miscellaneous functions for working with the CPI data. These are really for my own use, and the code is unannotated, so probably won't be of general interest.