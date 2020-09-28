# AERE - Structural Topic Models (STM) of Ag. and Env. Econ. research topics

A project in collaboration with *Bartosz Bartkowski* and *Robert Finger*. This is the analytical code.

The paper **Topics in agricultural and environmental economics – a large-scale bibliometric analysis** has been submitted.

Please cite the source code with:

[![DOI](https://zenodo.org/badge/121742448.svg)](https://zenodo.org/badge/latestdoi/121742448)

**BUT** preferably the paper once published.

In this repository you find:

## Overview

- The AERE.R file provides information on the general order of scripts (and would run them silently if executed, not recommended. More detail is provided if scripts are run on their own). Yet, it loads (and installs if need be) the required packages.


## In Detail

The seperate files are

1. The cleaning.R file reads in WoS data (on Linux)

2. The preprocess.R file prepares the data for the STM model

3. The analysis.R file estimates the STM models including uncertainties

4. The descriptivePlots.R file provides the code for plotting bibliometric info

5. The stmDeep.R file extracts further details, plots topic figures, and outputs supplementary material


## Additionally

+ The helperFunctions.R provide some helpful functions for processing text as data and extracting data from text.
The fieldtags.csv file provides some additional information on the WoS data variable codings.
Reference: https://github.com/aknutas/nails

+ The stminsights subfolder provides the shiny app for stminsights that can be accessed at: https://ndroste.shinyapps.io/stminsights/, where the stminsights.RData can be uploaded to inspect the STM model.
Reference: https://github.com/cschwem2er/stminsights
