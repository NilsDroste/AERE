
########################################################################################
# Content Analysis of Ag., Env. & Res. Economics (AERE)
# through Structural Topic Models
# Script authors: N. Droste (nils.droste@ufz.de)
# Publication authors: B. Bartowski, N. Droste, R. Finger
########################################################################################

# 0 setting wd, loading packages, preps -----------------------------------------------

# Libraries
if (!require("stm")) install.packages("stm", dep=T)
if (!require("stminsights")) install.packages("stminsights", dep=T)
if (!require("tidystm")) devtools::install_github("mikaelpoul/tidystm", dependencies = TRUE)
if (!require("tidyverse")) install.packages("tidyverse", dep=T)
if (!require("here")) install.packages("here", dep=T)
# if (!require("tidystm")) install.packages("tidystm", dep=T)

# graphics packages
if (!require("cowplot")) install.packages("cowplot", dep=T)
if (!require("gridGraphics")) install.packages("gridGraphics", dep=T)
if (!require("ggplotify")) install.packages("ggplotify", dep=T)
if (!require("igraph")) install.packages("igraph", dep=T)
if (!require("gridExtra")) install.packages("gridExtra", dep=T)

# additional packages
if (!require("emmeans")) install.packages("emmeans", dep=T)
if (!require("xlsx")) install.packages("xlsx", dep=T)
if (!require("bibliometrix")) install.packages("bibliometrix", dep=T)
if (!require("tm")) install.packages("tm", dep=T)
if (!require("magick")) install.packages("magick", dep=T)

# if (!require("wosr")) install.packages("wosr", dep = T)


# Session Info
sessionInfo()

# 1 data loading / cleaning --------------------------------------------------------
# CAREFUL: this only works properly on a linux machine and potentially mac, else there is an encoding hell on windows, i.e. for UTF-16LE Chinese letters.
# UPDATE: with the 2020 update of our date there are parts from WoS that are read in with bibliometrix

source(paste(here(), "/cleaning.R", sep = ""), chdir = T) # writes out literature.RData (on a linux machine!), else this does not work, but could on a Mac, if paths are adatpted

# 2 pre-processing corpus --------------------------------------------------------------

source(paste(here(), "/preprocess.R", sep = ""), chdir = T) # reports back e.g. on creation of stm textProcessor()

# 3 STM model ---------------------------------------------------------------

source(paste(here(), "/analysis.R", sep = ""), chdir = T) # estimates STM model and writes stminsights app input file

# 4 descriptive Plots -------------------------------------------------------------

source(paste(here(), "/descriptivePlots.R", sep= ""), chdir = T) # plotting code, writes the plots to ../figs/ 

# 5 Diving deeper and extracting more detail of analysis ---------------------------------------------------------------

source(paste(here(), "/stmDeep.R", sep = ""), chdir = T) #extracts more in depth-detail of the STM model, plots the topic description figures, and outputs supplementary information.


