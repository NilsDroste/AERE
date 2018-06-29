
########################################################################################
# Content Analysis of Ag., Env. & Res. Economics (AERE)
# through Structural Topic Models
# Script authors: N. Droste (nils.droste@ufz.de)
# Publication authors: B. Bartowski, N. Droste, R. Finger
########################################################################################

# 0 setting wd, loading packages, preps -----------------------------------------------

# working directory
mainDir <- "C:\\Users\\nilsd\\Dropbox\\Dokumente\\doctorate\\agriculture\\AERE\\analysis"
setwd(file.path(mainDir))

# Libraries
if (!require("readr")) install.packages("readr")
if (!require("stm")) install.packages("stm")
if (!require("quanteda")) install.packages("quanteda")


# Session Info
sessionInfo()

# 1 data loading / cleaning --------------------------------------------------------

# CAREFUL: this only works properly on a linux machine and potentially mac, else there is an encoding hell on windows, i.e. for UTF-16LE Chinese letters.

source(paste(getwd(), "/cleaning.R", sep = ""), chdir = T) #takes a while # writes out literature.RData (on a linux machine!), else this does not work, but could on a Mac, if paths are adatpted

# 2 pre-processing corpus --------------------------------------------------------------

source(paste(getwd(), "/preprocessing.R", sep = ""), chdir = T) #takes another while # reports back e.g. on creation of stm textProcessor()

# 3 preliminary analysis ---------------------------------------------------------------

source(paste(getwd(), "/analysis.R", sep = ""), chdir = T) #takes another while


# 4 write out workspace

save.image()
