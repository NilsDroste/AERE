
########################################################################################
# Bibliometric Analysis of Ag., Env. & Res. Economics (AERE)
# through Structural Topic Models
# Script authors: N. Droste (nils.droste@ufz.de)
########################################################################################

# 0 setting wd, loading packages, preps -----------------------------------------------

mainDir <- "C:\\Users\\droste\\Documents\\R\\AERE"
setwd(file.path(mainDir))

# Session Info
sessionInfo()

# 1 data preparation / cleaning --------------------------------------------------------

#List files in input folder
filelist <- list()
filelist <- list.files(paste(getwd(), "/data/", sep=""), full.names = T)

# Load files in the input folder and merge into a single file
literatureList <- list()
# journals <- c("AE","AEPP","AJARE","AnnRev","CJAE","EcoEcon93-00","EcoEcon01-08","EcoEcon09-15","EcoEcon16-17","EJ","EnEcon93-10","EnEcon11-17","ERAE","ERE","foodpol","JA","JAE","JEEM","JEL93-05","JEL06-17","LE","MRE","REE","REEP")
for (file in filelist) {
  literature <- read.csv(file, header = T, sep=",", row.names = NULL, stringsAsFactors=FALSE )
  # Merge data
  literatureList[[file]] <- literature
}

par(mfrow=c(5,5))
for (i in (1:length(filelist))){if (is.numeric(literatureList[[i]]$Year)){hist(literatureList[[i]]$Year,breaks = length(na.omit(unique(literatureList[[i]]$Year))) , main=literatureList[[i]]$Source.title[1])} else {hist(1:100,main=literatureList[[i]]$Source.title[1],col="gray")}}
plot.new()
#dev.off()