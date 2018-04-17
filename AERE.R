
########################################################################################
# Bibliometric Analysis of Ag., Env. & Res. Economics (AERE)
# through Structural Topic Models
# Script authors: N. Droste (nils.droste@ufz.de)
########################################################################################

# 0 setting wd, loading packages, preps -----------------------------------------------

mainDir <- "C:\\Users\\nilsd\\Dropbox\\Dokumente\\doctorate\\agriculture\\AERE\\analysis"
setwd(file.path(mainDir))

# Session Info
sessionInfo()

# 1 data preparation / cleaning --------------------------------------------------------

#List files in input folder
filelist <- list()
filelist <- list.files("C:\\Users\\nilsd\\Dropbox\\Dokumente\\doctorate\\agriculture\\AERE\\WoStxt", full.names = T)

# Load files in the input folder and merge into a single file
literatureList <- list()
# journals <- c("AE","AEPP","AJARE","AnnRev","CJAE","EcoEcon93-00","EcoEcon01-08","EcoEcon09-15","EcoEcon16-17","EJ","EnEcon93-10","EnEcon11-17","ERAE","ERE","foodpol","JA","JAE","JEEM","JEL93-05","JEL06-17","LE","MRE","REE","REEP")
for (file in filelist) {
  literature <- read.delim(file, header = T, 
                           encoding = "UTF-16", row.names = NULL,
                           quote = "", stringsAsFactors=FALSE,
                           skipNul=T, comment.char="")
  
  # Fix misplaced column names
  data.names <- names(literature)[2:length(names(literature))]
  data.names[1] <- "PT"
  literature <- literature[, 1:(ncol(literature) - 1)]
  names(literature) <- data.names
  
  # Merge data
  literatureList[[file]] <- literature
}

v <- as.vector(1:77)
for (n in v){
  v[n] <- as.character(strsplit(strsplit(names(literatureList[n]),"/")[[1]][2],"[.]")[[1]][1])
}
names(literatureList) <- v

# par(mfrow=c(5,5))
# for (i in (1:length(filelist))){if (is.numeric(literatureList[[i]]$Year)){hist(literatureList[[i]]$Year,breaks = (max(na.omit(literatureList[[i]]$Year))-min(na.omit(literatureList[[i]]$Year))) , main=literatureList[[i]]$Source.title[1])} else {hist(1:100,main=literatureList[[i]]$Source.title[1],col="gray")}}
# plot.new()
# #dev.off()


#trial with txt tab delim

# literatureTEST <- read.delim2("C:\\Users\\nilsd\\Dropbox\\Dokumente\\doctorate\\agriculture\\AERE\\WoStxt\\AJAE_1-500.txt", header = T, encoding = "UTF-16", row.names = NULL, quote = "", stringsAsFactors=FALSE, skipNul=T,comment.char="")

# # Fix misplaced column names
# data.names <- names(literature)[2:length(names(literature))]
# literature <- literature[, 1:(ncol(literature) - 1)]
# names(literature) <- data.names


# for later use when matching acronyms with full variable names0:
fieldtags <- read.csv(paste(getwd(),  "/AERE/", "/fieldtags.csv", sep=""), header = T, sep = ";")
