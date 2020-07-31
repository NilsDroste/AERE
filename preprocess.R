# 02 generate document matrix ----

# # Import data
cat("loading data...\n")
load(paste(here(), "/literature.RData", sep=""))

cat("processing raw data...\n")
# literature <- literature[!(literature$ISOSourceAbbreviation %in% c("Forest Policy Econ.","J. For. Econ.")),] #subsetting to exclude forest journals that where not in triangulation
literature$Abstract <- tm:::removeWords(literature$Abstract, "All rights reserved") # removing the Elsevier B.V. copy right notice 
literature$Abstract <- tm:::removeWords(literature$Abstract, "Wiley Periodicals") # Wiley Periodicals

processed <- textProcessor(literature$Abstract, metadata = as.data.frame(cbind(literature$id,literature$Authors, literature$YearPublished, literature$DocumentTitle, literature$ISOSourceAbbreviation, literature$Abstract)), lowercase = TRUE, removestopwords = TRUE, removenumbers = TRUE, removepunctuation = TRUE, stem = TRUE, wordLengths = c(3, Inf), customstopwords = c(tm::stopwords("french"),"elsevier", "wiley", "sons", "inc", "may","ltd"), verbose = T) # removes 'smart' stopwords, but also needs the removal of french stopwords for the canadian agr. journal and publisher words such as elsevier, wiley, and may (since it is not in smart!?)

cat("aligning literature data with processed corpus...\n")
if(length(processed$docs.removed)>0){literature <- literature[-processed$docs.removed,]} #would exclude from literature if there were any docs removed through pre-processing to maintain same data-base

cat("preparing output...\n")
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, verbose = T)

# default setting lower.thresh=1 means that words which appear in only one document will be dropped
names(out$meta) <- c("id", "author","year","title","journal","abstract")
out$meta$year <- as.numeric(as.character(out$meta$year))
out$meta[out$meta$journal=="ANNU. REV. RESOUR. ECON","journal"] <- "ANNU. REV. RESOUR. ECON." # correcting typo
out$meta$journal <- out$meta$journal[,drop = T] #dropping unused factor level
out$meta$AEorERE <- as.factor(ifelse(out$meta$journal %in% toupper(c("Agric. Econ.","Appl. Econ. Perspect. Policy","Am. J. Agr. Econ.","Aust. J. Agr. Resour. Econ.","Can. J. Agric. Econ.-Rev. Can. Agroecon.","Eur. Rev. Agric. Econ.","Food Policy","Agribusiness","J. Agric. Econ.","J. Agric. Resour. Econ.")), "AE","ERE"))
out$meta$abstract <- as.character(out$meta$abstract)
out$meta$title <- as.character(out$meta$title)
out$meta$author <- as.character(out$meta$author)


docs <- out$documents
vocab <- out$vocab
meta <-out$meta

rm(list=c("processed"))

# Removing 20421 of 36274 terms (20421 of 1557362 tokens) due to frequency 
# Your corpus now has 24828 documents, 15853 terms and 1536941 tokens.