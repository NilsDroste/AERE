
# 01 cleaning ----

# CAREFUL: this only works properly on a linux machine and potentially mac, else there is an encoding hell on windows.

#List files in input folder
filelist <- list()
filelist <- list.files("/home/ubuntu/Desktop/WoStxt", full.names = T) # this needs to be the directory where WoS Data is stored

# TODO: use readr packages to omit enconding hell on windows!
# Load files in the input folder and merge into a single file
literature_full <- data.frame()
cat("Reading data...\n")
for (file in filelist) {
  # read in partial data
  literature_part <- read.delim(file, header = T,
                                fileEncoding = "UTF-16LE", row.names = NULL,
                                quote = "", stringsAsFactors=FALSE)
  # Merge data
  literature_full <- rbind(literature_full,literature_part)
}
rm(list=c("file","filelist","literature_part")) # cleaning up

cat("Cleaning data...\n")
# Fix misplaced column names
data.names <- names(literature_full)[2:length(names(literature_full))]
data.names[1] <- "PT"
literature_full <- literature_full[, 1:(ncol(literature_full) - 1)]
names(literature_full) <- data.names
rm(data.names) # cleaning up

# Create and add id variable
id <- c(1:nrow(literature_full))
literature_full <- cbind(as.data.frame(id), literature_full)
rm(id) # cleaning up

# for later use when matching acronyms with full variable names:
# TODO: update fieldtags, which are not complete
fieldtags <- read.csv(paste("/home/ubuntu/Desktop", "/fieldtags.csv", sep=""), header = T, sep = ";")

# Fix variable names
tags <- names(literature_full)       # Extract column names
# Match column names (acronyms) with full column names
fields <- as.character(fieldtags$field[match(tags, fieldtags$tag)])
fields[is.na(fields)] <- tags[is.na(fields)]     # Throws warnings but seems to be working
fields <- gsub(" ", "", fields)
rm(list= c("tags","fieldtags")) # cleaning up

# Change literature column names and fix weird names
names(literature_full) <- fields
names(literature_full)[names(literature_full) == "PublicationType(conference,book,journal,bookinseries,orpatent)"] <- "PublicationType"
names(literature_full)[names(literature_full) == "29-CharacterSourceAbbreviation"] <- "SourceAbbreviation"
names(literature_full)[names(literature_full) == "DigitalObjectIdentifier(DOI)" ] <- "DOI"
rm(fields) # cleaning up

# subset to those with a non-empty abstract of the initial set of 32443 entries
literature <- literature_full[literature_full$Abstract != "",] #excluding 8304 empty abstracts
literature <- literature_full[-which(is.na(literature_full$Abstract)),]# exluding 500 entries with DocumentType == "Meeting Abstract" that only has "NA" abstracts


#Format Data
literature$AuthorFullName <- toupper(literature$AuthorFullName)
literature$AuthorFullName <- gsub("'", "", literature$AuthorFullName)
literature$AuthorFullName <- gsub('"', "", literature$AuthorFullName)

literature$AuthorKeywords <- tolower(literature$AuthorKeywords)
literature$AuthorKeywords <- gsub("'", "", literature$AuthorKeywords)
literature$AuthorKeywords <- gsub('"', "", literature$AuthorKeywords)

literature$KeywordsPlus <- tolower(literature$KeywordsPlus)
literature$KeywordsPlus <- gsub("'", "", literature$KeywordsPlus)
literature$KeywordsPlus <- gsub('"', "", literature$KeywordsPlus)

literature$DocumentTitle <- gsub("'", "", literature$DocumentTitle)
literature$DocumentTitle <- gsub('"', "", literature$DocumentTitle)

literature$SubjectCategory <- tolower(literature$SubjectCategory)
literature$SubjectCategory <- gsub("'", "", literature$SubjectCategory)
literature$SubjectCategory <- gsub('"', "", literature$SubjectCategory)

literature$CitedReferences <- gsub("'", "", literature$CitedReferences)
literature$CitedReferences <- gsub('"', "", literature$CitedReferences)
literature$CitedReferences <- toupper(literature$CitedReferences)
literature$CitedReferences <- gsub("DOI DOI", "DOI", literature$CitedReferences)

literature$TimesCited <- as.numeric(as.character(literature$TimesCited))

literature$YearPublished <- as.numeric(as.character(literature$YearPublished))

literature$DOI <- toupper(literature$DOI)

literature$SourceAbbreviation[literature$SourceAbbreviation == "AGR ECON-BLACKWELL"] <-
  "AGR ECON"
literature$SourceAbbreviation[literature$SourceAbbreviation == "ENERGY J"] <-
  "ENERG J"

#write out literature.RData
save(literature, literature_full, file = paste(here(),"literature.RData",sep=""))

