
# 01 cleaning ----

# CAREFUL: this only works properly on a linux machine and potentially mac, else there is an encoding hell on windows.

#List files in input folder
filelist <- list()
filelist <- list.files(paste0(here(),"/WoStxt"), full.names = T) # this needs to be the directory where WoS Data is stored

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


# for later use when matching acronyms with full variable names:
# TODO: update fieldtags, which are not complete
fieldtags <- read.csv(paste0(here(), "/fieldtags.csv"), header = T, sep = ";")

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


# UPDATE
# this works on windows, too

# read in data
files <-  paste0(here(),"/data/update/", list.files(paste0(here(),"/data/update"))) %>% str_subset( "update_2017-19")
M <- convert2df(files, dbsource = "isi", format = "plaintext")

fieldtags <- read.csv(paste0(here(), "/fieldtags.csv"), header = T, sep = ";")

# Fix variable names
tags <- names(M)       # Extract column names
# Match column names (acronyms) with full column names
fields <- as.character(fieldtags$field[match(tags, fieldtags$tag)])
fields[is.na(fields)] <- tags[is.na(fields)]     # Throws warnings but seems to be working
fields <- gsub(" ", "", fields)
rm(list= c("tags","fieldtags"))

names(M) <- fields
names(M)[names(M) == "PublicationType(conference,book,journal,bookinseries,orpatent)"] <- "PublicationType"
names(M)[names(M) == "29-CharacterSourceAbbreviation"] <- "SourceAbbreviation"
names(M)[names(M) == "DigitalObjectIdentifier(DOI)" ] <- "DOI"

rm(M,D,fields, files)

# merge data and exclude duplicates
literature_update <- M %>% mutate(CitedReferenceCount = as.numeric(CitedReferenceCount), Z9 = as.numeric(Z9), U1 = as.numeric(U1), U2 = as.numeric(U2), Volume =as.numeric(Volume), PageCount = as.numeric(PageCount), PM = as.numeric(PM)) 
literature_full_to_update <- bind_rows(literature_full, literature_update) %>% filter(YearPublished <=2019) %>% mutate( SourceAbbreviation = ifelse(SourceAbbreviation == "AGR ECON-BLACKWELL", "AGR ECON", SourceAbbreviation), SourceAbbreviation = ifelse(SourceAbbreviation == "ENERG J", "ENERGY J", SourceAbbreviation))
literature_full_update <- literature_full_to_update %>% filter(!duplicated(literature_full_to_update$UniqueArticleIdentifier))


# Create and add id variable
literature_full_update <- literature_full_update %>% mutate(id = c(1:nrow(literature_full_update)))

# subset to those with a non-empty abstract of the initial set of 33317 entries
literature <- literature_full_update %>% mutate(Abstract=na_if(Abstract, "")) %>% filter(!is.na(Abstract)) #excluding empty abstracts

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

literature$ISOSourceAbbreviation <- toupper(literature$ISOSourceAbbreviation)

literature$TimesCited <- as.numeric(as.character(literature$TimesCited))

literature$YearPublished <- as.numeric(as.character(literature$YearPublished))

# literature$DOI <- toupper(literature$DOI)
# literature <- literature %>% filter(!duplicated(literature$DOI)) # they all need to be upper to find all duplicates


literature$SourceAbbreviation[literature$SourceAbbreviation == "AGR ECON-BLACKWELL"] <-
  "AGR ECON"
literature$SourceAbbreviation[literature$SourceAbbreviation == "ENERGY J"] <-
  "ENERG J"

# get article numbers per journal
literature_full_update %>% select(SourceAbbreviation) %>% table()
literature %>% select(SourceAbbreviation) %>% table()

# translate the french abstracts from Can J Ag Econ
source(paste(here(), "/CJAE.R", sep = ""))

# update citation counts
# source("Userdata.R") # reading in userdata for WOS
# sid <- auth() # Get session ID
# wosr::pull_incites()


#write out literature.RData
save(literature, literature_full, literature_full_update, literature_update, file = paste0(here(),"/literature.RData"))
             
             