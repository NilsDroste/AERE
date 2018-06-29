# 02 document matrix ----

# # Import data
load(paste(getwd(), "literature.RData", sep=""))

# # old rather manual pre-processing 
# corpus <- gsub("'", "", corpus)  # remove apostrophes
# corpus <- gsub("[[:punct:]]", " ", corpus)  # replace punctuation with space
# corpus <- gsub("[[:cntrl:]]", " ", corpus)  # replace control characters with space
# corpus <- gsub("^[[:space:]]+", "", corpus) # remove whitespace at beginning of documents
# corpus <- gsub("[[:space:]]+$", "", corpus) # remove whitespace at end of documents
# corpus <- gsub("[[:digit:]]", " ", corpus) # remove digits

processed <- textProcessor(literature$Abstract, metadata = as.data.frame(cbind(literature$ISOSourceAbbreviation, literature$YearPublished)), lowercase = TRUE, removestopwords = TRUE, removenumbers = TRUE, removepunctuation = TRUE, stem = TRUE, wordLengths = c(3, Inf), customstopwords = tm::stopwords("french"), verbose = T) # needs the addition of french stopwords for the canadian agr. journal

# # check how many words would be removed with different thresholds
# cat("Plot: How many words would be removed with different thresholds...")
# plotRemoved(processed$documents, lower.thresh = seq(1, 101, by = 5))

# subsetting literature 
if(length(processed$docs.removed)>0){literature <- literature[-processed$docs.removed,]}

# way quicker: but TODO: how to provide metadata?
# # use quanteda in order to supply meta data for later use in stm models
# dfm.data <- dfm(literature$Abstract, tolower = T, stem = T, remove = stopwords(source = "smart"), verbose = T)
# # TODO: check whether to use "groups= literature$PublicationName (what is the benefit?)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, verbose = T)
# TODO why is this removing documents, and terms, 

docs <- out$documents
vocab <- out$vocab
meta <-out$meta
names(meta) <- c("journal","year")
meta$year <- as.numeric(as.character(meta$year))

rm(list=c("out","processed"))

# OLD way per "hand" ... to compute a document term matrix
# # tokenize on space and output as a list
# doc.list <- strsplit(corpus, "[[:space:]]+")
# 
# # compute the table of terms
# term.table <- table(unlist(doc.list))
# term.table <- sort(term.table, decreasing = TRUE)
# 
# # remove terms that are stop words 
# del <- names(term.table) %in% stop_words 
# term.table <- term.table[!del]
# vocab <- names(term.table)
# rm(del)
# 
# # now put the documents into the format required by the lda package
# get.terms <- function(x) {
#   index <- match(x, vocab)
#   index <- index[!is.na(index)]
#   rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
# }
# # mclapply is the multicore enabled version of lapply
# documents <- lapply(doc.list, get.terms)
# 
# # Compute some statistics related to the data set:
# doc.no <- length(documents)  # number of documents 
# term.no <- length(vocab)  # number of terms in the vocab 
# doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
# tot.term.no <- sum(doc.length)  # total number of tokens in the data 
# term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus 
