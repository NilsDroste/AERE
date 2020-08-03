
# 05 Extract details of STM model and plot topic figures ----

load("stminsights_alt.RData")
load("literature.RData")

# get keywords for topics information ----

n = 10 #how many words

# get topic description / keywords by different measures
sageLabs.int <- sageLabels(stm.mod.spec.LeeMimno.cont.int.alt, n = n)
labels.int <- labelTopics(stm.mod.spec.LeeMimno.cont.int.alt, n = n)

# write xlsx file with keyword topic descriptions on all topics

# extract most propable words by different measures 
probs <- vector(); probs_journals <- list() # frexs <- vector(); probsAE <- vector(); probsERE <- vector(); frexsAE <- vector(); frexsERE <- vector()

for (i in 1:stm.mod.spec.LeeMimno.cont.int.alt$settings$dim$K) {
  p <-
    paste(sageLabs.int$marginal$prob[i, ], collapse = ", ")
  probs[i] <-  p
}
# probs_all <- probs %>% as.data.frame()
# names(probs_all)

# for (i in 1:stm.mod.spec.LeeMimno.cont.int.alt$settings$dim$K) {
for (j in 1:length(sageLabs.int$cov.betas)) {
  p <- sageLabs.int$cov.betas[[j]][[1]] %>% as.data.frame()
  p_collapsed <- p %>% unite(col="z", sep=", ")
  names(p_collapsed) <- paste("most probable words in", sageLabs.int$covnames[[j]])
  probs_journals[[j]] <- p_collapsed 
  }

probs_journals_dataframe <- as.data.frame(do.call(cbind, probs_journals))


# built a dataframe
allx <- as.data.frame(cbind("Topic" = 1:stm.mod.spec.LeeMimno.cont.int.alt$settings$dim$K, "most probable words overall" = probs, probs_journals_dataframe))

# write out xlss
wb <- createWorkbook(type = "xlsx")
sheet  <- createSheet(wb, sheetName = "keywords")
addDataFrame(allx, sheet,col.names = TRUE, row.names = F)
saveWorkbook(wb, paste(here(),"/SI/AllKeywords2020.xlsx", sep = ""))


# top 250 most probable doc per topic ----

thoughts.int <-
  findThoughts(
    stm.mod.spec.LeeMimno.cont.int.alt,
    texts = literature$Abstract,
    n = 250,
    topics = 1:stm.mod.spec.LeeMimno.cont.int.alt$settings$dim$K
  )
top.most.prob.docs.int <-
  as.data.frame(t(as.data.frame(thoughts.int$index)))


# time trends per AE or ERE ----

effect <- lapply(c("AE", "ERE"), function(i) {
  extract.estimateEffect(x = prep.LeeMimno.cont.int.alt,
                         covariate = "year",
                         method = "continuous",
                         model = stm.mod.spec.LeeMimno.cont.int.alt,
                         #labeltype = "frex",
                         #n = 4,
                         moderator = "AEorERE",
                         moderator.value = i)
})
effect <- do.call("rbind", effect)

# write out topic absolut change over different periods
wb <- createWorkbook(type = "xlsx")

sheet0  <- createSheet(wb, sheetName = "avg_estim_change_2yr")
avg_estim_change_2yr <- effect %>% mutate(year = floor(covariate.value)) %>% group_by(topic, moderator.value, year) %>% summarise(estimate=mean(estimate)) %>% mutate(lag_estimate= lag(estimate,2), estimate_change=estimate-lag_estimate) %>% filter(year>=2017 & year<2019) %>% group_by(topic, moderator.value, year) %>% summarise(estimate_change = mean(estimate_change)) %>% ungroup() %>% filter(year==2018) %>% select(-year) %>% arrange(moderator.value, -estimate_change) %>% rename(AEorERE=moderator.value) %>% as.data.frame()
addDataFrame(avg_estim_change_2yr, sheet0,col.names = TRUE, row.names = F)

sheet1  <- createSheet(wb, sheetName = "avg_estim_change_5yr")
avg_estim_change_5yr <- effect %>% mutate(year = floor(covariate.value)) %>% group_by(topic, moderator.value, year) %>% summarise(estimate=mean(estimate)) %>% mutate(lag_estimate= lag(estimate,5), estimate_change=estimate-lag_estimate) %>% filter(year>=2014 & year<2019) %>% group_by(topic, moderator.value, year)%>% summarise(estimate_change = mean(estimate_change)) %>% ungroup() %>% filter(year==2018) %>% select(-year) %>% arrange(moderator.value, -estimate_change) %>% rename(AEorERE=moderator.value) %>% as.data.frame()
addDataFrame(avg_estim_change_5yr, sheet1,col.names = TRUE, row.names = F)

sheet2  <- createSheet(wb, sheetName = "avg_estim_change_10yr")
avg_estim_change_10yr <- effect %>% mutate(year = floor(covariate.value)) %>% group_by(topic, moderator.value, year) %>% summarise(estimate=mean(estimate)) %>% mutate(lag_estimate= lag(estimate,10), estimate_change=estimate-lag_estimate) %>% filter(year>=2009 & year<2019) %>% group_by(topic, moderator.value, year) %>% summarise(estimate_change = mean(estimate_change)) %>% ungroup() %>% filter(year==2018) %>% select(-year) %>% arrange(moderator.value, -estimate_change) %>% rename(AEorERE=moderator.value) %>% as.data.frame()
addDataFrame(avg_estim_change_10yr, sheet2,col.names = TRUE, row.names = F)

sheet3  <- createSheet(wb, sheetName = "avg_estim_change_15yr")
avg_estim_change_15yr <- effect %>% mutate(year = floor(covariate.value)) %>% group_by(topic, moderator.value, year) %>% summarise(estimate=mean(estimate)) %>% mutate(lag_estimate= lag(estimate,15), estimate_change=estimate-lag_estimate) %>% filter(year>=2004 & year<2019) %>% group_by(topic, moderator.value, year) %>% summarise(estimate_change = mean(estimate_change)) %>% ungroup() %>% filter(year==2018) %>% select(-year) %>% arrange(moderator.value, -estimate_change) %>% rename(AEorERE=moderator.value) %>% as.data.frame()
addDataFrame(avg_estim_change_15yr, sheet3,col.names = TRUE, row.names = F)

saveWorkbook(wb, paste(here(),"/SI/HottestTopics2020_change.xlsx", sep = ""))

# create dataframe of selected hot topics ----

topiclistpub <- as.data.frame(cbind(
  "Topic.No" = c(3, 10, 12, 13, 14, 17, 33, 38, 45, 50) #hottest topics
  ,
  "Topic.Name" = c("farm business", "agents' behaviour", "choice experiments", "agricultural insurance", "market shocks",  "supply chain", "energy efficiency", "household income", "policy design", "biodiversity conservation"  ),
  "Field" = c("AE", "both", "AE", "AE", "ERE", "AE", "ERE", "AE", "ERE", "ERE")
))


# write top 250 most propable articles to excel file ----

# get the updated citation counts
final_update <- read_csv("data/update/final_update.csv")
literature_updated_citations <- literature %>% left_join(final_update %>% select(code, citation) %>% rename(citation_update = citation), by = c("UniqueArticleIdentifier" = "code")) %>% mutate(TimesCited = ifelse(is.na(citation_update), TimesCited, citation_update))

# library(xlsx)
wb <- createWorkbook(type = "xlsx")
for (i in as.numeric(as.character(topiclistpub$Topic.No))) {
  df <-
    literature_updated_citations[as.numeric(top.most.prob.docs.int[i, ]), c("Authors",
                                                          "YearPublished",
                                                          "ISOSourceAbbreviation", "DocumentTitle",
                                                          "Abstract","DOI", "TimesCited" )]
  df <- df[order(df$TimesCited, decreasing = T),]
  sheet  <- createSheet(wb, sheetName = paste0("Topic # ", i, " ", topiclistpub$Topic.Name[which(as.numeric(as.character(topiclistpub$Topic.No))==i)]))
  addDataFrame(df, sheet, col.names = TRUE, row.names = F)
}
saveWorkbook(wb, paste(here(), "/SI/Top250Articles.xlsx", sep=""))
rm(list = c("wb", "sheet", "df"))

# plot time trends ----
p1 <-
  ggplot(
    effect %>% filter(topic %in% topiclistpub$Topic.No) %>% left_join(
      topiclistpub %>% mutate(Topic.No = as.numeric(Topic.No)),
      by = c("topic" = "Topic.No")
    ),
    aes(
      x = covariate.value,
      y = estimate,
      ymin = ci.lower,
      ymax = ci.upper,
      group = moderator.value,
      scales = "free_y",
      fill = factor(moderator.value),
      col = factor(moderator.value)
    )
  ) +
  facet_wrap( ~ Topic.Name, nrow = 8, scales = "free_y") +
  geom_line() +
  geom_ribbon(alpha = .5) +
  theme(legend.position = "bottom") +
  labs(x = "year",
       y = "Expected Topic Proportion",
       fill = "",
       col = "") +
  theme(plot.margin=unit(c(t=5.5, r=10, b=5.5, l=5.5), "points")) 

ggsave( paste0(here(),"/Figs/Fig3.pdf"), p1,  height = 11.75, width = 8.8, units = "in")

# wordcloud nach field ----

# create new corpus by topics
newcorpus <- data.frame()
for (i in as.numeric(topiclistpub$Topic.No)) {
  newcorpus <- literature %>% slice(top.most.prob.docs.int[i,] %>% as.numeric()) %>% bind_cols(topic = c(rep(topiclistpub$Topic.Name[which(topiclistpub$Topic.No==i)], 250))) %>% bind_rows(newcorpus)
} 

newcorpus <- newcorpus %>% select(Authors, YearPublished, DocumentTitle, ISOSourceAbbreviation, Abstract, topic)
newcorpus[newcorpus$ISOSourceAbbreviation=="ANNU. REV. RESOUR. ECON","ISOSourceAbbreviation"] <- "ANNU. REV. RESOUR. ECON." # correcting typo
newcorpus$AEorERE <- as.factor(ifelse(newcorpus$ISOSourceAbbreviation %in% toupper(c("Agric. Econ.","Appl. Econ. Perspect. Policy","Am. J. Agr. Econ.","Aust. J. Agr. Resour. Econ.","Can. J. Agric. Econ.-Rev. Can. Agroecon.","Eur. Rev. Agric. Econ.","Food Policy","Agribusiness","J. Agric. Econ.","J. Agric. Resour. Econ.")), "AE","ERE"))

for (i in as.numeric(topiclistpub$Topic.No)) {
  
    newcorp_sub <- newcorpus %>% filter(topic == topiclistpub$Topic.Name[which(topiclistpub$Topic.No==i)], AEorERE=="AE")
  vocabfreq_AE <- newcorp_sub$Abstract %>% VectorSource() %>% Corpus() %>% tm_map(content_transformer(tolower)) %>% tm_map(removeWords, c(stopwords("smart"), stopwords("french"),"elsevier", "wiley", "sons", "inc", "may","ltd")) %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stemDocument, language = "english") %>% TermDocumentMatrix() %>% as.matrix() %>% rowSums() %>% as.data.frame() %>% bind_cols("AE")
  vocabfreq_AE$vocab <- vocabfreq_AE %>% rownames
  names(vocabfreq_AE) <- c("freq","AEorERE","vocab")
  rownames(vocabfreq_AE) <- c()
  vocabfreq_AE <- vocabfreq_AE %>% arrange(-freq) %>% head(30)
  
  newcorp_sub <- newcorpus %>% filter(topic == topiclistpub$Topic.Name[which(topiclistpub$Topic.No==i)], AEorERE=="ERE")
  vocabfreq_ERE <- newcorp_sub$Abstract %>% VectorSource() %>% Corpus() %>% tm_map(content_transformer(tolower)) %>% tm_map(removeWords, c(stopwords("smart"), stopwords("french"),"elsevier", "wiley", "sons", "inc", "may","ltd")) %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stemDocument, language = "english") %>% TermDocumentMatrix() %>% as.matrix() %>% rowSums() %>% as.data.frame() %>% bind_cols("ERE")
  vocabfreq_ERE$vocab <- vocabfreq_ERE %>% rownames
  names(vocabfreq_ERE) <- c("freq","AEorERE","vocab")
  rownames(vocabfreq_ERE) <- c()
  vocabfreq_ERE <- vocabfreq_ERE %>% arrange(-freq) %>% head(30)
  
  vocabfreq <- bind_rows(vocabfreq_AE,vocabfreq_ERE) %>% arrange(desc(vocab))
  vocabfreq$freq <- (vocabfreq$freq-min(vocabfreq$freq))/(max(vocabfreq$freq)-min(vocabfreq$freq))

  
  cloudlist[[which(arrange(topiclistpub, Topic.Name)$Topic.No==i)]] <- 
    
    ggplot(
      vocabfreq,
      aes(
        label = vocab, size = freq, x = AEorERE, col = AEorERE
      )
    ) +
    ggwordcloud::geom_text_wordcloud_area() +
    #scale_size_area(max_size = 10) +
    scale_x_discrete(breaks = NULL) +
    theme_minimal() +
    labs(x=NULL) +
    #facet_wrap(~AEorERE) + 
    ggtitle(topiclistpub$Topic.Name[which(topiclistpub$Topic.No==i)])
}
p2 <- plot_grid(plotlist = cloudlist, nrow=5)

ggsave( paste0(here(),"/figs/fig4.pdf"), p2,  height = 10, width = 8.8, units = "in")


# co-citation network from bibliometrix ----

net = list()
networkplots <- list()
for (l in topiclistpub$Topic.No %>% as.numeric()) {
  lit <-
    literature %>%
    slice(thoughts.int$index[topiclistpub$Topic.No %>% as.numeric()][[which(topiclistpub$Topic.No %>% as.numeric() == l)]]) %>%   # subsetting to row indices of 100 most probable word for topic in topiclistpub
    select(CitedReferences)
  CR <- lit$CitedReferences
  FCAU = list(NULL)
  CCR = NULL
  size = dim(lit)
  listCAU = strsplit(as.character(CR), ";")
  for (i in 1:size[1]) {
    elem = strsplit(as.character(listCAU[[i]]), ",")
    ind = lengths(elem)
    if (max(ind) > 2) {
      elem = elem[ind > 2]
      FCAU[[i]] = bibliometrix::trim.leading(unlist(lapply(elem,
                                                           function(l)
                                                             l[[3]])))
      CCR[i] = paste(FCAU[[i]], collapse = ";")
    }
    else {
      CCR[[i]] = NA
    }
  }
  
  Fi <- strsplit(CCR, ";")
  Fi <- lapply(Fi, bibliometrix::trim.leading)
  
  uniqueField <- names(table(literature$SourceAbbreviation))
  
  Fi <- lapply(Fi, function(l) {
    l <- gsub("\\,", ";", l)
    l <- sub("\\;", ",", l)
    l <- sub("\\;", ",", l)
    l <- gsub("\\;.*", "", l)
    l <- l[nchar(l) > 0]
    return(l)
  })
  
  WSO <-
    matrix(0, size[1], length(uniqueField)) # quite big may need carbage collection gc() or a new R session
  colnames(WSO) <- uniqueField

  for (i in 1:size[1]) {
    if (length(Fi[[i]]) > 0) {
      WSO[i, uniqueField %in% Fi[[i]]] <- 1
    }
  }
  
  WSO = WSO[, !is.na(uniqueField)]
  
  NetMatrix = NA
  NetMatrix = crossprod(WSO, WSO)
  NetMatrix = NetMatrix[nchar(colnames(NetMatrix)) != 0, nchar(colnames(NetMatrix)) !=
                          0]
  net[[which(topiclistpub$Topic.No %>% as.numeric() == l)]] = bibliometrix::networkPlot(
    NetMatrix,
    degree = 2,
    Title = paste("Co-Citation Network for topic",l, sep=" "),
    type = "kamada",
    size.cex = TRUE,
    size = 15,
    remove.multiple = T,
    remove.isolates = T,
    noloops = T,
    labelsize = 0.7,
    edgesize = 10,
    edges.min = 2,
    cluster = "edge_betweenness",
    halo = F
  )
  
}


# plot co-citation network
netw.plot.list <- list()


for (i in topiclistpub$Topic.No %>% as.numeric) {

  V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$color <- as.character(ifelse(V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$name %in% tolower(c("AGR ECON","APPL ECON PERSPECT P","AM J AGR ECON","AUST J AGR RESOUR EC","CAN J AGR ECON","EUR REV AGRIC ECON","FOOD POLICY","AGRIBUSINESS","J AGR ECON","J AGR RESOUR ECON")), "#F8766D","#00BFC4"))

  V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$name <- V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$name %>% recode("agr econ" = "AE", "agribusiness" = "AB", "am j agr econ" = "AJAE", "annu rev resour econ" = "ARRE", "appl econ perspect p" = "AEPP", "aust j agr resour ec" = "AJARE", "can j agr econ" = "CJAE", "ecol econ" = "EcE", "energ econ" = "EnE", "energ j" = "EJ", "environ resour econ" = "ERE", "eur rev agric econ" = "ERAE", "food policy" = "FP", "j agr econ" = "JAE","j agr resour econ" = "JARE", "j environ econ manag" = "JEEM", "land econ" = "LE", "mar resour econ" = "MRE", "resour energy econ" = "REE", "rev env econ policy" = "REEP")

  V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$label.cex = V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$size/10

  V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$size <- V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$size*2

  png(paste(here(), "/figs/networkplot",i,".png",sep = ""),width = 14, height = 14, units = "cm", res = 600)
  
  plot(
    net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph,
    rescale = T,
    asp = 0,
    ylim = c(-1.2, 1.2),
    xlim = c(-1.2, 1.2),
    edge.curved = F,
    layout =  layout_in_circle,
    vertex.label.family = "sans",
    vertex.label.dist = 0.7,
    vertex.frame.color = "black",
    vertex.label.color = "black",
    vertex.label.font = 1,
    main = "",
    vertex.label = V(net[[which(topiclistpub$Topic.No %>% as.numeric() == i)]]$graph)$name,
    edge.color = "gray"
  )
  title(paste0( topiclistpub$Topic.Name[which(topiclistpub$Topic.No %>% as.numeric() == i)]), font.main = 1 ,cex.main=1.8, line = -1)
  dev.off()
  
  magick::image_crop(magick::image_read(paste(here(), "/figs/networkplot",i,".png",sep = "")), "2168x2104+752+800",  gravity = "southwest") %>% magick::image_write(paste(here(), "/figs/networkplot",i,".png",sep = ""))

  netw.plot.list[[which(arrange(topiclistpub, Topic.Name)$Topic.No==i)]] <- ggdraw() + draw_image(paste(here(),"/figs/networkplot",i,".png",sep = ""), scale = 1)

}

(p3 <- plot_grid(plotlist = netw.plot.list, nrow=5))

ggsave( paste0(here(),"/figs/Fig5.pdf"), p3,  height = 16, width = 12, units = "in")

