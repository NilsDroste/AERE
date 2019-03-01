
# 05 Extract details of STM model and plot topic figures ----


# Journal prevalence -----

# extract STM interaction model details
cthis.int <-
  stm:::produce_cmatrix(
    prep = prep.LeeMimno.cont.int,
    covariate = "journal",
    method = "pointestimate",
    cov.value1 = NULL,
    cov.value2 = NULL,
    npoints = 100,
    moderator = NULL,
    moderator.value = NULL
  )
cdata.int <- cthis.int$cdata
cmat.int <- cthis.int$cmatrix
simbetas.int <- stm:::simBetas(prep.LeeMimno.cont.int$parameter)
ci.level <-  0.95
offset <- (1 - ci.level)/2 # with ci.level = 0.95


# iterate the extraction over all topics and generate journal prevalence plots per field
sims.int = list()
gplots.cont.int <- list()

#for all topics do ...
for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  #building the simulations for topic model proportions
  sims.int[[i]] <-
    cmat.int %*% t(simbetas.int[[which(prep.LeeMimno.cont.int$topics == prep.LeeMimno.cont.int$topics[i])]])
  dimnames(sims.int[[i]]) <- list(unique(meta$journal))
  
  #built dataframe
  trial.int <- stack(as.data.frame(t(sims.int[[i]])))
  trial.int$AEorERE <-
    as.factor(ifelse(
      trial.int$ind %in% c(
        "Agric. Econ.",
        "Appl. Econ. Perspect. Policy",
        "Am. J. Agr. Econ.",
        "Aust. J. Agr. Resour. Econ.",
        "Can. J. Agric. Econ.-Rev. Can. Agroecon.",
        "Eur. Rev. Agric. Econ.",
        "Food Policy",
        "Agribusiness",
        "J. Agric. Econ.",
        "J. Agric. Resour. Econ."
      ),
      "AE",
      "ERE"
    ))
  names(trial.int)[2] <- "journal"
  levels(trial.int$journal)[which(levels(trial.int$journal) == "Can. J. Agric. Econ.-Rev. Can. Agroecon.")] <-
    "Can. J. Agric. Econ."

  #plotting group wise facets
  gplots.cont.int[[i]] <- ggplot(trial.int) +
    geom_boxplot(aes(x = journal, y = values, fill = AEorERE)) + coord_flip() +
    facet_grid(AEorERE ~ ., scales = "free_y") +
    theme(
      legend.position = "none",
      axis.title.y = element_blank()
    ) +
    labs(title="Journal prevalence per field", y = "Expected Topic Proportion" )
}

# freeing up space
rm(list = c(
  "cdata.int",
  "cmat.int",
  "cthis.int",
  "simbetas.int",
  "trial.int"
))



# Time prevalence ----

# extract time prevalence interaction model details and plot them 
cthis_y.int <-
  stm:::produce_cmatrix(
    prep = prep.LeeMimno.cont.int,
    covariate = "year",
    method = "continuous",
    cov.value1 = NULL,
    cov.value2 = NULL,
    npoints = 25,
    moderator = NULL,
    moderator.value = NULL
  )
cdata_y.int <- cthis_y.int$cdata
cmat_y.int <- cthis_y.int$cmatrix
simbetas_y.int <- stm:::simBetas(prep.LeeMimno.cont.int$parameter)

sims_y.int = list()
means_y.int = list()

# iterate over all topics
for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  #building the simulations for topic model proportions
  sims_y.int[[i]] <-
    cmat_y.int %*% t(simbetas_y.int[[which(prep.LeeMimno.cont.int$topics == prep.LeeMimno.cont.int$topics[i])]])
  # dimnames(sims_y.int[[i]]) <- list(unique(meta$year))
  means_y.int[[i]] <- rowMeans(sims_y.int[[i]])
}

# # extract linear time trend coefficient
# m_y.int <- vector()
# for (i in 1:length(means_y.int)) {
#   m_y.int[i] <- mean(means_y.int[[i]])
# }
# # get the list with hottest (strongest increase)
# top9.estTopProp.cont.year.int <- order(m_y.int, decreasing = T)[1:9]


# Hottest topics ----
# (by strongest increase in prevalence over time) 
# includes plotting time prevalence figures

hottest9.LeeMimno.cont.int <- data.frame()

dattoestimAE <- list()
dattoestimERE <- list()
time.plots <- list()
time.dataAE <- list()
time.dataERE <- list()
time.data.both <- list()

hottest9.LeeMimno.cont.intAE <- data.frame()
hottest9.LeeMimno.cont.intERE <- data.frame()
hottest9.LeeMimno.cont.intBOTH <- data.frame()

for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  
  # AE
  time.dataAE.intermed <- data.frame()
  
  for (j in c(
    "Agric. Econ.",
    "Appl. Econ. Perspect. Policy",
    "Am. J. Agr. Econ.",
    "Aust. J. Agr. Resour. Econ.",
    "Can. J. Agric. Econ.-Rev. Can. Agroecon.",
    "Eur. Rev. Agric. Econ.",
    "Food Policy",
    "Agribusiness",
    "J. Agric. Econ.",
    "J. Agric. Resour. Econ."
  )) {dattoestimAE[[j]] <- extract.estimateEffect(
        prep.LeeMimno.cont.int,
        covariate = "year",
        method = "continuous",
        topics = i,
        moderator = "journal",
        moderator.value = j
      )
    
    time.dataAE.intermed <- rbind(time.dataAE.intermed,dattoestimAE[[j]])
  }
  names(time.dataAE.intermed)[names(time.dataAE.intermed)=="covariate.value"] <- "year"
  time.dataAE[[i]] <- time.dataAE.intermed %>% group_by(year) %>% summarise(means=mean(estimate),ci_min=mean(ci.lower),ci_max=mean(ci.upper))
  
  # ERE
  time.dataERE.intermed <- data.frame()
  
  for (j in c(
    "Annu. Rev. Resour. Econ.",
    "Ecol. Econ.",
    "Energy J.",
    "Energy Econ.",
    "Environ. Resour. Econ.",
    "J.Environ.Econ.Manage.",
    "Land Econ.",
    "Mar. Resour. Econ.",
    "Resour. Energy Econ.",
    "Rev. Env. Econ. Policy"
  )) {
    dattoestimERE[[j]] <- extract.estimateEffect(
        prep.LeeMimno.cont.int,
        covariate = "year",
        method = "continuous",
        topics = i,
        moderator = "journal",
        moderator.value = j
      )
    
    time.dataERE.intermed <- rbind(time.dataERE.intermed,dattoestimERE[[j]])
  }
  names(time.dataERE.intermed)[names(time.dataERE.intermed)=="covariate.value"] <- "year"
  time.dataERE[[i]] <- time.dataERE.intermed %>% group_by(year) %>% summarise(means=mean(estimate),ci_min=mean(ci.lower),ci_max=mean(ci.upper))
  
  # both
  time.data.both[[i]] <- rbind(time.dataAE[[i]] %>% mutate(AEorERE="AE"),time.dataERE[[i]] %>% mutate(AEorERE="ERE"))
  
  time.plots[[i]] <-
    ggplot(time.data.both[[i]],
           aes(
             x = year,
             y = means,
             ymin = ci_min,
             ymax = ci_max,
             group = AEorERE,
             fill = AEorERE
           )) +
    geom_line(aes(colour = AEorERE), size = 1.25) +
    geom_ribbon(alpha = 0.3) +
    labs(y = "Expected Topic Proportion") +
    ggtitle("Average journal prevalence over time per field")

  resAE <- lm(means~year,time.dataAE[[i]])
  resERE <- lm(means~year,time.dataERE[[i]])
  resBOTH <- lm(means~year,time.data.both[[i]])
  hottest9.LeeMimno.cont.intAE <- rbind(hottest9.LeeMimno.cont.intAE,c(i,resAE$coefficients[2]))
  hottest9.LeeMimno.cont.intERE <- rbind(hottest9.LeeMimno.cont.intERE,c(i,resERE$coefficients[2]))
  hottest9.LeeMimno.cont.intBOTH <- rbind(hottest9.LeeMimno.cont.intBOTH,c(i,resBOTH$coefficients[2]))
}

# renaming
names(hottest9.LeeMimno.cont.intAE) <- c("topic","coefficient")
names(hottest9.LeeMimno.cont.intERE) <- c("topic","coefficient")
names(hottest9.LeeMimno.cont.intBOTH) <- c("topic","coefficient")

# extract hottest topics
hottest9.LeeMimno.cont.intAE <- order(hottest9.LeeMimno.cont.intAE$coefficient,decreasing = T)[1:9]
hottest9.LeeMimno.cont.intERE <- order(hottest9.LeeMimno.cont.intERE$coefficient,decreasing = T)[1:9]
hottest9.LeeMimno.cont.intBOTH <- order(hottest9.LeeMimno.cont.intBOTH$coefficient,decreasing = T)[1:9]

# extract topics to present ----

# top 9 journal per average theta
top9.estTopProp.cont.int <-
  order(colMeans(stm.mod.spec.LeeMimno.cont.int$theta[, 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K]),
        decreasing = T)[1:9] # similar but not equal

# combine prevalent, thematical, methodological and hottest topics by top 3 in each
topiclistpub <- as.data.frame(cbind(
  "Topic.No" = as.numeric(c( top9.estTopProp.cont.int[c(1:3)], #most prevalent and most boring topics
                  top9.estTopProp.cont.int[c(4,8,9)], #most prevalent thematical topics
                  top9.estTopProp.cont.int[c(5:7)], #most prevalent method topics
                  c(hottest9.LeeMimno.cont.intBOTH[1],hottest9.LeeMimno.cont.intAE[1],hottest9.LeeMimno.cont.intERE[1]) #hottest topics
  )),
  "Topic.Name" =c("Reviews of economic literature", "Model calibration", "Behavioural modelling", "Sustainable development", "Optimal resource management",  "Economic development", "Sectoral modelling", "Causal inference", "Non-market valuation",  "Spatial valuation", "Species conservation", "Climate change")
))

topiclistpub$Topic.No <- as.numeric(as.character(topiclistpub$Topic.No))


# produce all plots together ----

# function to capitalize first letter
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


j <- 1
for (i in topiclistpub$Topic.No) {
  plot.STM(
    stm.mod.spec.LeeMimno.cont,
    type = "perspectives",
    plabels = c("AE","ERE"),
    main = "Field perspective wordcloud",
    topics = i,
    n = 50
  )
  grid.echo()
  persp.plot <- grid.grab()
  grid.newpage()
  persp.plot <-
    editGrob(
      persp.plot,
      vp = viewport(width = unit(6, "in"), height = unit(7.5, "in")),
      gp = gpar(fontsize = 12)
    )
  grid.draw(persp.plot)
  persp.plot <- as.ggplot(persp.plot) # require(ggplotify)
  
  plot(
    net[[which(topiclistpub$Topic.No == i)]]$graph,
    rescale = T,
    asp = 0,
    ylim = c(-1, 1),
    xlim = c(-1, 1),
    edge.curved = F,
    layout =  layout_in_circle,
    vertex.label.family = "sans",
    vertex.label.dist = 0.7,
    vertex.frame.color = "black",
    vertex.label.color = "black",
    vertex.label.font = 1,
    vertex.label = sapply(tolower(V(net[[which(topiclistpub == i)]]$graph)$name),simpleCap),
    main = "Co-Citation Network",
    edge.color = "gray"
  )
  netw.plot <- recordPlot()
  dev.off()
  png(paste(here(), "/figs/networkplot",i,".png",sep = ""),width = 16, height = 12, units = "cm", res = 300)
  replayPlot(netw.plot)
  dev.off()
  
  netw.plot <- ggdraw() + draw_image(paste(here(),"/figs/networkplot",i,".png",sep = ""), scale = 1)
  
  plot.new()

  title <- ggdraw() + draw_label(topiclistpub$Topic.Name[which(topiclistpub$Topic.No == i)], fontface = "bold", size = 18)
  
  #plotting in grid
  plot_grid(
    title,
    plot_grid(#persp.plot,
                      gplots.cont.int[[i]],
                      persp.plot,
                      ncol = 2,
                      rel_widths = c(2,1)),
    plot_grid(
              #netw.plot,
              # plot.new(),
              netw.plot,
              time.plots[[i]],
              ncol = 2,
              rel_widths = c(1, 1.5)
            ),
    ncol = 1,
    rel_heights = c(0.15, 1, 1)
    )

    j <- j + 1
  
  ggsave(
    paste("Topic", i, ".png", sep = ""),
    width = 40,
    height = 25,
    units = "cm",
    dpi = 300
  )
}

rm(list = c("i", "j", "m", "n"))


# output supplementary information ----

n = 20 #how many words

# get topic description / keywords by different measures
sageLabs.int <- sageLabels(stm.mod.spec.LeeMimno.cont.int, n = n)
labels.int <- labelTopics(stm.mod.spec.LeeMimno.cont.int, n = n)

# write xlsx file with keyword topic descriptions on all topics

# extract most propable words by different measures 
probs <- vector();frexs <- vector(); probsAE <- vector(); probsERE <- vector(); frexsAE <- vector(); frexsERE <- vector()

for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  p <-
    paste(sageLabs.int$marginal$prob[i, ], collapse = ", ")
  probs <- cbind(probs, p)
}
for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  p <-
    paste(sageLabs.int$marginal$frex[i, ], collapse = ", ")
  frexs <- cbind(frexs, p)
}
for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  p <-
    paste(sageLabs.int$cov.betas[[1]]$problabels[i,], collapse = ", ")
  
  probsAE <-
    cbind(probsAE, p)
}
for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  p <-
    paste(sageLabs.int$cov.betas[[2]]$problabels[i,], collapse = ", ")
  
  probsERE <-
    cbind(probsERE, p)
}
for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  p <-
    paste(sageLabs.int$cov.betas[[1]]$frexlabels[i,], collapse = ", ")
  
  frexsAE <-
    cbind(frexsAE, p)
}
for (i in 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K) {
  p <-
    paste(sageLabs.int$cov.betas[[2]]$frexlabels[i,], collapse = ", ")
  
  frexsERE <-
    cbind(frexsERE, p)
}

# built a dataframe
allx <- as.data.frame(cbind(c(1:64),t(probs),t(frexs),t(probsAE),t(frexsAE),t(probsERE),t(frexsERE)))
names(allx) <- c("Topic#","Top10 prob keyword","Top10 frex keywords","Top10 prob keyword AE","Top10 frex keywords AE","Top10 prob keyword ERE","Top10 frex keywordsERE")

# write out xlss
wb <- createWorkbook(type = "xlsx")
sheet  <- createSheet(wb, sheetName = "keywords")
addDataFrame(allx, sheet,col.names = TRUE, row.names = F)
saveWorkbook(wb, paste(here(),"/SI/AllKeywords.xlsx", sep = ""))


# top 250 most probable doc per topic and order by citation counts
thoughts.int <-
  findThoughts(
    stm.mod.spec.LeeMimno.cont.int,
    texts = literature$Abstract,
    n = 250,
    topics = 1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K
  )
top.most.prob.docs.int <-
  as.data.frame(t(as.data.frame(thoughts.int$index)))

# write top 250 most propable articles to excel file
wb <- createWorkbook(type = "xlsx")
for (i in topiclistpub$Topic.No) {
  df <-
    literature[as.numeric(top.most.prob.docs.int[i, ]), c("Authors",
                                                          "YearPublished",
                                                          "ISOSourceAbbreviation", "DocumentTitle",
                                                          "Abstract", "TimesCited")]
  df <- df[order(df$TimesCited, decreasing = T),]
  sheet  <- createSheet(wb, sheetName = paste(topiclistpub$Topic.Name[which(topiclistpub$Topic.No==i)]))
  addDataFrame(df, sheet, col.names = TRUE, row.names = F)
}
saveWorkbook(wb, paste(here(), "/SI/Top250Articles.xlsx", sep=""))
rm(list = c("wb", "sheet", "df"))

