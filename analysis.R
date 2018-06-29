
# 01 run stm ----

cat("Estimating model:\n")
stm.mod1 <- stm(docs, vocab, K=15, prevalence = ~ journal + s(year) , content = NULL, 
                data = meta, init.type = c("Spectral"), seed = 48613, max.em.its = 500,
                emtol = 1e-05, verbose = TRUE, reportevery = 5, LDAbeta = TRUE, 
                interactions = TRUE, ngroups = 1, model = NULL, gamma.prior = "Pooled", 
                sigma.prior = 0, kappa.prior = "L1", control = list(maxV=5000)) 
# K=0 uses Lee and Mimno (2014) algorithm to set number of topics which works only with "spectral" algorithm given in Arora et al 2014

# print topic labels
cat("Outcome of the model:\n")
labelTopics(stm.mod1, topics = NULL, n = 7, frexweight = 0.5)
labTop <- labelTopics(stm.mod1, topics = NULL, n = 7, frexweight = 0.5)
keywordsTopics <- as.data.frame(labTop$prob)
row.names(keywordsTopics) <- paste("Topic", row.names(keywordsTopics), sep = " ")
write.csv(keywordsTopics,"keywordsTopics.csv")

# plot topic proportions
cat("Topic proportion plot...\n")
plot.STM(stm.mod1, type = "summary", xlim = c(0, .3))

# calculate uncertainty
cat("Calculating uncertainty...\n")
prep <- estimateEffect(1:15 ~ journal + s(year), stm.mod1,
                       meta = meta, uncertainty = "Global")


# plots
par(mfrow=c(3,5))
for (i in 1:15){
  
  #cat("Plotting model point estimates for journals in model 11:\n")
  plot.estimateEffect(prep, covariate = "journal", topics = i,
                      model = stm.mod1, method = "pointestimate",
                      # cov.value1 = "Liberal", cov.value2 = "Conservative",
                      # xlab = "More Conservative ... More Liberal",
                      main = paste("Journal prevalence for topic no. ", i, sep=""),
                      # + xlim = c(-.1, .1), 
                      labeltype = "custom", custom.labels = as.vector(unique(meta$journal))
                      )
}

for (i in 1:15){
  #cat("Plotting continous time for model 11:\n")
  plot.estimateEffect(prep, covariate = "year", method = "continuous", topics = i,
                      # cov.value1 = "Liberal", cov.value2 = "Conservative",
                      # xlab = "More Conservative ... More Liberal",
                      printlegend = F, main = paste("Time prevalence for topic no. ", i, sep="")
                      )
}

for (i in 1:15){
  #cat("Plotting word cloud for model 11:\n")
  cloud(stm.mod1, topic = i, scale = c(2,.25))

}

dev.off()

# TODO: compute plot interaction of of journal and time per topic