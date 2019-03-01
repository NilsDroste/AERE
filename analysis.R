
# 03 run stm ----

cat("Estimating stm model:\n")
cat("Using Lee and Mimno (2014) algorithm to define no. of topics")

# stm.mod.spec.LeeMimno <- stm(docs, vocab, K=0, prevalence = ~ journal + s(year), content = NULL,
#                        data = meta, init.type = c("Spectral"), seed = 48613, max.em.its = 500,
#                        emtol = 1e-05, verbose = TRUE, reportevery = 5, LDAbeta = TRUE,
#                        interactions = TRUE, ngroups = 1, model = NULL, gamma.prior = "Pooled",
#                        sigma.prior = 0, kappa.prior = "L1", control = list(maxV=5000))
# 
# stm.mod.spec.LeeMimno.cont <- stm(docs, vocab, K=0, prevalence = ~ journal + s(year), 
#                                   content = ~ AEorERE, data = meta, init.type = c("Spectral"), 
#                                   seed = 48613, max.em.its = 500, emtol = 1e-05, verbose = TRUE,
#                                   reportevery = 5, LDAbeta = TRUE, interactions = TRUE, ngroups = 1,
#                                   model = NULL, gamma.prior = "Pooled", sigma.prior = 0, 
#                                   kappa.prior = "L1", control = list(maxV=1000))

stm.mod.spec.LeeMimno.cont.int <- stm(docs, vocab, K=0, prevalence = ~ journal * year, 
                                  content = ~ AEorERE, data = meta, init.type = c("Spectral"), 
                                  seed = 48613, max.em.its = 500, emtol = 1e-05, verbose = TRUE,
                                  reportevery = 5, LDAbeta = TRUE, interactions = TRUE, ngroups = 1,
                                  model = NULL, gamma.prior = "Pooled", sigma.prior = 0, 
                                  kappa.prior = "L1", control = list(maxV=1000))


# calculate uncertainty
cat("Estimating effects, calculating uncertainty...\n")

# prep.LeeMimno <- estimateEffect(1:stm.mod.spec.LeeMimno$settings$dim$K ~ journal + s(year), 
#                                 stm.mod.spec.LeeMimno, meta = meta, uncertainty = "Global")
# prep.LeeMimno.cont <- estimateEffect(1:stm.mod.spec.LeeMimno.cont$settings$dim$K ~ journal + s(year), 
#                                 stm.mod.spec.LeeMimno.cont, meta = meta, uncertainty = "Global")
prep.LeeMimno.cont.int <- estimateEffect(1:stm.mod.spec.LeeMimno.cont.int$settings$dim$K ~ journal * year, stm.mod.spec.LeeMimno.cont.int, meta = meta, uncertainty = "Global")


# write output for stminsights
cat("Writing stminsights output.\n")
save(stm.mod.spec.LeeMimno.cont.int,prep.LeeMimno.cont.int,out,file="stminsights.RData",compress = "xz")
# stminsights::run_stminsights()
