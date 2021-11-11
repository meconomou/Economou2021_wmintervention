# Description -------------------------------------------------------------

# This script executes the longitudinal analysis
# (i.e. global and along-tract prepost intervention effects)

# Load data
source(here::here("code","01_load_data.R"))

# Average tract models --------------------------------------------------------------

m.laf <- lmer(mean_fa ~ time*intervention + mean_RMSt + (1|id),
              data=meantracts_long[meantracts_long$tract=="Left_Arcuate",])
m.raf <- lmer(mean_fa ~ time*intervention + mean_RMSt + (1|id),
              data=meantracts_long[meantracts_long$tract=="Right_Arcuate",])
m.lslf <- lmer(mean_fa ~ time*intervention + mean_RMSt + (1|id),
               data=meantracts_long[meantracts_long$tract=="Left_SLF",])
m.rslf <- lmer(mean_fa ~ time*intervention + mean_RMSt + (1|id),
               data=meantracts_long[meantracts_long$tract=="Right_SLF",])
m.lifof <- lmer(mean_fa ~ time*intervention + mean_RMSt + (1|id),
                data=meantracts_long[meantracts_long$tract=="Left_IFOF",])
m.rifof <- lmer(mean_fa ~ time*intervention + mean_RMSt + (1|id),
                data=meantracts_long[meantracts_long$tract=="Right_IFOF",])
m.lilf <- lmer(mean_fa ~ time*intervention + mean_RMSt + (1|id),
               data=meantracts_long[meantracts_long$tract=="Left_ILF",])
m.rilf <- lmer(mean_fa ~ time*intervention + mean_RMSt + (1|id),
               data=meantracts_long[meantracts_long$tract=="Right_ILF",])


# Along-tract models --------------------------------------------------------------

leftAF_models<-lapply(nodelist, function(x) {
  lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                  list(i=as.name(x))), data=leftAF_cast)
})

rightAF_models<-lapply(nodelist, function(x) {
  lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                  list(i=as.name(x))), data=rightAF_cast)
})

leftSLF_models<-lapply(nodelist, function(x) {
  lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                  list(i=as.name(x))), data=leftSLF_cast)
})

rightSLF_models<-lapply(nodelist, function(x) {
  lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                  list(i=as.name(x))), data=rightSLF_cast)
})

leftIFOF_models<-lapply(nodelist, function(x) {
  lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                  list(i=as.name(x))), data=leftIFOF_cast)
})

rightIFOF_models<-lapply(nodelist, function(x) {
  lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                  list(i=as.name(x))), data=rightIFOF_cast)
})

leftILF_models<-lapply(nodelist, function(x) {
  lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                  list(i=as.name(x))), data=leftILF_cast)
})

rightILF_models<-lapply(nodelist, function(x) {
  lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                  list(i=as.name(x))), data=rightILF_cast)
})

# Compile along-tract results --------------------------------------------------------------

leftAF_results <- models2df(leftAF_models)
leftAF_results$tract <- "leftAF"
rightAF_results <- models2df(rightAF_models)
rightAF_results$tract <- "rightAF"
leftIFOF_results <- models2df(leftIFOF_models)
leftIFOF_results$tract <- "leftIFOF"
rightIFOF_results <- models2df(rightIFOF_models)
rightIFOF_results$tract <- "rightIFOF"
leftILF_results <- models2df(leftILF_models)
leftILF_results$tract <- "leftILF"
rightILF_results <- models2df(rightILF_models)
rightILF_results$tract <- "rightILF"
leftSLF_results <- models2df(leftSLF_models)
leftSLF_results$tract <- "leftSLF"
rightSLF_results <- models2df(rightSLF_models)
rightSLF_results$tract <- "rightSLF"


prepost_alongtract_mod_res <-
  rbind(leftAF_results, rightAF_results, leftIFOF_results, rightIFOF_results,
        leftILF_results, rightILF_results, leftSLF_results, rightSLF_results)

# Write model outputs --------------------------------------------------------------

prepost_avg_mod_list =list(m.laf, m.raf, m.lslf, m.rslf,
                           m.lifof, m.rifof, m.lilf, m.rilf)
saveRDS(prepost_avg_mod_list, here("data","models","prepost_avg_mod.RDS"))

prepost_alongtract_mod_list =list(leftAF_models, rightAF_models,
                                  leftSLF_models, rightSLF_models,
                                  leftIFOF_models, rightIFOF_models,
                                  leftILF_models, rightILF_models)
# takes several minutes to write!!
saveRDS(prepost_alongtract_mod_list, here("data","models","prepost_alongtract_mod.RDS"))

fwrite(prepost_alongtract_mod_res, file=here("data","models","prepost_alongtract_mod_res.csv"),
       col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")
