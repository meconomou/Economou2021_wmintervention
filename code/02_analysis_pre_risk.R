# Description -------------------------------------------------------------

# This script executes the baseline risk-related analysis
# (i.e. global and along-tract differences between at-risk and typically developing children)

# Load data
source(here::here("code","01_load_data.R"))

# Average tract models --------------------------------------------------------------

m.laf_pre <- lm(mean_fa_pre ~ risk + mean_RMSt_pre,
                data=meantracts_wide[meantracts_wide$tract=="Left_Arcuate",])

m.raf_pre <- lm(mean_fa_pre ~ risk + mean_RMSt_pre ,
                data=meantracts_wide[meantracts_wide$tract=="Right_Arcuate",])

m.lslf_pre <- lm(mean_fa_pre ~ risk + mean_RMSt_pre,
                 data=meantracts_wide[meantracts_wide$tract=="Left_SLF",])

m.rslf_pre <- lm(mean_fa_pre ~ risk + mean_RMSt_pre,
                 data=meantracts_wide[meantracts_wide$tract=="Right_SLF",])

m.lifof_pre <- lm(mean_fa_pre ~ risk + mean_RMSt_pre,
                  data=meantracts_wide[meantracts_wide$tract=="Left_IFOF",])

m.rifof_pre <- lm(mean_fa_pre ~ risk + mean_RMSt_pre,
                  data=meantracts_wide[meantracts_wide$tract=="Right_IFOF",])

m.lilf_pre <- lm(mean_fa_pre ~ risk + mean_RMSt_pre,
                 data=meantracts_wide[meantracts_wide$tract=="Left_ILF",])

m.rilf_pre <- lm(mean_fa_pre ~ risk + mean_RMSt_pre,
                 data=meantracts_wide[meantracts_wide$tract=="Right_ILF",])

# Along-tract models --------------------------------------------------------------

leftAF_models_base <-lapply(nodelist, function(x) {
  lm(substitute(i ~ risk + mean_RMSt, list(i=as.name(x))),
     data=leftAF_cast[leftAF_cast$time=="ses-01",])
})

rightAF_models_base <-lapply(nodelist, function(x) {
  lm(substitute(i ~ risk + mean_RMSt, list(i=as.name(x))),
     data=rightAF_cast[rightAF_cast$time=="ses-01",])
})

leftSLF_models_base <-lapply(nodelist, function(x) {
  lm(substitute(i ~ risk + mean_RMSt, list(i=as.name(x))),
     data=leftSLF_cast[leftSLF_cast$time=="ses-01",])
})

rightSLF_models_base <-lapply(nodelist, function(x) {
  lm(substitute(i ~ risk + mean_RMSt, list(i=as.name(x))),
     data=rightSLF_cast[rightSLF_cast$time=="ses-01",])
})

leftIFOF_models_base <-lapply(nodelist, function(x) {
  lm(substitute(i ~ risk + mean_RMSt, list(i=as.name(x))),
     data=leftIFOF_cast[leftIFOF_cast$time=="ses-01",])
})

rightIFOF_models_base <-lapply(nodelist, function(x) {
  lm(substitute(i ~ risk + mean_RMSt, list(i=as.name(x))),
     data=rightIFOF_cast[rightIFOF_cast$time=="ses-01",])
})

leftILF_models_base <-lapply(nodelist, function(x) {
  lm(substitute(i ~ risk + mean_RMSt, list(i=as.name(x))),
     data=leftILF_cast[leftILF_cast$time=="ses-01",])
})

rightILF_models_base <-lapply(nodelist, function(x) {
  lm(substitute(i ~ risk + mean_RMSt, list(i=as.name(x))),
     data=rightILF_cast[rightILF_cast$time=="ses-01",])
})

# Compile along-tract results ---------------------------------------------

leftAF_results_base <- models2df(leftAF_models_base)
leftAF_results_base$tract <- "leftAF"
rightAF_results_base <- models2df(rightAF_models_base)
rightAF_results_base$tract <- "rightAF"
leftSLF_results_base <- models2df(leftSLF_models_base)
leftSLF_results_base$tract <- "leftSLF"
rightSLF_results_base <- models2df(rightSLF_models_base)
rightSLF_results_base$tract <- "rightSLF"
leftIFOF_results_base <- models2df(leftIFOF_models_base)
leftIFOF_results_base$tract <- "leftIFOF"
rightIFOF_results_base <- models2df(rightIFOF_models_base)
rightIFOF_results_base$tract <- "rightIFOF"
leftILF_results_base <- models2df(leftILF_models_base)
leftILF_results_base$tract <- "leftILF"
rightILF_results_base <- models2df(rightILF_models_base)
rightILF_results_base$tract <- "rightILF"

pre_alongtract_mod_res <-
  rbind(leftAF_results_base, rightAF_results_base, leftSLF_results_base,
        rightSLF_results_base, leftIFOF_results_base, rightIFOF_results_base,
        leftILF_results_base, rightILF_results_base)

# Write model outputs --------------------------------------------------------------

pre_avg_mod_list =list(m.laf_pre, m.raf_pre, m.lslf_pre, m.rslf_pre,
                       m.lifof_pre, m.rifof_pre, m.lilf_pre, m.rilf_pre)
saveRDS(pre_avg_mod_list, here("data","models","pre_avg_mod.RDS"))

pre_alongtract_mod_list =list(leftAF_models_base, rightAF_models_base,
                              leftSLF_models_base, rightSLF_models_base,
                              leftIFOF_models_base, rightIFOF_models_base,
                              leftILF_models_base, rightILF_models_base)
saveRDS(pre_alongtract_mod_list, here("data","models","pre_alongtract_mod.RDS"))

fwrite(pre_alongtract_mod_res, file=here("data","models","pre_alongtract_mod_res.csv"),
       col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")

