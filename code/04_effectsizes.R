# Description -------------------------------------------------------------

# This scripts computes effect sizes
# (Cohen's d for baseline risk-related effects and Cohen's f for longitudinal intervention effects)


# Load data
source(here::here("code","01_load_data.R"))

# Load models
pre_avg_mod_list <- readRDS(here("data","models", "pre_avg_mod.RDS"))
pre_alongtract_mod_list <- readRDS(here("data","models", "pre_alongtract_mod.RDS"))
prepost_avg_mod_list <- readRDS(here("data","models", "prepost_avg_mod.RDS"))
prepost_alongtract_mod_list <- readRDS(here("data","models", "prepost_alongtract_mod.RDS"))


# Baseline effect sizes (tract average) -----------------------------------

pre_avg_effsizes <- meantracts_wide %>% group_by(tract) %>% rstatix::cohens_d(mean_fa_pre ~ risk, ci=T)

fwrite(pre_avg_effsizes, file=here("data","models","pre_avg_effsizes.csv"),
       col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")


# Along-tract Cohen's d ---------------------------------------------------

## takes several minutes to compute!!

# es_pre_nodes <- fa_long %>%
#   filter(tract %in% c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
#                       "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF")) %>%
#   select(id, time, risk, intervention, tract, node, fa) %>%
#   mutate(time=recode_factor(time, "ses-01"="pre","ses-02"="post")) %>%
#   pivot_wider(id_cols=c(id, intervention, risk, node, tract), values_from=fa, names_from=time) %>%
#   group_by(node, tract) %>%
#   rstatix::cohens_d(pre ~ risk, ci=T)

# fwrite(es_pre_nodes, file=here("data","models","es_pre_nodes.csv"),
#        col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")

# Along-tract Cohen's f ---------------------------------------------------

leftAF_es_results <- models2es(prepost_alongtract_mod_list[[1]])
leftAF_es_results$tract <- "Left_Arcuate"
rightAF_es_results <- models2es(prepost_alongtract_mod_list[[2]])
rightAF_es_results$tract <- "Right_Arcuate"
leftSLF_es_results <- models2es(prepost_alongtract_mod_list[[3]])
leftSLF_es_results$tract <- "Left_SLF"
rightSLF_es_results <- models2es(prepost_alongtract_mod_list[[4]])
rightSLF_es_results$tract <- "Right_SLF"
leftIFOF_es_results <- models2es(prepost_alongtract_mod_list[[5]])
leftIFOF_es_results$tract <- "Left_IFOF"
rightIFOF_es_results <- models2es(prepost_alongtract_mod_list[[6]])
rightIFOF_es_results$tract <- "Right_IFOF"
leftILF_es_results <- models2es(prepost_alongtract_mod_list[[7]])
leftILF_es_results$tract <- "Left_ILF"
rightILF_es_results <- models2es(prepost_alongtract_mod_list[[8]])
rightILF_es_results$tract <- "Right_ILF"

prepost_alongtract_mod_ES <-
  rbind(leftAF_es_results, rightAF_es_results, leftIFOF_es_results, rightIFOF_es_results,
        leftILF_es_results, rightILF_es_results, leftSLF_es_results, rightSLF_es_results)

fwrite(prepost_alongtract_mod_ES, file=here("data","models","es_prepost_nodes.csv"),
       col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")

