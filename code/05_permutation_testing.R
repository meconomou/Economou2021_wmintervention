# Description -------------------------------------------------------------

# This script provides code to reproduce the permutation testing approach that
# was used to correct for multiple comparisons (Nichols and Holmes 2002).
# These functions take several hours to run so the output files are already
# provided and can be loaded to look at the results
# Jump to section 'Permutation results' to load the output files

source(here::here("code","01_load_data.R"))

# Baseline analysis (risk-related effects) --------------------------------

set.seed(007)
start_time <- Sys.time()
list_dfs <- list(leftAF_cast, rightAF_cast, leftSLF_cast,
                 rightSLF_cast, leftIFOF_cast, rightIFOF_cast,
                 leftILF_cast, rightILF_cast)

boot1k_relabeled <- lapply(list_dfs, function(tracts) {
  nBoots<- 1000 # number of repetitions
  bootResult<-list() # create empty list of results
  for (i in seq_len(nBoots)){
    bootResult[[i]] <- tracts %>% filter(time %in% "ses-01") %>%
      mutate(random = sample(c(0,1), replace=TRUE, size=79)) # store dataframe in list
  }
  models <- lapply(bootResult, function(datasims) { # run all models
    lapply(nodelist, function(x) {
      lm(substitute(i ~ random + mean_RMSt, list(i=as.name(x))),
         data=datasims)
    })
  })

  simresults <- lapply(models, models2df) # apply tidy function
  simresults_df <- lapply(seq(simresults), # combine into single df
                          function(x) "[[<-"(simresults[[x]], paste0("simrun"), value = x)) %>%
    bind_rows()

})

boot1k_relabeled_df <- lapply(seq(boot1k_relabeled),
                              function(x) "[[<-"(boot1k_relabeled[[x]], "tract", value = x)) %>%
  bind_rows()

fwrite(boot1k_relabeled_df, file=here("data","models","boot1k_pre.csv"),
       col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")

end_time <- Sys.time()
end_time - start_time



# Longitudinal analysis (intervention effects) ----------------------------

set.seed(007)
start_time <- Sys.time()
list_dfs <- list(leftAF_cast, rightAF_cast, leftSLF_cast,
                 rightSLF_cast, leftIFOF_cast, rightIFOF_cast,
                 leftILF_cast, rightILF_cast)

# system.time(res4 <- pblapply(1:B, function(i) fun(bid[,i])))

boot1k_relabeled_prepost <- pblapply(list_dfs, function(tracts) {
  nBoots <- 1000 # number of repetitions
  bootResult<-list() # create empty list of results
  for (i in seq_len(nBoots)){
    idOnly <- tracts %>%
      select(id, time, mean_RMSt) %>%
      mutate(time=recode_factor(time, `ses-01` = "pre", `ses-02` = "post")) %>%
      pivot_wider(names_from=time, values_from=mean_RMSt) %>%
      mutate(intervention=as.factor(sample(1:3, replace=TRUE, size=nrow(.)))) %>%
      select(id, intervention)
    dat <- tracts %>% select(-c(intervention))
    # current_time <- Sys.time()
    # message(paste0(current_time, " Working on simrun ", i))
    bootResult[[i]] <- merge(idOnly, dat, by.y="id") # store dataframe in list
  }
  models <- lapply(bootResult, function(datasims) { # run all models
    lapply(nodelist, function(x) {
      lmer(substitute(i ~ time*intervention + mean_RMSt + (1|id),
                      list(i=as.name(x))), data=datasims)

    })
  })

  simresults <- lapply(models, lmer2df) # apply tidy function
  simresults_df <- lapply(seq(simresults), # combine into single df
                          function(x) "[[<-"(simresults[[x]], paste0("simrun"),
                                             value = x)) %>% bind_rows()

})

boot1k_relabeled_prepost_df <- lapply(seq(boot1k_relabeled_prepost),
                                      function(x) "[[<-"(boot1k_relabeled_prepost[[x]], "tract",
                                                         value = x)) %>% bind_rows() %>%
  mutate(warning = as.factor(ifelse(is.na(message), "no","yes")))

# 100 iterations take 2 hours

fwrite(boot1k_relabeled_prepost_df, file=here("data","models","boot1k_prepost.csv"),
       col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")

end_time <- Sys.time()
end_time - start_time

# boot1k_relabeled_prepost <-
#   fread(here::here("data","models","boot1k_relabeled_prepost.csv"),
#         header=T, dec=".", sep=",", na="NA")



# Permutation results -----------------------------------------------------

# Load permutation analysis (assuming the code above has already been run)
boot1k_pre <-
  fread(here::here("data","models","boot1k_pre.csv"), header=T, dec=".", sep=",", na="NA")

boot1k_prepost <-
  fread(here::here("data","models","boot1k_prepost.csv"), header=T, dec=".", sep=",", na="NA")

alpha = 0.05 # set alpha level
nperm = 1000 # set number of permutations
cThresh = alpha # set cluster level threshold (here, same as alpha)

# Alpha thresholds for baseline analysis (risk effect)
alphaFWE_pre <- boot1k_pre %>%
  mutate(tract = case_when(tract == 1 ~ "LeftAF",
                           tract == 2 ~ "RightAF",
                           tract == 3 ~ "LeftSLF",
                           tract == 4 ~ "RightSLF",
                           tract == 5 ~ "LeftIFOF",
                           tract == 6 ~ "RightIFOF",
                           tract == 7 ~ "LeftILF",
                           tract == 8 ~ "RightILF")) %>%
  filter(variable %in% "random") %>%
  arrange(tract, simrun, node) %>%
  group_by(tract, simrun) %>%
  summarize(alphaFWE = min(pval)) %>% # find the lowest p-value per tract and per simrun (across all 98 nodes)
  ungroup() %>%
  group_by(tract) %>% # regroup by tract only
  arrange(tract, alphaFWE) %>% # sort by ascending order (lowest p-value first) while keeping tract grouping
  slice(round(alpha*nperm)) %>%
  mutate(alphaFWE = round(alphaFWE, digits=4)) %>%
  select(-c(simrun))

# Cluster thresholds for baseline analysis (risk effect)

clusFWE_pre <- boot1k_pre %>%
  mutate(tract = case_when(tract == 1 ~ "LeftAF",
                           tract == 2 ~ "RightAF",
                           tract == 3 ~ "LeftSLF",
                           tract == 4 ~ "RightSLF",
                           tract == 5 ~ "LeftIFOF",
                           tract == 6 ~ "RightIFOF",
                           tract == 7 ~ "LeftILF",
                           tract == 8 ~ "RightILF")) %>%
  filter(variable %in% "random") %>%
  select(node, pval, simrun, tract) %>%
  arrange(tract, simrun, node) %>%
  mutate(p05 = case_when(pval >= 0.05 ~ 0,
                         pval < 0.05 ~ 1)) %>%
  group_by(tract, simrun) %>%
  mutate(clusSiz = rep(rle(p05)$lengths, rle(p05)$lengths)) %>%
  filter(p05 %in% "1") %>%
  dplyr::summarize(clusterFWE = max(clusSiz)) %>%
  ungroup() %>%
  group_by(tract) %>%
  arrange(tract, desc(clusterFWE)) %>%
  slice(round(alpha*nperm)) %>%
  select(-c(simrun))

# Alpha thresholds for the prepost analysis (effect of session)

alphaFWE_time <- boot1k_prepost %>%
  mutate(tract = case_when(tract == 1 ~ "LeftAF",
                           tract == 2 ~ "RightAF",
                           tract == 3 ~ "LeftSLF",
                           tract == 4 ~ "RightSLF",
                           tract == 5 ~ "LeftIFOF",
                           tract == 6 ~ "RightIFOF",
                           tract == 7 ~ "LeftILF",
                           tract == 8 ~ "RightILF")) %>%
  filter(variable %in% "time") %>%
  arrange(tract, simrun, node) %>%
  group_by(tract, simrun) %>%
  summarize(alphaFWE = min(pval)) %>%
  ungroup() %>%
  group_by(tract) %>%
  arrange(tract, alphaFWE) %>%
  slice(round(alpha*nperm)) %>%
  mutate(alphaFWE = round(alphaFWE, digits=4)) %>%
  select(-c(simrun))

# Cluster thresholds for the prepost analysis (effect of session)

clusFWE_time <- boot1k_prepost %>%
  mutate(tract = case_when(tract == 1 ~ "LeftAF",
                           tract == 2 ~ "RightAF",
                           tract == 3 ~ "LeftSLF",
                           tract == 4 ~ "RightSLF",
                           tract == 5 ~ "LeftIFOF",
                           tract == 6 ~ "RightIFOF",
                           tract == 7 ~ "LeftILF",
                           tract == 8 ~ "RightILF")) %>%
  filter(variable %in% "time") %>%
  select(node, pval, simrun, tract) %>%
  arrange(tract, simrun, node) %>%
  mutate(p05 = case_when(pval >= 0.05 ~ 0,
                         pval < 0.05 ~ 1)) %>%
  group_by(tract, simrun) %>%
  mutate(clusSiz = rep(rle(p05)$lengths, rle(p05)$lengths)) %>%
  filter(p05 %in% "1") %>%
  dplyr::summarize(clusterFWE = max(clusSiz)) %>%
  ungroup() %>%
  group_by(tract) %>%
  arrange(tract, desc(clusterFWE)) %>%
  slice(round(alpha*nperm)) %>%
  select(-c(simrun))

# Alpha thresholds for the prepost analysis (group-by-session interaction effect)

alphaFWE_interaction <- boot1k_prepost %>%
  mutate(tract = case_when(tract == 1 ~ "LeftAF",
                           tract == 2 ~ "RightAF",
                           tract == 3 ~ "LeftSLF",
                           tract == 4 ~ "RightSLF",
                           tract == 5 ~ "LeftIFOF",
                           tract == 6 ~ "RightIFOF",
                           tract == 7 ~ "LeftILF",
                           tract == 8 ~ "RightILF")) %>%
  filter(variable %in% "timeintervention") %>%
  arrange(tract, simrun, node) %>%
  group_by(tract, simrun) %>%
  summarize(alphaFWE = min(pval)) %>%
  ungroup() %>%
  group_by(tract) %>%
  arrange(tract, alphaFWE) %>%
  slice(round(alpha*nperm)) %>%
  mutate(alphaFWE = round(alphaFWE, digits=4)) %>%
  select(-c(simrun))

# Cluster thresholds for the prepost analysis (group-by-session interaction effect)

clusFWE_interaction <- boot1k_prepost %>%
  mutate(tract = case_when(tract == 1 ~ "LeftAF",
                           tract == 2 ~ "RightAF",
                           tract == 3 ~ "LeftSLF",
                           tract == 4 ~ "RightSLF",
                           tract == 5 ~ "LeftIFOF",
                           tract == 6 ~ "RightIFOF",
                           tract == 7 ~ "LeftILF",
                           tract == 8 ~ "RightILF")) %>%
  filter(variable %in% "timeintervention") %>%
  select(node, pval, simrun, tract) %>%
  arrange(tract, simrun, node) %>%
  mutate(p05 = case_when(pval >= 0.05 ~ 0,
                         pval < 0.05 ~ 1)) %>%
  group_by(tract, simrun) %>%
  mutate(clusSiz = rep(rle(p05)$lengths, rle(p05)$lengths)) %>%
  filter(p05 %in% "1") %>%
  dplyr::summarize(clusterFWE = max(clusSiz)) %>%
  ungroup() %>%
  group_by(tract) %>%
  arrange(tract, desc(clusterFWE)) %>%
  slice(round(alpha*nperm)) %>%
  select(-c(simrun))


# Apply alpha thresholds to original results ------------------------------

# Load results from baseline along-tract models
pre_alongtract_mod_res <- fread(here::here("data","models", "pre_alongtract_mod_res.csv"), header=T, dec=".", sep=",", na="NA")
pre_alongtract_mod_res <- pre_alongtract_mod_res %>%
  mutate(tract = case_when(tract == "leftAF" ~ "LeftAF",
                           tract == "rightAF" ~ "RightAF",
                           tract == "leftSLF" ~ "LeftSLF",
                           tract == "rightSLF" ~ "RightSLF",
                           tract == "leftIFOF" ~ "LeftIFOF",
                           tract == "rightIFOF" ~ "RightIFOF",
                           tract == "leftILF" ~ "LeftILF",
                           tract == "rightILF" ~ "RightILF"))

# Load results from prepost along-tract models
prepost_alongtract_mod_res <-
  fread(here::here("data","models", "prepost_alongtract_mod_res.csv"),
        header=T, dec=".", sep=",", na="NA")

prepost_alongtract_mod_res <- prepost_alongtract_mod_res %>%
  mutate(tract = case_when(tract == "leftAF" ~ "LeftAF",
                           tract == "rightAF" ~ "RightAF",
                           tract == "leftSLF" ~ "LeftSLF",
                           tract == "rightSLF" ~ "RightSLF",
                           tract == "leftIFOF" ~ "LeftIFOF",
                           tract == "rightIFOF" ~ "RightIFOF",
                           tract == "leftILF" ~ "LeftILF",
                           tract == "rightILF" ~ "RightILF"))

## Get dataframes with corrected results

# baseline
alpha_corrected_results_pre <-
  merge(pre_alongtract_mod_res, alphaFWE_pre, by="tract") %>%
  filter(variable %in% "risk") %>%
  mutate(survives = case_when(pval < alphaFWE ~ "yes",
                              pval > alphaFWE ~ "no")) %>%
  filter(survives %in% "yes") %>%
  arrange(tract, node)

# effect of session
alpha_corrected_results_session <-
  merge(prepost_alongtract_mod_res, alphaFWE_time, by="tract") %>%
  filter(variable %in% "time") %>%
  mutate(survives = case_when(pval < alphaFWE ~ "yes",
                              pval > alphaFWE ~ "no")) %>%
  filter(survives %in% "yes") %>%
  arrange(tract, node)

# group-by-session interaction
alpha_corrected_results_interaction <-
  merge(prepost_alongtract_mod_res, alphaFWE_interaction, by="tract") %>%
  filter(variable %in% "timeintervention") %>%
  mutate(survives = case_when(pval < alphaFWE ~ "yes",
                              pval > alphaFWE ~ "no")) %>%
  filter(survives %in% "yes") %>%
  arrange(tract, node)

# Apply cluster thresholds to original results ----------------------------

## Get dataframes with corrected results

# baseline
cluster_corrected_results_pre <-
  merge(pre_alongtract_mod_res, clusFWE_pre, by="tract") %>%
  mutate_at("tract", factor) %>%
  filter(variable %in% "risk") %>%
  arrange(tract, node) %>%
  mutate(p05 = case_when(pval >= 0.05 ~ 0,
                         pval < 0.05 ~ 1)) %>%
  group_by(tract) %>%
  mutate(lag05= lag(p05)) %>%
  mutate(count = p05 + lag05) %>%
  mutate(lagcount = lag(count)) %>%
  mutate(cluster = case_when(count == 2 & lagcount == 1 ~ "yes")) %>%
  mutate(length = rep(rle(p05)$lengths, rle(p05)$lengths)) %>%
  ungroup() %>%
  filter(cluster %in% "yes") %>%
  mutate(survives = case_when(length > clusterFWE ~ "yes",
                              length ==  clusterFWE ~ "yes",
                              length < clusterFWE ~ "no")) %>%
  filter(survives %in% "yes") %>%
  arrange(tract, node)

# effect of session
cluster_corrected_results_session <-
  merge(prepost_alongtract_mod_res, clusFWE_time, by="tract") %>%
  mutate_at("tract", factor) %>%
  filter(variable %in% "time") %>%
  arrange(tract, node) %>%
  mutate(p05 = case_when(pval >= 0.05 ~ 0,
                         pval < 0.05 ~ 1)) %>%
  group_by(tract) %>%
  mutate(lag05= lag(p05)) %>%
  mutate(count = p05 + lag05) %>%
  mutate(lagcount = lag(count)) %>%
  mutate(cluster = case_when(count == 2 & lagcount == 1 ~ "yes")) %>%
  mutate(length = rep(rle(p05)$lengths, rle(p05)$lengths)) %>%
  ungroup() %>%
  filter(cluster %in% "yes") %>%
  mutate(survives = case_when(length > clusterFWE ~ "yes",
                              length == clusterFWE ~ "yes",
                              length < clusterFWE ~ "no")) %>%
  filter(survives %in% "yes") %>%
  arrange(tract, node)

# group-by-session interaction
cluster_corrected_results_interaction <-
  merge(prepost_alongtract_mod_res, clusFWE_interaction, by="tract") %>%
  mutate_at("tract", factor) %>%
  filter(variable %in% "timeintervention") %>%
  arrange(tract, node) %>%
  mutate(p05 = case_when(pval >= 0.05 ~ 0,
                         pval < 0.05 ~ 1)) %>%
  group_by(tract) %>%
  mutate(lag05= lag(p05)) %>%
  mutate(count = p05 + lag05) %>%
  mutate(lagcount = lag(count)) %>%
  mutate(cluster = case_when(count == 2 & lagcount == 1 ~ "yes")) %>%
  mutate(length = rep(rle(p05)$lengths, rle(p05)$lengths)) %>%
  ungroup() %>%
  filter(cluster %in% "yes") %>%
  mutate(survives = case_when(length > clusterFWE ~ "yes",
                              length == clusterFWE ~ "yes",
                              length < clusterFWE ~ "no")) %>%
  filter(survives %in% "yes") %>%
  arrange(tract, node)


# Small data --------------------------------------------------------------

alphaFWE_pre_min <- min(alphaFWE_pre$alphaFWE)
alphaFWE_pre_max <- max(alphaFWE_pre$alphaFWE)

clusFWE_pre_min <- min(clusFWE_pre$clusterFWE)
clusFWE_pre_max <- max(clusFWE_pre$clusterFWE)

alphaFWE_time_min <- min(alphaFWE_time$alphaFWE)
alphaFWE_time_max <- max(alphaFWE_time$alphaFWE)

clusFWE_time_min <- min(clusFWE_time$clusterFWE)
clusFWE_time_max <- max(clusFWE_time$clusterFWE)

alphaFWE_interaction_min <- min(alphaFWE_interaction$alphaFWE)
alphaFWE_interaction_max <- max(alphaFWE_interaction$alphaFWE)

clusFWE_interaction_min <- min(clusFWE_interaction$clusterFWE)
clusFWE_interaction_max <- max(clusFWE_interaction$clusterFWE)

save(alphaFWE_pre_min, alphaFWE_pre_max, clusFWE_pre_min, clusFWE_pre_max,
     alphaFWE_time_min, alphaFWE_time_max, clusFWE_time_min, clusFWE_time_max,
     alphaFWE_interaction_min, alphaFWE_interaction_max, clusFWE_interaction_min,
     clusFWE_interaction_max, file=here("data","models","thresholds_multicompcorr.RData"))

