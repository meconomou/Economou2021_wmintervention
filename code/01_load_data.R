# Load libraries --------------------------------------------------------------

# Assuming these packages are already locally installed

library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
library(plotrix);
library(readr)
library(rstatix)
library(lubridate)
library(english)
library(png)
library(tiff)
library(grid)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(cowplot)
library(gridExtra)
library(lme4)
library(lmerTest)
library(ggeffects)
library(phia)
library(knitr)
library(kableExtra)
library(gt)
library(effects)
library(emmeans)
library(gtsummary)
library(here)
library(papaja)
library(pbapply)
library(ggtext)
library(metafor)


# Helper functions --------------------------------------------------------

mergeId       <- function(x, y){
  df            <- merge(x, y, by="id", all=T)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}

mergeIdTime       <- function(x, y){
  df            <- merge(x, y, by=c("id","time"), all.x=T)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

regexp_digit <- "[[:digit:]]+"
models2df <- function(m){
  dat<-do.call(rbind, lapply(m, anova))
  dat<-as.data.frame(dat)
  dat<-setDT(dat, keep.rownames = "variable")[]
  dat$variable<-gsub(":","", dat$variable)
  dat <- dat %>%
    mutate(model=str_extract(dat$variable, regexp_digit)) %>%
    mutate_at("model", as.numeric) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate_if(is.character,~factor(.)) %>%
    mutate(model=model + 1) %>%
    mutate_at("model", factor) %>%
    mutate(variable=str_remove(dat$variable, regexp_digit)) %>%
    dplyr::select(model, `F value`, `Pr(>F)`, variable)
  dat$model <- as.numeric(dat$model)
  dat<-merge(link, dat, by.y="model", all=T)
  dat <- dat %>%
    dplyr::rename(fstat=`F value`) %>% dplyr::rename(pval=`Pr(>F)`) %>%
    dplyr::rename(node=nodelist) %>%
    mutate(node=gsub("n","", node)) %>%
    mutate_at("node", as.numeric)
}

lmer2df <- function(m){
  summaries <- lapply(m, summary)
  messages <- list()
  for (i in 1:length(summaries)) {
    messages[[i]] <- toString(summaries[[i]]$optinfo$conv$lme4$messages)
  }
  messages_df <- as.data.frame(do.call(rbind, messages))
  messages_df <- messages_df %>%
    rename(message = V1) %>%
    mutate(model = 1:nrow(messages_df)) %>%
    mutate_all(na_if,"")
  dat<-do.call(rbind, lapply(m, anova))
  dat<-as.data.frame(dat)
  dat<-setDT(dat, keep.rownames = "variable")[]
  dat$variable<-gsub(":","", dat$variable)
  dat <- dat %>%
    mutate(model=str_extract(dat$variable, regexp_digit)) %>%
    mutate_at("model", as.numeric) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate_if(is.character,~factor(.)) %>%
    mutate(model=model + 1) %>%
    mutate_at("model", factor) %>%
    mutate(variable=str_remove(dat$variable, regexp_digit)) %>%
    dplyr::select(model, `F value`, `Pr(>F)`, variable)
  dat$model <- as.numeric(dat$model)
  dat<-merge(link, dat, by.y="model", all=T)
  dat <- dat %>%
    dplyr::rename(fstat=`F value`) %>% dplyr::rename(pval=`Pr(>F)`) %>%
    dplyr::rename(node=nodelist) %>%
    mutate(node=gsub("n","", node)) %>%
    mutate_at("node", as.numeric)
  dat <- merge(dat, messages_df, by.y="model")
}

models2es <- function(m){
  dat<-do.call(rbind, lapply(lapply(m, anova), effectsize::cohens_f))
  dat<-as.data.frame(dat)
  dat$Parameter<-gsub(":","", dat$Parameter)
  dat <- dat %>%
    mutate(model=rep(seq(2:99), each=4)) %>%
    # mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate_if(is.character,~factor(.)) %>%
    mutate_at("model", factor)
  dat$model <- as.numeric(dat$model)
  dat<-merge(link, dat, by.y="model", all=T)
  dat <- dat %>%
    dplyr::rename(node=nodelist) %>%
    mutate(node=gsub("n","", node)) %>%
    mutate_at("node", as.numeric)
}

# Load data --------------------------------------------------------------

subject_data <- fread(here("data","tidy","subject_data.csv"), header=T, dec=".", sep=",", na="NA")
subject_data <- subject_data %>%
  mutate_if(is.character, ~factor(.)) %>%
  mutate_at("M_SES", factor) %>%
  mutate_at("FR", factor)

fa_long <- fread(here("data","tidy","fa_long.csv"), header=T, dec=".", sep=",", na="NA")
fa_long_noExcl <- fread(here("data","tidy","fa_long_noExcl.csv"), header=T, dec=".", sep=",", na="NA")

fa_long$intervention <- factor(fa_long$intervention, levels = c("ActiveControl", "GG_NE", "CONTROL"))

meantracts_long <- fread(here("data","tidy","meantracts_long.csv"), header=T, dec=".", sep=",", na="NA")
meantracts_wide <- fread(here("data","tidy","meantracts_wide.csv"), header=T, dec=".", sep=",", na="NA")
meanprofiles <- fread(here("data","tidy","meanprofiles.csv"), header=T, dec=".", sep=",", na="NA")
riskprofiles <- fread(here("data","tidy","riskprofiles.csv"), header=T, dec=".", sep=",", na="NA")

#  Prepare for along-tract analyses --------------------------------------------------------------

leftAF_cast <- fa_long %>%
  filter(tract %in% "Left_Arcuate") %>%
  arrange(node) %>%
  mutate(nodeF=as.factor(paste0("n", node))) %>%
  pivot_wider(id_cols=c(id, intervention, risk, time, mean_RMSt, ageM), values_from=fa, names_from=nodeF)

rightAF_cast <- fa_long %>%
  filter(tract %in% "Right_Arcuate") %>%
  arrange(node) %>%
  mutate(nodeF=as.factor(paste0("n", node))) %>%
  pivot_wider(id_cols=c(id, intervention, risk, time, mean_RMSt, ageM), values_from=fa, names_from=nodeF)

leftIFOF_cast <- fa_long %>%
  filter(tract %in% "Left_IFOF") %>%
  arrange(node) %>%
  mutate(nodeF=as.factor(paste0("n", node))) %>%
  pivot_wider(id_cols=c(id, intervention, risk, time, mean_RMSt, ageM), values_from=fa, names_from=nodeF)

rightIFOF_cast <- fa_long %>%
  filter(tract %in% "Right_IFOF") %>%
  arrange(node) %>%
  mutate(nodeF=as.factor(paste0("n", node))) %>%
  pivot_wider(id_cols=c(id, intervention, risk, time, mean_RMSt, ageM), values_from=fa, names_from=nodeF)

leftILF_cast <- fa_long %>%
  filter(tract %in% "Left_ILF") %>%
  arrange(node) %>%
  mutate(nodeF=as.factor(paste0("n", node))) %>%
  pivot_wider(id_cols=c(id, intervention, risk, time, mean_RMSt, ageM), values_from=fa, names_from=nodeF)

rightILF_cast <- fa_long %>%
  filter(tract %in% "Right_ILF") %>%
  arrange(node) %>%
  mutate(nodeF=as.factor(paste0("n", node))) %>%
  pivot_wider(id_cols=c(id, intervention, risk, time, mean_RMSt, ageM), values_from=fa, names_from=nodeF)

leftSLF_cast <- fa_long %>%
  filter(tract %in% "Left_SLF") %>%
  arrange(node) %>%
  mutate(nodeF=as.factor(paste0("n", node))) %>%
  pivot_wider(id_cols=c(id, intervention, risk, time, mean_RMSt, ageM), values_from=fa, names_from=nodeF)

rightSLF_cast <- fa_long %>%
  filter(tract %in% "Right_SLF") %>%
  arrange(node) %>%
  mutate(nodeF=as.factor(paste0("n", node))) %>%
  pivot_wider(id_cols=c(id, intervention, risk, time, mean_RMSt, ageM), values_from=fa, names_from=nodeF)

# create node list and model-node link

nodelist <- leftAF_cast %>% dplyr::select(n2:n99) %>% colnames()
link<-as.data.frame(nodelist)
link <- link %>% mutate(model=1:98) %>% mutate_at("model", factor)
