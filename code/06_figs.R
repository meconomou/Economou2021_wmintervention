# Description -------------------------------------------------------------

# Theme and formatting ----------------------------------------------------

# Load data
source(here::here("code","01_load_data.R"))

# Set formatting preferences

myColorsGroups<- brewer.pal(3,"Dark2")
myShapesGroups<- c(15,17,19)
names(myColorsGroups)<-c("GG_NE","ActiveControl","CONTROL")
names(myShapesGroups)<-c("GG_NE","ActiveControl","CONTROL")
groupcolor<-scale_colour_manual(name="Group",
                                values=myColorsGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                                labels=c("GraphoGame-Flemish","Active control","Typical control"))
groupfill<-scale_fill_manual(name="Group",
                             values=myColorsGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                             labels=c("GraphoGame-Flemish","Active control","Typical control"))
groupshape <- scale_shape_manual(name="Group",
                                 values=myShapesGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                                 labels=c("GraphoGame-Flemish","Active control","Typical control"))

greyGroups <- grey.colors(3, start = 0.2, end = 0.7)
names(greyGroups) <- c("GG_NE","ActiveControl","CONTROL")

myColorsTime<-brewer.pal(2, "Set2")
names(myColorsTime)<-levels(meanprofiles$time)
timecolor<-scale_colour_manual(name="Session",
                               values=myColorsTime, limits=c("ses-01", "ses-02"),
                               labels=c("pre-test","post-test"))
timefill<-scale_fill_manual(name="Session",
                            values=myColorsTime, limits=c("ses-01", "ses-02"),
                            labels=c("pre-test","post-test"))


myColorsRisk <- c("#1F78B4", "#33A02C")
# myColorsRisk<- brewer.pal(2,"Paired")
names(myColorsRisk)<-c("0","1")
riskcolor<-scale_colour_manual(name="Group",
                               values=myColorsRisk, limits=c("0", "1"),
                               labels=c("Typical control","At-risk"))
riskfill<-scale_fill_manual(name="Group",
                            values=myColorsRisk, limits=c("0", "1"),
                            labels=c("Typical control","At-risk"))
# Axes

time.labs <- c("pre-test", "post-test")
names(time.labs) <- c("ses-01", "ses-02")
group.labs <- c("GraphoGame-Flemish", "Active control", "Typical control")
names(group.labs) <- c("GG_NE", "ActiveControl", "CONTROL")
risk.labs <- c("Typical control", "At-risk")
names(risk.labs) <- c("0", "1")
tract.labs <- c("L AFdirect", "R AFdirect", "L AFanterior","R AFanterior",
                "L IFOF", "R IFOF", "L ILF", "R ILF")
names(tract.labs) <- c("Left_Arcuate", "Right_Arcuate", "Left_SLF",
                       "Right_SLF", "Left_IFOF", "Right_IFOF", "Left_ILF", "Right_ILF")

# Theme preferences
mytheme <- theme_cowplot() +
  theme(legend.position="right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

jitter <- position_jitter(width=0.05, seed=123)
dodge <- position_dodge(width = 0.4)


# Import image files with rendered tract reconstructions ------------------

laf_img <- readTIFF(here("figs", "tract_img","laf_alpha07_cut.tiff"))
raf_n11n13_img <- readTIFF(here("figs", "tract_img","raf_n11n13_alpha07_cut.tiff"))
lslf_img <- readTIFF(here("figs", "tract_img","lslf_alpha07_cut.tiff"))
rslf_img <- readTIFF(here("figs", "tract_img","rslf_alpha07_cut.tiff"))
lifof_img <- readTIFF(here("figs", "tract_img","lifof_alpha07_cut.tiff"))
rifof_n64n65_img <- readTIFF(here("figs", "tract_img","rifof_n64n65_alpha07_cut.tiff"))
lilf_img <- readTIFF(here("figs", "tract_img","lilf_alpha07_cut.tiff"))
rilf_n92n93_img <- readTIFF(here("figs", "tract_img","rilf_n92n93_alpha07_cut.tiff"))


# Figure 2 ----------------------------------------------------------------

p.risk_laf<-riskprofiles %>%
  mutate_at("risk", factor) %>%
  filter(tract %in% "Left_Arcuate") %>%
  filter(time %in% "ses-01") %>%
  ggplot(aes(x=node, y=mean_fa, ymin=mean_fa-se, ymax=mean_fa+se, group=risk)) +
  geom_line(aes(group=risk, color=risk), size=1) +
  geom_ribbon(alpha=0.3, aes(group=risk, fill=risk)) +
  mytheme + riskfill + riskcolor + labs(y="Mean FA", x="Node") +
  theme(legend.position="none") +
  facet_wrap(~tract, ncol=4, labeller=labeller(tract=tract.labs)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11), plot.title=element_text(size=14),
        strip.text = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size=2))

p.risk_lslf<-riskprofiles %>%
  mutate_at("risk", factor) %>%
  filter(tract %in% "Left_SLF") %>%
  filter(time %in% "ses-01") %>%
  mutate_if(is.character, ~factor(.)) %>%
  group_by(risk) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  ungroup() %>%
  ggplot(aes(x=nodeAP, y=mean_fa, ymin=mean_fa-se, ymax=mean_fa+se, group=risk)) +
  geom_line(aes(group=risk, color=risk), size=1) +
  geom_ribbon(alpha=0.3, aes(group=risk, fill=risk)) +
  mytheme + riskfill + riskcolor + labs(y="Mean FA", x="Node") +
  theme(legend.position="none") +
  facet_wrap(~tract, ncol=4, labeller=labeller(tract=tract.labs)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11), plot.title=element_text(size=14),
        strip.text = element_blank())

p.risk_lifof <- riskprofiles %>%
  mutate_at("risk", factor) %>%
  filter(tract %in% "Left_IFOF") %>%
  filter(time %in% "ses-01") %>%
  mutate_if(is.character, ~factor(.)) %>%
  group_by(risk) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  ungroup() %>%
  ggplot(aes(x=nodeAP, y=mean_fa, ymin=mean_fa-se, ymax=mean_fa+se, group=risk)) +
  geom_line(aes(group=risk, color=risk), size=1) +
  geom_ribbon(alpha=0.3, aes(group=risk, fill=risk)) +
  mytheme + riskfill + riskcolor + labs(y="Mean FA", x="Node") +
  theme(legend.position="none") +
  facet_wrap(~tract, ncol=4, labeller=labeller(tract=tract.labs)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11), plot.title=element_text(size=14),
        strip.text = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size=2))

p.risk_lilf<-riskprofiles %>%
  mutate_at("risk", factor) %>%
  filter(tract %in% "Left_ILF") %>%
  filter(time %in% "ses-01") %>%
  mutate_if(is.character, ~factor(.)) %>%
  group_by(risk) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  ungroup() %>%
  ggplot(aes(x=nodeAP, y=mean_fa, ymin=mean_fa-se, ymax=mean_fa+se, group=risk)) +
  geom_line(aes(group=risk, color=risk), size=1) +
  geom_ribbon(alpha=0.3, aes(group=risk, fill=risk)) +
  mytheme + riskfill + riskcolor + labs(y="Mean FA", x="Node") +
  theme(legend.position="none") +
  facet_wrap(~tract, ncol=4, labeller=labeller(tract=tract.labs)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11), plot.title=element_text(size=14),
        strip.text = element_blank())

p.risk_raf<-riskprofiles %>%
  mutate_at("risk", factor) %>%
  filter(tract %in% "Right_Arcuate") %>%
  filter(time %in% "ses-01") %>%
  ggplot(aes(x=node, y=mean_fa, ymin=mean_fa-se, ymax=mean_fa+se, group=risk)) +
  geom_rect(fill="gray", alpha=0.1, aes(xmin=11, xmax=13, ymin=-Inf, ymax=max(mean_fa))) +
  geom_line(aes(group=risk, color=risk), size=1) +
  geom_ribbon(alpha=0.3, aes(group=risk, fill=risk)) +
  mytheme + riskfill + riskcolor + labs(y="Mean FA", x="Node") +
  theme(legend.position="none") +
  facet_wrap(~tract, ncol=4, labeller=labeller(tract=tract.labs)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11), plot.title=element_text(size=14),
        strip.text = element_blank())


p.risk_rslf<-riskprofiles %>%
  mutate_at("risk", factor) %>%
  filter(tract %in% "Right_SLF") %>%
  filter(time %in% "ses-01") %>%
  mutate_if(is.character, ~factor(.)) %>%
  group_by(risk) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  ungroup() %>%
  ggplot(aes(x=nodeAP, y=mean_fa, ymin=mean_fa-se, ymax=mean_fa+se, group=risk)) +
  geom_line(aes(group=risk, color=risk), size=1) +
  geom_ribbon(alpha=0.3, aes(group=risk, fill=risk)) +
  mytheme + riskfill + riskcolor + labs(y="Mean FA", x="Node") +
  theme(legend.position="none") +
  facet_wrap(~tract, ncol=4, labeller=labeller(tract=tract.labs)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11), plot.title=element_text(size=14),
        strip.text = element_blank())

p.risk_rifof<- riskprofiles %>%
  mutate_at("risk", factor) %>%
  filter(tract %in% "Right_IFOF") %>%
  filter(time %in% "ses-01") %>%
  mutate_if(is.character, ~factor(.)) %>%
  group_by(risk) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  ungroup() %>%
  ggplot(aes(x=nodeAP, y=mean_fa, ymin=mean_fa-se, ymax=mean_fa+se, group=risk)) +
  geom_rect(fill="gray", alpha=0.1, aes(xmin=36, xmax=37, ymin=-Inf, ymax=max(mean_fa))) +
  geom_line(aes(group=risk, color=risk), size=1) +
  geom_ribbon(alpha=0.3, aes(group=risk, fill=risk)) +
  mytheme + riskfill + riskcolor + labs(y="Mean FA", x="Node") +
  theme(legend.position="none") +
  facet_wrap(~tract, ncol=4, labeller=labeller(tract=tract.labs)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11), plot.title=element_text(size=14),
        strip.text = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size=2))

p.risk_rilf<-riskprofiles %>%
  mutate_at("risk", factor) %>%
  filter(tract %in% "Right_ILF") %>%
  filter(time %in% "ses-01") %>%
  mutate_if(is.character, ~factor(.)) %>%
  group_by(risk) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  ungroup() %>%
  ggplot(aes(x=nodeAP, y=mean_fa, ymin=mean_fa-se, ymax=mean_fa+se, group=risk)) +
  geom_rect(fill="gray", alpha=0.1, aes(xmin=8, xmax=9, ymin=-Inf, ymax=max(mean_fa))) +
  geom_line(aes(group=risk, color=risk), size=1) +
  geom_ribbon(alpha=0.3, aes(group=risk, fill=risk)) +
  mytheme + riskfill + riskcolor + labs(y="Mean FA", x="Node") +
  theme(legend.position="none") +
  facet_wrap(~tract, ncol=4, labeller=labeller(tract=tract.labs)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11), plot.title=element_text(size=14),
        strip.text = element_blank())

risk_profiles_af_plot <- ggarrange(p.risk_laf, p.risk_raf, ncol=2, nrow=1) +
  theme(plot.margin=unit(c(0,0,1,0),"cm"))

risk_profiles_slf_plot <- ggarrange(p.risk_lslf, p.risk_rslf, ncol=2, nrow=1) +
  theme(plot.margin=unit(c(0,0,1,0),"cm"))

risk_profiles_ifof_plot <- ggarrange(p.risk_lifof, p.risk_rifof, ncol=2, nrow=1) +
  theme(plot.margin=unit(c(0,0,1,0),"cm"))

risk_profiles_ilf_plot <- ggarrange(p.risk_lilf, p.risk_rilf, ncol=2, nrow=1) +
  theme(plot.margin=unit(c(0,0,1,0),"cm"))

risk_legend_img <-readPNG(here("figs", "main","risk_legend.png"))

p.all_risk_profiles <- ggarrange(risk_profiles_af_plot, risk_profiles_slf_plot,
                                 risk_profiles_ifof_plot, risk_profiles_ilf_plot,
                                 ncol=1, nrow=4) +
  theme(plot.margin=unit(c(1,4.5,0.5,4.5),"cm"))

p.all_risk_profiles_img <- ggdraw() +
  draw_image(laf_img,  x = -0.38, y = 0.39, scale = 0.2) +
  draw_image(raf_n11n13_img,  x = 0.38, y = 0.39, scale = .2) +
  draw_image(lslf_img,  x = -0.38, y = 0.16, scale = .2) +
  draw_image(rslf_img,  x = 0.38, y = 0.16, scale = .2) +
  draw_image(lifof_img,  x = -0.38, y = -0.08, scale = .2) +
  draw_image(rifof_n64n65_img,  x = 0.38, y = -0.08, scale = .2) +
  draw_image(lilf_img,  x = -0.38, y = -0.31, scale = .2) +
  draw_image(rilf_n92n93_img,  x = 0.38, y = -0.31, scale = .2) +
  draw_label("Left", size = 11, x = 0.12, y = 0.98) +
  draw_label("Right", size = 11, x = 0.88, y = 0.98) +
  draw_label("Left", size = 11, x = 0.12, y = 0.75) +
  draw_label("Right", size = 11, x = 0.88, y = 0.75) +
  draw_label("Left", size = 11, x = 0.12, y = 0.51) +
  draw_label("Right", size = 11, x = 0.88, y = 0.51) +
  draw_label("Left", size = 11, x = 0.12, y = 0.28) +
  draw_label("Right", size = 11, x = 0.88, y = 0.28) +
  draw_label("AFdirect", size = 14, x = 0.52, y = 0.98, fontface = "bold") +
  draw_label("AFanterior", size = 14, x = 0.52, y = 0.75, fontface = "bold") +
  draw_label("IFOF", size = 14, x = 0.53, y = 0.51, fontface = "bold") +
  draw_label("ILF", size = 14, x = 0.52, y = 0.28, fontface = "bold") +
  draw_image(risk_legend_img,  x = 0.04, y = -0.47, scale=.4) +
  draw_plot(p.all_risk_profiles)


ggsave(filename=here("figs","main","Figure2.pdf"), plot=p.all_risk_profiles_img,
       height=190, width=170, units=c("mm"), dpi=300)
ggsave(filename=here("figs","main","Figure2.png"), plot=p.all_risk_profiles_img,
       height=190, width=170, units=c("mm"), dpi=300)
tiff(here("figs","main","Figure2.tiff"), units="mm", width=170, height=190, res=300)
p.all_risk_profiles_img
dev.off()


# Supplementary Figure 1 --------------------------------------------------

# load pre-test effect sizes
es_pre_nodes <- fread(here::here("data","models","es_pre_nodes.csv"), header=T, dec=".", sep=",", na="NA")

# get data in anterior-posterior direction for plotting
es_pre_nodes_arcuate <- es_pre_nodes %>%
  filter(tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  pivot_wider(names_from=tract, values_from=effsize, id_cols=node)

es_pre_nodes_othertracts <- es_pre_nodes %>%
  filter(!tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  pivot_wider(names_from=tract, values_from=effsize, id_cols=node) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  select(-c(node)) %>%
  rename(node=nodeAP)

es_pre_nodes_AP <- merge(es_pre_nodes_arcuate, es_pre_nodes_othertracts, by.y="node")
es_pre_nodes_AP <- es_pre_nodes_AP %>%
  pivot_longer(c(Left_Arcuate:Right_SLF), names_to="tract", values_to="effsize")

es_pre_nodes_AP$tract <- factor(es_pre_nodes_AP$tract,
                                levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                                           "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))

# create dataframe with values for shading risk-related differences
dat_rect <- data.frame(tract = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                                 "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"),
                       ymin = c(NA, -Inf, NA, NA, NA, -Inf, NA, -Inf),
                       ymax = c(NA, Inf, NA, NA, NA, Inf, NA, Inf),
                       xmin = c(NA, 11, NA, NA, NA, 37, NA, 8),
                       xmax = c(NA, 13, NA, NA, NA, 38, NA, 9))
dat_rect$tract <- as.factor(dat_rect$tract)

# plot
p_es_pre_nodes <- es_pre_nodes_AP %>%
  mutate(effsize=abs(effsize)) %>%
  ggplot() +
  geom_rect(data=dat_rect, fill="black", alpha=0.4, inherit.aes=F,
            aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  geom_line(aes(x=node, y=effsize)) +
  mytheme +
  geom_hline(aes(yintercept=0.2, linetype = "d = 0.2"), size=.5) +
  geom_hline(aes(yintercept=0.5, linetype = "d = 0.5"), size=.5) +
  geom_hline(aes(yintercept=0.8, linetype = "d = 0.8"), size=.5) +
  stat_summary(fun = mean, aes(x = 1, y=effsize, yintercept = ..y.., linetype = "Average d"),
               geom = "hline", size=.5) +
  scale_linetype_manual(name="Legend", values = c(3,2,4,1),
                        limits=c("d = 0.8", "d = 0.5", "d = 0.2", "Average d"),
                        labels=c("*d* = 0.8","*d* = 0.5", "*d* = 0.2", "*d*<sub>average<sub>")) +
  labs(y="Cohen's *d*", title="", x="Node") +
  facet_wrap(~tract, labeller = labeller(tract=tract.labs), ncol=2, nrow=4) +
  theme(legend.position = "right",
        legend.text = element_markdown(),
        axis.title.y = element_markdown())

# Write output
ggsave(filename=here("figs","suppl","SupplementaryFigure1.pdf"), plot=p_es_pre_nodes,
       height=170, width=140, units=c("mm"))
tiff(here("figs","suppl","SupplementaryFigure1.tiff"), units="mm", height=170, width=140, res=300)
p_es_pre_nodes
dev.off()


# Supplementary Figure 2 --------------------------------------------------

dodge = position_dodge(width=.8)
p_indivchange <- meantracts_long %>%
  mutate(tract = factor(tract, levels=c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                                        "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))) %>%
  mutate(intervention = factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  ggplot(aes(x = time, y = mean_fa, colour = intervention, shape=intervention, fill=intervention)) +
  # geom_point(alpha=.1, position=dodge) +
  # geom_line(alpha=.2, aes(group=interaction(id, intervention)), position=dodge) +
  lemon::geom_pointline(aes(group=interaction(id, intervention)), position=dodge, distance=0, alpha=.11) +
  stat_summary(fun=mean, geom="point", size=1, aes(shape=intervention), position=dodge) +
  stat_summary(fun=mean, geom="line", size=1, aes(group=intervention), position=dodge) +
  stat_summary(fun.data=mean_cl_normal, geom="pointrange", size=1, position=dodge) +
  facet_wrap(~tract, labeller=labeller(tract=tract.labs), ncol=4, nrow=2) +
  mytheme +
  theme(legend.position="top") +
  groupcolor + groupfill + groupshape +
  scale_x_discrete(labels=c("pre-test","post-test")) +
  labs(x="Session",y="Tract average FA")

ggsave(filename=here("figs","suppl","p_indivchange.png"), plot=p_indivchange, height=190, width=170, units=c("mm"), dpi=300)
ggsave(filename=here("figs","suppl","SupplementaryFigure2.pdf"), plot=p_indivchange,
       height=190, width=170, units=c("mm"))
tiff(here("figs","suppl","SupplementaryFigure2.tiff"), units="mm", height=190, width=170, res=300)
p_indivchange
dev.off()

# Supplementary Figure 3 --------------------------------------------------

# get data in anterior-posterior direction
es_prepost_nodes <- fread(here::here("data","models","es_prepost_nodes.csv"),
                          header=T, dec=".", sep=",", na="NA")
es_prepost_nodes_time_arcuate <- es_prepost_nodes %>%
  filter(Parameter %in% "time") %>%
  filter(tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  pivot_wider(names_from=tract, values_from=Cohens_f_partial, id_cols=node)

es_prepost_nodes_time_othertracts <- es_prepost_nodes %>%
  filter(Parameter %in% "time") %>%
  filter(!tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  pivot_wider(names_from=tract, values_from=Cohens_f_partial, id_cols=node) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  select(-c(node)) %>%
  rename(node=nodeAP)

es_prepost_nodes_time_AP <- merge(es_prepost_nodes_time_arcuate,
                                  es_prepost_nodes_time_othertracts, by.y="node")
es_prepost_nodes_time_AP <- es_prepost_nodes_time_AP %>%
  pivot_longer(c(Left_Arcuate:Right_SLF), names_to="tract", values_to="effsize")

es_prepost_nodes_time_AP$tract <- factor(es_prepost_nodes_time_AP$tract,
      levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))

# create dataframe for shading significant effects
es_session_rect <- data.frame(tract = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                                        "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"),
                              ymin = c(NA, NA, -Inf, NA, NA, NA, NA, NA),
                              ymax = c(NA, NA, Inf, NA, NA, NA, NA, NA),
                              xmin = c(NA, NA, 44, NA, NA, NA, NA, NA),
                              xmax = c(NA, NA, 57, NA, NA, NA, NA, NA))
es_session_rect$tract <- as.factor(es_session_rect$tract)

# plot
p_es_session_nodes <- es_prepost_nodes_time_AP %>%
  mutate_if(is.character,~factor(.)) %>%
  ggplot() +
  geom_rect(data=es_session_rect, fill="black", alpha=0.4, inherit.aes=F,
            aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  geom_line(aes(x=node, y=effsize)) +
  mytheme +
  geom_hline(aes(yintercept=0.25, linetype ="f = 0.10")) +
  geom_hline(aes(yintercept=0.25, linetype ="f = 0.25")) +
  geom_hline(aes(yintercept=0.40, linetype = "f = 0.40")) +
  stat_summary(fun = mean, aes(x = 1, y=effsize, yintercept = ..y.., linetype="Average f"), geom = "hline") +
  facet_wrap(~tract, labeller = labeller(tract=tract.labs)) +
  scale_y_continuous(limits=c(0,0.5)) +
  scale_linetype_manual(name="Legend", values = c(3,2,4,1),
                        limits=c("f = 0.40", "f = 0.25", "f = 0.10", "Average f"),
                        labels=c("*f* = 0.40", "*f* = 0.25", "*f* = 0.10", "*f*<sub>average<sub>")) +
  labs(y="Partial Cohen's *f*", title="", x="Node") +
  facet_wrap(~tract, labeller = labeller(tract=tract.labs), ncol=2, nrow=4) +
  theme(legend.position = "right",
        legend.text = element_markdown(),
        axis.title.y = element_markdown())

# write output
ggsave(filename=here("figs","suppl","SupplementaryFigure3.pdf"),
       plot=p_es_session_nodes,
       height=170, width=140, units=c("mm"))
tiff(here("figs","suppl","SupplementaryFigure3.tiff"), units="mm",
     height=170, width=140, res=300)
p_es_session_nodes
dev.off()

# Supplementary Figure 4 --------------------------------------------------

# get data in anterior-posterior direction
es_prepost_nodes_interaction_arcuate <- es_prepost_nodes %>%
  filter(Parameter %in% "timeintervention") %>%
  filter(tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  pivot_wider(names_from=tract, values_from=Cohens_f_partial, id_cols=node)

es_prepost_nodes_interaction_othertracts <- es_prepost_nodes %>%
  filter(Parameter %in% "timeintervention") %>%
  filter(!tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  pivot_wider(names_from=tract, values_from=Cohens_f_partial, id_cols=node) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  select(-c(node)) %>%
  rename(node=nodeAP)

es_prepost_nodes_interaction_AP <- merge(es_prepost_nodes_interaction_arcuate,
                                         es_prepost_nodes_interaction_othertracts, by.y="node")
es_prepost_nodes_interaction_AP <- es_prepost_nodes_interaction_AP %>%
  pivot_longer(c(Left_Arcuate:Right_SLF), names_to="tract", values_to="effsize")

es_prepost_nodes_interaction_AP$tract <-
  factor(es_prepost_nodes_interaction_AP$tract,
         levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                    "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))

# plot
p_es_groupsession_nodes <- es_prepost_nodes_interaction_AP %>%
  mutate_if(is.character,~factor(.)) %>%
  ggplot() +
  stat_summary(fun = mean, aes(x = 1, y=effsize, yintercept = ..y.., linetype="Average f"), geom = "hline") +
  geom_line(aes(x=node, y=effsize)) +
  facet_wrap(~tract, labeller = labeller(tract=tract.labs)) +
  scale_y_continuous(limits=c(0,0.5)) +
  geom_hline(aes(yintercept=0.25, linetype ="f = 0.10")) +
  geom_hline(aes(yintercept=0.25, linetype ="f = 0.25")) +
  geom_hline(aes(yintercept=0.40, linetype = "f = 0.40")) +
  mytheme +
  scale_linetype_manual(name="Legend", values = c(3,2,4,1),
                        limits=c("f = 0.40", "f = 0.25", "f = 0.10", "Average f"),
                        labels=c("*f* = 0.40", "*f* = 0.25", "*f* = 0.10", "*f*<sub>average<sub>")) +
  labs(y="Partial Cohen's *f*", title="", x="Node") +
  facet_wrap(~tract, labeller = labeller(tract=tract.labs), ncol=2, nrow=4) +
  theme(legend.position = "right",
        legend.text = element_markdown(),
        axis.title.y = element_markdown())

# write output
ggsave(filename=here("figs","suppl","SupplementaryFigure4.pdf"), plot=p_es_groupsession_nodes,
       height=170, width=140, units=c("mm"))
tiff(here("figs","suppl","SupplementaryFigure4.tiff"), units="mm", height=170, width=140, res=300)
p_es_groupsession_nodes
dev.off()

# Prepare data for Supplementary Figures 5-7 ------------------------------

# Load and clean data (uncorrected p-values)
pre_alongtract_mod_res <-
  fread(here::here("data","models", "pre_alongtract_mod_res.csv"),
        header=T, dec=".", sep=",", na="NA")

prepost_alongtract_mod_res<-
  fread(here::here("data","models", "prepost_alongtract_mod_res.csv"),
        header=T, dec=".", sep=",", na="NA")

pre_alongtract_mod_res <- pre_alongtract_mod_res %>%
  mutate(tract = case_when(tract == "leftAF" ~ "Left_Arcuate",
                           tract == "rightAF" ~ "Right_Arcuate",
                           tract == "leftSLF" ~ "Left_SLF",
                           tract == "rightSLF" ~ "Right_SLF",
                           tract == "leftIFOF" ~ "Left_IFOF",
                           tract == "rightIFOF" ~ "Right_IFOF",
                           tract == "leftILF" ~ "Left_ILF",
                           tract == "rightILF" ~ "Right_ILF"))

prepost_alongtract_mod_res <- prepost_alongtract_mod_res %>%
  mutate(tract = case_when(tract == "leftAF" ~ "Left_Arcuate",
                           tract == "rightAF" ~ "Right_Arcuate",
                           tract == "leftSLF" ~ "Left_SLF",
                           tract == "rightSLF" ~ "Right_SLF",
                           tract == "leftIFOF" ~ "Left_IFOF",
                           tract == "rightIFOF" ~ "Right_IFOF",
                           tract == "leftILF" ~ "Left_ILF",
                           tract == "rightILF" ~ "Right_ILF"))

# Get data from all tracts in anterior-posterior direction for plotting
pre_alongtract_mod_res_arcuate <- pre_alongtract_mod_res %>%
  filter(tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  filter(variable %in% "risk") %>%
  pivot_wider(names_from=tract, values_from=pval, id_cols=c(model,node)) %>%
  select(-c(model))

pre_alongtract_mod_res_othertracts <- pre_alongtract_mod_res %>%
  filter(!tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  filter(variable %in% "risk") %>%
  pivot_wider(names_from=tract, values_from=pval, id_cols=c(model,node)) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  select(-c(node, model)) %>%
  rename(node=nodeAP)

pre_alongtract_mod_res_nodes_AP <- merge(pre_alongtract_mod_res_arcuate,
                                         pre_alongtract_mod_res_othertracts, by.y=c("node"))
pre_alongtract_mod_res_nodes_AP <- pre_alongtract_mod_res_nodes_AP %>%
  pivot_longer(c(Left_Arcuate:Right_ILF), names_to="tract", values_to="pval")

pre_alongtract_mod_res_nodes_AP$tract <-
  factor(pre_alongtract_mod_res_nodes_AP$tract,
         levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                    "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))

prepost_alongtract_mod_res_time_arcuate <- prepost_alongtract_mod_res %>%
  filter(tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  filter(variable %in% "time") %>%
  pivot_wider(names_from=tract, values_from=pval, id_cols=c(model,node)) %>%
  select(-c(model))

prepost_alongtract_mod_res_time_othertracts <- prepost_alongtract_mod_res %>%
  filter(!tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  filter(variable %in% "time") %>%
  pivot_wider(names_from=tract, values_from=pval, id_cols=c(model,node)) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  select(-c(node, model)) %>%
  rename(node=nodeAP)

prepost_alongtract_mod_res_time_nodes_AP <-
  merge(prepost_alongtract_mod_res_time_arcuate,
        prepost_alongtract_mod_res_time_othertracts, by.y=c("node"))

prepost_alongtract_mod_res_time_nodes_AP <- prepost_alongtract_mod_res_time_nodes_AP %>%
  pivot_longer(c(Left_Arcuate:Right_SLF), names_to="tract", values_to="pval")

prepost_alongtract_mod_res_time_nodes_AP$tract <-
  factor(prepost_alongtract_mod_res_time_nodes_AP$tract,
         levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                    "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))


prepost_alongtract_mod_res_interaction_arcuate <- prepost_alongtract_mod_res %>%
  filter(tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  filter(variable %in% "timeintervention") %>%
  pivot_wider(names_from=tract, values_from=pval, id_cols=c(model,node)) %>%
  select(-c(model))

prepost_alongtract_mod_res_interaction_othertracts <- prepost_alongtract_mod_res %>%
  filter(!tract %in% c("Left_Arcuate","Right_Arcuate")) %>%
  filter(variable %in% "timeintervention") %>%
  pivot_wider(names_from=tract, values_from=pval, id_cols=c(model,node)) %>%
  arrange(node) %>%
  mutate(nodeAP = 99:2) %>%
  select(-c(node, model)) %>%
  rename(node=nodeAP)

prepost_alongtract_mod_res_interaction_nodes_AP <-
  merge(prepost_alongtract_mod_res_interaction_arcuate,
        prepost_alongtract_mod_res_interaction_othertracts, by.y=c("node"))

prepost_alongtract_mod_res_interaction_nodes_AP <- prepost_alongtract_mod_res_interaction_nodes_AP %>%
  pivot_longer(c(Left_Arcuate:Right_SLF), names_to="tract", values_to="pval")

prepost_alongtract_mod_res_interaction_nodes_AP$tract <-
  factor(prepost_alongtract_mod_res_interaction_nodes_AP$tract,
         levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                    "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))


# Supplementary Figure 5 --------------------------------------------------

p_raw_pvals_pre <- pre_alongtract_mod_res_nodes_AP %>%
  mutate(tract = factor(tract,
                        levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                                   "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))) %>%
  ggplot() +
  geom_line(aes(x=node, y=pval)) +
  geom_hline(aes(yintercept=0.05, linetype = "p = .05"), size=.8) +
  geom_hline(aes(yintercept=0.01, linetype = "p = .01"), size=.8) +
  scale_linetype_manual(name="Legend", values = c(3,2),
                        limits=c("p = .05", "p = .01"),
                        labels=c("*p* = .05","*p* = .01")) +
  facet_wrap(~tract, labeller = labeller(tract=tract.labs), ncol=2, nrow=4) +
  mytheme +
  labs(y="Uncorrected *p*-value", x="Node") +
  scale_y_log10() +
  theme(legend.text = element_markdown(),
        axis.title.y = element_markdown())

# Write output
ggsave(filename=here("figs","suppl","SupplementaryFigure5.pdf"), plot=p_raw_pvals_pre,
       height=170, width=140, units=c("mm"))
tiff(here("figs","suppl","SupplementaryFigure5.tiff"), units="mm", height=170, width=140, res=300)
p_raw_pvals_pre
dev.off()

# Supplementary Figure 6 --------------------------------------------------

p_raw_pvals_time <- prepost_alongtract_mod_res_time_nodes_AP %>%
  mutate(tract = factor(tract,
                        levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                                   "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))) %>%
  ggplot() +
  geom_line(aes(x=node, y=pval)) +
  geom_hline(aes(yintercept=0.05, linetype = "p = .05"), size=.8) +
  geom_hline(aes(yintercept=0.01, linetype = "p = .01"), size=.8) +
  scale_linetype_manual(name="Legend", values = c(3,2),
                        limits=c("p = .05", "p = .01"),
                        labels=c("*p* = .05","*p* = .01")) +
  facet_wrap(~tract, labeller = labeller(tract=tract.labs), ncol=2, nrow=4) +
  mytheme +
  labs(y="Uncorrected *p*-value", x="Node") +
  scale_y_log10() +
  theme(legend.text = element_markdown(),
        axis.title.y = element_markdown())

# Write output
ggsave(filename=here("figs","suppl","SupplementaryFigure6.pdf"), plot=p_raw_pvals_time,
       height=170, width=140, units=c("mm"))
tiff(here("figs","suppl","SupplementaryFigure6.tiff"), units="mm", height=170, width=140, res=300)
p_raw_pvals_time
dev.off()

# Supplementary Figure 7 --------------------------------------------------

p_raw_pvals_interaction <- prepost_alongtract_mod_res_interaction_nodes_AP %>%
  mutate(tract = factor(tract,
                        levels = c("Left_Arcuate","Right_Arcuate","Left_SLF","Right_SLF",
                                   "Left_IFOF","Right_IFOF","Left_ILF","Right_ILF"))) %>%
  ggplot() +
  geom_line(aes(x=node, y=pval)) +
  geom_hline(aes(yintercept=0.05, linetype = "p = .05"), size=.8) +
  geom_hline(aes(yintercept=0.01, linetype = "p = .01"), size=.8) +
  scale_linetype_manual(name="Legend", values = c(3,2),
                        limits=c("p = .05", "p = .01"),
                        labels=c("*p* = .05","*p* = .01")) +
  facet_wrap(~tract, labeller = labeller(tract=tract.labs), ncol=2, nrow=4) +
  mytheme +
  labs(y="Uncorrected *p*-value", x="Node") +
  scale_y_log10() +
  theme(legend.text = element_markdown(),
        axis.title.y = element_markdown())

# Write output
ggsave(filename=here("figs","suppl","SupplementaryFigure7.pdf"), plot=p_raw_pvals_interaction,
       height=170, width=140, units=c("mm"))
tiff(here("figs","suppl","SupplementaryFigure7.tiff"), units="mm", height=170, width=140, res=300)
p_raw_pvals_interaction
dev.off()
