### STUDY NAME: Social group membership does not facilitate spatial learning of fine-scale resource locations
### SCRIPT: Flocks_LM_Figures.R
###
### DESCRIPTION: Code for making figures in the manuscript. 
###
### OUTPUT: ggplots (1 raincloud plot & 1 panel of 2 visual representations of cognitive testing performance over time)
###         

# Libraries used: -----
library(cowplot)
library(emmeans)
library(ggplot2)
library(lme4)
library(lmerTest)

# Functions used: ------
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R") 

# read in data: -----
dat.path <- "./icloud/LabThings/MOCHData/AnimalSocialNetworks/Flocks/"
dat.file <- "Flocks_TestingResults.RData"

setwd("~/")
load(paste0(dat.path, dat.file))

# Raincloud plots ----------

ggplot(subset(TestingResults, FL.TotTrials >= 20 & !is.na(FL.TotTrials)), 
       aes(x = plot.asst, y = FL.20.LocErr, fill = plot.color, shape = plot.color), na.rm = T) +
  facet_grid(~plot.elev, labeller = labeller(plot.elev = labels.elev)) +
  geom_flat_violin(width = 1, fill = "grey", color = NA, alpha = 0.7) +
  geom_boxplot(position = position_nudge(x = -.1, y = 0),
               width = .1, outlier.shape = NA, alpha = 0.7, fill = "grey", color = "black") +
  geom_dotplot(aes(fill = plot.color), binaxis = 'y', stackdir = "up", binwidth = .03, stackratio = 1.3, dotsize = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "point", 
               position = position_nudge(x = -0.1, y = 0), size = 2, stroke = 0.7) +
  scale_fill_manual(values = c("black","transparent", "black","transparent")) +
  scale_shape_manual(values = c(16, 1, 16, 1)) +
  ylab("Mean location errors") +
  scale_x_discrete(labels = c("Individual", "Flock")) +
  xlab("") +
    annotate("segment",x = 0.5 ,xend = 2.75,y = 1.8,yend = 1.8, size = 0.5) + 
  coord_cartesian(ylim = c(0, 1.7), clip="off") +
  theme_classic() +
  theme(
    plot.margin = margin(0.25,0.5,0.25,0.5,"cm"), 
    legend.position = "none",
    strip.text.x = element_text( size = 13, vjust = -0.5, color = "black"), 
    strip.background = element_rect(fill = "transparent", color = NA),
    strip.placement = "inside",
    axis.title.y = element_text(vjust = 3, size = 13),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))


# learning curves B&W: --------
# Model comparisons with repeated measures :-------

# Start with 3-way interaction...
three.way.int<-summary(emmeans(lmer(MeanLocErr ~ nTrials * AssignmentType * Elevation+ (1|BirdID), data = longTestingResults, REML = F), ~nTrials * AssignmentType * Elevation))

ggplot(three.way.int, aes(x = nTrials, y = emmean, group = paste(Elevation, AssignmentType), linetype = AssignmentType), size = paste(Elevation, AssignmentType)) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                alpha = 0.9, width = 0.4, linetype = 1, size = 0.5, 
                show.legend=FALSE, position = position_dodge(width = 0.2)) +
  geom_line (aes(linetype = AssignmentType), 
             position = position_dodge(width = 0.2), 
             size = 0.5, show.legend = F) +
  geom_point(aes(shape = paste(Elevation, AssignmentType), 
                 stroke = 0.7, size = paste(Elevation, AssignmentType)), 
             position = position_dodge(width = 0.2), fill = "white", color = "black") +
  scale_linetype_manual(values = c(2,1))+
  scale_shape_manual(values = c(23,18,22,15))+
  scale_size_manual(values = c(3.5,5,3.5,3.5))+
  scale_x_discrete(labels = c("3", "5", "10", "20")) +
  scale_y_continuous(limits = c(0.5, 2.1), expand = c(0,0), breaks = seq(0.5, 2, 0.5))+
  ylab("Mean location errors") +
  xlab("Trials")+
  theme_classic() +
  theme(
    plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"), 
    legend.position = "none",
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.ticks = element_line(color = "black"),
    axis.title.y = element_text(vjust = 2,       size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.x  = element_text(color = "black", size = 12),
    axis.text.y  = element_text(color = "black", size = 12),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))

#two-way int with trial# + Elevation
two.way.int<-summary(emmeans(lmer(MeanLocErr ~ nTrials * Elevation + AssignmentType + (1|BirdID), data = longTestingResults, REML = F), ~nTrials*Elevation))

ggplot(two.way.int, aes(x = nTrials, y = emmean, group = Elevation, size = Elevation))+
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE),
                alpha = 0.9, width = 0.2, linetype = 1, size = 0.5, 
                show.legend = FALSE, position = position_dodge(width = 0.2)) +
  geom_line(linetype = 1, size = 0.5, 
            position = position_dodge(width = 0.2), show.legend = F) +
  geom_point(aes(shape = Elevation, size = Elevation),
             stroke = 0.7, position = position_dodge(width = 0.2), 
             fill = "grey", color = "black") +
  scale_linetype_manual(values = c(2,1))+
  scale_shape_manual(values = c(23,22))+
  scale_size_manual(values = c(3.5,3.5))+
  scale_y_continuous(limits = c(0.5, 2.1), expand = c(0,0), breaks = seq(0.5, 2, 0.5))+
  scale_x_discrete(labels = c("3", "5", "10", "20")) +
  ylab("Mean location errors") +
  xlab("Trials") +
  theme_classic() +
  theme(
    plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"), 
    legend.position = "none",
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.ticks = element_line(color = "black"),
    axis.title.y = element_text(vjust = 2, size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.x  = element_text(color = "black", size = 12),
    axis.text.y  = element_text(color = "black", size = 12),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))
