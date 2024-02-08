# Libraries used: -----
library(cowplot)
library(emmeans)
library(ggplot2)
library(lme4)
library(lmerTest)

# Functions used: ------
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R") # This is how we can create flat violin plots

setwd("~/")

# read in data: -----
dat.path   <- "./icloud/LabThings/MOCHData/AnimalSocialNetworks/Flocks/"
dat.file   <- "GrpLearn_TestingResults.RData"
load(paste0(dat.path, dat.file))

# add a few columns for ease of plotting everything up... (very very hacky)------

V1.Results$plot.asst <- ""
V1.Results$plot.asst[which(V1.Results$AssignmentType == "Split")] <- "A_I"
V1.Results$plot.asst[which(V1.Results$AssignmentType == "Flock")] <- "B_F"
V1.Results$plot.elev <- ""
V1.Results$plot.elev[which(V1.Results$Elevation == "H")] <- "B_H"
V1.Results$plot.elev[which(V1.Results$Elevation == "L")] <- "A_L"
V1.Results$plot.color <- paste(V1.Results$Elevation, V1.Results$AssignmentType)
V1.Results$plot.color[which(V1.Results$plot.color == "L Split")] <- "1"
V1.Results$plot.color[which(V1.Results$plot.color == "L Flock")] <- "2"
V1.Results$plot.color[which(V1.Results$plot.color == "H Split")] <- "3"
V1.Results$plot.color[which(V1.Results$plot.color == "H Flock")] <- "4"

# B&W version of rainclouds ----------

labels.elev <- c(A_L = "Low elevation", B_H = "High elevation")

ggplot(subset(V1.Results, FL.TotTrials >= 20 & !is.na(FL.TotTrials)), 
       aes(x = plot.asst, y = FL.20.LocErr, fill = plot.color, shape = plot.color), na.rm = T) +
  facet_grid(~plot.elev, labeller = labeller(plot.elev = labels.elev)) +
  geom_flat_violin(width = 1, fill = "grey", color = NA,
                   alpha = 0.7)+#,
  geom_boxplot(position = position_nudge(x = -.1, y = 0),
               width = .1, outlier.shape = NA, alpha = 0.7, fill = "grey", color = "black") +
  geom_dotplot(binaxis = 'y', stackdir = "up", binwidth = .03, aes(fill = plot.color), stackratio = 1.3, dotsize = 0.8) +
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
v1.summ <- reshape2::melt(subset(V1.Results, FL.TotTrials >= 20 & !is.na(FL.TotTrials)), 
                          id.vars      = c("AssignmentType", "Elevation", "BirdID"), 
                          measure.vars = c("FL.3.LocErr", "FL.5.LocErr", "FL.10.LocErr", "FL.20.LocErr"), 
                          value.name   = "MeanLocErr")

v1.summ$Trials <- as.character(v1.summ$variable)
v1.summ$Trials[which(v1.summ$Trials == "FL.3.LocErr" )] <- "03"
v1.summ$Trials[which(v1.summ$Trials == "FL.5.LocErr" )] <- "05"
v1.summ$Trials[which(v1.summ$Trials == "FL.10.LocErr")] <- "10"
v1.summ$Trials[which(v1.summ$Trials == "FL.20.LocErr")] <- "20"

# Start with 3-way interaction...
three.way.int<-summary(emmeans(lmer(MeanLocErr ~ variable*AssignmentType*Elevation+ (1|BirdID), data = v1.summ, REML = F), ~variable*AssignmentType*Elevation))

ggplot(three.way.int, aes(x=variable, y=emmean, group=paste(Elevation, AssignmentType), linetype=AssignmentType), size=paste(Elevation, AssignmentType)) +
  geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE), 
                alpha = 0.9, width = 0.4, linetype = 1, size = 0.5, 
                show.legend=FALSE, position = position_dodge(width = 0.2)) +
  geom_line (aes(linetype=AssignmentType), 
             position = position_dodge(width = 0.2), 
             size = 0.5, show.legend = F) +
  geom_point(aes(shape=paste(Elevation, AssignmentType), 
                 stroke = 0.7, size = paste(Elevation, AssignmentType)), 
             position = position_dodge(width = 0.2), fill = "white", color = "black") +
  scale_linetype_manual(values = c(2,1))+
  scale_shape_manual(values = c(23,18,22,15))+
  scale_size_manual(values = c(3.5,5,3.5,3.5))+
  scale_x_discrete(labels = c("3", "5", "10", "20")) +
  scale_y_continuous(limits = c(0.5, 2.1), expand = c(0,0), breaks =seq(0.5, 2, 0.5))+
  ylab("Mean location errors") +
  xlab("Trials")+
  theme_classic() +
  theme(
    plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"), 
    legend.position = "none",
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.ticks = element_line(color = "black"),
    axis.title.y = element_text(vjust = 2,       size = 13),
    axis.title.x = element_text(      size = 13),
    axis.text.x  = element_text(color = "black", size = 12),
    axis.text.y  = element_text(color = "black", size = 12),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))

#two-way int with trial# + Elevation
two.way.int<-summary(emmeans(lmer(MeanLocErr ~ variable*Elevation+AssignmentType + (1|BirdID), data = v1.summ, REML = F), ~variable*Elevation))

ggplot(two.way.int, aes(x=variable, y=emmean, group=Elevation, size=Elevation))+
  geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),
                alpha = 0.9, width=0.2, linetype=1, size = 0.5, 
                show.legend=FALSE, position = position_dodge(width = 0.2)) +
  geom_line(linetype = 1, size = 0.5, 
            position = position_dodge(width = 0.2), show.legend = F) +
  geom_point(aes(shape=Elevation, size = Elevation),
             stroke = 0.7, position = position_dodge(width = 0.2), 
             fill = "grey", color = "black") +
  scale_linetype_manual(values = c(2,1))+
  scale_shape_manual(values = c(23,22))+
  scale_size_manual(values = c(3.5,3.5))+
  scale_y_continuous(limits = c(0.5, 2.1), expand = c(0,0), breaks =seq(0.5, 2, 0.5))+
  scale_x_discrete(labels = c("3", "5", "10", "20")) +
  ylab("Mean location errors") +
  xlab("Trials") +
  theme_classic() +
  theme(
    plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"), 
    legend.position = "none",
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.ticks = element_line(color = "black"),
    axis.title.y = element_text(vjust = 2,       size = 13),
    axis.title.x = element_text(      size = 13),
    axis.text.x  = element_text(color = "black", size = 12),
    axis.text.y  = element_text(color = "black", size = 12),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"))
