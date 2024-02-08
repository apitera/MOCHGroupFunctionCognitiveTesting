
# Libraries used: -----
library(car)
library(emmeans)
library(ggplot2)
library(lme4)
library(lmerTest)

# Load in data: ----
setwd("~/")
dat.path <- "./icloud/LabThings/MOCHData/AnimalSocialNetworks/Flocks/"
dat.file <- "GrpLearn_TestingResults.RData"

load(paste0(dat.path, dat.file))

# let's first plot out performance...

tmp <- subset(V1.Results, FL.TotTrials >= 20 & !is.na(FL.TotTrials))
tmp$Elevation <- as.factor(tmp$Elevation)
tmp$Array <- as.factor(tmp$Array)
tmp$AssignmentType <- as.factor(tmp$AssignmentType)
tmp$tmp.array <- as.character(tmp$Array)

tmp$tmp.array[which(tmp$tmp.array == "H1")] <- "A"
tmp$tmp.array[which(tmp$tmp.array == "L3")] <- "A"
tmp$tmp.array[which(tmp$tmp.array == "H3")] <- "B"
tmp$tmp.array[which(tmp$tmp.array == "L8")] <- "B"

tmp$tmp.array <- as.factor(tmp$tmp.array)


# Let's run a model to see if flock treatment -----


int.mod   <- lm(FL.20.LocErr ~ AssignmentType*Elevation*I.20.LocErr+Elevation/tmp.array, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))
t.int.mod <-lm(FL.20.LocErr ~ (AssignmentType+Elevation+I.20.LocErr)^2 +Elevation/tmp.array, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))

anova(int.mod, t.int.mod)


no.ai.mod <- lm(FL.20.LocErr ~ AssignmentType*Elevation+Elevation*I.20.LocErr + Elevation/tmp.array, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))
anova(t.int.mod, no.ai.mod)

no.int <- lm(FL.20.LocErr ~ AssignmentType+Elevation+I.20.LocErr + Elevation/tmp.array, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))
anova(no.ai.mod, no.int)

no.arr <- lm(FL.20.LocErr ~ AssignmentType+Elevation+I.20.LocErr, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))
anova(no.arr, no.int)

Anova(no.arr, type = 3)
summary(no.arr)$adj.r.squared

anova(no.int, int.mod) # no diffs b/n these two, cut interaction???


no.arr <- lm(FL.20.LocErr ~ AssignmentType+Elevation, data = tmp)
car::Anova(no.arr, type = 3)
anova(no.int, no.arr) # no diffs, drop array





# Relationships between initial & reversal testing and flock testing...
# *** JFW says to keep AssignmentType in there 

Anova(lm(FL.20.LocErr ~ I.20.LocErr*Elevation*AssignmentType + Elevation/tmp.array, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials))), type = 3)
Anova(lm(FL.20.LocErr ~ (I.20.LocErr+Elevation+AssignmentType)^2 + Elevation/tmp.array, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials))), type = 3)

init <- lm(FL.20.LocErr ~ I.20.LocErr*Elevation +AssignmentType + Elevation/tmp.array, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))

car::Anova(init, type = 3)

no.int.init <- lm(FL.20.LocErr ~ I.20.LocErr+Elevation+ AssignmentType + Elevation/tmp.array, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))

car::Anova(no.int.init, type = 3)
anova(init, no.int.init) # no diffs, so dropping Array

nar.init <- lm(FL.20.LocErr ~ I.20.LocErr+Elevation + AssignmentType, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))
anova(nar.init, no.int.init) # no diffs, so dropping Assignment type

car::Anova(nar.init, type = 3)
summary(nar.init)$adj.r.squared

nass.init <- lm(FL.20.LocErr ~ I.20.LocErr + Elevation, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))
summary(nass.init)$adj.r.squared
car::Anova(nass.init, type = 3)
anova(nass.init, nar.init) # no diffs b/n model with Elevation and elevation/array and without. Going without

ne.init <- lm(FL.20.LocErr ~ I.20.LocErr, data = subset(tmp, I.TotTrials >= 20 & !is.na(I.TotTrials)))
anova(ne.init, nass.init) # no.diffs, moving on with removing elevation.

car::Anova(ne.init, type = 3)
summary(ne.init)$adj.r.squared


# Looking at a repeated measures learning curve...

v1.summ <- reshape2::melt(subset(V1.Results, FL.TotTrials >= 20 & !is.na(FL.TotTrials)), 
                          id.vars      = c("AssignmentType", "Elevation", "BirdID", "ArrayAssigned"    ), 
                          measure.vars = c("FL.3.LocErr", "FL.5.LocErr", "FL.10.LocErr", "FL.20.LocErr"), 
                          value.name   = "MeanLocErr"                                                  )

v1.summ$ArrayAssigned <- as.factor(v1.summ$ArrayAssigned)
v1.summ$Trials <- as.character(v1.summ$variable)
v1.summ$Trials[which(v1.summ$Trials == "FL.3.LocErr" )] <- "03"
v1.summ$Trials[which(v1.summ$Trials == "FL.5.LocErr" )] <- "05"
v1.summ$Trials[which(v1.summ$Trials == "FL.10.LocErr")] <- "10"
v1.summ$Trials[which(v1.summ$Trials == "FL.20.LocErr")] <- "20"

v1.summ$Trials <- as.numeric(v1.summ$Trials)


library(lme4)
library(lmerTest)


# including 3 trials ----
a.curve <- lmer(MeanLocErr ~ variable*AssignmentType*Elevation+ (1|BirdID), data = v1.summ)
#summary(a.curve)
Anova(a.curve, type = "III", test.statistic = "F")

b.curve <- lmer(MeanLocErr ~ (variable+AssignmentType+Elevation)^2 + (1|BirdID), data = v1.summ)
#summary(b.curve)
car::Anova(b.curve, type = 3, test.statistic = "F")
anova(a.curve, b.curve) # no diffs, drop

c.curve <- lmer(MeanLocErr ~ variable*Elevation+variable*AssignmentType + (1|BirdID), data = v1.summ)
#summary(c.curve)
car::Anova(c.curve, type = "III", test.statistic = "F")
anova(b.curve, c.curve) # no diffs, drop...

d.curve <- lmer(MeanLocErr ~ variable+ Elevation +AssignmentType +(1|BirdID), data = v1.summ)
#summary(d.curve)
car::Anova(d.curve, type = "III", test.statistic = "F")

anova(c.curve, d.curve)


nar.emm <- emmeans(c.curve, ~ variable*Elevation) 
pairs(nar.emm, simple = "Elevation")

# Excluding 3 trials -------
a.curve <- lmer(MeanLocErr ~ variable*AssignmentType*Elevation + (1|BirdID), data = subset(v1.summ, Trials > 3), REML = F)
summary(a.curve)
car::Anova(a.curve, type = 3)

b.curve <- lmer(MeanLocErr ~ (variable+AssignmentType+Elevation)^2 + (1|BirdID), data = subset(v1.summ, Trials > 3), REML = F)
summary(b.curve)
car::Anova(b.curve, type = 3)
anova(a.curve, b.curve)

c.curve <- lmer(MeanLocErr ~ AssignmentType + variable*Elevation + (1|BirdID), data = subset(v1.summ, Trials > 3), REML = F)

summary(c.curve)
car::Anova(c.curve, type = 3)
summary(lsmeans(c.curve, pairwise~variable:Elevation), mode = "satterthwaite")
anova(b.curve, c.curve)

d.curve<- lmer(MeanLocErr ~ variable*Elevation + (1|BirdID), data = subset(v1.summ, Trials > 3), REML = F)
summary(d.curve)
car::Anova(d.curve, type = 3)
summary(lsmeans(d.curve, pairwise~variable:Elevation), mode = "satterthwaite")