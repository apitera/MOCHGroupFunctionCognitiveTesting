### STUDY NAME: Social group membership does not facilitate spatial learning of fine-scale resource locations
### SCRIPT: Flocks_LM_Analysis.R
###
### DESCRIPTION: General steps for conducting LMM 
###
### OUTPUT: Results of LMM tests

# Libraries used: -----
library(car)
library(emmeans)
library(ggplot2)
library(lme4)
library(lmerTest)

# Load in data: ----
setwd("~/")
dat.path <- "./icloud/LabThings/MOCHData/AnimalSocialNetworks/Flocks/GroupCognitiveTesting/"
dat.file <- "Flocks_TestingResults.RData" # file with cognitive testing results

load(paste0(dat.path, dat.file))



# Let's run a model to see if flock treatment -----


int.mod   <- lm(FL.20.LocErr ~ AssignmentType*Elevation*I.20.LocErr+Elevation/NestedArrayAssigned, data = TestingResults)
t.int.mod <-lm(FL.20.LocErr ~ (AssignmentType+Elevation+I.20.LocErr)^2 +Elevation/NestedArrayAssigned, data = TestingResults)

anova(int.mod, t.int.mod)


no.ai.mod <- lm(FL.20.LocErr ~ AssignmentType*Elevation+Elevation*I.20.LocErr + Elevation/NestedArrayAssigned, data = TestingResults)
anova(t.int.mod, no.ai.mod)

no.int <- lm(FL.20.LocErr ~ AssignmentType+Elevation+I.20.LocErr + Elevation/NestedArrayAssigned, data = TestingResults)
anova(no.ai.mod, no.int)

no.arr <- lm(FL.20.LocErr ~ AssignmentType+Elevation+I.20.LocErr, data = TestingResults)
anova(no.arr, no.int)

Anova(no.arr, type = 3)
summary(no.arr)$adj.r.squared

anova(no.int, int.mod) # no diffs b/n these two, cut interaction???


no.arr <- lm(FL.20.LocErr ~ AssignmentType+Elevation, data = TestingResults)
car::Anova(no.arr, type = 3)
anova(no.int, no.arr) # no diffs, drop array





# Relationships between initial & reversal testing and flock testing...
# *** JFW says to keep AssignmentType in there 

Anova(lm(FL.20.LocErr ~  I.20.LocErr * Elevation * AssignmentType    + Elevation/NestedArrayAssigned, data = TestingResults), type = 3)
Anova(lm(FL.20.LocErr ~ (I.20.LocErr + Elevation + AssignmentType)^2 + Elevation/NestedArrayAssigned, data = TestingResults), type = 3)



car::Anova(init, type = 3)
init        <- lm(FL.20.LocErr ~ I.20.LocErr * Elevation + AssignmentType + Elevation/NestedArrayAssigned, data = TestingResults)
no.int.init <- lm(FL.20.LocErr ~ I.20.LocErr + Elevation + AssignmentType + Elevation/NestedArrayAssigned, data = TestingResults)

car::Anova(no.int.init, type = 3)
anova(init, no.int.init) # no diffs, so dropping Array

nar.init <- lm(FL.20.LocErr ~ I.20.LocErr + Elevation + AssignmentType, data = TestingResults)
anova(nar.init, no.int.init) # no diffs, so dropping Assignment type

car::Anova(nar.init, type = 3)
summary(nar.init)$adj.r.squared

nass.init <- lm(FL.20.LocErr ~ I.20.LocErr + Elevation, data = TestingResults)
summary(nass.init)$adj.r.squared
car::Anova(nass.init, type = 3)
anova(nass.init, nar.init) # no diffs b/n model with Elevation and elevation/array and without. Going without

ne.init <- lm(FL.20.LocErr ~ I.20.LocErr, data = TestingResults)
anova(ne.init, nass.init) # no.diffs, moving on with removing elevation.

car::Anova(ne.init, type = 3)
summary(ne.init)$adj.r.squared


# Looking at a repeated measures learning curve...



# including 3 trials ----
a.curve <- lmer(MeanLocErr ~ nTrials * AssignmentType * Elevation + (1|BirdID), data = longTestingResults)
#summary(a.curve)
Anova(a.curve, type = "III", test.statistic = "F")

b.curve <- lmer(MeanLocErr ~ (nTrials + AssignmentType + Elevation)^2 + (1|BirdID), data = longTestingResults)
#summary(b.curve)
car::Anova(b.curve, type = 3, test.statistic = "F")
anova(a.curve, b.curve) # no diffs, drop

c.curve <- lmer(MeanLocErr ~ nTrials * Elevation + nTrials * AssignmentType + (1|BirdID), data = longTestingResults)
#summary(c.curve)
car::Anova(c.curve, type = "III", test.statistic = "F")
anova(b.curve, c.curve) # no diffs, drop...

d.curve <- lmer(MeanLocErr ~ nTrial s+ Elevation + AssignmentType + (1|BirdID), data = longTestingResults)
#summary(d.curve)
car::Anova(d.curve, type = "III", test.statistic = "F")

anova(c.curve, d.curve)


nar.emm <- emmeans(c.curve, ~ nTrials * Elevation) 
pairs(nar.emm, simple = "Elevation")
