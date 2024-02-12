### STUDY NAME: Social group membership does not facilitate spatial learning of fine-scale resource locations
### SCRIPT: Flocks_LM_Analysis.R
###
### DESCRIPTION: General steps for conducting LMM 
###
### OUTPUT: Results of LMM tests

# Libraries used: -----
library(car)
library(emmeans)
library(lme4)
library(lmerTest)

# Load in data: ----
setwd("~/")
dat.path <- "./icloud/LabThings/MOCHData/AnimalSocialNetworks/Flocks/GroupCognitiveTesting/"
dat.file <- "Flocks_TestingResults.RData" # file with cognitive testing results

load(paste0(dat.path, dat.file))



# Let's run a model to see if flock treatment -----

# model with 3-way interaction + NestedArray
int.mod   <- lm(FL.20.LocErr ~ AssignmentType*Elevation*I.20.LocErr+Elevation/NestedArrayAssigned, data = TestingResults)
# 2-way interactions + NestedArray
t.int.mod <-lm(FL.20.LocErr ~ (AssignmentType+Elevation+I.20.LocErr)^2 +Elevation/NestedArrayAssigned, data = TestingResults)

anova(int.mod, t.int.mod)# no significant differences between models, dropping 3-way interaction

# Interactions only between two sets of fixed effects + NestedArray
no.ai.mod <- lm(FL.20.LocErr ~ AssignmentType*Elevation+Elevation*I.20.LocErr + Elevation/NestedArrayAssigned, data = TestingResults)
anova(t.int.mod, no.ai.mod) # no significant differences between models, dropping initial testing performance * Assignment type (treatment) interaction

# Removing all interactions but keeping array nested within elevation
no.int <- lm(FL.20.LocErr ~ AssignmentType+Elevation+I.20.LocErr + Elevation/NestedArrayAssigned, data = TestingResults)
anova(no.ai.mod, no.int)# no significant differences between models, dropping all interactions

# removing array
no.arr <- lm(FL.20.LocErr ~ AssignmentType+Elevation+I.20.LocErr, data = TestingResults)
anova(no.arr, no.int)# no significant differences between models, dropping array

Anova(no.arr, type = 3)
summary(no.arr)$adj.r.squared






# Looking at a repeated measures learning curve...

a.curve <- lmer(MeanLocErr ~ nTrials * AssignmentType * Elevation + (1|BirdID), data = longTestingResults)

Anova(a.curve, type = "III", test.statistic = "F")

b.curve <- lmer(MeanLocErr ~ (nTrials + AssignmentType + Elevation)^2 + (1|BirdID), data = longTestingResults)

car::Anova(b.curve, type = 3, test.statistic = "F")
anova(a.curve, b.curve) # no diffs, dropping 3-way interaction

c.curve <- lmer(MeanLocErr ~ nTrials * Elevation + nTrials * AssignmentType + (1|BirdID), data = longTestingResults)

car::Anova(c.curve, type = "III", test.statistic = "F")
anova(b.curve, c.curve) # no diffs, drop interactions

d.curve <- lmer(MeanLocErr ~ nTrials+ Elevation + AssignmentType + (1|BirdID), data = longTestingResults)
#summary(d.curve)
car::Anova(d.curve, type = "III", test.statistic = "F")

anova(c.curve, d.curve) # no difference but reporting results from both c & d models


nar.emm <- emmeans(c.curve, ~ nTrials * Elevation) 
pairs(nar.emm, simple = "Elevation")
