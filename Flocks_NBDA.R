
# Install NBDA package if needed
 install.packages("remotes")
 remotes::install_github("whoppitt/NBDA")
 library(NBDA)
 library(tidyr)

 # Load in data: -------------------------------------------------------
 dat.path = "/home/virginia/Dropbox/ChickadeeLab/Analysis/SEASON_19-20_FlockLearningNBDA/VersionForSubmit/"
 
  # Networks
   load(paste0(dat.path, "GrpLearn_Networks4NBDA.RData"))
   openNet = networks.threshold[["open"]]           # Observed social network
   presenceNet = networks.null.binary[["open"]]     # Homologous social network
   
  # Order-of-acquisition data
   load(paste0(dat.path, "Flocks_NBDA_Vectors.Rdata"))
   
   # ILVs
   ilvs = readRDS(paste0(dat.path, "Flocks_BirdInfo.Rds"))

# Convert networks into 3D arrays ----------------------------------
  # Observed social network
  socNet = array(NA,dim=c(227,227,1))
  socNet[,,1] = openNet
  
   # Homologous network
  homogNet = array(NA,dim=c(227,227,1))
  homogNet[,,1] = presenceNet
  
  # Both networks
  bothNet = array(NA,dim=c(227,227,2))
  bothNet[,,1] = openNet
  bothNet[,,2] = presenceNet

# Extract ILVs ---------------------------------------------------------------------

#Each ILV needs to be stored as a column matrix
# Elevation and assignment type separately
  elevation = cbind(ilvs$Elevation)                # Baseline: High elevation
  assignmentType = cbind(ilvs$AssignmentType)      # Baseline: split treatment

# Elevation and assignment type interactions
  high_split = cbind(ilvs$H.Split)
  high_flock = cbind(ilvs$H.Flock)
  low_split = cbind(ilvs$L.Split)
  low_flock = cbind(ilvs$L.Flock)

# Spatial learning and memory scores
  init = cbind(replace_na(ilvs$MemScoreStd, mean(ilvs$MemScoreStd, na.rm=T)))

# Create a vector of variables to be included in the analysis 
  # Interactions between elevation and treatment
    asoc_int = c("init", "high_flock", "low_split", "low_flock")  # High/Split is baseline
  
  # Version with no interactions
   asoc_noint = c("init", "elevation", "assignmentType")  # High and Split are baseline
   
# ============================================================================================
# AIC tables: Models with interactions 
# ============================================================================================
   
# Create an NBDA model object
 nbdaList = list()
 names = names(orderAqList)  # List of all diffusions
 
 for (i in 1:length(names)) {
   nbdaList[[names[i]]] = nbdaData(label = names[i],
                                   assMatrix = bothNet,
                                   orderAcq = orderAqList[[names[i]]],
                                   timeAcq = timeAqList[[names[i]]], 
                                   endTime = lastTimeList[[names[i]]],
                                   presenceMatrix = presenceMatrixList[[names[i]]],
                                   asoc_ilv = asoc_int, 
                                   int_ilv = asoc_int)
 }  
 
 fullTadaModel = tadaFit(nbdaList, method = "both", iterations =  200)
 # data.frame(Variable = fullTadaModel@varNames, MLE = fullTadaModel@outputPar, SE = fullTadaModel@se)
 # Vars 1&2 are networks, 2,4,5,6 asocial ILVs, 7,8,9,10 social ILVs
 
 # Get constraints vectors
   # Regular network only
   vObservedNet = as.matrix(expand.grid(c(1), c(0), c(0,3), c(0,4), c(0,5), 
                                        c(0,6), c(0,7), c(0,8), c(0,9), c(0,10)))
   # Homogenous network only 
   vHomogNet = as.matrix(expand.grid(c(0), c(2), c(0,3), c(0,4), c(0,5), 
                                     c(0,6), c(0,7), c(0,8), c(0,9), c(0,10)))
   # Asocial only
   vAsoc = as.matrix(expand.grid(c(0), c(0), c(0,3), c(0,4), c(0,5), 
                                 c(0,6), c(0), c(0), c(0), c(0)))
   # Combine into matrix
   cvMatrix = rbind(vObservedNet, vHomogNet, vAsoc)
 
   #3x for 3 baseline functions:
   cvMatrixFull = rbind(cvMatrix, cvMatrix, cvMatrix)
   baselineVect = rep(c("constant","weibull","gamma"), each = dim(cvMatrix)[1])
 
 # Run AIC table
 modelSet_tada = tadaAICtable(nbdadata = nbdaList,
                                 constraintsVectMatrix =cvMatrixFull,
                                 baselineVect =  baselineVect,
                                 cores = 8,
                                 iterations = 2000)
 

 networksSupport(modelSet_tada) # Compare support for networks; ok for homogeneous vs observed, not for social vs asocial
 baselineSupport(modelSet_tada) # Compare support for baselines
 
# Create a new set of models for model averaging
# All social, observed net, weibull baseline
 nbdaListObs = list()
 nbdaListObs[[names[i]]] = nbdaData(label = names[i],
                                       assMatrix = socNet,
                                       orderAcq = orderAqList[[names[i]]],
                                       timeAcq = timeAqList[[names[i]]], 
                                       endTime = lastTimeList[[names[i]]],
                                       presenceMatrix = presenceMatrixList[[names[i]]],
                                       asoc_ilv = asoc_int, 
                                       int_ilv = asoc_int)
 

 vObsSoc = as.matrix(expand.grid(c(1), c(0,2), c(0,3), c(0,4), c(0,5), 
                                 c(0,6), c(0,7), c(0,8), c(0,9)))
 vObsAsoc = as.matrix(expand.grid(c(0), c(0,2), c(0,3), c(0,4), c(0,5), 
                                  c(0), c(0), c(0), c(0)))
 cvMatrixObs = cbind(vObsSoc, vObsAsoc)
 baselineVectObs = rep("weibull",each = dim(cvMatrixObs)[1])
 
modelSet_tadaObs = tadaAICtable(nbdadata = nbdaListObs,
                                     constraintsVectMatrix = cvMatrixObs,
                                     baselineVect = baselineVectObs, 
                                     cores = 8,
                                     iterations = 2000)
 save(modelSet_tadaObs, file = paste0(savePath, "AICTable_Interactions_SocOnly_ObsOnly.Rdata")) 
   
 rbind(
   support = variableSupport(modelSet_tadaObs, includeAsocial = F),
   MAE = modelAverageEstimates(modelSet_tadaObs, includeAsocial = F),
   USE = unconditionalStdErr(modelSet_tadaObs, includeAsocial = F))

# Get weighted estimates for ILVs
 ms = modelSet_tadaObs@printTable
 ms = subset(ms, deltaAICc <= 6)
 ms$AkaikeWeight = ms$AkaikeWeight / sum(ms$AkaikeWeight)
   
 ms = ms %>%
   mutate(ASOCIALhigh_flock = replace_na(ASOCIALhigh_flock, mean(ASOCIALhigh_flock, na.rm=T)))
 
 sum(ms$ASOCIALinit * ms$AkaikeWeight)
 sum(ms$ASOCIALhigh_flock * ms$AkaikeWeight)
 sum(ms$ASOCIALlow_split * ms$AkaikeWeight, na.rm=T)
 sum(ms$ASOCIALlow_flock * ms$AkaikeWeight, na.rm=T)
 
 sum(ms$SOCIALinit * ms$AkaikeWeight, na.rm=T)
 sum(ms$SOCIALhigh_flock * ms$AkaikeWeight, na.rm=T)
 
 sum(ms$SOCIALlow_split * ms$AkaikeWeight, na.rm=T)
 sum(ms$SOCIALlow_flock * ms$AkaikeWeight, na.rm=T)
 
 
 # Estimate CIs for s using the top model ---------------------------------------------------------------
 
 # Top model: low_flock as asocial ILV and social ILV
 nbdaListm1 = list()
 for (i in 1:length(names)) {
   nbdaListm1[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock"), 
                                     int_ilv = c("low_flock"))
 }  
 
 m1 = tadaFit(nbdaListm1, iterations =  2000, baseline = "weibull")

 # plotProfLik(which = 1,model = m1,range = c(0,3), resolution = 20)  # Get an idea of where to look...
 profLikCI(which = 1,model = m1,upperRange = c(2,4),lowerRange = c(0,2)) # Get CI
 # Lower CI  Upper CI 
 # 0.5681132 3.9978084 
 
 # Model averaged estimate for s lower limit
 lowerLimitsByModel_s1 = multiModelLowerLimits(which = 1,aicTable = modelSet_tadaObs, conf = 0.95)
 sum(lowerLimitsByModel_s1$propST*lowerLimitsByModel_s1$adjAkWeight)
 #[1] 0.5857381
 
 # Estimate %ST -----------------------------------------------------------------------------
 # In order to estimate %ST, we need to input values for every parameter in the model. 
 # So we need to find the values of the other parameters that correspond to the 
 # upper and lower limits of s.
 # We do this by fitting models in which s is constrained to its upper and lower limits:
 
 #Lower limit first:
 nbdaListm1_l = list()
 
 for (i in 1:length(names)) {
   nbdaListm1_l[[names[i]]] = constrainedNBDAdata(nbdaListm1[[names[i]]], 
                                                  constraintsVect = c(1,2,3),
                                                  offsetVect = c(0.5681132,0,0))
   
 }
 
 sLowerModel_add = tadaFit(nbdaListm1_l, type = "asocial", iterations = 200, baseline = "weibull")
 # Recover the values of the other two parameters:
 sLowerModel_add@outputPar
 # Input them to get %ST
 nbdaPropSolveByST(par = c(0.5681132, sLowerModel_add@outputPar), model = sLowerModel_add)
 # P(S offset) 
 # 0.22843 
 
 # upper limit
 nbdaListm1_h = list()
 
 for (i in 1:length(names)) {
   nbdaListm1_h[[names[i]]] = constrainedNBDAdata(nbdaListm1[[names[i]]], 
                                                  constraintsVect = c(1,2,3),
                                                  offsetVect = c(3.9978084,0,0))
   
 }
 
 sUpperModel_add = tadaFit(nbdaListm1_h, type="asocial", iterations = 200, baseline = "weibull")
 # Recover the values of the other two parameters:
 sUpperModel_add@outputPar
 # Input them to get %ST:
 nbdaPropSolveByST(par = c(3.9978084, sUpperModel_add@outputPar), model = sUpperModel_add)
 # P(S offset) 
 # 0.53004
 
 # Get CIs for other parameters -----------------------------------------------------------------
 
 # Asocial low flock
 # plotProfLik(which = 2,model = m1, range = c(-4,0), resolution = 20)
 profLikCI(which = 2, model = m1, upperRange = c(-1,1), lowerRange = c(-3,-1))
 # Lower CI   Upper CI 
 # -2.3955065 -0.4704636 
 
 # Social low flock
 # plotProfLik(which = 3,model = m1,range = c(0,5), resolution = 20)
 profLikCI(which = 3,model = m1,upperRange = c(0,4),lowerRange = c(-2,3))
 # Lower CI   Upper CI 
 # -0.2062121  1.5440467 
 
 # For vars not in the top model, build the best model that *does* contain it...
 # Asocial low split --------------------------------
 nbdaListm2 = list()
 for (i in 1:length(names)) {
   nbdaListm2[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_split", "low_flock"))
 }  
 
 m2 = tadaFit(nbdaListm2,iterations =  200, baseline = "weibull")
 # data.frame(Variable = m2@varNames, MLE = m2@outputPar, SE = m2@se)
 
 # plotProfLik(which = 2,model = m2, range = c(-5,5), resolution = 20)
 profLikCI(which = 2,model = m2, upperRange = c(-1,2),lowerRange = c(-2,0))
 # Lower CI   Upper CI 
 # -0.7138731  0.1399815 

 # Social low split --------------------------------
 nbdaListm3 = list()
 for (i in 1:length(names)) {
   nbdaListm3[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock"), 
                                     int_ilv = c("low_split"))
 }  
 
 m3 = tadaFit(nbdaListm3, iterations =  200, baseline = "weibull")
 # data.frame(Variable = m3@varNames, MLE = m3@outputPar, SE = m3@se)
 
 # THIS ONE IS WEIRD WE'LL COME BACK TO IT
 # plotProfLik(which = 3,model = m3,range = c(-10,30), resolution = 20)
 profLikCI(which = 3,model = m3, upperRange = c(-1,2),lowerRange = c(-2,0))
 # Everything comes out with LL as -Inf
 
 # Asocial high flock --------------------------------
 nbdaListm4 = list()
 for (i in 1:length(names)) {
   nbdaListm4[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock", "low_split"), 
                                     int_ilv = c("low_flock"))
 }  
 
 m4 = tadaFit(nbdaListm4, iterations =  200, baseline = "weibull")
# data.frame(Variable = m4@varNames, MLE = m4@outputPar, SE = m4@se)
 
# plotProfLik(which = 2,model = m4,range = c(-1,2), resolution = 20)
 profLikCI(which = 2, model = m4, upperRange = c(0,1), lowerRange = c(-1,0))
 # Lower CI   Upper CI 
 # -0.3622845  0.5128227 
 
 # Asocial initial --------------------------------
 nbdaListm5 = list()
 for (i in 1:length(names)) {
   nbdaListm5[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init", "low_flock"), 
                                     int_ilv = c("low_flock"))
 }  
 
 m5 = tadaFit(nbdaListm5, iterations =  200, baseline = "weibull")
# data.frame(Variable = m5@varNames, MLE = m5@outputPar, SE = m5@se)
 
 # plotProfLik(which = 2,model = m5,range = c(-2,3), resolution = 20)
 profLikCI(which = 2,model = m5,upperRange = c(0,1),lowerRange = c(-1,0))
 # Lower CI   Upper CI 
 # -0.3091914  0.2044474 
 
 # Social initial --------------------------------
 nbdaListm6 = list()
 for (i in 1:length(names)) {
   nbdaListm6[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock"), 
                                     int_ilv = c("init", "low_flock"))
 }  
 
 m6 = tadaFit(nbdaListm6, iterations =  200, baseline = "weibull")
 # data.frame(Variable = m6@varNames, MLE = m6@outputPar, SE = m6@se)
 
 # plotProfLik(which = 3,model = m6, range = c(-2,2), resolution=20)
 profLikCI(which = 3,model = m6, upperRange = c(0,1), lowerRange = c(-1,0))
 # Lower CI   Upper CI 
 # -0.5888892  0.3201956 
 
 # Social high flock --------------------------------
 nbdaListm7 = list()
 for (i in 1:length(names)) {
   nbdaListm7[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock"), 
                                     int_ilv = c("high_flock", "low_flock"))
 }  
 
 m7 = tadaFit(nbdaListm7, iterations =  200, baseline = "weibull")
 #data.frame(Variable = m7@varNames, MLE = m7@outputPar, SE = m7@se)

 #plotProfLik(which = 3,model = m7, range=c(-6,5), resolution = 40)
 profLikCI(which = 3,model = m7, upperRange = c(0,2), lowerRange = c(-6,-3))
 # Lower CI  Upper CI 
 # -4.457900  1.282484 
 
 # ============================================================================================
 # AIC tables: Models without interactions 
 # ============================================================================================
 nbdaList_noint = list()
 names = names(orderAqList)
   
 for (i in 1:length(names)) {
   nbdaList_noint[[names[i]]] = nbdaData(label = names[i],
                                         assMatrix = bothNet,
                                         orderAcq = orderAqList[[names[i]]],
                                         timeAcq = timeAqList[[names[i]]], 
                                         endTime = lastTimeList[[names[i]]],
                                         presenceMatrix = presenceMatrixList[[names[i]]],
                                         asoc_ilv = asoc_noint, 
                                         int_ilv = asoc_noint)
 } 
   
 fullTadaModel_noint = tadaFit(nbdaList_noint)
 #data.frame(Variable = fullTadaModel_noint@varNames, ML = fullTadaModel_noint@outputPar, SE = fullTadaModel_noint@se)   
 
 
 # Get constraints vectors
   # Regular network only
   vObservedNet_noint = as.matrix(expand.grid(c(1), c(0), c(0,3), c(0,4), c(0,5), 
                                              c(0,6), c(0,7), c(0,8)))
   # Homogenous network only 
   vHomogNet_noint = as.matrix(expand.grid(c(0), c(2), c(0,3), c(0,4), c(0,5), 
                                           c(0,6), c(0,7), c(0,8)))
   # Asocial only
   vAsoc_noint = as.matrix(expand.grid(c(1), c(0), c(0,3), c(0,4), c(0,5), 
                                       c(0), c(0), c(0)))
   # Combine into matrix
   cvMatrix_noint = rbind(vObservedNet_noint, vHomogNet_noint, vAsoc_noint)
   
   #3x for 3 baseline functions:
   cvMatrixFull_noint = rbind(cvMatrix_noint, cvMatrix_noint, cvMatrix_noint)
   baselineVect_noint = rep(c("constant","weibull","gamma"), each = dim(cvMatrix_noint)[1])
 
 modelSet_tada_noint = tadaAICtable(nbdadata = nbdaList_noint,
                                   constraintsVectMatrix = cvMatrixFull_noint,
                                   baselineVect = baselineVect_noint,
                                   iterations = 200,
                                   cores = 8)
 
 networksSupport(modelSet_tada_noint) # Compare support for networks; ok for homogeneous vs observed, not for social vs asocial
 baselineSupport(modelSet_tada_noint) # Compare support for baselines
 
 # Create a new set of models for model averaging --------------------------------------------------
 # All social, observed net, weibull baseline
 nbdaListObs_noint = list()

 for (i in 1:length(names)) {
   nbdaListObs_noint[[names[i]]] = nbdaData(label = names[i],
                                         assMatrix = socNet,
                                         orderAcq = orderAqList[[names[i]]],
                                         timeAcq = timeAqList[[names[i]]], 
                                         endTime = lastTimeList[[names[i]]],
                                         presenceMatrix = presenceMatrixList[[names[i]]],
                                         asoc_ilv = asoc_noint, 
                                         int_ilv = asoc_noint)
 } 
 
 fullTadaModelObs_noint = tadaFit(nbdaListObs_noint)
 
 cvMatrixObs_noint = as.matrix(expand.grid(c(1), c(0,2), c(0,3), c(0,4), 
                                           c(0,5), c(0,6), c(0,7)))
 baselineVectObs_noint = rep("weibull",each = dim(cvMatrixObs_noint)[1])
 
 modelSet_tadaObs_noint = tadaAICtable(nbdadata = nbdaListObs_noint,
                                           constraintsVectMatrix =  cvMatrixObs_noint,
                                           baselineVect = baselineVectObs_noint,
                                           iterations = 2000,
                                           cores = 8)
 
 # Model averaged estimate for s lower limit
 lowerLimitsByModel_noint = multiModelLowerLimits(which = 1,aicTable = modelSet_tadaObs_noint, conf = 0.95)
 sum(lowerLimitsByModel_noint$propST*lowerLimitsByModel_noint$adjAkWeight)
 
# MAEs for ILVs --------------------------------------------------
 rbind(
   support = variableSupport(modelSet_tadaObs_noint),
   MAE = modelAverageEstimates(modelSet_tadaObs_noint),
   USE = unconditionalStdErr(modelSet_tadaObs_noint))
 

 msN = modelSet_tadaObs_noint@printTable
   sum(msN$ASOCIALassignmentType * msN$AkaikeWeight, na.rm=T)
   sum(msN$ASOCIALelevation * msN$AkaikeWeight, na.rm=T)
   sum(msN$ASOCIALinit * msN$AkaikeWeight, na.rm=T)
   
   sum(msN$SOCIALassignmentType * msN$AkaikeWeight, na.rm=T)
   sum(msN$SOCIALelevation * msN$AkaikeWeight, na.rm=T)
   sum(msN$SOCIALinit * msN$AkaikeWeight, na.rm=T)
 
# Use best model to get CIs for s --------------------------------------------------
 nbdaListm1ni = list()
 for (i in 1:length(names)) {
   nbdaListm1ni[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("elevation"))
 }  
 
 m1ni = tadaFit(nbdaListm1ni, iterations = 200, baseline = "weibull")
 # data.frame(Variable = m1ni@varNames, MLE = m1ni@outputPar, SE = m1ni@se)
 
 # plotProfLik(which = 1, model = m1ni, range = c(0,5), resolution = 20)
 profLikCI(which = 1, model = m1ni, upperRange = c(2,4), lowerRange = c(0,1))
 # Lower CI  Upper CI 
 # 2.6311683 0.1133852
 
# Estimate %ST, get values for other params for upper/lower limits --------------------------------------------------
 #Lower limit:
   nbdaListm1_l = list()
   
   for (i in 1:length(names)) {
     nbdaListm1_l[[names[i]]] = constrainedNBDAdata(nbdaListm1[[names[i]]], 
                                                    constraintsVect = c(1,2),
                                                    offsetVect = c(0.1133852,0))
     
   }
   
   sLowerModel_add<-tadaFit(nbdaListm1_l, type="asocial", iterations = 200, baseline = "weibull")
   sLowerModel_add@outputPar # Get values
   nbdaPropSolveByST(par = c(0.1133852, sLowerModel_add@outputPar), model = sLowerModel_add) # Get %ST
   # P(S offset) 
   # 0.04238 
   
 # upper limit
   nbdaListm1_h = list()
   
   for (i in 1:length(names)) {
     nbdaListm1_h[[names[i]]] = constrainedNBDAdata(nbdaListm1[[names[i]]], 
                                                    constraintsVect = c(1,2),
                                                    offsetVect = c(2.6311683,0))
     
   }
   
   sUpperModel_add = tadaFit(nbdaListm1_h, type="asocial", iterations = 200, baseline = "weibull")
   sUpperModel_add@outputPar # Get values
   nbdaPropSolveByST(par = c(2.6311683, sUpperModel_add@outputPar), model = sUpperModel_add) # Get %ST
   # P(Network 1)  P(S offset) 
   # 0.30036      0.00000 
   
# Get CIs for ILVs from top models ---------------------------------------------
#Elevation asocial; 
 plotProfLik(which = 2, model = m1ni, range = c(-2,0), resolution = 20)
 profLikCI(which = 2, model = m1ni, upperRange = c(-0.5,0), lowerRange = c(-1,-0.5))
 
# Elevation social -----------------------------------------------------------
 nbdaListm2ni = list()
 for (i in 1:length(names)) {
   nbdaListm2ni[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     int_ilv = c("elevation"), 
                                     asoc_ilv = c("elevation"))
 }  
 
 m2ni = tadaFit(nbdaListm2ni, iterations = 1000, baseline = "weibull")
 # data.frame(Variable = m2ni@varNames, MLE = m2ni@outputPar, SE = m2ni@se)
 
 # plotProfLik(which = 3,model = m2ni, range = c(-2,10), resolution = 20)
 profLikCI(which = 3,model = m2ni, upperRange = c(0,2), lowerRange = c(-1,0))
 
 
# Asocial assignment type ------------------------------------------------------------
 nbdaListm3ni = list()
 for (i in 1:length(names)) {
   nbdaListm3ni[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("elevation", "assignmentType"))
 }  
 
 m3ni = tadaFit(nbdaListm3ni, iterations = 200, baseline = "weibull")
# data.frame(Variable = m3ni@varNames, MLE = m3ni@outputPar, SE = m3ni@se)
 
# plotProfLik(which = 3, model = m3ni, range = c(-2,2), resolution = 20)
 profLikCI(which = 3, model = m3ni, upperRange = c(0,1), lowerRange = c(-1,0))
 
 
# Asocial initial, social iniial -------------------------------------------------
 nbdaListm4ni = list()
 for (i in 1:length(names)) {
   nbdaListm4ni[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     int_ilv = c("init"), 
                                     asoc_ilv = c("elevation", "init"))
 }  
 
 m4ni = tadaFit(nbdaListm4ni, iterations = 1000, baseline = "weibull")
# data.frame(Variable = m4ni@varNames, MLE = m4ni@outputPar, SE = m4ni@se)
 
 # Asocial initial
 # plotProfLik(which = 3, model = m4ni, range = c(-1,1), resolution=20)
 profLikCI(which = 3, model = m4ni, upperRange = c(0.2,0.6), lowerRange = c(-0.5,0))

 # Social initial
 # plotProfLik(which = 4,model = m4ni,range = c(-3,1), resolution=20)
 profLikCI(which = 4,model = m4ni,upperRange = c(0,1), lowerRange = c(-2,-1))

# Social assignmentType ------------------------------------------------------------
 nbdaListm5ni = list()
 for (i in 1:length(names)) {
   nbdaListm5ni[[names[i]]] = nbdaData(label = names[i],
                                     assMatrix = socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     int_ilv = c("assignmentType"), 
                                     asoc_ilv = c("elevation"))
 }  
 
 m5ni = tadaFit(nbdaListm5ni, iterations = 1000, baseline = "weibull")
# data.frame(Variable = m5ni@varNames, MLE = m5ni@outputPar, SE = m5ni@se)
 
 # plotProfLik(which = 3,model = m5ni,range = c(-2,2), resolution = 20)
 profLikCI(which = 3,model = m5ni, upperRange = c(0,2), lowerRange = c(-1,0))

 
   