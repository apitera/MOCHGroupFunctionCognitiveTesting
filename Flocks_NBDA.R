
# Install NBDA package if needed
 install.packages("remotes")
 remotes::install_github("whoppitt/NBDA")
 library(NBDA)

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
   

# AIC tables: Models with interactions -------------------------------------------------------
# Create an NBDA model object
 nbdaList = list()
 names = names(orderAqList)  # List of all diffusions
 
 for (i in 1:length(names)) {
   nbdaList[[names[i]]] = nbdaData(label=names[i],
                                   assMatrix=bothNet,
                                   orderAcq = orderAqList[[names[i]]],
                                   timeAcq = timeAqList[[names[i]]], 
                                   endTime = lastTimeList[[names[i]]],
                                   presenceMatrix = presenceMatrixList[[names[i]]],
                                   asoc_ilv = asoc_int, 
                                   int_ilv = asoc_int)
 }  
 
 fullTadaModel<-tadaFit(nbdaList, method = "both", iterations =  200)
 # data.frame(Variable=fullTadaModel@varNames,MLE=fullTadaModel@outputPar,SE=fullTadaModel@se)
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
 cvMatrixFull = rbind(cvMatrix,cvMatrix,cvMatrix)
 baselineVect = rep(c("constant","weibull","gamma"),each=dim(cvMatrix)[1])
 
 # Run AIC table
 modelSet_tada = tadaAICtable(nbdadata = nbdaList,
                                 constraintsVectMatrix =cvMatrixFull,
                                 baselineVect =  baselineVect,
                                 cores=8,
                                 iterations=2000)
 

 networksSupport(modelSet_tada) # Compare support for networks; ok for homogeneous vs observed, not for social vs asocial
 baselineSupport(modelSet_tada) # Compare support for baselines
 
# Create a new set of models for model averaging
# All social, observed net, weibull baseline
 nbdaListObs = list()
 nbdaListObs[[names[i]]] = nbdaData(label= names[i],
                                       assMatrix = socNet,
                                       orderAcq = orderAqList[[names[i]]],
                                       timeAcq = timeAqList[[names[i]]], 
                                       endTime = lastTimeList[[names[i]]],
                                       presenceMatrix = presenceMatrixList[[names[i]]],
                                       asoc_ilv = asoc_int, 
                                       int_ilv = asoc_int)
 

 vObsSoc = as.matrix(expand.grid(c(1), c(0,2), c(0,3), c(0,4), c(0,5), c(0,6), c(0,7), c(0,8), c(0,9)))
 vObsAsoc = as.matrix(expand.grid(c(0), c(0,2), c(0,3), c(0,4), c(0,5), c(0), c(0), c(0), c(0)))
 cvMatrixObs = cbind(vObsSoc, vObsAsoc)
 baselineVectObs<-rep("weibull",each=dim(cvMatrixObs)[1])
 
modelSet_tadaObs<-tadaAICtable(nbdadata = nbdaListObs,
                                     constraintsVectMatrix = cvMatrixObs,
                                     baselineVect = baselineVectObs, 
                                     cores=8,
                                     iterations=2000)
 save(modelSet_tadaObs, file=paste0(savePath, "AICTable_Interactions_SocOnly_ObsOnly.Rdata")) 
   
 rbind(
   support=variableSupport(modelSet_tadaObs, includeAsocial = F),
   MAE=modelAverageEstimates(modelSet_tadaObs, includeAsocial = F),
   USE=unconditionalStdErr(modelSet_tadaObs, includeAsocial = F))

# Get weighted estimates
 ms = modelSet_tadaObs@printTable
 ms = subset(ms, deltaAICc <= 6)
 ms$AkaikeWeight = ms$AkaikeWeight / sum(ms$AkaikeWeight)
   
 ms = ms %>%
   mutate(ASOCIALhigh_flock = replace_na(ASOCIALhigh_flock, mean(ASOCIALhigh_flock, na.rm=T)))
 sum(ms$ASOCIALhigh_flock * ms$AkaikeWeight)
 
 sum(ms$ASOCIALinit * ms$AkaikeWeight)
 sum(ms$ASOCIALhigh_flock * ms$AkaikeWeight)
 sum(ms$ASOCIALlow_split * ms$AkaikeWeight, na.rm=T)
 sum(ms$ASOCIALlow_flock * ms$AkaikeWeight, na.rm=T)
 
 
 sum(ms$SOCIALinit * ms$AkaikeWeight, na.rm=T)
 sum(ms$SOCIALhigh_flock * ms$AkaikeWeight, na.rm=T)
 
 sum(ms$SOCIALlow_split * ms$AkaikeWeight, na.rm=T)
 sum(ms$SOCIALlow_flock * ms$AkaikeWeight, na.rm=T)
   
 # AIC tables: Models without interactions -------------------------------------------------------
   
   
   
   
   
   
   
   