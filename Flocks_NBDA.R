
# Install NBDA package if needed
 install.packages("remotes")
 remotes::install_github("whoppitt/NBDA")
 library(NBDA)

 # Load in data: ----
 dat.path = "/home/virginia/Dropbox/ChickadeeLab/Analysis/SEASON_19-20_FlockLearningNBDA/VersionForSubmit/"
 
# Networks
 load(paste0(dat.path, "GrpLearn_Networks4NBDA.RData"))
 openNet = networks.threshold[["open"]]           # Observed social network
 presenceNet = networks.null.binary[["open"]]     # Homologous social network
 
# Order-of-acquisition data
 load(paste0(dat.path, "Flocks_NBDA_Vectors.Rdata"))
 
 # ILVs
 ilvs = readRDS(paste0(dat.path, "Flocks_BirdInfo.Rds"))

# Convert networks into 3D arrays
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

#============================================================================================
# Extract ILVs 

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
   asoc_noint = c("init", "elevation", "assignmentType")         

 #==================================================================
 # MODELS WITH INTERACTIONS
 
# Create an NBDA model object
 nbdaList = list()
 names = names(orderAqList)
 
 for (i in 1:length(names)) {
   nbdaList[[names[i]]] = nbdaData(label=names[i],
                                   assMatrix=socNet,
                                   orderAcq = orderAqList[[names[i]]],
                                   timeAcq = timeAqList[[names[i]]], 
                                   endTime = lastTimeList[[names[i]]],
                                   presenceMatrix = presenceMatrixList[[names[i]]],
                                   asoc_ilv = asoc_int, 
                                   int_ilv = asoc_int)
 }  
 
 # Check variable names...
 fullTadaModel=tadaFit(nbdaList, method = "both", iterations =  200)
 data.frame(Variable=fullTadaModel@varNames,MLE=fullTadaModel@outputPar,SE=fullTadaModel@se)

# Specify a set of models to compare
# Network, MemScore(asocial), HighFlock(asocial), LowSplit(asocial), LowFlock(asocial), MemScore(social), HighFlock(social), LowSplit(social), LowFlock(social)

 constraintsVectMatrix=rbind(
  #social models
  c(1,0,0,0,0,0,0,0,0),
  
  c(1,2,0,0,0,0,0,0,0),
  c(1,2,3,4,5,0,0,0,0),
  c(1,2,0,0,0,0,7,8,9),
  c(1,2,3,4,5,0,7,8,9),
  
  c(1,2,0,0,0,6,0,0,0),
  c(1,2,3,4,5,6,0,0,0),
  c(1,2,0,0,0,6,7,8,9),
  c(1,2,3,4,5,6,7,8,9),
  
  c(1,0,0,0,0,6,0,0,0),
  c(1,0,3,4,5,6,0,0,0),
  c(1,0,0,0,0,6,7,8,9),
  c(1,0,3,4,5,6,7,8,9),
  
  c(1,0,3,4,5,0,0,0,0),
  c(1,0,0,0,0,0,7,8,9),
  c(1,0,3,4,5,0,7,8,9),
  
  #asocial models
  c(0,0,0,0,0,0,0,0,0),
  c(0,2,0,0,0,0,0,0,0),
  c(0,2,3,4,5,0,0,0,0),
  c(0,0,3,4,5,0,0,0,0))

#3x for 3 baseline functions:
constraintsVectMatrixAll = rbind(constraintsVectMatrix,constraintsVectMatrix,constraintsVectMatrix)
baselineVect = rep(c("constant","weibull","gamma"),each=dim(constraintsVectMatrix)[1])

#Run AIC table
modelSet_tada = tadaAICtable(nbdadata = nbdaList,
                            constraintsVectMatrix = constraintsVectMatrixAll,
                            baselineVect = baselineVect,cores=6)

# Repeat with homogeneous networks (all connections between birds sharing an assignment = 1)
# This was run seperately for Compute Reasons
nbdaList_homog = list()
names = names(orderAqList)

for (i in 1:length(names)) {
  nbdaList_homog[[names[i]]] = nbdaData(label=names[i],
                                        assMatrix=homogNet,
                                        orderAcq = orderAqList[[names[i]]],
                                        timeAcq = timeAqList[[names[i]]], 
                                        endTime = lastTimeList[[names[i]]],
                                        presenceMatrix = presenceMatrixList[[names[i]]],
                                        asoc_ilv = asoc_int, 
                                        int_ilv = asoc_int)
}  

# Constraints vectors
# No asocial models here, we did them already
constraintsVectMatrix2 = rbind(
  #social models
  c(1,0,0,0,0,0,0,0,0),
  
  c(1,2,0,0,0,0,0,0,0),
  c(1,2,3,4,5,0,0,0,0),
  c(1,2,0,0,0,0,7,8,9),
  c(1,2,3,4,5,0,7,8,9),
  
  c(1,2,0,0,0,6,0,0,0),
  c(1,2,3,4,5,6,0,0,0),
  c(1,2,0,0,0,6,7,8,9),
  c(1,2,3,4,5,6,7,8,9),
  
  c(1,0,0,0,0,6,0,0,0),
  c(1,0,3,4,5,6,0,0,0),
  c(1,0,0,0,0,6,7,8,9),
  c(1,0,3,4,5,6,7,8,9),
  
  c(1,0,3,4,5,0,0,0,0),
  c(1,0,0,0,0,0,7,8,9),
  c(1,0,3,4,5,0,7,8,9))

#3x for 3 baseline functions:
constraintsVectMatrixAll2 = rbind(constraintsVectMatrix2,constraintsVectMatrix2,constraintsVectMatrix2)
baselineVect2 = rep(c("constant","weibull","gamma"),each=dim(constraintsVectMatrix2)[1])

#Run AIC table
modelSet_tadaH<-tadaAICtable(nbdadata = nbdaList_homog,
                             constraintsVectMatrix =constraintsVectMatrixAll2,
                             baselineVect = baselineVect2,cores=6)

save(fullTadaModel, modelSet_tada, modelSet_tadaH, file = paste0(savePath, "FL_AICTabs.Rdata"))

#=============================================================================
# NO INTERACTIONS
# This time we ran the model with both type
asoc_noint = c("elevation", "assignmentType", "init")

nbdaList_noint = list()
names = names(orderAqList)

for (i in 1:length(names)) {
  nbdaList_noint[[names[i]]] = nbdaData(label=names[i],
                                        assMatrix=bothNet,
                                        orderAcq = orderAqList[[names[i]]],
                                        timeAcq = timeAqList[[names[i]]], 
                                        endTime = lastTimeList[[names[i]]],
                                        presenceMatrix = presenceMatrixList[[names[i]]],
                                        asoc_ilv = asoc_noint, 
                                        int_ilv = asoc_noint)
}  

fullTadaModel_noint<-tadaFit(nbdaList_noint)
data.frame(Variable=fullTadaModel_noint@varNames,MLE=fullTadaModel_noint@outputPar,SE=fullTadaModel_noint@se)

constraintsVectMatrix3 = 
  as.matrix(expand.grid(c(1), c(0), c(0,3), c(0,4), c(0,5), c(0,6), c(0,7), c(0,8)))

constraintsVectMatrix32 = 
  as.matrix(expand.grid(c(0), c(2), c(0,3), c(0,4), c(0,5), c(0,6), c(0,7), c(0,8)))

constraintsVectMatrix3 = rbind(constraintsVectMatrix3, constraintsVectMatrix32)

constraintsVectMatrix3_socOnly = 
  as.matrix(expand.grid(c(1), c(0,2), c(0,3), c(0,4), c(0,5), c(0,6), c(0,7)))


baselineVect3<-rep("weibull",each=dim(constraintsVectMatrix3)[1])
baselineVect3<-rep("weibull",each=dim(constraintsVectMatrix3_socOnly)[1])

modelSet_tada_noint<-tadaAICtable(nbdadata = nbdaList_noint,
                                  constraintsVectMatrix =constraintsVectMatrix3,
                                  baselineVect = baselineVect3,
                                  iterations = 200,
                                  cores=6)
#save(fullTadaModel, modelSet_tada, modelSet_tadaH, modelSet_tada_noint,modelSet_tadaBoth, modelSet_tadaSocOnly, file = paste0(savePath, "FL_AICTabs.Rdata"))
#save(modelSet_tada_noint, file = paste0(savePath, "AICTab_Noint_Homog"))

networksSupport(modelSet_tada_noint)

modelSet_tada_noint@printTable

lowerLimitsByModel_noint<-multiModelLowerLimits(which=1,aicTable = modelSet_tada_noint, conf=0.95)
save(lowerLimitsByModel_noint, file=paste0(savePath, "LowerLimitsByModel_NoInt.Rdata"))
lowerLimitsByModel_noint

modelSet_tada_noint_socOnly<-tadaAICtable(nbdadata = nbdaList_noint,
                                          constraintsVectMatrix = constraintsVectMatrix3_socOnly,
                                          baselineVect = baselineVect3,
                                          cores=6,
                                          iterations = 2000)


# save(modelSet_tada_noint_socOnly, file = paste0(savePath, "AICTab_NoInt_SocOnly.Rdata"))

rbind(
  support=variableSupport(modelSet_tada_noint_socOnly),
  MAE=modelAverageEstimates(modelSet_tada_noint_socOnly),
  USE=unconditionalStdErr(modelSet_tada_noint_socOnly))

View(modelSet_tada_noint_socOnly@printTable)
