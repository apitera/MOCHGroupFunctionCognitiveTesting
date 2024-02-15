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

#================================================
# Need MAEs for
# Social high flock 
# Social low flock 
# Social low split 


# Social high flock 
nbdaListm169 = list()
for (i in 1:length(names)) {
  nbdaListm169[[names[i]]] = nbdaData(label=names[i],
                                    assMatrix=socNet,
                                    orderAcq = orderAqList[[names[i]]],
                                    timeAcq = timeAqList[[names[i]]], 
                                    endTime = lastTimeList[[names[i]]],
                                    presenceMatrix = presenceMatrixList[[names[i]]],
                                    asoc_ilv = c("low_flock"), 
                                    int_ilv = c("low_flock", "high_flock"))
}  

m169=tadaFit(nbdaListm169, iterations =  200, baseline = "weibull")
data.frame(Variable=m169@varNames,MLE=m169@outputPar,SE=m169@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8037869 9.450103e+03
# 2                   Shape     0.4829339 5.145827e-02
# 3 1 Social transmission 1     1.3880298 1.291665e+00
# 4    2 Asocial: low_flock    -1.2618981 5.059938e-01
# 5     3 Social: low_flock     0.6279169 6.912901e-01
# 6    4 Social: high_flock     0.0563970 6.502909e-01


nbdaListm41 = list()
for (i in 1:length(names)) {
  nbdaListm41[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_flock"), 
                                      int_ilv = c("high_flock"))
}  

m41=tadaFit(nbdaListm41, iterations =  200, baseline = "weibull")
data.frame(Variable=m41@varNames,MLE=m41@outputPar,SE=m41@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8050181 5.779955e+03
# 2                   Shape     0.4774864 3.955039e-02
# 3 1 Social transmission 1     1.8461583 7.709746e-01
# 4    2 Asocial: low_flock    -1.0095413 4.191129e-01
# 5    3 Social: high_flock    -0.1990150 6.145747e-01


nbdaListm45 = list()
for (i in 1:length(names)) {
  nbdaListm45[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock", "low_split"), 
                                     int_ilv = c("high_flock"))
}  

m45=tadaFit(nbdaListm45, iterations =  200, baseline = "weibull")
data.frame(Variable=m45@varNames,MLE=m45@outputPar,SE=m45@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8040536 1.045137e+04
# 2                   Shape     0.4776333 5.196341e-02
# 3 1 Social transmission 1     1.9119030 1.202559e+00
# 4    2 Asocial: low_flock    -1.0347288 4.237776e-01
# 5    3 Asocial: low_split    -0.2537902 2.967193e-01
# 6    4 Social: high_flock    -0.2347318 6.810346e-01



nbdaListm173 = list()
for (i in 1:length(names)) {
  nbdaListm173[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock", "low_split"), 
                                     int_ilv = c("high_flock", "low_flock"))
}  

m173=tadaFit(nbdaListm173, iterations =  200, baseline = "weibull")
data.frame(Variable=m173@varNames,MLE=m173@outputPar,SE=m173@se)

# Variable           MLE         SE
# 1         Scale (1/rate):  2.117780e+04        NaN
# 2                   Shape  4.828563e-01        NaN
# 3 1 Social transmission 1  1.475131e+00        NaN
# 4    2 Asocial: low_flock -1.262009e+00 0.42756454
# 5    3 Asocial: low_split -2.199782e-01 0.01334807
# 6    4 Social: high_flock -4.047151e-03 0.63459361
# 7     5 Social: low_flock  5.672691e-01        NaN



nbdaListm105 = list()
for (i in 1:length(names)) {
  nbdaListm105[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                       timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_flock"), 
                                      int_ilv = c("high_flock", "low_split"))
}  

m105=tadaFit(nbdaListm105, iterations =  200, baseline = "weibull")
data.frame(Variable=m105@varNames,MLE=m105@outputPar,SE=m105@se)


# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8051042 6.945257e+03
# 2                   Shape     0.4835788 4.464148e-02
# 3 1 Social transmission 1     1.8891993 8.805427e-01
# 4    2 Asocial: low_flock    -1.0300472 4.223325e-01
# 5    3 Social: high_flock    -0.2554357 6.517900e-01
# 6     4 Social: low_split    -2.0448707 7.739353e+00



nbdaListm171 = list()
for (i in 1:length(names)) {
  nbdaListm171[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("high_flock", "low_flock"), 
                                      int_ilv = c("high_flock", "low_flock"))
}  

m171=tadaFit(nbdaListm171, iterations =  200, baseline = "weibull")
data.frame(Variable=m171@varNames,MLE=m171@outputPar,SE=m171@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8039251 8.541134e+03
# 2                   Shape     0.4955592 4.845736e-02
# 3 1 Social transmission 1     1.2975708 1.145404e+00
# 4   2 Asocial: high_flock     0.2653624 3.317776e-01
# 5    3 Asocial: low_flock    -1.2465051 5.032901e-01
# 6    4 Social: high_flock    -0.9383101 2.860615e+00
# 7     5 Social: low_flock     0.6618827 6.818830e-01


#
nbdaListm233 = list()
for (i in 1:length(names)) {
  nbdaListm233[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_flock"), 
                                      int_ilv = c("high_flock", "low_flock", "low_split"))
}  

m233=tadaFit(nbdaListm233, iterations =  200, baseline = "weibull")
data.frame(Variable=m233@varNames,MLE=m233@outputPar,SE=m171@se)

# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 6.503196e+03
# 2                   Shape  4.873991e-01 4.461585e-02
# 3 1 Social transmission 1  1.452977e+00 8.133333e-01
# 4    2 Asocial: low_flock -1.256426e+00 3.260609e-01
# 5    3 Social: high_flock -1.409434e-02 4.180825e-01
# 6     4 Social: low_flock  5.703471e-01 3.026599e+00
# 7     5 Social: low_split -1.988660e+00 1.995479e+01



nbdaListm43 = list()
for (i in 1:length(names)) {
  nbdaListm43[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("high_flock", "low_flock"), 
                                      int_ilv = c("high_flock"))
}  

m43=tadaFit(nbdaListm43, iterations =  200, baseline = "weibull")
data.frame(Variable=m43@varNames,MLE=m43@outputPar,SE=m43@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8059296 5.556830e+03
# 2                   Shape     0.4890003 4.093856e-02
# 3 1 Social transmission 1     1.7611812 7.285495e-01
# 4   2 Asocial: high_flock     0.2429590 3.311107e-01
# 5    3 Asocial: low_flock    -0.9835658 4.151479e-01
# 6    4 Social: high_flock    -1.0216570 2.298008e+00


nbdaListm170 = list()
for (i in 1:length(names)) {
  nbdaListm170[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init", "low_flock"), 
                                      int_ilv = c("high_flock", "low_flock"))
}  

m170=tadaFit(nbdaListm170, iterations =  200, baseline = "weibull")
data.frame(Variable=m170@varNames,MLE=m170@outputPar,SE=m170@se)

# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 9.787828e+03
# 2                   Shape  4.838514e-01 5.311601e-02
# 3 1 Social transmission 1  1.376686e+00 1.335748e+00
# 4         2 Asocial: init -4.247856e-02 1.350047e-01
# 5    3 Asocial: low_flock -1.259560e+00 5.081942e-01
# 6    4 Social: high_flock  4.808283e-02 6.570778e-01
# 7     5 Social: low_flock  6.355298e-01 7.160542e-01


nbdaListm40 = list()
for (i in 1:length(names)) {
  nbdaListm40[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init", "low_flock"), 
                                     int_ilv = c("high_flock"))
}  

m40=tadaFit(nbdaListm40, iterations =  200, baseline = "weibull")
data.frame(Variable=m40@varNames,MLE=m40@outputPar,SE=m40@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117781e+04 5.808234e+03
# 2                   Shape  4.780570e-01 3.975626e-02
# 3 1 Social transmission 1  1.841813e+00 7.731408e-01
# 4         2 Asocial: init -3.282765e-02 1.328115e-01
# 5    3 Asocial: low_flock -1.006279e+00 4.190606e-01
# 6    4 Social: high_flock -2.085282e-01 6.213883e-01


nbdaListm185 = list()
for (i in 1:length(names)) {
  nbdaListm185[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock"), 
                                     int_ilv = c("init","high_flock", "low_flock"))
}  

m185=tadaFit(nbdaListm185, iterations =  200, baseline = "weibull")
data.frame(Variable=m185@varNames,MLE=m185@outputPar,SE=m185@se)

# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 9.748125e+03
# 2                   Shape  4.823069e-01 5.292148e-02
# 3 1 Social transmission 1  1.393558e+00 1.330251e+00
# 4    2 Asocial: low_flock -1.261622e+00 5.090856e-01
# 5          3 Social: init -3.863189e-02 2.281113e-01
# 6    4 Social: high_flock  7.285629e-02 6.497213e-01
# 7     5 Social: low_flock  6.329015e-01 6.939168e-01


nbdaListm57 = list()
for (i in 1:length(names)) {
  nbdaListm57[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_flock"), 
                                      int_ilv = c("init","high_flock"))
}  

m57=tadaFit(nbdaListm57, iterations =  200, baseline = "weibull")
data.frame(Variable=m57@varNames,MLE=m57@outputPar,SE=m57@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117781e+04 5934.9051099
# 2                   Shape  4.773179e-01    0.0402067
# 3 1 Social transmission 1  1.848790e+00    0.7943660
# 4    2 Asocial: low_flock -1.008095e+00    0.4198985
# 5          3 Social: init -1.371518e-02    0.2621073
# 6    4 Social: high_flock -1.933197e-01    0.6283688



nbdaListm47 = list()
for (i in 1:length(names)) {
  nbdaListm47[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock", "low_split","low_flock"), 
                                     int_ilv = c("high_flock"))
}  

m47=tadaFit(nbdaListm47, iterations =  200, baseline = "weibull")
data.frame(Variable=m47@varNames,MLE=m47@outputPar,SE=m47@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8039982 9.501225e+03
# 2                   Shape     0.4895014 4.876981e-02
# 3 1 Social transmission 1     1.8231652 1.066015e+00
# 4   2 Asocial: high_flock     0.2446985 3.426989e-01
# 5    3 Asocial: low_split    -0.2529438 2.874270e-01
# 6    4 Asocial: low_flock    -1.0084807 4.202636e-01
# 7    5 Social: high_flock    -1.0717748 2.405583e+00




nbdaListm175 = list()
for (i in 1:length(names)) {
  nbdaListm175[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock", "low_split","low_flock"), 
                                     int_ilv = c("high_flock", "low_flock"))
}  

m175=tadaFit(nbdaListm175, iterations =  200, baseline = "weibull")
data.frame(Variable=m175@varNames,MLE=m175@outputPar,SE=m175@se)

# Variable           MLE        SE
# 1         Scale (1/rate): 21177.8020557       NaN
# 2                   Shape     0.4958019       NaN
# 3 1 Social transmission 1     1.3797988       NaN
# 4   2 Asocial: high_flock     0.2661729 0.2808106
# 5    3 Asocial: low_split    -0.2187406       NaN
# 6    4 Asocial: low_flock    -1.2462355 0.3952954
# 7    5 Social: high_flock    -1.0087542 2.5821883
# 8     6 Social: low_flock     0.5997987       NaN


nbdaListm109 = list()
for (i in 1:length(names)) {
  nbdaListm109[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_split","low_flock"), 
                                      int_ilv = c("high_flock", "low_split"))
}  

m109=tadaFit(nbdaListm109, iterations =  200, baseline = "weibull")
data.frame(Variable=m109@varNames,MLE=m109@outputPar,SE=m109@se)

# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8038790 1.038644e+04
# 2                   Shape     0.4795437 5.256066e-02
# 3 1 Social transmission 1     1.9179442 1.199241e+00
# 4    2 Asocial: low_split    -0.2093785 3.351287e-01
# 5    3 Asocial: low_flock    -1.0382544 4.244512e-01
# 6    4 Social: high_flock    -0.2483584 6.899130e-01
# 7     5 Social: low_split    -0.3347444 1.536763e+00


nbdaListm107 = list()
for (i in 1:length(names)) {
  nbdaListm107[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("high_flock","low_flock"), 
                                      int_ilv = c("high_flock", "low_split"))
}  

m107=tadaFit(nbdaListm107, iterations =  2000, baseline = "weibull")
data.frame(Variable=m107@varNames,MLE=m107@outputPar,SE=m107@se)

# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8077406 6.503196e+03
# 2                   Shape     0.4967392 4.461585e-02
# 3 1 Social transmission 1     1.7980547 8.133333e-01
# 4   2 Asocial: high_flock     0.2692893 3.260609e-01
# 5    3 Asocial: low_flock    -1.0035420 4.180825e-01
# 6    4 Social: high_flock    -1.3092153 3.026599e+00
# 7     5 Social: low_split    -2.9868892 1.995479e+01

nbdaListm46 = list()
for (i in 1:length(names)) {
  nbdaListm46[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init", "low_split","low_flock"), 
                                      int_ilv = c("high_flock"))
}  

m46=tadaFit(nbdaListm46, iterations =  2000, baseline = "weibull")
data.frame(Variable=m46@varNames,MLE=m46@outputPar,SE=m46@se)



# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 1.046883e+04
# 2                   Shape  4.779205e-01 5.220238e-02
# 3 1 Social transmission 1  1.908878e+00 1.204905e+00
# 4         2 Asocial: init -1.659396e-02 1.404739e-01
# 5    3 Asocial: low_split -2.511029e-01 2.967204e-01
# 6    4 Asocial: low_flock -1.032663e+00 4.239252e-01
# 7    5 Social: high_flock -2.392302e-01 6.868709e-01


nbdaListm61 = list()
for (i in 1:length(names)) {
  nbdaListm61[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_split","low_flock"), 
                                     int_ilv = c("init", "high_flock"))
}  

m61=tadaFit(nbdaListm61, iterations =  2000, baseline = "weibull")
data.frame(Variable=m61@varNames,MLE=m61@outputPar,SE=m61@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 1.154901e+04
# 2                   Shape  4.774771e-01 5.611597e-02
# 3 1 Social transmission 1  1.914414e+00 1.332257e+00
# 4    2 Asocial: low_split -2.537523e-01 3.119061e-01
# 5    3 Asocial: low_flock -1.033567e+00 4.252022e-01
# 6          4 Social: init -1.196594e-02 2.760974e-01
# 7    5 Social: high_flock -2.298317e-01 7.295463e-01



nbdaListm253 = list()
for (i in 1:length(names)) {
  nbdaListm253[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock","low_flock"), 
                                     int_ilv = c("high_flock", "low_split", "low_flock"))
}  

m253=tadaFit(nbdaListm253, iterations =  2000, baseline = "weibull")
data.frame(Variable=m253@varNames,MLE=m253@outputPar,SE=m253@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.7950774 1.185993e+04
# 2                   Shape     0.5013098 6.170828e-02
# 3 1 Social transmission 1     1.3600690 1.519974e+00
# 4   2 Asocial: high_flock     0.2843082 3.363329e-01
# 5    3 Asocial: low_flock    -1.2396596 5.251933e-01
# 6    4 Social: high_flock    -1.2231326 3.834412e+00
# 7     5 Social: low_split    -3.5435861 6.293683e+01
# 8     6 Social: low_flock     0.5996717 7.832595e-01



nbdaListm174 = list()
for (i in 1:length(names)) {
  nbdaListm174[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init", "low_split","low_flock"), 
                                      int_ilv = c("high_flock","low_flock"))
}  

m174=tadaFit(nbdaListm174, iterations =  2000, baseline = "weibull")
data.frame(Variable=m174@varNames,MLE=m174@outputPar,SE=m174@se)

# Variable           MLE        SE
# 1         Scale (1/rate):  2.117780e+04       NaN
# 2                   Shape  4.834714e-01       NaN
# 3 1 Social transmission 1  1.465444e+00       NaN
# 4         2 Asocial: init -2.833751e-02 0.1381984
# 5    3 Asocial: low_split -2.152690e-01 0.0600124
# 6    4 Asocial: low_flock -1.260224e+00 0.4293784
# 7    5 Social: high_flock -8.576552e-03 0.6390861
# 8     6 Social: low_flock  5.734124e-01       NaN




nbdaListm189 = list()
for (i in 1:length(names)) {
  nbdaListm189[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_split","low_flock"), 
                                      int_ilv = c("init", "high_flock","low_flock"))
}  

m189=tadaFit(nbdaListm189, iterations =  2000, baseline = "weibull")
data.frame(Variable=m189@varNames,MLE=m189@outputPar,SE=m189@se)

# Variable           MLE        SE
# 1         Scale (1/rate):  2.117780e+04       NaN
# 2                   Shape  4.822557e-01       NaN
# 3 1 Social transmission 1  1.480154e+00       NaN
# 4    2 Asocial: low_split -2.197364e-01 0.0643064
# 5    3 Asocial: low_flock -1.261791e+00 0.4315777
# 6          4 Social: init -3.700630e-02 0.1989355
# 7    5 Social: high_flock  1.217588e-02 0.6297164
# 8     6 Social: low_flock  5.725163e-01       NaN



nbdaListm237 = list()
for (i in 1:length(names)) {
  nbdaListm237[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_split","low_flock"), 
                                      int_ilv = c("high_flock", "low_split", "low_flock"))
}  

m237=tadaFit(nbdaListm237, iterations =  2000, baseline = "weibull")
data.frame(Variable=m237@varNames,MLE=m237@outputPar,SE=m237@se)

# Variable           MLE        SE
# 1         Scale (1/rate):  2.117780e+04       NaN
# 2                   Shape  4.834677e-01       NaN
# 3 1 Social transmission 1  1.480106e+00       NaN
# 4    2 Asocial: low_split -2.051929e-01 0.1565490
# 5    3 Asocial: low_flock -1.261241e+00 0.4265103
# 6    4 Social: high_flock -1.079482e-02 0.6402743
# 7     5 Social: low_split -1.291876e-01 1.6101395
# 8     6 Social: low_flock  5.622683e-01       NaN



nbdaListm106 = list()
for (i in 1:length(names)) {
  nbdaListm106[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init","low_flock"), 
                                      int_ilv = c("high_flock", "low_split"))
}  

m106=tadaFit(nbdaListm106, iterations =  2000, baseline = "weibull")
data.frame(Variable=m106@varNames,MLE=m106@outputPar,SE=m106@se)

# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 6.966109e+03
# 2                   Shape  4.839816e-01 4.479511e-02
# 3 1 Social transmission 1  1.885557e+00 8.819949e-01
# 4         2 Asocial: init -2.745307e-02 1.298098e-01
# 5    3 Asocial: low_flock -1.027319e+00 4.223439e-01
# 6    4 Social: high_flock -2.632129e-01 6.581677e-01
# 7     5 Social: low_split -1.988767e+00 7.298529e+00


nbdaListm121 = list()
for (i in 1:length(names)) {
  nbdaListm121[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_flock"), 
                                      int_ilv = c("init", "high_flock", "low_split"))
}  

m172=tadaFit(nbdaListm121, iterations =  2000, baseline = "weibull")
data.frame(Variable=m121@varNames,MLE=m121@outputPar,SE=m121@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117781e+04 7.151618e+03
# 2                   Shape  4.833632e-01 4.556528e-02
# 3 1 Social transmission 1  1.892821e+00 9.096055e-01
# 4    2 Asocial: low_flock -1.027998e+00 4.232780e-01
# 5          3 Social: init -1.931622e-02 2.525336e-01
# 6    4 Social: high_flock -2.473908e-01 6.674903e-01
# 7     5 Social: low_split -2.059615e+00 7.922076e+00


nbdaListm172 = list()
for (i in 1:length(names)) {
  nbdaListm172[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init", "high_flock","low_flock"), 
                                      int_ilv = c("high_flock", "low_flock"))
}  

m172=tadaFit(nbdaListm172, iterations =  2000, baseline = "weibull")
data.frame(Variable=m172@varNames,MLE=m172@outputPar,SE=m172@se)


# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 8.666474e+03
# 2                   Shape  4.960808e-01 4.833192e-02
# 3 1 Social transmission 1  1.289619e+00 1.156635e+00
# 4         2 Asocial: init -4.065248e-02 1.238322e-01
# 5   3 Asocial: high_flock  2.604217e-01 3.277417e-01
# 6    4 Asocial: low_flock -1.245146e+00 5.044480e-01
# 7    5 Social: high_flock -9.108756e-01 2.672959e+00
# 8     6 Social: low_flock  6.686189e-01 6.922496e-01



nbdaListm234 = list()
for (i in 1:length(names)) {
  nbdaListm234[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init", "low_flock"), 
                                      int_ilv = c("high_flock", "low_split", "low_flock"))
}  

m234=tadaFit(nbdaListm234, iterations =  2000, baseline = "weibull")
data.frame(Variable=m234@varNames,MLE=m234@outputPar,SE=m234@se)

# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 1.591161e+04
# 2                   Shape  4.880727e-01 8.064685e-02
# 3 1 Social transmission 1  1.442407e+00 2.063776e+00
# 4         2 Asocial: init -3.681920e-02 1.339513e-01
# 5    3 Asocial: low_flock -1.254586e+00 5.528283e-01
# 6    4 Social: high_flock -2.057682e-02 6.859085e-01
# 7     5 Social: low_split -1.895317e+00 1.375307e+01
# 8     6 Social: low_flock  5.775063e-01 9.345081e-01


nbdaListm249 = list()
for (i in 1:length(names)) {
  nbdaListm249[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_flock"), 
                                      int_ilv = c("init", "high_flock", "low_split", "low_flock"))
}  

m249=tadaFit(nbdaListm249, iterations =  2000, baseline = "weibull")
data.frame(Variable=m249@varNames,MLE=m249@outputPar,SE=m249@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 1.624605e+04
# 2                   Shape  4.867892e-01 8.228147e-02
# 3 1 Social transmission 1  1.458335e+00 2.103307e+00
# 4    2 Asocial: low_flock -1.255916e+00 5.574831e-01
# 5          3 Social: init -4.001849e-02 2.394844e-01
# 6    4 Social: high_flock  3.524064e-03 6.865586e-01
# 7     5 Social: low_split -2.011229e+00 1.585144e+01
# 8     6 Social: low_flock  5.756433e-01 9.173757e-01


nbdaListm187 = list()
for (i in 1:length(names)) {
  nbdaListm187[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("high_flock","low_flock"), 
                                      int_ilv = c("init", "high_flock", "low_flock"))
}  

m187=tadaFit(nbdaListm187, iterations =  2000, baseline = "weibull")
data.frame(Variable=m187@varNames,MLE=m187@outputPar,SE=m187@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 8.708505e+03
# 2                   Shape  4.958478e-01 4.989052e-02
# 3 1 Social transmission 1  1.295055e+00 1.173728e+00
# 4   2 Asocial: high_flock  2.680678e-01 3.322757e-01
# 5    3 Asocial: low_flock -1.246050e+00 5.036952e-01
# 6          4 Social: init  9.257705e-03 2.137832e-01
# 7    5 Social: high_flock -9.663684e-01 3.011847e+00
# 8     6 Social: low_flock  6.608516e-01 6.873847e-01


nbdaListm44 = list()
for (i in 1:length(names)) {
  nbdaListm44[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init", "high_flock","low_flock"), 
                                      int_ilv = c("high_flock"))
}  

m44=tadaFit(nbdaListm44, iterations =  2000, baseline = "weibull")
data.frame(Variable=m44@varNames,MLE=m44@outputPar,SE=m44@se)

# Variable           MLE           SE
# 1         Scale (1/rate):  2.117781e+04 5.567981e+03
# 2                   Shape  4.893713e-01 4.067391e-02
# 3 1 Social transmission 1  1.758684e+00 7.266837e-01
# 4         2 Asocial: init -3.249731e-02 1.238299e-01
# 5   3 Asocial: high_flock  2.403991e-01 3.257329e-01
# 6    4 Asocial: low_flock -9.811499e-01 4.150334e-01
# 7    5 Social: high_flock -1.017682e+00 2.221799e+00



nbdaListm59 = list()
for (i in 1:length(names)) {
  nbdaListm59[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock","low_flock"), 
                                     int_ilv = c("init", "high_flock"))
}  

m59=tadaFit(nbdaListm59, iterations =  200, baseline = "weibull")
data.frame(Variable=m59@varNames,MLE=m59@outputPar,SE=m59@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117781e+04 5.640111e+03
# 2                   Shape  4.903662e-01 4.143741e-02
# 3 1 Social transmission 1  1.742478e+00 7.493434e-01
# 4   2 Asocial: high_flock  2.570675e-01 3.213784e-01
# 5    3 Asocial: low_flock -9.847674e-01 4.149034e-01
# 6          4 Social: init  4.889464e-02 2.493665e-01
# 7    5 Social: high_flock -1.142862e+00 2.553274e+00



nbdaListm58 = list()
for (i in 1:length(names)) {
  nbdaListm58[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init","low_flock"), 
                                     int_ilv = c("init", "high_flock"))
}  

m58=tadaFit(nbdaListm58, iterations =  200, baseline = "weibull")
data.frame(Variable=m58@varNames,MLE=m58@outputPar,SE=m58@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 6629.2652401
# 2                   Shape  4.808443e-01    0.0454459
# 3 1 Social transmission 1  1.796073e+00    0.9387253
# 4         2 Asocial: init -8.053499e-02    0.2146037
# 5    3 Asocial: low_flock -1.003504e+00    0.4193067
# 6          4 Social: init  1.015989e-01    0.3641399
# 7    5 Social: high_flock -2.631579e-01    0.7082605

nbdaListm186 = list()
for (i in 1:length(names)) {
  nbdaListm186[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init","low_flock"), 
                                     int_ilv = c("init", "high_flock", "low_flock"))
}  

m186=tadaFit(nbdaListm186, iterations =  200, baseline = "weibull")
data.frame(Variable=m186@varNames,MLE=m186@outputPar,SE=m186@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 1.171398e+04
# 2                   Shape  4.841827e-01 6.338402e-02
# 3 1 Social transmission 1  1.373692e+00 1.601681e+00
# 4         2 Asocial: init -4.782373e-02 2.018174e-01
# 5    3 Asocial: low_flock -1.258633e+00 5.157830e-01
# 6          4 Social: init  1.249751e-02 3.201202e-01
# 7    5 Social: high_flock  4.128123e-02 6.862631e-01
# 8     6 Social: low_flock  6.338519e-01 7.877119e-01

nbdaListm111 = list()
for (i in 1:length(names)) {
  nbdaListm111[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("high_flock", "low_split", "low_flock"), 
                                      int_ilv = c("high_flock", "low_split"))
}  

m111=tadaFit(nbdaListm111, iterations =  200, baseline = "weibull")
data.frame(Variable=m111@varNames,MLE=m111@outputPar,SE=m111@se)

# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8037902 9.372682e+03
# 2                   Shape     0.4923971 4.915644e-02
# 3 1 Social transmission 1     1.8277027 1.056843e+00
# 4   2 Asocial: high_flock     0.2546557 3.423002e-01
# 5    3 Asocial: low_split    -0.1959687 3.305907e-01
# 6    4 Asocial: low_flock    -1.0122254 4.207797e-01
# 7    5 Social: high_flock    -1.1683323 2.649054e+00
# 8     6 Social: low_split    -0.4697577 1.804830e+00

nbdaListm63 = list()
for (i in 1:length(names)) {
  nbdaListm63[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("high_flock", "low_split", "low_flock"), 
                                      int_ilv = c("init", "high_flock"))
}  

m63=tadaFit(nbdaListm63, iterations =  200, baseline = "weibull")
data.frame(Variable=m63@varNames,MLE=m63@outputPar,SE=m63@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 1.004531e+04
# 2                   Shape  4.909004e-01 5.088683e-02
# 3 1 Social transmission 1  1.803865e+00 1.140342e+00
# 4   2 Asocial: high_flock  2.586919e-01 3.334907e-01
# 5    3 Asocial: low_split -2.528704e-01 2.949558e-01
# 6    4 Asocial: low_flock -1.009624e+00 4.201482e-01
# 7          5 Social: init  4.851895e-02 2.537918e-01
# 8    6 Social: high_flock -1.194177e+00 2.713022e+00

nbdaListm48 = list()
for (i in 1:length(names)) {
  nbdaListm48[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init", "high_flock", "low_split", "low_flock"), 
                                     int_ilv = c("high_flock"))
}  

m48=tadaFit(nbdaListm48, iterations =  200, baseline = "weibull")
data.frame(Variable=m48@varNames,MLE=m48@outputPar,SE=m48@se)

# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 9.464320e+03
# 2                   Shape  4.897392e-01 4.845356e-02
# 3 1 Social transmission 1  1.820620e+00 1.059762e+00
# 4         2 Asocial: init -1.852665e-02 1.294691e-01
# 5   3 Asocial: high_flock  2.437925e-01 3.401680e-01
# 6    4 Asocial: low_split -2.499967e-01 2.876200e-01
# 7    5 Asocial: low_flock -1.006552e+00 4.202942e-01
# 8    6 Social: high_flock -1.072709e+00 2.365324e+00

nbdaListm176 = list()
for (i in 1:length(names)) {
  nbdaListm176[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init", "high_flock", "low_split", "low_flock"), 
                                     int_ilv = c("high_flock", "low_flock"))
}  

m176=tadaFit(nbdaListm176, iterations =  200, baseline = "weibull")
data.frame(Variable=m176@varNames,MLE=m176@outputPar,SE=m176@se)
# Variable           MLE        SE
# 1         Scale (1/rate):  2.117780e+04       NaN
# 2                   Shape  4.962018e-01       NaN
# 3 1 Social transmission 1  1.371971e+00       NaN
# 4         2 Asocial: init -2.849149e-02 0.1279527
# 5   3 Asocial: high_flock  2.634358e-01 0.2677271
# 6    4 Asocial: low_split -2.140629e-01       NaN
# 7    5 Asocial: low_flock -1.245040e+00 0.3934824
# 8    6 Social: high_flock -9.941105e-01 2.5277651
# 9     7 Social: low_flock  6.057776e-01       NaN

nbdaListm239 = list()
for (i in 1:length(names)) {
  nbdaListm239[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("high_flock", "low_split", "low_flock"), 
                                      int_ilv = c("high_flock", "low_split", "low_flock"))
}  

m239=tadaFit(nbdaListm239, iterations =  200, baseline = "weibull")
data.frame(Variable=m239@varNames,MLE=m239@outputPar,SE=m239@se)
# Variable           MLE        SE
# 1         Scale (1/rate): 21177.8012234       NaN
# 2                   Shape     0.4971362       NaN
# 3 1 Social transmission 1     1.3874173       NaN
# 4   2 Asocial: high_flock     0.2705979 0.2759201
# 5    3 Asocial: low_split    -0.1912225       NaN
# 6    4 Asocial: low_flock    -1.2446089 0.3904454
# 7    5 Social: high_flock    -1.0653622 2.7346355
# 8     6 Social: low_split    -0.2680405 1.9271068
# 9     7 Social: low_flock     0.5907492       NaN


nbdaListm191 = list()
for (i in 1:length(names)) {
  nbdaListm191[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("high_flock", "low_split", "low_flock"), 
                                      int_ilv = c("init", "high_flock", "low_flock"))
}  

m191=tadaFit(nbdaListm191, iterations =  200, baseline = "weibull")
data.frame(Variable=m191@varNames,MLE=m191@outputPar,SE=m191@se)
# Variable           MLE        SE
# 1         Scale (1/rate):  2.117780e+04       NaN
# 2                   Shape  4.961467e-01       NaN
# 3 1 Social transmission 1  1.376853e+00       NaN
# 4   2 Asocial: high_flock  2.693592e-01 0.2939538
# 5    3 Asocial: low_split -2.187985e-01       NaN
# 6    4 Asocial: low_flock -1.245662e+00 0.4072902
# 7          5 Social: init  1.101342e-02 0.1981990
# 8    6 Social: high_flock -1.042498e+00 2.6546492
# 9     7 Social: low_flock  5.983857e-01       NaN


nbdaListm108 = list()
for (i in 1:length(names)) {
  nbdaListm108[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init", "high_flock", "low_flock"), 
                                      int_ilv = c("high_flock", "low_split"))
}  

m108=tadaFit(nbdaListm108, iterations =  200, baseline = "weibull")
data.frame(Variable=m108@varNames,MLE=m108@outputPar,SE=m108@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117781e+04 6.496832e+03
# 2                   Shape  4.968809e-01 4.425493e-02
# 3 1 Social transmission 1  1.796518e+00 8.096907e-01
# 4         2 Asocial: init -2.658729e-02 1.198433e-01
# 5   3 Asocial: high_flock  2.659662e-01 3.221055e-01
# 6    4 Asocial: low_flock -1.001709e+00 4.180906e-01
# 7    5 Social: high_flock -1.290579e+00 2.887817e+00
# 8     6 Social: low_split -2.819051e+00 1.680161e+01


nbdaListm110 = list()
for (i in 1:length(names)) {
  nbdaListm110[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("init", "low_split", "low_flock"), 
                                      int_ilv = c("high_flock", "low_split"))
}  

m110=tadaFit(nbdaListm110, iterations =  200, baseline = "weibull")
data.frame(Variable=m110@varNames,MLE=m110@outputPar,SE=m110@se)
# 
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 1.039623e+04
# 2                   Shape  4.798962e-01 5.278782e-02
# 3 1 Social transmission 1  1.914821e+00 1.200574e+00
# 4         2 Asocial: init -1.793714e-02 1.383905e-01
# 5    3 Asocial: low_split -2.055696e-01 3.351266e-01
# 6    4 Asocial: low_flock -1.036131e+00 4.245608e-01
# 7    5 Social: high_flock -2.535207e-01 6.960909e-01
# 8     6 Social: low_split -3.438261e-01 1.552771e+00

nbdaListm125 = list()
for (i in 1:length(names)) {
  nbdaListm125[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_split", "low_flock"), 
                                      int_ilv = c("init", "high_flock", "low_split"))
}  

m125=tadaFit(nbdaListm125, iterations =  200, baseline = "weibull")
data.frame(Variable=m125@varNames,MLE=m125@outputPar,SE=m125@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 1.140792e+04
# 2                   Shape  4.793747e-01 5.633468e-02
# 3 1 Social transmission 1  1.920825e+00 1.320561e+00
# 4    2 Asocial: low_split -2.091084e-01 3.483457e-01
# 5    3 Asocial: low_flock -1.036925e+00 4.259321e-01
# 6          4 Social: init -1.372635e-02 2.735291e-01
# 7    5 Social: high_flock -2.427176e-01 7.360713e-01
# 8     6 Social: low_split -3.366551e-01 1.539712e+00


nbdaListm137 = list()
for (i in 1:length(names)) {
  nbdaListm137[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_flock"), 
                                      int_ilv = c("low_flock"))
}  

m137=tadaFit(nbdaListm137, iterations =  200, baseline = "weibull")
data.frame(Variable=m137@varNames,MLE=m137@outputPar,SE=m137@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8037562 9466.6869240
# 2                   Shape     0.4830051    0.0515121
# 3 1 Social transmission 1     1.4063779    1.2952127
# 4    2 Asocial: low_flock    -1.2618129    0.5060928
# 5     3 Social: low_flock     0.6146099    0.6753050


nbdaListm141 = list()
for (i in 1:length(names)) {
  nbdaListm141[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c("low_split", "low_flock"), 
                                      int_ilv = c("low_flock"))
}  

m141=tadaFit(nbdaListm141, iterations =  200, baseline = "weibull")
data.frame(Variable=m141@varNames,MLE=m141@outputPar,SE=m141@se)
# Variable           MLE        SE
# 1         Scale (1/rate): 21177.8033134       NaN
# 2                   Shape     0.4828513       NaN
# 3 1 Social transmission 1     1.4736944       NaN
# 4    2 Asocial: low_split    -0.2198452       NaN
# 5    3 Asocial: low_flock    -1.2620528 0.4257436
# 6     4 Social: low_flock     0.5682836       NaN

nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                      assMatrix=socNet,
                                      orderAcq = orderAqList[[names[i]]],
                                      timeAcq = timeAqList[[names[i]]], 
                                      endTime = lastTimeList[[names[i]]],
                                      presenceMatrix = presenceMatrixList[[names[i]]],
                                      asoc_ilv = c( "low_flock"), 
                                      int_ilv = c("low_split", "low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8013057 1.469971e+04
# 2                   Shape     0.4873656 7.478386e-02
# 3 1 Social transmission 1     1.4480880 1.941016e+00
# 4    2 Asocial: low_flock    -1.2564947 5.417432e-01
# 5     3 Social: low_split    -1.9834336 1.430223e+01
# 6     4 Social: low_flock     0.5738163 8.836381e-01

nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c( "low_flock"), 
                                     int_ilv = c("low_split"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8055052 6.449831e+03
# 2                   Shape     0.4824738 4.277048e-02
# 3 1 Social transmission 1     1.8207585 8.385428e-01
# 4    2 Asocial: low_flock    -1.0029023 4.139284e-01
# 5     3 Social: low_split    -1.9549357 7.117550e+00


nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock", "low_flock"), 
                                     int_ilv = c("low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8042782 8.973047e+03
# 2                   Shape     0.4882827 4.884889e-02
# 3 1 Social transmission 1     1.2776356 1.174643e+00
# 4   2 Asocial: high_flock     0.1277660 2.288223e-01
# 5    3 Asocial: low_flock    -1.2553655 5.046803e-01
# 6     4 Social: low_flock     0.6966083 6.973102e-01

nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init", "low_flock"), 
                                     int_ilv = c("low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 9.789915e+03
# 2                   Shape  4.839151e-01 5.310232e-02
# 3 1 Social transmission 1  1.392139e+00 1.337969e+00
# 4         2 Asocial: init -4.288196e-02 1.348071e-01
# 5    3 Asocial: low_flock -1.259487e+00 5.082293e-01
# 6     4 Social: low_flock  6.242299e-01 7.011474e-01

nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock"), 
                                     int_ilv = c("init","low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117780e+04 9.765122e+03
# 2                   Shape  4.824452e-01 5.297307e-02
# 3 1 Social transmission 1  1.416648e+00 1.347998e+00
# 4    2 Asocial: low_flock -1.261661e+00 5.089686e-01
# 5          3 Social: init -3.527585e-02 2.246630e-01
# 6     4 Social: low_flock  6.156164e-01 6.824284e-01


nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock", "low_split", "low_flock"), 
                                     int_ilv = c("low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE        SE
# 1         Scale (1/rate): 21177.8035710       NaN
# 2                   Shape     0.4876430       NaN
# 3 1 Social transmission 1     1.3559821       NaN
# 4   2 Asocial: high_flock     0.1137883 0.1971051
# 5    3 Asocial: low_split    -0.2122352       NaN
# 6    4 Asocial: low_flock    -1.2560643 0.4123904
# 7     5 Social: low_flock     0.6387742       NaN


nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_split", "low_flock"), 
                                     int_ilv = c("low_split"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8041380 8.950239e+03
# 2                   Shape     0.4784311 4.772162e-02
# 3 1 Social transmission 1     1.8504901 1.076045e+00
# 4    2 Asocial: low_split    -0.2105761 3.210049e-01
# 5    3 Asocial: low_flock    -1.0118096 4.160663e-01
# 6     4 Social: low_split    -0.2841144 1.506128e+00

nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init", "low_split", "low_flock"), 
                                     int_ilv = c("low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE         SE
# 1         Scale (1/rate):  2.117780e+04        NaN
# 2                   Shape  4.834574e-01        NaN
# 3 1 Social transmission 1  1.462583e+00        NaN
# 4         2 Asocial: init -2.825538e-02 0.13816178
# 5    3 Asocial: low_split -2.150377e-01 0.05646788
# 6    4 Asocial: low_flock -1.260230e+00 0.42757711
# 7     5 Social: low_flock  5.753859e-01        NaN


nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_split", "low_flock"), 
                                     int_ilv = c("init", "low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE        SE
# 1         Scale (1/rate):  2.117780e+04       NaN
# 2                   Shape  4.822921e-01       NaN
# 3 1 Social transmission 1  1.484029e+00       NaN
# 4    2 Asocial: low_split -2.201558e-01 0.0613310
# 5    3 Asocial: low_flock -1.261881e+00 0.4294592
# 6          4 Social: init -3.651049e-02 0.1985862
# 7     5 Social: low_flock  5.698159e-01       NaN


nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_split", "low_flock"), 
                                     int_ilv = c("low_split", "low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE        SE
# 1         Scale (1/rate): 21177.8033259       NaN
# 2                   Shape     0.4834400       NaN
# 3 1 Social transmission 1     1.4762641       NaN
# 4    2 Asocial: low_split    -0.2052113 0.1508826
# 5    3 Asocial: low_flock    -1.2613248 0.4246097
# 6     4 Social: low_split    -0.1262188 1.5992268
# 7     5 Social: low_flock     0.5649816       NaN


nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock", "low_flock"), 
                                     int_ilv = c("low_split", "low_flock"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE           SE
# 1         Scale (1/rate): 21177.8021797 1.323658e+04
# 2                   Shape     0.4920444 6.586128e-02
# 3 1 Social transmission 1     1.3269536 1.644212e+00
# 4   2 Asocial: high_flock     0.1188385 2.444164e-01
# 5    3 Asocial: low_flock    -1.2507775 5.335445e-01
# 6     4 Social: low_split    -2.2170061 1.819274e+01
# 7     5 Social: low_flock     0.6487806 8.492798e-01


nbdaListm73 = list()
for (i in 1:length(names)) {
  nbdaListm73[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("init", "low_flock"), 
                                     int_ilv = c("low_split"))
}  

m73=tadaFit(nbdaListm73, iterations =  200, baseline = "weibull")
data.frame(Variable=m73@varNames,MLE=m73@outputPar,SE=m73@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117781e+04 6.463198e+03
# 2                   Shape  4.828013e-01 4.287962e-02
# 3 1 Social transmission 1  1.816004e+00 8.397385e-01
# 4         2 Asocial: init -2.401046e-02 1.292120e-01
# 5    3 Asocial: low_flock -9.997620e-01 4.139695e-01
# 6     4 Social: low_split -1.907084e+00 6.777206e+00


nbdaListm75 = list()
for (i in 1:length(names)) {
  nbdaListm75[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("high_flock", "low_flock"), 
                                     int_ilv = c("low_split"))
}  

m75=tadaFit(nbdaListm75, iterations =  200, baseline = "weibull")
data.frame(Variable=m75@varNames,MLE=m75@outputPar,SE=m75@se)
# Variable           MLE           SE
# 1         Scale (1/rate):  2.117781e+04 6.696835e+03
# 2                   Shape  4.837440e-01 4.237439e-02
# 3 1 Social transmission 1  1.793271e+00 8.230945e-01
# 4   2 Asocial: high_flock  3.595309e-02 2.465370e-01
# 5    3 Asocial: low_flock -9.928771e-01 4.173131e-01
# 6     4 Social: low_split -2.000821e+00 7.585441e+00

nbdaListm75 = list()
for (i in 1:length(names)) {
  nbdaListm75[[names[i]]] = nbdaData(label=names[i],
                                     assMatrix=socNet,
                                     orderAcq = orderAqList[[names[i]]],
                                     timeAcq = timeAqList[[names[i]]], 
                                     endTime = lastTimeList[[names[i]]],
                                     presenceMatrix = presenceMatrixList[[names[i]]],
                                     asoc_ilv = c("low_flock"), 
                                     int_ilv = c("init", "low_split"))
}  

m75=tadaFit(nbdaListm75, iterations =  200, baseline = "weibull")
data.frame(Variable=m75@varNames,MLE=m75@outputPar,SE=m75@se)