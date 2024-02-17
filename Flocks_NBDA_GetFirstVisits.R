### Libraries used: ----
library(dplyr)

# Load in data: ----
dat.path = "/home/virginia/Dropbox/ChickadeeLab/Analysis/SEASON_19-20_FlockLearningNBDA/VersionForSubmit/"

# Networks
load(paste0(dat.path, "GrpLearn_Networks4NBDA.RData"))
  openNet = networks.threshold[["open"]]

# Visitation data
visits = readRDS(paste0(dat.path, "Flocks_NBDA_Visitation.Rds"))

# ILVs
ilvs = readRDS(paste0(dat.path, "Flocks_NBDA_BirdInfo.Rds"))
     
# Get matrix index position for all birds
visits = visits %>%
  mutate(netIndex = match(RFID, rownames(openNet))) %>%    # get the network matrix index for each tag
  filter(!is.na(netIndex))                               # remove birds that weren't in the network   


# get each bird's first visit to its assigned feeder
firstVisits = visits %>% group_by(BirdID, FeederID) %>%
  filter(Correct == 1) %>%
  slice(which.min(DaylightElapsed)) %>%
  ungroup() %>%
  arrange(FeederID, DaylightElapsed) %>%
  ungroup()

# get the last correct visit made to each feeder
lastVisits = visits %>%
  filter(Correct == 1) %>%
  group_by(FeederID) %>%
  slice(which.max(DaylightElapsed)) %>%
  ungroup()
#=======================================================
# Functions used to extract vectors for NBDA analysis 

# Get order-of-aquisition vector
order_aq_vector_get = function(df, feedGroup){
  orderAq = df %>%
    filter(.data$FeederID == feedGroup) %>%
    select(.data$netIndex) %>% .[["netIndex"]]  
  return(orderAq)}

# Get time-of-acquisition vector
time_aq_vector_get = function(df, feedGroup){
  timeAq = df %>%
    filter(.data$FeederID == feedGroup) %>%
    select(.data$DaylightElapsed) %>% .[["DaylightElapsed"]]   
  return(timeAq)}

# Get last visit to each feeder (end of diffusion)
last_time_vector_get = function(df_last, feedGroup){
  lastTime = df_last %>%
    filter(.data$FeederID == feedGroup) %>%
    select(.data$DaylightElapsed) %>% .[["DaylightElapsed"]]   
  return(lastTime)}

# Each feeder within the array is its own diffusion, so get a list of all feeders
feederNames = unique(firstVisits$FeederID)

orderAqList = list()
timeAqList = list()
lastTimeList = list()

# Feeders w/ too few acquisitions to use
# 3L_1, 8L_3, 8L_4
feederNames = feederNames[!(feederNames %in% c("3L_1", "8L_3", "8L_4"))]

for (i in 1:length(feederNames)) {
  label = feederNames[i]
  orderAqList[[label]] = order_aq_vector_get(firstVisits, feederNames[i])
  timeAqList[[label]] = time_aq_vector_get(firstVisits, feederNames[i])
  lastTimeList[[label]] = last_time_vector_get(firstVisits, feederNames[i])
}

# Get presence matrix
# Should specify which birds are available 
# e.g. birds at low are not available for diffusion that happened at high, and vice versa 
# 1 denotes that an individual was present in the diffusion 
# 0 denotes that an individual was absent, and so could neither learn nor transmit the behaviour
# recall that elevation the ILV is 0 for H, 1 for L
presenceMatrixList = list()

# Get vectors indicating which birds were present in each diffusion
# Since each diffusion is about learning a single feeder, only the birds assigned to that feeder are involved
feederGroups = ilvs$Target

names = names(timeAqList)
presenceMatrixList = list()
for (i in 1:length(names)) {
  presMat = matrix(nrow = length(feederGroups), ncol = length(orderAqList[[names[i]]]))
  
  presMat[,1:ncol(presMat)] = as.numeric(feederGroups == names[i])
  
  presenceMatrixList[[names[i]]] = presMat
}

save(orderAqList, timeAqList, lastTimeList, presenceMatrixList, 
     file = paste0(dat.path, "Flocks_NBDA_Vectors.Rdata"))

