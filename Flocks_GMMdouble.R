### STUDY NAME: Social group membership does not facilitate spatial learning of fine-scale resource locations
### SCRIPT: Flocks_GMMdouble.R
###
### DESCRIPTION: General steps for reading in data and running double GMMevents.
###              This script takes in feeder visitation data and creates 3 different types of networks needed for NBDA
###
### OUTPUT: "GrpLearn_GbisMetas.RData".    # File with results of double GMM
###         "GrpLearn_Networks4NBDA.RData" # File with the 3 types of networks needed for NBDA

### Libraries used: ----
library(asnipe)
library(dplyr)    
library(lubridate)

### Double GMM function:----
dblGMM <- function(meta_events, ids, data) { # Metadata from first GMM (in minutes), global ids, raw observation file
  
  gmmList  <- vector("list", length = nrow(meta_events)) # Store output GMM
  gbiList  <- vector("list", length = nrow(meta_events)) # Extract GBI from GMM and store
  metaList <- vector("list", length = nrow(meta_events)) # Extract Metadata from GMM and store
#  visList  <- vector("list", length = nrow(meta_events)) # Extract B (number of visits per event per bird) from GMM and store
  
  # We want to loop over all the time slices identified by minute GMM
  for (i in 1:nrow(meta_events)) {
    date_event <- subset(data, data$Date_Loc == meta_events[i,3]) # Date_Loc will be 3rd col in metadata, this will subset obs data by Date_Loc that match this time slice
    date_event <- subset(date_event, date_event$Time_min >= meta_events[i,1] & date_event$Time_min <= meta_events[i,2]) # Further subsetting our observed data to only fit the time window identified by the minute interval GMM (and current slice we are on in our loop! We use start [i,1] and stop [i,2] times to identify this time interval)
    
    if(nrow(date_event) <= 1) { # If the event is too short, you will get an error, so skip the event in this case...
      next()
    }
    
    # Now that everything is set up, we can run our GMM on this slice! (1 s resolution)
    
    gmmE = NULL
    
    try({
      gmmE <- gmmevents(time = date_event$Time_sec, 
                        identity = date_event$ID, 
                        location = date_event$Date_Loc, 
                        global_ids = ids)
    })
    
    gmmList[[i]]  <- gmmE
    gbiList[[i]]  <- gmmE$gbi
    metaList[[i]] <- gmmE$metadata
    
  }
  
  gmmList  <<- gmmList
  gbiList  <<- gbiList
  metaList <<- metaList
}

### Load in data: -----
dat_path <- "./icloud/LabThings/MOCHData/AnimalSocialNetworks/Flocks/"
dat_file <- "Flocks_VisitationData.RData"

load(paste0(dat_path, dat_file))

rm(dat_file)

# Run two rounds of GMMs ------
lapply(names(in.dat), function(x, 
                               input.data = in.dat,
                               dat.path   = dat_path){
  mode           <- strsplit(x, split = '[.]')[[1]][1]
  birds          <- in.dat[[x]]
  time.temp      <- strptime(paste(birds$Date, birds$Time, sep = " "), format = "%m/%d/%y %H:%M:%S")
  birds$Time_sec <- as.numeric(difftime(time.temp, min(time.temp), units = "secs") + 1)
  birds$Time_min <- round(birds$Time_sec/60) + 1
  birds$Date_Loc <- ""
  
  rm(time.temp)
  
  loc.tmp        <- strsplit(birds$Location, split = "[.]")
  loc.tmp        <- do.call("rbind", loc.tmp)
  birds$Feeder   <- birds$Location
  birds$Location <- loc.tmp[,1]
  birds$Date_Loc <- paste(mdy(birds$Date), birds$Location, sep = '_')
  
  rm(loc.tmp)
  
  birds      <- arrange(birds, Time_sec, Date_Loc)
  global_ids <- unique(birds$ID)

  # first round of GMM (@ 1 minute resolution)
  gmm <- gmmevents(time       = birds$Time_min, 
                   identity   = birds$ID, 
                   location   = birds$Date_Loc, 
                   global_ids = global_ids)
  
  # second round of GMM
  dblGMM(meta_events = gmm$metadata, 
         ids         = global_ids, 
         data        = birds)
  
  gbi  <- do.call("rbind", gbiList)
  
  meta <- do.call("rbind", metaList)
  tmp  <- strsplit(meta$Location, "_")
  tmp  <- do.call("rbind", tmp)
  
  meta$Location <- tmp[,2]
  meta$Date     <- tmp[,1]
  meta$FeedMode <- mode
  
  rm(tmp)
  
  save(gbi, meta, file = paste0(dat.path, x, "_dblGMM_GrpLearning.RData"))
  
})


# Combine everything into a single GBI for OPEN mode and one for ALL mode
# I bound these into a single file other places...
load(paste0(dat_path, "GrpLearn_GbisMetas.RData"))

open <- matrix(data = 0,
               nrow = dim(gbis$open.L)[1] + dim(gbis$open.H)[1], 
               ncol = dim(gbis$open.L)[2] + dim(gbis$open.H)[2])
open[1:dim(gbis$open.L)[1], 1:dim(gbis$open.L)[2]] <- gbis$open.L
open[(dim(gbis$open.L)[1]+1):dim(open)[1], (dim(gbis$open.L)[2]+1):dim(open)[2]] <- gbis$open.H

colnames(open) <- c(colnames(gbis$open.L), colnames(gbis$open.H))
meta.o <- rbind(metas$open.L, metas$open.H)
rownames(meta.o) <- seq(1,dim(meta.o)[1])

gbis$open  <- open
metas$open <- meta.o

all <- matrix(data  = 0,
               nrow = dim(gbis$all.L)[1] + dim(gbis$all.H)[1], 
               ncol = dim(gbis$all.L)[2] + dim(gbis$all.H)[2])

all[1:dim(gbis$all.L)[1], 1:dim(gbis$all.L)[2]] <- gbis$all.L
all[(dim(gbis$all.L)[1]+1):dim(all)[1], (dim(gbis$all.L)[2]+1):dim(all)[2]] <- gbis$all.H

colnames(all)    <- c(colnames(gbis$all.L), colnames(gbis$all.H))
meta.a           <- rbind(metas$all.L, metas$all.H)
rownames(meta.a) <- seq(1,dim(meta.a)[1])
metas$all        <- meta.a
gbis$all         <- all

save(gbis, in.dat, metas, file = paste0(dat_path, "GrpLearn_GbisMetas.RData"))

# Make 3 networks for open and all mode each ------
# 1) full net, 
# 2) edges thresholded to only include edges occurring between birds assigned to the same feeder
# 3) where all birds assigned to the same feeder have a '1' edge between them. --------
inds_path <- "./icloud/LabThings/MOCHData/AnimalSocialNetworks/Assort/"
inds_dat  <- "AssortElevIndDatETC.RData"
targ_dat  <- "PreGrpTestingSetup/FlockAssignments_2019_20.RData" # has flock testing assignments

load(paste0(inds_path, inds_dat))
load(paste0(dat_path , targ_dat))

networks4nbda <- function(which.dat.  ,
                          gbi.dat     ,
                          meta.dat    ,
                          meta.dat.col,
                          toic.dat    ,
                          toic.col    ,
                          toic.id.col ,
                          targ.dat    ,
                          targ.col    ,
                          targ.id.col ,
                          type.col    ,
                          test.ver.col,
                          which.ver.  ){
  
  require(asnipe)
  gbi      <- gbi.dat [[which.dat]]
  meta     <- meta.dat[[which.dat]]
  targ.dat <- subset(targ.dat, targ.dat[[test.ver.col]] == which.ver) #so we don't include two different targets
  
  toics <- data.frame(RFID   = colnames(gbi),
                      TOIC   = toic.dat[[toic.col]][match(colnames(gbi), toic.dat[[toic.id.col]])],
                      Target = targ.dat[[targ.col]][match(colnames(gbi), targ.dat[[targ.id.col]])],
                      stringsAsFactors = F)
  
  gbi   <- gbi[ ,order(toics$Target)]
  toics <- arrange(toics, Target)
  
  full <- get_network(association_data = gbi, 
                      data_format      = "GBI", 
                      times            = meta[[meta.dat.col]],
                      enter_time       = toics$TOIC)
  
  thresh     <- full
  bin.thresh <- full
  
  for(i in 1:dim(thresh)[1]){
    if(is.na(toics$Target[[i]])) {
      thresh    [i, ] <- 0
      bin.thresh[i, ] <- 0
    }else{
      thresh[i, which(toics$Target[[i]] != toics$Target)] <- 0
      thresh[i, which(is.na(toics$Target))]               <- 0
      
      bin.thresh[i, which(toics$Target[[i]] != toics$Target)] <- 0
      bin.thresh[i, which(toics$Target[[i]] == toics$Target)] <- 1
      bin.thresh[i, which(is.na(toics$Target))]               <- 0
    }
  }
  rm(i)
  diag(bin.thresh) <- 0
  
  inds       <- data.frame(RFID           = colnames(full),
                           Target         = toics$Target[match(rownames(full), toics$RFID)],
                           AssignmentType = targ.dat[[type.col]][match(rownames(full), targ.dat[[targ.id.col]])],
                           TestVersion    = which.ver,
                           stringsAsFactors = F)
  
  output <- list()
  output$sorted.gbi          <- gbi
  output$network.full        <- full
  output$network.threshold   <- thresh
  output$network.null.binary <- bin.thresh
  output$inds                <- inds
  
  return(output)
  
}

networks <- lapply(names(gbis), function(x){
                                 networks4nbda(which.dat    = x               ,
                                               gbi.dat      = gbis            ,
                                               meta.dat     = metas           ,
                                               meta.dat.col = "SubDate"       ,
                                               toic.dat     = birds           ,
                                               toic.col     = "sub.toic"      ,
                                               toic.id.col  = "RFID"          ,
                                               targ.dat     = flock.targets   ,
                                               targ.col     = "Target"        ,
                                               targ.id.col  = "RFID"          ,
                                               type.col     = "AssignmentType",
                                               test.ver.col = "TestVersion"   ,
                                               which.ver    = 1 # here 1 == first testing attempt, 2 == second at Low
                                              )
                                  })

names(networks) <- names(gbis)

gbis.sorted          <- lapply(networks, function(x){return(x$sorted.gbi         )})
networks.full        <- lapply(networks, function(x){return(x$network.full       )})
networks.threshold   <- lapply(networks, function(x){return(x$network.threshold  )})
networks.null.binary <- lapply(networks, function(x){return(x$network.null.binary)})
inds                 <- lapply(networks, function(x){return(x$inds               )})

network.dates <- list(open.L = c("2020-01-29", "2020-02-05"),
                      open.H = c("2020-02-13", "2020-02-19"),
                      all.L  = c("2020-02-05", "2020-02-10"),
                      all.H  = c("2020-02-19", "2020-02-24"))

network.dates <- lapply(network.dates, function(x){
                         names(x) <- c('begin', 'end')
                         return(x)
                       })

# saving out to a file----------
save(flock.targets, gbis, gbis.sorted, metas, network.dates, networks, networks.full, networks.null.binary, networks.threshold, new.old.ids, inds, 
     file = paste0(dat_path, "GrpLearn_Networks4NBDA.RData"))

