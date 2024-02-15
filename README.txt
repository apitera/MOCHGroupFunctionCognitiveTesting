README file regarding the contents of the "MOCHGroupFunctionCognitiveTesting" repository
This repository contains code and data for associated with the unpublished work "Social group membership does not facilitate spatial learning of fine-scale resource locations"
Data may not be used without permission from the authors!

SCRIPTS:-------------------------
"Flocks_GMMdouble.R"
"Flocks_LM_Analysis.R"
"Flocks_LM_Figures.R"


FILES:---------------------------
"Flocks_VisitationData.RData"
"FlockAssignments_2019_20.RData"
"GrpLearn_GbisMetas.RData"
"GrpLearn_Networks4NBDA.RData"
"Flocks_TestingResults.RData"




============================================================================================
============================================================================================
SCRIPTS
============================================================================================
============================================================================================


-----------------------------------------
SCRIPT: "Flocks_GMMdouble.R"
-----------------------------------------
Runs a double Gaussian Mixture Model on automated feeder visitation data to identify grouping events
Returns group by individual matrices and their associated metadata
Calculates three types of social networks used in network-based diffusion analysis (NBDA)





============================================================================================
============================================================================================
FILES
============================================================================================
============================================================================================


-----------------------------------------
FILE "Flocks_TestingResults.RData"
-----------------------------------------
This file contains cognitive testing performance data

this file contains the following objects: 
	- longTestingResults
	- TestingResults
	
	


-----------------------------------------
FILE "Flocks_VisitationData.RData"
-----------------------------------------
This file contains feeder visitation data of PIT-tagged Mountain Chickadees to RFID-enabled bird feeders
This is used with the "Flocks_GMMdouble.R" script to produce group by individual matrices and ultimately social networks


-----------------------------------------
"FlockAssignments_2019_20.RData"
-----------------------------------------
Feeder assignment data for spatial cognitive testing (used by "Flocks_GMMdouble.R")



-----------------------------------------
FILE "GrpLearn_GbisMetas.RData"
-----------------------------------------
Intermediate file output by the "01_grpCogTest_GMMdouble.R" script. 
This file contains group by individual matrices (GBIs) and metadata connected to grouping events in each GBI


-----------------------------------------
FILE "GrpLearn_Networks4NBDA.RData"
-----------------------------------------
This file contains lists of networks to be used for NBDA on flock assignment cognitive tests

this file contains the following objects: 
		-flock.targets
		-gbis 
		-metas 
		-network.dates
		-gbis.sorted
		-networks
		-networks.full
		-networks.null.binary
		-networks.threshold
		-inds
		-new.old.ids
		


Many of these are lists, some are lists of lists, all of which (except 'network.dates') have the same names: *These pertain to the 'open' and 'all' modes run immediately before flock learning experiment!*
     -'open.L' indicating data taken when array feeders in 'open' mode at low elevation (arrays L3 & L8)
     -'open.H' indicating data taken when array feeders in 'open' mode at high elevation (arrays H1 & H3)
     -'all.L'  indicating data taken when array feeders in 'all' mode at low elevation (arrays L3 & L8)
     -'all.H'  indicating data taken when array feeders in 'all' mode at high elevation (arrays H1 & H3)
     -'open'   indicating data taken when array feeders in 'open' mode at both elevations (arrays L3, L8, H1, & H3)
     -'all'    indicating data taken when array feeders in 'all' mode at both elevations (arrays L3, L8, H1, & H3)




Below are brief descriptions of the objects contained in this file:

-flock.targets 		  - dataframe with the feeder assignments during the flock assignment cognitive tests, 
						   this includes all birds assigned during the experiment, both versions of the experiment


-gbis                 - list of gbis created from double GMMs, 
                        separated out by the 6 data types listed above
                 
                 
-metas         		  - list of metadata dataframes that accompany 'gbis' created from double GMMs, 
						   separated out by the 6 data types listed above
				 
				 
-network.dates 		  - list of beginning and end dates for data used to construct networks, 
						   I have only included the first 4 data types listed above
				 
				 
-gbis.sorted  		  - the same gbis as 'gbis', but column names (bird IDs) are sorted by their corresponding target feeder number, 
						  separated out by the 6 data types listed above
				 
-networks      		  - a list of lists separated out by the 6 data types listed above, 
						   within each there are the data for 'networks.full', 'networks.null.binary', 'networks.threshold', 'gbis.sorted', and 'inds' 
						  	--this is redundant, but I figured I'd just keep both ways I had the data stored.
						  	
-networks.full		  - list of association matrices with all calculated edges present, 
						rows and columns (both of which are bird IDs) are sorted by their corresponding target feeder number,
						birds that had not been assigned during this experiment or were double assigned have their calculated edge weights
		                     -these birds would be included in any analysis that uses these networks
						matrices are separated out by the 6 data types listed above
						
						
-networks.null.binary - list of association matrices with *BINARY* edges where edges are assigned based on target feeder number,
						birds assigned to the same target feeder have an edge of 1 and birds assigned to different targets have an edge of 0,
						rows and columns (both of which are bird IDs) are sorted by their corresponding target feeder number
						birds that had not been assigned during this experiment or were double assigned have all 0-value edges
							  -these birds would thus not be included in any analysis (even though they are present in the network)
						matrices are separated out by the 6 data types listed above
	

-networks.threshold   - list of association matrices with weighted edges where edges are assigned based on target feeder number
					    birds assigned to the same target feeder retain the edges calculated in 'networks.full', but birds assigned to different targets have an edge of 0,
					    rows and columns (both of which are bird IDs) are sorted by their corresponding target feeder number
					    birds that had not been assigned during this experiment or were double assigned have all 0-value edges
					  		  -these birds would thus not be included in any analysis (even though they are present in the network)
					    matrices are separated out by the 6 data types listed above
	

-inds - list of dataframes that include:
			- 'RFID'          : PIT-tag IDs of birds that are present in the three network types above -- the order of these IDs is the same as in the networks
			- 'Target'        : assigned target feeder 
			- 'AssignmentType': assignment type (either 'flock' or 'split'. birds with assignment type of 'both')
			- 'TestVersion'   : all birds in this file will have a 1 here, indicating these assignments are from the first run of this experiment
			these dataframes are separated out by the 6 data types listed above


-new.old.ids - a dataframe of the new and old PIT-tag IDs as of 05 Sept. 2020 to be used for updating PIT-tag IDs

-----------------------------------------
FILE "Flocks_NBDA_BirdInfo.Rds"
-----------------------------------------
This dataframe contains information about birds formatted for use as individual-level variables in NBDA analysis.  This is used with the "Flocks_NBDA.R" script.  This file only contains birds that were present in the social networks.
			- 'RFID'          : PIT-tag IDs of birds 
			- 'Target'        : assigned array and target feeder 
			- 'AssignmentType': assignment type; 'split = 0, flock' = 1
			- 'Elevation'	  : high elevation = 0, low elevation = 1
			- 'MemScore'	  : mean number of location errors in the first 20 trials of spatial learning and memory testing
			- 'MemScoreStd'   : 'MemScore' standardized to mean 0, SD 1
			- 'H.Split', 'L.Split', 'H.Flock', 'L.Flock': the four elevation * treatment types possible. 1 indicates that the bird was tested at the given elevation and treatment.

-----------------------------------------
FILE "Flocks_NBDA_Vectors.Rds"
-----------------------------------------
Intermediate file containing objects used for NBDA analysis.
			- 'orderAqList'   	: List of vectors, one for each feeder.  Contain the order in which birds (identified by matrix index) first visited their assigned feeder.
			- 'timeAqList'    	: List of vectors, one for each feeder.  Contain the elapsed time (in seconds) between feeder activation and each bird's first visit.
			- 'lastTimeList'    	: List of vectors, one for each feeder.  Contain the elapsed time (in seconds) between feeder activation and each bird's last visit.
			- 'presenceMatrixList '	: List of matrices, one for each feeder.  Each indicates which birds are assigned to that feeder, and therefore which birds coud participate in diffusion.

-----------------------------------------
FILE "Flocks_NBDA_Visitation.Rdata"
-----------------------------------------
This dataframe contains feeder visitation data of PIT-tagged birds to the feeder during the flock learning experiment, formatted for NBDA analysis
			- 'RFID'          : PIT-tag IDs of birds 
			- 'Date'	  : date of visit, ymd format
			- 'Time'	  : time of visit, hms format
			- 'FeederID'	  : ID of unique feeder within the array. First two characters indicate elevation and array identity, final character indicates the feeder number within the array.
			- 'Correct'	  : 1 indicates the bird visited its assigned feeder and was rewarded, 0 indicates it was not
			- 'DaylightElapsed': Time elapsed (in seconds) since the feeder was activated, not counting nighttime

-----------------------------------------
SCRIPT "Flocks_NBDA_GetFirstVisits.R"
-----------------------------------------
This script creates order-of-acquisition and time-of-acquisition vectors for each feeder discovery diffusion.

-----------------------------------------
SCRIPT "Flocks_NBDA.R"
-----------------------------------------
This script includes setting up the NBDA models, getting AIC tables, and obtaining model-averaged estimates for ILVs

-----------------------------------------
SCRIPT "Flocks_NBDA_GetILVsForAveraging.R"
-----------------------------------------
Some ILVs returned NAs when calculated in the AIC table; these models had to be run individually to retrieve them.
