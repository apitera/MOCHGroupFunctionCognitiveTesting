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
"GrpLearn_TestingResults.RData"




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
