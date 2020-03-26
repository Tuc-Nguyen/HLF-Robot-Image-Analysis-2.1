source("log.fit.R") ##script for fitting models

##Specify the input file
##this file represents the processed size data after time points with issues near the end have been removed

time_points_removed = readline("Do you want to use data with time points removed? (Y/N):\n")

if(time_points_removed == "Y"){
  file = "../Process/time_points_removed.tab"
} else if(time_points_removed == "N"){
  file = "../Process/all_time_points.tab"
} else{
  stop("Invalid input. Please answer the question with Y or N. Re-run the script.")
}

file = "../Process/all_time_points.tab"

##specify the desired class for each column of data
classes = c("numeric","numeric","character","numeric","factor","numeric","factor","factor",  "factor", "character")
#           Row        Col         Name    Size     Media     Timepoint Temp    Array(1-6)   "Condition   "Well"  

##read in the data
df = read.table(file,header=T, colClasses=classes,sep="\t")

##Specify an output data frame to serve as a frame of reference
##the output data frame is identical to the initial data frame in format
##however, for each Strain, there will only be one row of data, instead of one for each time point
##So we subset the initial dataframe for just one time point
out_df = subset(df, df$Time.Point == 0)

##specify place holder vectors for our output variables
CC = vector(length=nrow(out_df)) ##carrying capacity
R = vector(length=nrow(out_df)) ##rate of growth
MinSize = vector(length = nrow(out_df)) ##initial population size
Corr = vector(length = nrow(out_df)) ##correlation of the model fit


##compile the place holder vectors into a single list
##so they can be passed through functions as a single variable
return_vectors = list(CC,R,MinSize,Corr) 
names(return_vectors) = c("CC","R","MinSize","Corr") #add names

####SPECIFY FUNCTIONS FOR HANDLING NORMAL CASES OR ERRORS
##The model can fail to fit the data for multiple reasons
##When this happens, the code will throw an error
##We want our code to continue when an error occurs
##and record information about which specific condition, strain, and spot caused the error

##To achieve this we will leverage the power of the tryCatch function in R
## tryCatch requires an expression you are trying to evaluate
## and a function to call when an error occurs
## We specify a function for each to handle the complexity of the call

##NORMAL_CASE

## During execution, this function is called to attempt to fit a model
## to the data for a single condition, strain, and spot
## takes in the list of output vectors
## and fills in the data for that specific condition, strain and spot
## each call to this function fills in one more "row" of the output vectors

##rv = the list of place holder vectors to be filled with output data
##subSpot = the data.frame containing data for the specific 

normal_case = function(rv,subSpot){
  ##fit the model by calling the log.fit function (separate R script)
  fm = log.fit("Size","Time.Point",subSpot) 
  
  ##fill in specific rows of the vectors of output data
  ##based on matching the correct row in the output data frame
  rv$CC[out_df$Condition == condition & 
          out_df$Array == array & 
          out_df$Gene == strain & 
          out_df$Well == spot] = fm[[1]][1] #Carrying capacity
  
  rv$R[out_df$Condition == condition & 
         out_df$Array == array & 
         out_df$Gene == strain & 
         out_df$Well == spot] = fm[[1]][2] #rate of growth
  
  rv$MinSize[out_df$Condition == condition & 
               out_df$Array == array & 
               out_df$Gene == strain & 
               out_df$Well == spot] = fm[[1]][3] #initial population size
  
  rv$Corr[out_df$Condition == condition & 
               out_df$Array == array & 
               out_df$Gene == strain & 
               out_df$Well == spot] = fm[[1]][4] #correlation
  
  ##return vectors of output data with output filled in
  ##for the specific condition, strain, and spot analyzed
  return(rv) 
}

##ERROR_CASE

##This function is run by tryCatch when an error is encountered
##The function does not try to fit a model, as the model failure to fit caused the error
##Instead, output is recored as "OMIT" in each output vector

##err = error message encountered during execution of normal_case()

error_case = function(err){
  ##we have to modify return_vectors directly here
  ##the error function in tryCatch has no clear way to accept them as a parameter
  title = paste("Logistic Fit Failure",subSpot$Condition[1],subSpot$Gene[1],subSpot$Well[1])
  
  print(ggplot(aes(Time.Point,Size),data=subSpot) +
          geom_point() +
          ggtitle(title)
  )
  ##save the graph, user must have previously created "model_success_graphs" folder
  ggsave(paste(title,"png",sep="."),path="model_failure_graphs")
  
  
  return_vectors$CC[out_df$Condition == condition &
                      out_df$Array == array &
                      out_df$Gene == strain &
                      out_df$Well == spot] = c("OMIT")
  
  return_vectors$R[out_df$Condition == condition &
                     out_df$Array == array &
                     out_df$Gene == strain &
                     out_df$Well == spot] = c("OMIT")
  
  return_vectors$MinSize[out_df$Condition == condition &
                           out_df$Array == array &
                           out_df$Gene == strain &
                           out_df$Well == spot] = c("OMIT")
  
  return_vectors$Corr[out_df$Condition == condition & 
            out_df$Array == array & 
            out_df$Gene == strain & 
            out_df$Well == spot] = c("OMIT")
  
  print(paste('ERROR',subSpot$Gene,subSpot$Condition,subSpot$Well))
  return(return_vectors)
}


####LOOP THROUGH THE DATA TO FIT THE MODEL

##make folders for graphs
dir.create("model_success_graphs")
dir.create("model_failure_graphs")

##delete old graphs
do.call(file.remove, list(list.files("model_success_graphs", full.names = TRUE)))
do.call(file.remove, list(list.files("model_failure_graphs", full.names = TRUE)))

##start by getting a list of each unique condition
conditions = sort(unique(df$Condition))


for(condition in conditions){
  ##subset the data for that condition
  subCondition = subset(df,df$Condition == condition)
  
  ##get a unique list of arrays in that condition
  arrays = sort(unique(subCondition$Array))
  
  
  for(array in arrays){
    ##subset the data for that specific array
    subArray = subset(subCondition, subCondition$Array == array)
    ##get a unique list of strain names within that condition
    strains = sort(unique(subArray$Gene))
    
    for(strain in strains){
      ##subset the data for that specific strain
      subStrain = subset(subArray, subArray$Gene == strain)
      
      ##get a unique list of spots
      ##each spot represents 1 technical replicate for that strain
      spots = sort(unique(subStrain$Well))
      
      for(spot in spots){
        ##data for all time points for a particular spot
        subSpot = subset(subStrain, subStrain$Well == spot)
        
        ##sort by time point ascending
        subSpot = subSpot[order(subSpot$Time.Point),]
        
        ##Turn values where size = 0 intonitial size = 1
        ##You can't have growth from a non-existent population so this causes model to fail
        
        subSpot$Size[subSpot$Size == 0] = 1
        

        ##Run the functions to fit the model
        ##This will fill in the output vector with the data
        ##or with "OMIT" if an error is encountered
        return_vectors = tryCatch(
          normal_case(return_vectors,subSpot),
          error = error_case)
          
      }
    }
  }
}

##Note: These vectors are character so that the OMITS can be includeded
##add the output vectors as columns to the output data.frame
out_df$CC = return_vectors$CC
out_df$R = return_vectors$R
out_df$MinSize= return_vectors$MinSize
out_df$Corr = return_vectors$Corr

#remove unnecessary columns from data.frame

out_df = out_df[,c(1,2,3,5,7,8,9,10,11,12,13,14)]

####Write the output to a tab delimited file
write.table(out_df,"logistic_growth.tab",row.names=F,sep="\t")

print("DONE")

