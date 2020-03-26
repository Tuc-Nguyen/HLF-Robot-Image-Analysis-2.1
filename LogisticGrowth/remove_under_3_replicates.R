###Remove any strain containing less than 3 remaining technical replicates after prior filtering

library(dplyr)

modeled_file = "logistic_growth.omit_errors_lowcorr_outlierRate_outlierMaxSize.tab"

classes = c("numeric","numeric","character","factor",
            #Row        Col         Name      Media
            "factor","factor",  "factor", "character",
            #Temp    Array(1-6)  Condition    Well      
            "numeric","numeric","numeric", "numeric")
#             CC          R        MinSize     Corr

modeled_df = read.table(modeled_file,header=T, colClasses=classes,sep="\t")

arrays = sort(unique(modeled_df$Array))

conditions = sort(unique(modeled_df$Condition))

outlierDF = data.frame(Gene = character(),
                       Media = factor(),
                       Temp = factor(),
                       Array = factor(),
                       Well = character())


for(array in arrays){
  subArray = subset(modeled_df, modeled_df$Array == array)
  for(condition in conditions){
    subCondition = subset(subArray, subArray$Condition == condition)
    
    names = sort(unique(subCondition$Gene))
    for(name in names){
      block = subset(subCondition,subCondition$Gene == name)
      
      #script fails if block has too few spots
      #need to remove blocks with < cutoff spots
      
      if(nrow(block) < 3){
        block = block[,c(3,4,5,6,8)]
        outlierDF = rbind(outlierDF,block)
        
      }
    }
  }
}


outlier_removed_df = anti_join(modeled_df, outlierDF, by = c("Media" , "Temp", "Array", "Gene"))

write.table(outlier_removed_df, "logistic_growth.omit_errors_lowcorr_outlierRate_outlierMaxSize_under3Reps.tab",row.names = F, sep = "\t")
