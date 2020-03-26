####Detect outlier spots within individual blocks, and remove them
####These outlier spots are likely caused by issues within the source plates

library(dplyr)
library(grDevices)

modeled_file = "logistic_growth.omit_errors_lowcorr.tab"

classes = c("numeric","numeric","character","factor",
            #Row        Col         Name      Media
            "factor","factor",  "factor", "character",
            #           Temp    Array(1-6)  Condition    Well      
            "numeric","numeric","numeric", "numeric")
#             CC          R        MinSize     Corr

modeled_df = read.table(modeled_file,header=T, colClasses=classes,sep="\t")

outlierDF = data.frame(Gene = character(),
                       Media = factor(),
                       Temp = factor(),
                       Array = factor(),
                       Well = character())

conditions = sort(unique(modeled_df$Condition))

  for(condition in conditions){
    subCondition = subset(modeled_df, Condition == condition)
    
    names = sort(unique(subCondition$Gene))
    for(name in names){
      block = subset(subCondition,subCondition$Gene == name)
      
      #script fails if block has too few spots
      #need to remove blocks with < cutoff spots
      
      if(nrow(block) < 3){
        block = block[,c(3,4,5,6,8)]
        outlierDF = rbind(outlierDF,block)
        
      }else{
       outliers_values <- as.list(boxplot.stats(block$R)$out)
         block = subset(block, R %in% outliers_values)
         block = block[,c(3,4,5,6,8)]
         outlierDF = rbind(outlierDF,block)
       }
       
        }
      }

  


outlier_removed_df = anti_join(modeled_df, outlierDF, by = c("Media" , "Temp", "Array", "Well"))

write.table(outlier_removed_df, "logistic_growth.omit_errors_lowcorr_outlierRate.tab",row.names = F, sep = "\t")
