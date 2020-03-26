modeled_file = "logistic_growth.SizeDiff.tab"

classes = c("numeric","numeric","character","factor",
            #Row        Col         Name      Media
            "factor","factor",  "factor", "character",
            #Temp    Array(1-6)  Condition    Well      
            "numeric","numeric","numeric", "numeric","numeric")
#             CC          R        MinSize     Corr   SizeDiff

modeled_df = read.table(modeled_file,header=T, colClasses=classes,sep="\t")

normCC = vector(length = nrow(modeled_df))
normR = vector(length = nrow(modeled_df))
normSizeDiff = vector(length = nrow(modeled_df))

conditions = sort(unique(modeled_df$Condition))

for(condition in conditions){
  
  subCondition = subset(modeled_df,modeled_df$Condition == condition)
  
  for(array in sort(unique(subCondition$Array))){
    
    subArray = subset(subCondition, subCondition$Array == array)
    
    subRef = subset(subArray, subArray$Gene == "R")
    
    ref_CC_average = mean(subRef$CC)
    ref_R_average = mean(subRef$R)
    ref_SizeDiff_average = mean(subRef$SizeDiff)
    
    for(strain in sort(unique(subArray$Gene))){
      
      subStrain = subset(subArray, subArray$Gene == strain)
      
      for(spot in sort(unique(subStrain$Well))){
        #data for all time points for a particular spot
        subSpot = subset(subStrain, subStrain$Well == spot)
        
        CC_norm = subSpot$CC / ref_CC_average
        
        normCC[modeled_df$Condition == condition & 
                 modeled_df$Array == array & 
                 modeled_df$Gene == strain & 
                 modeled_df$Well == spot] = CC_norm
        
        R_norm = subSpot$R / ref_R_average
        
        normR[modeled_df$Condition == condition & 
                modeled_df$Array == array & 
                modeled_df$Gene == strain & 
                modeled_df$Well == spot] = R_norm
        
        SizeDiff_norm = subSpot$SizeDiff / ref_SizeDiff_average
        
        normSizeDiff[modeled_df$Condition == condition & 
                modeled_df$Array == array & 
                modeled_df$Gene == strain & 
                modeled_df$Well == spot] = SizeDiff_norm
      }
    }
  }
}


modeled_df$Norm_CC = normCC
modeled_df$Norm_R = normR
modeled_df$Norm_SizeDiff = normSizeDiff

write.table(modeled_df,"logistic_growth.normalized.tab",row.names=F,sep="\t")
