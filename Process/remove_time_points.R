file = "all_time_points.tab"

classes = c("numeric","numeric","character","numeric","factor","numeric","factor","factor",  "factor", "character")
#           Row        Col         Name    Size     Media     Timepoint Temp    Array(1-6)   "Condition   "Well"  

df = read.table(file,header=T, colClasses=classes,sep="\t")

time_df = read.table("Maximal_Time_Points.txt",header=T,sep="\t")

out_df = as.data.frame(matrix(nrow = 0,ncol = ncol(df), dimnames = list(NULL,names(df))))

media = sort(unique(df$Media))

for(m in media){
  subMedia = subset(df, df$Media == m)
  
  temperatures = sort(unique(subMedia$Temp))
  
  for(temp in temperatures){
    
    subTemp = subset(subMedia,subMedia$Temp == temp)
    
    arrays = sort(unique(subTemp$Array))
    
    for(array in arrays){
      
      subArray = subset(subTemp, subTemp$Array == array)
      
      last = time_df$Max[time_df$Media == m &
                           time_df$Temp == temp &
                           time_df$Array == array]
      
      tempDF = subset(subArray , subArray$Time.Point <= last)
      
      out_df = rbind(out_df, tempDF)
      
    }
  }
}

write.table(out_df, "time_points_removed.tab",sep="\t",row.names = F)
      