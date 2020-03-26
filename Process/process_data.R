#####################################################################################
#######Exclude Data points from final analysis
#####################################################################################

####Read in data
classes = c("numeric","numeric","character","numeric","factor","numeric","factor","factor")
#           Row        Col         Name    Size     Media     Timepoint Temp    Array(1-6)

# setwd("./organize_data")

df = read.table("../Analyze/consolidated_data.txt",header=T, colClasses=classes,sep="\t")

##Add a column with "Condition whichis the combination of Media and Temperature
Condition = paste(df$Media, df$Temp, sep = "_")
Well=paste("r",df$Row,"c",df$Col,sep="")
df$Condition = factor(Condition)
df$Well = Well

################################
##CLEAN AND TRANSFORM THE DATA

##remove strains that are problematic
strain_remove_table = read.table("strains_to_remove.txt",header=T,sep="\t")

df = subset(df, !(df$Gene %in% strain_remove_table$Gene ))
            

##remove spots that are problematic
spot_remove_table = read.table("spots_to_remove.txt",header=T,sep="\t",
                               colClasses = c("factor","factor","factor",
                                              "numeric","numeric","character"))

library(dplyr)

df = anti_join(df, spot_remove_table, by = c("Array","Media" , "Temp", "Row","Column"))

##Remove the outer ring in each block, outer ring of whole plate, and second most outer ring of whole plate

unusable_rows = c(1,2,4,5,8,9,12,13,16,17,20,21,24,25,28,29,31,32) 
unusable_cols = c(1,2,8,9,16,17,24,25,32,33,40,41,47,48)

df = subset(df, !(df$Row %in% unusable_rows))
df = subset(df, !(df$Col %in% unusable_cols))

##At this stage, the df has had all the spots in the perimeters of the blocks removed
df = df[order(df$Array,df$Condition,df$Time.Point,df$Row,df$Col),]
df = subset(df, Media!="YPD")

####Output all the time points
write.table(df, "all_time_points.tab",row.names=F,sep="\t")


