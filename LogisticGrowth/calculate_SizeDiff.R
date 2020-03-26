modeled_file = "logistic_growth.omit_errors_lowcorr_outlierRate_outlierMaxSize_under3Reps.tab"

classes = c("numeric","numeric","character","factor",
            #Row        Col         Name      Media
            "factor","factor",  "factor", "character",
            #Temp    Array(1-6)  Condition    Well      
            "numeric","numeric","numeric", "numeric")
#             CC          R        MinSize     Corr

modeled_df = read.table(modeled_file,header=T, colClasses=classes,sep="\t")

modeled_df$SizeDiff = modeled_df$CC - modeled_df$MinSize

write.table(modeled_df,"logistic_growth.SizeDiff.tab",row.names=F,sep="\t")
