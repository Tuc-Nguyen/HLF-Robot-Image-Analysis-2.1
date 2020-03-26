##Remove technical replicates for which the model did not have a good correlation

modeled_file = "logistic_growth.omit_errors.tab"

classes = c("numeric","numeric","character","factor",
            #Row        Col         Name      Media
            "factor","factor",  "factor", "character",
            #           Temp    Array(1-6)  Condition    Well      
            "numeric","numeric","numeric", "numeric")
#             CC          R        MinSize     Corr

modeled_df = read.table(modeled_file,header=T, colClasses=classes,sep="\t")


####SET ARBITRARY CORRELATION CUTOFF
corr_cutoff = 0.95

modeled_df = subset(modeled_df, modeled_df$Corr > corr_cutoff)


write.table(modeled_df, "logistic_growth.omit_errors_lowcorr.tab",sep="\t",row.names = F)
