require(ggplot2)

modeled_file = "logistic_growth.tab"

classes = c("numeric","numeric","character","factor",
            #Row        Col         Name      Media
            "factor","factor",  "factor", "character",
#           Temp    Array(1-6)  Condition    Well      
            "character","character","character", "character")
#             CC          R        MinSize     Corr

modeled_df = read.table(modeled_file,header=T, colClasses=classes,sep="\t")

modeled_df = subset(modeled_df, modeled_df$CC != "OMIT")

##Now that "OMITS" are removed
#T#he vectors can be turned into numeric format
modeled_df$CC = as.numeric(modeled_df$CC)
modeled_df$R = as.numeric(modeled_df$R)
modeled_df$MinSize = as.numeric(modeled_df$MinSize)
modeled_df$Corr = as.numeric(modeled_df$Corr)

write.table(modeled_df, "logistic_growth.omit_errors.tab",sep="\t",row.names = F)

