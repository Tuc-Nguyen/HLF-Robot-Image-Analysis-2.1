##delete any pre-existing gridded images
do.call(file.remove, list(list.files("gridded", full.names = TRUE)))


file = "../../Process/all_time_points.tab"

classes = c("numeric","numeric","character","numeric","factor","numeric","factor","factor",  "factor", "character")
#           Row        Col         Name    Size     Media     Timepoint Temp    Array(1-6)   "Condition "Well"

df = read.table(file,header=T, colClasses=classes,sep="\t")

timepoints = sort(unique(df$Time.Point))

# df$Time.Point = as.numeric(as.character(df$Time.Point))

agg = aggregate(formula = Size ~ Gene + Condition + Array + Time.Point, data = df, FUN=mean)

require(ggplot2)

conditions = sort(unique(df$Condition))

arrays = sort(unique(df$Array))

for(condition in conditions){
  subCondition = subset(agg,agg$Condition == condition)
  
  for(array in arrays){
    subArray = subset(subCondition,subCondition$Array == array)
    
    title = paste(condition, "Array", array)
    
    print(ggplot(subArray,aes(x=as.numeric(Time.Point),y=Size,color=Gene,group=Gene)) + 
            geom_line() + 
            ggtitle(title) + 
            theme(legend.position="none"))
    fn = paste("growth_curves",condition,array,"png",sep=".")
    ggsave(fn,path="graphs")
    
  }
  
}

print(timepoints)
