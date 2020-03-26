###converts a matrix file to a table with "Row", "Col", and "Gene" columns

###speficy the input


inputs = c("Array.96.matrix.txt")

for(inputFileName in inputs){
  

##read in the matrix

array_values = scan(inputFileName,what="character",sep="\t")


##Determine the array type

array_type = length(array_values)

if(array_type == 96){
  num_col = 12
  num_row = 8
} else if(array_type == 384){
  num_col = 24
  num_ro1 = 16
} else if(array_type == 1536){
  num_col = 48
  num_row = 32
} else{
  stop("INVALID ARRAY SIZE")
}

###convert to data frame

rows = rep(seq(1:num_row), each = num_col)

columns = rep(seq(1:num_col),num_row)

output = data.frame(Row=rows,Column=columns,Gene=array_values)


##write output
base_file_name = strsplit(inputFileName, "\\.")[[1]][1]

fn = paste(base_file_name,array_type,"table","txt",sep=".")

write.table(output,fn, row.names = FALSE,sep="\t")


}



