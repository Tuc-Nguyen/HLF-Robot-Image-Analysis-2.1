####This script takes in a 96 array
####and outputs the 1536 array resulting from 1x96->1x384-> 1x1536

#The program outputs the 384 array as a matrix and row,col,gene table
#and the same for the 1536 array

#----INPUTS

#specify the list of input arrays
#program will loop and run each input seperately

inputs = c("Array.96.table.txt")

#note that the code here is the same for 16 or 32 blocks, only the inputs change


#####everything below this point should be automatic



for(inputFileName in inputs){
  
  classes = c("numeric","numeric","character")
  
  input = read.table(inputFileName, header=T,colClasses = classes) #read input data
  
  base_file_name = strsplit(inputFileName, "\\.")[[1]][1]
  
  #sort input by row then by column
  
  input = input[order(input$Row,input$Column),]
  
  ##build the 384 array
  
  genes_384 = vector(length=384)
  
  i = 1
  
  for(row in seq(1,8)){
    row_values = input$Gene[input$Row == row]
    
    dup_values = rep(row_values,each=2)
    
    genes_384[i:(i+23)] = dup_values
    
    i = i + 24
    
    genes_384[i:(i+23)] = dup_values
    
    i = i + 24
  }
  
  array384 = matrix(genes_384,nrow = 16, ncol = 24, byrow = T)
  
  fn = paste(base_file_name,"384","matrix","txt", sep = ".")
  
  write(t(array384),fn, ncolumns = 24, sep = "\t")
  
  
  
  ##build the 384 data frame
  rows = rep(seq(1:16), each = 24)
  
  columns = rep(seq(1:24),16)
  
  df384 = data.frame(Row=rows,Column=columns,Gene=genes_384)
  
  df384$Gene = as.character(df384$Gene)
  
  fn = paste(base_file_name,"384","table","txt",sep=".")
  
  write.table(df384,fn, row.names = FALSE,sep="\t")
  
  ###build the 1536 array
  
  genes_1536 = vector(length=1536)
  
  i = 1
  
  for(row in seq(1,16)){
    row_values = df384$Gene[df384$Row == row]
    
    dup_values = rep(row_values,each=2)
    
    genes_1536[i:(i+47)] = dup_values
    
    i = i + 48
    
    genes_1536[i:(i+47)] = dup_values
    
    i = i + 48
  }
  
  array1536 = matrix(genes_1536,nrow = 32, ncol = 48, byrow = T)
  
  fn = paste(base_file_name,"1536","matrix","txt", sep = ".")
  
  write(t(array1536),fn, ncolumns = 48, sep = "\t")
  
  
  ##build the 1536 data frame
  rows = rep(seq(1:32), each = 48)
  
  columns = rep(seq(1:48),32)
  
  df1536 = data.frame(Row=rows,Column=columns,Gene=genes_1536)
  
  df1536$Gene = as.character(df1536$Gene)
  
  fn = paste(base_file_name,"1536","table","txt",sep=".")
  
  write.table(df1536,fn, row.names = FALSE,sep="\t")
  
}


