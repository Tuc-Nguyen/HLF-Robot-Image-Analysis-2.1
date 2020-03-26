#####################################################################################
#######ANALYZE PHOTOS AND CONSOLIDATE DATA FOR ALL PHOTOS INTO ONE DATA FRAME
#####################################################################################


####gitter package is used to facilitate image analysis
require(gitter)

####Prepare folders and files for analysis

##read in file containing array file names
array_file_desc = read.table("array_file_desc.txt",header=T)

##import picture information from file
desc_image <- read.table("../Photos/photo_descriptions.txt", sep="\t", header=T, colClasses = c("factor"))


##create the folder for the gridded images
dir.create("gridded")
##delete any pre-existing gridded images
do.call(file.remove, list(list.files("gridded", full.names = TRUE)))
##create the folder for the analyzed data
dir.create("analysis_files")
##delete any pre-existing analysis files
do.call(file.remove, list(list.files("analysis_files", full.names = TRUE)))
##note: it will print TRUE for each file removed, don't be concerned by this output

####REAL ANALYSIS: Process the images to obtain size data 

##without reference image
gitter.batch(image.files=paste("../Photos/all", as.character(desc_image$FileName),sep = "/"),
             plate.format=1536,
             grid.save = "gridded",
             dat.save ="analysis_files",
             verbose='p')

#example code for use with a ref image, do not use unless necessary
#ref image
ref_image = "../Photos/ref/test_-05-24-17_22-09-50.JPG"
gitter.batch(image.files=
paste("../Photos/all", as.character(desc_image$FileName), sep = "/") ,
ref.image.file=ref_image, plate.format=1536, grid.save = "gridded", dat.save =
"analysis_files", verbose='p')

####Combine Information

## Combine the Size Data with the descriptive information

## process each line of the file individually. pick the data file, open it, read it and keep data.
## we take each row from the the desc_image and read the name of the text file in this row. 
## Then we associate each of the text file with the information in desc_image

## get the data using information from the desc_image file. this leads to image file etc.
## we go through each row, we ask for the name of the file
## and bind the information called data, selection, cond, time, colsizes to it

#set the folder where the analysis.txt files are located as a subdirectory of the working directory
folder = "analysis_files"

##create empty dataframe, must have same columns as array data frames and image_desc
df = data.frame(matrix(vector(), 0, 8, dimnames=list(
  c(), c("Strain","Column", "Row", "Size", "Media","Temp", "TimePoint","Array"))))


for (i in 1:nrow(desc_image)){  #starting from first row, and go to last one (nrow) in file desc_image
  fn = as.character(desc_image$FileName[i])
  file = paste(fn,"dat",sep=".")
  file = paste(folder,file,sep = "/")
  data = read.table(file, sep="\t", header=F) #we take the filenames e.g. IMG_123.txt
  
  ##analyze the image to get size data

  
  ##add in info from image description
  media = desc_image$Media[i]
  time.point = desc_image$TimePointByHour[i]
  temp = desc_image$Temp[i]
  array_number = desc_image$Array[i]
  

  print(i) #printing each line we worked with after another makes sure that we went through all lines in the file
  
  ##we read the text files according to the filename under desc_image[i,1]
  ##and stores the file in a new vector
  strain_info_file = as.character(
    array_file_desc$FileName[array_file_desc$Array == array_number]
    )
  
  ##modify the file name to point to correct folder
  strain_info_file = paste("../Arrays",strain_info_file,sep="/")
  
  ##read tha table containing which strains are in which row,column
  info = read.table(strain_info_file,header=T,sep="\t")
  

  ##sort the colsizes so it matches the ordering in info
  ##r 1 c 1, r1 c2, etc
  
  colsizes = data[,3]
                            #row      #column
  colsizes = colsizes[order(data[,1],data[,2])]
  
  info$Size = colsizes
  
  media = rep(media,nrow(info))
  info$Media = media
  time.point = rep(time.point,nrow(info))
  info$Time.Point = time.point
  temp = rep(temp,nrow(info))
  info$Temp = temp
  array_numbers = rep(array_number,nrow(info))
  info$Array = array_numbers
  
  df = rbind(df,info)
}

df$Time.Point = as.numeric(as.character(df$Time.Point))

write.table(df, "consolidated_data.txt", sep = "\t", row.names=F)

