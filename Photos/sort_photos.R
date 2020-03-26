classes = c("character")


file_desc = read.table("photo_descriptions.txt",header=T,sep='\t', colClasses = classes)

arrays = sort(unique(file_desc$Array))

main_dir = getwd()

photo_dir = "all"


###Sort by Array, then Media, then Temperature

for(array in arrays){
  
  sub_desc = subset(file_desc,file_desc$Array == array)
  
  dir_name = paste("Array",array,"bytempbymedia",sep="")
  
  dir.create(dir_name)
  
  medias = sort(unique(sub_desc$Media))
  
  temps = sort(unique(sub_desc$Temp))
  
  for(media in medias){
    media_dir = paste(dir_name,media,sep="/")
    
    if(!(dir.exists(media_dir))){
      dir.create(media_dir)
    }
    for(temp in temps){
      temp_dir = paste(dir_name,media,temp,sep = "/")
      if(!(dir.exists(temp_dir))){
        dir.create(temp_dir)
      }
      
      file_names = sub_desc$FileName[sub_desc$Media == media & sub_desc$Temp == temp]
      
      for(name in file_names){
        photo_file = paste("all",name,sep="/")
        # dest = paste(sub_dir,name,sep="/")
        file.copy(photo_file,temp_dir)
      }
    }
  }
}

####Sort by Array, then by Media

# for(array in arrays){
#   
#   sub_desc = subset(file_desc,file_desc$Array == array)
#   
#   dir_name = paste("Array",array,"by_media",sep="")
#   
#   dir.create(dir_name)
#   
#   medias = sort(unique(sub_desc$Media))
#   
#   for(media in medias){
#     media_dir = paste(dir_name,media,sep="/")
#     
#     if(!(dir.exists(media_dir))){
#       dir.create(media_dir)
#     }
#     
#     file_names = sub_desc$FileName[sub_desc$Media == media]
#       
#     for(name in file_names){
#       dest = paste(media_dir,name,sep="/")
#       file.copy(name,media_dir)
#     }
#   }
# }
# 
# 

####Sort by Array only
# 
# for(array in arrays){
#   
#   sub_desc = subset(file_desc,file_desc$Array == array)
#   
#   dir_name = paste("Array",array,sep="")
#   
#   dir.create(dir_name)
#   
#   file_names = sub_desc$FileName
#     
#   for(name in file_names){
#       file.copy(name,dir_name)
#   }
# }

