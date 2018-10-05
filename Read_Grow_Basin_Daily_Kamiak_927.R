# read the daily files on kamiak to find the distribution

library('gsubfn')
library('dplyr')
library('tidyr')
library('purrr')

dir<-'./fire'
 
filePattern <- '*\\d_grow_basin.daily$'  ## \\d means numbers before grow

fileList <- gtools::mixedsort(list.files(path=dir, recursive=TRUE,
                                         pattern=filePattern, full.names=TRUE))
print(head(fileList))
print(length(fileList))

                                         
#fileList

importRHESSYS<-function(filename){
  
  tmp<-read.table(filename, sep=" ", header=F, skip=1)
  tmp_names<-readLines(filename,1)
  names(tmp)<-unlist(strsplit(tmp_names,split=" "))
  tmp <- tmp %>% purrr::discard(~all(is.na(.x))) # remove the total na rows
  return(tmp)
  
}

#filename<- fileList[1]

fire.grow_daily <- importRHESSYS(fileList[1])
fire.grow_daily <- fire.grow_daily %>% select(day, month, year, understory_biomassc, understory_leafc, understory_stemc, overstory_biomassc, overstory_leafc, overstory_stemc,
                                    gpsn, plant_resp, soil_resp, root_depth,soilc) %>% 
                                mutate(index=1)


head(fire.grow_daily)
tail(fire.grow_daily)

for (i in 2: length(fileList)) {
 
# for ( i in 2:3) {
 
  file <- fileList[i]
  temp <- importRHESSYS(file)
  temp2 <- temp %>% select(day, month, year, understory_biomassc, understory_leafc, understory_stemc, overstory_biomassc, overstory_leafc, overstory_stemc,
                           gpsn, plant_resp, soil_resp, root_depth,soilc) %>% 
                         mutate(index=i)
  
  
  fire.grow_daily <- rbind(fire.grow_daily, temp2)
  
  
}


data = tibble (File=fileList) %>%  
  mutate(names= tools::file_path_sans_ext(basename(File))) 


save(fire.grow_daily, file=paste("output",data$names[1], "data_1916-2017.rds",sep="_"))

save(fileList, file=paste("output",data$names[1], "fileList_1916_2017.rds",sep="_"))









