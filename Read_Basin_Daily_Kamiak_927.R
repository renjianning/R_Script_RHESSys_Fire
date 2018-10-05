# read the daily files on kamiak to find the distribution

library('gsubfn')
library('dplyr')
library('tidyr')

dir<-'./fire'

filePattern <- '*\\d_basin.daily$'

fileList <- gtools::mixedsort(list.files(path=dir, recursive=TRUE,
                                         pattern=filePattern, full.names=TRUE))

data = tibble (File=fileList) %>%  
  mutate(names= tools::file_path_sans_ext(basename(File))) 
                                         
print(head(fileList))

importRHESSYS<-function(filename){
  
  tmp<-read.table(filename, sep=" ", header=F, skip=1)
  tmp_names<-readLines(filename,1)
  names(tmp)<-unlist(strsplit(tmp_names,split=" "))
 # tmp <- tmp %>% discard(~all(is.na(.x))) # remove the total na rows
  return(tmp)
  
}

fire.daily <- importRHESSYS(fileList[1])
fire.daily <- fire.daily %>% select(day, month, year, sat_def, unsat_stor, rz_storage, lai, plantc, litrc, evap, pet, trans, precip, tavg) %>% 
  mutate(index=1)


print(head(fire.daily))
print(tail(fire.daily))
print(length(fileList))
for (i in 2: length(fileList)) {
  
  file <- fileList[i]
  temp <- importRHESSYS(file)
  temp2 <- temp %>% select(day, month, year, sat_def, unsat_stor, rz_storage, lai, plantc, litrc, evap, pet, trans, precip, tavg) %>% 
    mutate(index=i)
  
  
  fire.daily <- rbind(fire.daily, temp2)
  
  
}


save(fire.daily, file=paste("output", data$names[1], "data.rds",sep="_"))

save(fileList, file=paste("output", data$names[1], "fileList.rds",sep="_"))







