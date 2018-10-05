# read the fire spread monthly data and find the vulnerable places
# By Ning Ren
# 2018-09-13

#install.packages("gsubfn")
library("gsubfn")
library('dplyr')
library('tidyr')
library('raster')


dir <-'./fire'
#dir<-'C:/Work_in_WSU2018/Data/RHESSys_output/jianning.ren_94787/fire'
#setwd(dir)
#dir()


filePattern <- "FireSpreadIter*.*.txt$"

fileList <-gtools::mixedsort(list.files(path=dir, recursive=TRUE,
                                        pattern=filePattern, full.names=TRUE))

# now get the file list 
testList<-fileList

print(head(testList))

print(length(testList))
# read these files as tibble this the best part 
data = tibble (File=testList) %>%  
  mutate(names= tools::file_path_sans_ext(basename(File))) %>%  # extract the file name
  mutate(month=gsubfn::strapply(names, "\\d+", as.numeric, simplify=TRUE)[2,]) %>% # extract the month
   mutate(year=gsubfn::strapply(names, "\\d+", as.numeric, simplify=TRUE)[1,]) %>% 
   mutate(yr_month = year*100 + month) %>% 
  dplyr::filter(yr_month >= 197908) %>% 
   mutate(Data = lapply(File, read.table)) 
  #mutate(names= tools::file_path_sans_ext(basename(File))) %>%  # extract the file name

print(head(data))
print(dim(data))
# group the result by month as calculate the sum, first convert all the numbers to 1 or zero

changeValues<-function(data){
  result<-ifelse(data>1,1,0)
  return(result)
}

data<- data %>% mutate(data2 = lapply(Data, changeValues))


# cut the fire size into two parts
patches <-raster('./patch_trai.tif')
patches.value<-patches[]

#########################
# build raster to read upper side and lower side
upper <- extent(222897.8, 250997.8, 4854129.72, 4869292)


lower <- extent(222897.8, 250997.8, 4835692, 4854129.72)

raster.upper <- crop(patches, upper)
raster.lower <- crop(patches, lower)





data <- data %>% dplyr::select(year,month, data2) 

print(head(data))
print(dim(data))

for ( i in 1: length(data$data2)){
# for ( i in 1:10) { 
  
 
  tmp=data$data2[[i]]
  

  January<-as.vector(t(tmp))
  
  January2<-floor(patches.value/1000000000+January) # keep the na value
  

  
  fire<-setValues(patches, January2)
  

  
  fire.upper<-crop(fire,raster.upper)
  fire.lower <- crop(fire, raster.lower)
  

  firesize.upper <- sum(fire.upper[], na.rm=T)
  firesize.lower <- sum(fire.lower[], na.rm=T)
  firesize.all <- sum(fire[], na.rm=T)
 
  data$fireUpper[[i]] = firesize.upper
  data$fireLower[[i]] = firesize.lower
  data$fireAll[[i]] = firesize.all
}



data4<-data%>% dplyr::select(year, month, fireUpper, fireLower, fireAll)

filename <- substr(fileList[1], 30,46)
save(data4, file=paste(filename,"Firepread_short_upper_lower.rds", sep="_"))
save(fileList, file=paste(filename,"Firepread_FileList_short.rds", sep="_"))

