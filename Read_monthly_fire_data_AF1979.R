# read the fire spread monthly data and find the vulnerable places
# By Ning Ren
# 2018-09-13

#install.packages("gsubfn")
library("gsubfn")
library('dplyr')
library('tidyr')


dir <-'./fire'
#dir<-'C:/Work_in_WSU2018/Data/RHESSys_output/jianning.ren_94787/fire'
#setwd(dir)
#dir()

#patches <-raster('../patch_trai.tif')
# list the 

filePattern <- "FireSpreadIter*.*.txt$"

fileList <-gtools::mixedsort(list.files(path=dir, recursive=TRUE,
                                        pattern=filePattern, full.names=TRUE))

# now get the file list 
testList<-fileList

print(head(testList))

# read these files as tibble this the best part 
data = tibble (File=testList) %>%  
  mutate(names= tools::file_path_sans_ext(basename(File))) %>%  # extract the file name
  mutate(month=gsubfn::strapply(names, "\\d+", as.numeric, simplify=TRUE)[2,]) %>% # extract the month
   mutate(year=gsubfn::strapply(names, "\\d+", as.numeric, simplify=TRUE)[1,]) %>% 
   mutate(yr_month = year*100 + month) %>% 
   filter(yr_month >= 197908) %>% 
   mutate(Data = lapply(File, read.table)) 
  #mutate(names= tools::file_path_sans_ext(basename(File))) %>%  # extract the file name



# group the result by month as calculate the sum, first convert all the numbers to 1 or zero

setvalues<-function(data){
  result<-ifelse(data>1,1,0)
  return(result)
}

data<- data %>% mutate(data2 = lapply(Data, setvalues))

#data_short <- data %>% filter(year>=1979) %>% mutate(data2 = lapply(Data, setvalues))

#save(data2,file="Month1_fire_big_hs.rds")

# group the result by month

data <- data %>% dplyr::select(data2,month) %>% group_by(month) %>% 
 nest()
 


for ( i in 1: length(data$data)){
  
  tmp=data$data[[i]]
  result= Reduce('+', tmp$data2)
  data$fire3[[i]] = result
  
}



data4<-data%>% dplyr::select(month,fire3)

filename <- substr(fileList[1], 30,46)
save(data4, file=paste(filename,"Firepread_short.rds", sep="_"))
# read the short data


#load(file="Month_fire.rds")

## now plot those spatial patches
