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
dir()

#patches <-raster('../patch_trai.tif')
# list the 


#filePattern <- "RelDefGrid*.*.txt$"
#filePattern <- "LoadGrid*.*.txt$"
#filePattern <- "VegLoadGrid*.*.txt$"
filePattern <- "SoilMoistGrid*.*.txt$"


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
   filter(yr_month < 197908) %>%
   mutate(Data = lapply(File, read.table)) 
  #mutate(names= tools::file_path_sans_ext(basename(File))) %>%  # extract the file name



# group the result by month as calculate the sum, first convert all the numbers to 1 or zero





#data_short <- data %>% filter(year>=1979) %>% mutate(data2 = lapply(Data, setvalues))

#save(data2,file="Month1_fire_big_hs.rds")

# group the result by month

data <- data %>% dplyr::select(Data,month) %>% group_by(month) %>% 
 nest()
 


for ( i in 1: length(data$data)){
  
  tmp=data$data[[i]]
  result= Reduce('+', tmp$Data)
  data$fire3[[i]] = result
  
}



data4<-data%>% dplyr::select(month,fire3)

filename <- substr(fileList[1], 30,46)
filename2 <- substr(filePattern, 1,8)

save(data4, file=paste(filename, filename2,"Month_long.rds", sep="_"))
# read the short data


#load(file="Month_fire.rds")

## now plot those spatial patches
