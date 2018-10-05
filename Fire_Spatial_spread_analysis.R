# read the fire spread monthly data and find the vulnerable places
# By Ning Ren
# 2018-09-13



library(raster)
library(RColorBrewer) # for color palettes
library(fields)

## now plot those spatial patches
dir<-'C:/Users/PETBUser/Dropbox/Work_in_WSU/rhessys_model/my_own_notes_2018/WMFire_running_NWCC_conferences-20180831/Last_run102'
setwd(dir)
dir()

load(file='historical_fire_lo_Firepread_short.rds')
hf.month<-data4
load(file='historical_suppres_Firepread_short.rds')
hs.month<-data4

load(file='removecc_fire_long_Firepread_short.rds')
rf.month<-data4

load(file='removecc_suppressi_Firepread_short.rds')
rs.month<-data4

###########################
# now analyzing the long time period from 1916 to 1979 only historical fire and remove cc fire

load(file='historical_fire_lo_Firepread_long.rds')
hf.long<-data4

load(file='removecc_fire_long_Firepread_long.rds')
rf.long <- data4

load(file='Month_fire_historical_fire_long.rds')

hf.month_long <-data4

load(file='Month_fire_removecc_fire_long_2.rds')

rf.month_long <- data4
### load the long run data from 1916


load(file="spread_month_rf.rds")

#dir<-'c:/Work_in_WSU2018/Data/RHESSys_output'
#setwd(dir)
#dir()



patches <-raster('c:/Work_in_WSU2018/Data/RHESSys_output/patch_trai.tif')
patches.value<-patches[]

#########################
# build raster to read upper side and lower side
#upper <- extent(222897.8, 250997.8, 4854129.72, 4869292)


#lower <- extent(222897.8, 250997.8, 4835692, 4854129.72)

#raster.upper <- crop(patches, upper)
#raster.lower <- crop(patches, lower)

# create a polygon from a raster

trail_polygon <-rasterToPolygons(patches, fun=NULL, n=4, na.rm=TRUE)


## the ignition have tried for each month
total.ignition<- 100*(2017-1979)*2 # 2 ignition permonth 100 simulation * total years

# if for august the total ignition is 
total.ignition2 <- (100*(2017-1979)) *2 # for august the 

#total.ignition<- 100*(2017-1916+1)*2

# build the function

 plotMonth<- function(patches.value, data4, type, legend, minz, maxz){
   
   for (i in 1: length(data4$fire3)){
   #i=9

January<-as.vector(t(data4$fire3[[i]]))

January2<-floor(patches.value/1000000000+January) # keep the na value
proportion<-January2/total.ignition
print(max(proportion))

if (data4$month[[i]]==8) {
  
  proportion <- January2/total.ignition2
}

fire<-setValues(patches, proportion)

fire.crop<-crop(fire,trail_polygon)


if (type=="diff") {
  
min <- min(fire.crop[], na.rm=TRUE)
max <- max(fire.crop[], na.rm=TRUE)


breaks <- seq(minz, maxz, by=0.001)
#breaks <- seq(0, 0.01, by=0.001)

cols<-colorRampPalette(c("red", "purple", "magenta","yellow", "greenyellow","cyan3","gray","blue","darkblue", "black"))(length(breaks)-1)

par(cex=1.1,font=4,font.lab=4,font.axis=4,font.main=4,cex.lab=1.8,cex.main=1.8,cex.axis=1.8,lwd=2)

raster::plot(fire.crop, col=(cols), breaks=breaks,  zlim=c(min, max),xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=eval(legend), asp=1, add=FALSE, main=paste0("Month/",data4$month[[i]], sep=''))



}
else if (type =="state"){
  min=0
  max=0.02
  
  par(cex=1.2,font=4,font.lab=4,font.axis=4,font.main=4,cex.lab=1.8,cex.main=1.8,cex.axis=1.8,lwd=2)
  raster::plot(fire.crop, col=(cols), zlim=c(min, max),xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=eval(legend), asp=1, add=FALSE, main=paste0("Month/",data4$month[[i]], sep=''))
  #return(fire.crop)
  
}
#min =0
#max = 0.01

# col=(r),



#image.plot(fire.crop, zlim=c(min, max))
#plot(fire.crop)
}

 }
 #################################################
 # plot the result
 # set the color
 cols = rev(brewer.pal(11,'Spectral'))
 cols = c(cols,rep(cols[11],5))
 
 rf <- colorRampPalette(cols)   # make colors
 r <- rf(64)
 
 # set the name
 
 png(filename = "R_control_middle_diff.png",
     width = 15, height = 12, units = "cm",
     bg = "white", res = 300, family = "", restoreConsole = TRUE)
 par(mfrow=c(3,4),oma = c(0,0,0,0),mar = c(0,0,2,0)) 

 
 plotMonth(patches.value, hf.month, "state","T")
 dev.off()
 plotMonth(patches.value, hs.month, "state", "F")
 dev.off()
 plotMonth(patches.value, rf.month, "state", "F")
 dev.off()
 plotMonth(patches.value, rs.month, "state", "F")
 dev.off()
 
 plotMonth(patches.value, hf.long, "state", "F")
 dev.off()
 
 plotMonth(patches.value, rf.long, "state", "F")
 dev.off()

 
 #####################################################
 
# hf - rf
 fire.diff.month <- hf.month
 # calculate the differneces
 for (i in 1:length(rf.month$month)) {
 
 fire.diff <- hf.month$fire3[[i]] - rf.month$fire3[[i]]
 fire.diff.month$fire3[[i]]<- fire.diff

}
 
 plotMonth(patches.value, fire.diff.month, "diff", "F",-0.006, 0.004)
 dev.off()
 
 
 # hs - rs
 fire.diff.month.s <- hs.month
 
 for (i in 1:length(hs.month$month)) {
   
   fire.diff <- hs.month$fire3[[i]] - rs.month$fire3[[i]]
   fire.diff.month.s$fire3[[i]]<- fire.diff
   
 }
 plotMonth(patches.value, fire.diff.month.s, "diff", "F",-0.006, 0.004)
 dev.off()
 
 
 # hs - hf 
 fire.diff3 <- hs.month
 
 for (i in 1:length(hs.month$month)) {
   
   fire.diff <- hs.month$fire3[[i]] - hf.month$fire3[[i]]
   fire.diff3$fire3[[i]]<- fire.diff
   
 }
 plotMonth(patches.value, fire.diff3, "diff", "F",-0.006, 0.004)
 dev.off()
 
 
 # rs - rf
 
 fire.diff4<- rs.month
 
 for (i in 1:length(hs.month$month)) {
   
   fire.diff <- rs.month$fire3[[i]] - rf.month$fire3[[i]]
   fire.diff4$fire3[[i]]<- fire.diff
   
 }
 plotMonth(patches.value, fire.diff4, "diff", "F",-0.006, 0.004)
 dev.off()
 
 
 
 
 # plot the long
 
 fire.diff.long <- hf.long
 
 for( i in 1:length(hf.long$month)) {
   fire.diff <- hf.long$fire3[[i]] - rf.long$fire3[[i]]
   fire.diff.long$fire3[[i]] <- fire.diff
   
 }
 
 plotMonth(patches.value, fire.diff.long, "diff", "F",-0.006, 0.004)
 dev.off()
 
 
 
 
 ### set the color scale
 install.packages('rasterVis')
 library(rasterVis)
 
 breaks <- seq(-0.005, 0.003, by=0.0005)
 #breaks <- seq(0, 0.01, by=0.001)
 
 cols<-colorRampPalette(c("red", "purple", "magenta","yellow", "greenyellow","cyan3","gray","blue","darkblue", "black"))(length(breaks)-1)
 
 
 
 
 
 
 
 
 
 
 
 
 
 ##########################
 # only plot month 9
 png(filename = "Rplot%03dAUGUST.png",
     width = 15, height = 12, units = "cm",
     bg = "white", res = 300, family = "", restoreConsole = TRUE)
 par(mfrow=c(2,2),oma = c(0,0,0,0),mar = c(0,0,0.8,0)) 
 
 plotMonth(patches.value, hf.month)

 plotMonth(patches.value, hs.month)

 plotMonth(patches.value, rf.month)

 plotMonth(patches.value, rs.month)
 dev.off()
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
####################################

############################
# below is code working on kamiak

#install.packages("gsubfn")
library("gsubfn")
library('tidyverse')

dir<-'c:/Work_in_WSU2018/Data/RHESSys_output/jianning.ren_298389'
setwd(dir)
dir()

#patches <-raster('../patch_trai.tif')
# list the 

filePattern <- "FireSpreadIter*.*.txt$"

fileList <-gtools::mixedsort(list.files(path=dir, recursive=TRUE,
                                        pattern=filePattern, full.names=TRUE))

# now get the file list 
testList<-fileList[1:200]

testList<-fileList[1:200]

# count the spread larger than 100ha which is 100* 10000m2
firespread<-function(matrix){
  sum<-sum(matrix>0)
  spread<-ifelse(sum>100, 1, 0)
  return(spread)
}


#  first convert all the numbers to 1 or zero
setvalues<-function(data){
  result<-ifelse(data>1,1,0)
  return(result)
}


# read these files as tibble this the best part 
data = tibble (File=testList) %>% mutate(Data = lapply(File, read.table)) %>%  
  mutate(names= tools::file_path_sans_ext(basename(File))) %>%  # extract the file name
  mutate(month=gsubfn::strapply(names, "\\d+", as.numeric, simplify=TRUE)[2,]) %>% # extract the month
  mutate(year=gsubfn::strapply(names, "\\d+", as.numeric, simplify=TRUE)[1,])%>%  # extract the year
  mutate(spread = lapply(Data, firespread)) %>% 
           mutate(burn = lapply(Data, setvalues)) %>% 
   mutate(spread2 = as.numeric(lapply(Data, firespread)))
         
         #mutate(names= tools::file_path_sans_ext(basename(File))) %>%  # extract the file name
         
         
         
         
         
         
         
         #save(data2,file="Month1_fire_big_hs.rds")
         
         # group the result by month
         
         data2 <- data %>% dplyr::select(burn,month) %>% group_by(month) %>% 
           nest()
         
         spread.month <- data %>% dplyr::select(year,month, spread)
         
         for ( i in 1: length(data2$data)){
           
           tmp=data2$data[[i]]
           result= Reduce('+', tmp$burn)
           data2$fire3[[i]] = result
           
         }
         
         
         
         data4<-data2 %>% dplyr::select(month,fire3)
         
         save(data4, file="Month_fire_hs.rds")
         save(spread.month, file="spread_month_hs.rds")
         