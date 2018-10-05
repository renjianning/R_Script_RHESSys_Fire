# read in the vegload, soilmoisture, and fuel laod
#By Ning Ren
# 20180930


library(raster)
library(RColorBrewer) # for color palettes
library(fields)

## now plot those spatial patches
dir<-'C:/Users/PETBUser/Dropbox/Work_in_WSU/rhessys_model/my_own_notes_2018/WMFire_running_NWCC_conferences-20180831/Last_run102/load_1916_1979'

setwd(dir)
dir()

# change place 1
filePattern<-'VegLoad*.*.rds$'
filePattern <-'_Load*.*.rds$'
filePattern <- 'RelD*.*.rds$'
filePattern <- 'SoilMoist*.*.rds$'



fileList <- gtools::mixedsort(list.files(path=dir, recursive=TRUE,
                                         pattern=filePattern, full.names=TRUE))
fileList
load(file=fileList[1])

VegLoad.hf <- data4

load(file=fileList[2])


VegLoad.rf <-data4

head(VegLoad.hf)



patches <-raster('c:/Work_in_WSU2018/Data/RHESSys_output/patch_trai.tif')
patches.value<-patches[]

# create a polygon from a raster

trail_polygon <-rasterToPolygons(patches, fun=NULL, n=4, na.rm=TRUE)



### set the color
cols = rev(brewer.pal(11,'Spectral'))
cols = c(cols,rep(cols[11],5))

rf <- colorRampPalette(cols)   # make colors
r <- rf(64)


# remove infite
remove.inf <- function(x) {
  replace(x, is.infinite(x),NA)
}

### plot the raster result
##########################

# total repeat is 
replicates = (1979-1916)*100

#####################


plotMonth.fuel<- function(patches.value, data4, var.leg, type){
  
  for (i in 1: length(data4$fire3)){
    #i=9
    
    January<-as.vector(t(data4$fire3[[i]]))
    
    January2<-floor(patches.value/1000000000000+January) # keep the na value
    January3<-January2/replicates

    
    fire<-setValues(patches, January3)
    
    fire.crop<-crop(fire,trail_polygon)
    
    
    if (type=="VegLoad"){
      min = 0
      max = 6000/replicates
    }
    
    else if (type=="Load") {
      min=0
      #min=0
      #max=max(fire.crop[], na.rm=TRUE)
      max=12500/replicates
    }
    else if (type =='Rel') {
      min=0
      #max=0.8
      max=max(fire.crop[], na.rm=TRUE)
    }
    
    else if (type == 'sm') {
      min =0.1
      max= max(fire.crop[], na.rm=TRUE)
      max=0.6
      
    }

        
    else if (type=="diff.veg") {
    min <--300/replicates
    max = 200/replicates
    breakPoint=10
    }
    
    else if (type=="diff.load") {
      
      min = min(fire.crop[], na.rm=T)
      max = max(fire.crop[], na.rm=T)
      min <--600/replicates
      max = 400/replicates
      breakPoint=10
    }
    
    else if (type == "diff.rel") {
      a <- remove.inf(fire.crop[])
      #min =min(a, na.rm=T)
      
      #max = max(a, na.rm=T)
      min = -900/replicates
      max = 600/replicates
      breakPoint = 10
      
    }
    
    else if (type =='diff.sm') {
     # min = min(fire.crop[], na.rm=T)
      #max = max(fire.crop[], na.rm=T)
      min = -150/replicates
      max = 100/replicates
      breakPoint = 10
    }
    
    if(type=="diff.veg" | type =="diff.load" | type=="diff.rel" | type=='diff.sm') {
      breaks <- seq(min, max, by=floor((max-min)/breakPoint))
      #breaks <- seq(0, 0.01, by=0.001)
      
      cols<-colorRampPalette(c("red", "purple", "magenta","yellow", "greenyellow","cyan3","gray","blue","darkblue", "black"))(length(breaks)-1)
      
      
    
    raster::plot(fire.crop, col=(cols), breaks=breaks, zlim=c(min, max),xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=eval(var.leg), asp=1, add=FALSE, main=paste0("Month/",data4$month[[i]], sep=''))

    }
    
    else {
      ### set the color
      cols = rev(brewer.pal(11,'Spectral'))
      cols = c(cols,rep(cols[11],5))
      
      rf <- colorRampPalette(cols)   # make colors
      r <- rf(64)
    
    raster::plot(fire.crop, col=(cols), zlim=c(min, max),xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=eval(var.leg), asp=1, add=FALSE, main=paste0("Month/",data4$month[[i]], sep=''))
    }
      
  }
  
}

# first plot the spatial data # change place 2 only need to change the last variable
par(mfrow=c(3,4),oma = c(0,0,0,0),mar = c(0,0,0.8,0)) 

plotMonth.fuel(patches.value, VegLoad.hf, "TRUE", "VegLoad")

plotMonth.fuel(patches.value, VegLoad.rf,"TRUE", "VegLoad")



## plot the load 
plotMonth.fuel(patches.value, VegLoad.hf, "TRUE", "Load")

plotMonth.fuel(patches.value, VegLoad.rf,"TRUE", "Load")

# plot the relative deficit

plotMonth.fuel(patches.value, VegLoad.hf, "TRUE", "Rel") 

plotMonth.fuel(patches.value, VegLoad.rf,"TRUE", "Rel")

plotMonth.fuel(patches.value, VegLoad.hf, "TRUE", "sm")




# vegetation  # run place 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

VegLoad.diff.month <- VegLoad.hf

for (i in 1:length(VegLoad.hf$month)) {
  
  fire.diff <- VegLoad.hf$fire3[[i]] - VegLoad.rf$fire3[[i]]
 
  VegLoad.diff.month$fire3[[i]]<- fire.diff
  
}

par(mfrow=c(3,4),oma = c(0,0,0,0),mar = c(0,0,0.8,0)) 

plotMonth.fuel(patches.value, VegLoad.diff.month,"TRUE", "diff.veg")

plotMonth.fuel(patches.value, VegLoad.diff.month,"TRUE", "diff.load")

plotMonth.fuel(patches.value, VegLoad.diff.month,"TRUE", "diff.rel")

plotMonth.fuel(patches.value, VegLoad.diff.month,"TRUE", "diff.sm")

