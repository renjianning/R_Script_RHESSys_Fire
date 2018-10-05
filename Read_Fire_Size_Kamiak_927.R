# read the daily files on kamiak to find the distribution

library('gsubfn')
library('dplyr')
library('tidyr')

dir<-'./fire'

filePattern <- 'FireSizes.txt$'

fileList <- gtools::mixedsort(list.files(path=dir, recursive=TRUE,
                                         pattern=filePattern, full.names=TRUE))

length(fileList)

firesize <- read.table(fileList[1])
firesize$index =1 

findNA<-vector()
maxline <-length(firesize$V1)

for (i in 2:length(fileList)) {
  filelist<-fileList[i]
  temp<-read.table(filelist)
  if(length(temp$V1) != maxline) { # here is to find extra firesize rows
    findNA[i]<-i
  }
  temp2 <-temp[!rev(duplicated((temp[length(temp$V1):1,2:3]))),] # here is to remove the repeated values 
  temp2$index <- i
  
  firesize<-rbind(firesize, temp2)
  
  
}

names(firesize)<-c("burn","year","month","wd","ws","ignition","index")

save.image('./LongRunFireSizeshort_long.RData')
