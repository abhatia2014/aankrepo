library(rvest)
#deciding on which TV show you want to download
tvshow='vikings'
#create download directory

directory=paste("~/Data Analysis/files/",tvshow,sep = "")
dir.create(directory,recursive = TRUE,showWarnings = FALSE)
setwd(directory)
getwd()
