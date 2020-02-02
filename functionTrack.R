library(dplyr)
library(Rvision)
previousRec<-function(){
  my_stream <- stream() # 0 will start your default webcam in general. 
  done<-F
  a<-readNext(my_stream)
while (!done) {
    a<-readNext(my_stream,)
    message("the webcam is in another window")
    drawText(a, "stop from R or RStudio", 50, 50, thickness = 2)
    display(a)
  }
  release(my_stream)
}


startRec<-function (obj, outputFolder, interval = 1, duration = Inf, format = "png",sgrey=F) {
  if (!isStream(obj)) 
    stop("This is not a Stream object.")
  outputFolder <- suppressWarnings(normalizePath(outputFolder))
  if (!file.exists(outputFolder)) {
    message("The output folder does not exist. It will be created.")
    dir.create(outputFolder)
  }
  counter <- 0
  end <- Sys.time() + duration 
  while (Sys.time() < end) {
    img <- obj$readNext()
    if(sgrey==T){img<-changeColorSpace(img, "GRAY")}
    
    img$write(paste0(outputFolder, "/", counter, ".", 
                     format))
    
    
    counter <- counter + 1
    
  
    }
  release(obj)
}


startLiveTrack<-function (obj, outputFolder,threshold=145, interval = 1, duration = 600,savePNG=F) {
  if (!isStream(obj)) 
    stop("This is not a Stream object.")
  outputFolder <- suppressWarnings(normalizePath(outputFolder))
  if (!file.exists(outputFolder)) {
    message("The output folder does not exist. It will be created.")
    dir.create(outputFolder)
  }
  counter <- 0
  end <- Sys.time() + duration 
  d<-data.frame("frame"=0,"mx"=500,"my"=270)
  
  while (Sys.time() < end) {
    img <- obj$readNext()
    img<-changeColorSpace(img, "GRAY")
    
    if(savePNG==T){img$write(paste0(outputFolder, "/", counter, ".png"))}
    
    w1<-cloneImage(img)
    fillPoly(w1,s[[1]])
      
    w2<-cloneImage(w1-img)
    contours <- findContours(w2>threshold)
    idxy<-group_by(contours$contours,id) %>% summarise(mx=mean(x),my=mean(y),frame=i)
    idxy$d<-idxy$mx*idxy$my
      
    wx<-abs(idxy$mx-d$mx[(i-1)])
    wy<-abs(idxy$my-d$my[(i-1)])
    wxy<-wx*wy
    idxy2<-idxy[which(wxy==min(wxy)),]
    
    d<-rbind(d,idxy2[,c(4,2,3)])
    counter <- counter + 1
  }
  write.csv(d,paste0(outputFolder, "/trackId.csv",sep=""))
  release(obj)
}


