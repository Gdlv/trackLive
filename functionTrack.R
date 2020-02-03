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


startRec<-function (obj=stream(), outputFolder, interval = 1, duration = Inf, format = "png",sgrey=F) {
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
    
    message(paste0("---------------------------------recording frame ",counter ))
    
    }
  release(obj)
  print(paste("images were taken at ",counter/duration," fps"))
}


startLiveTrack<-function (obj=stream(), outputFolder,threshold=145, polRvis=NULL, duration = 600,savePNG=F) {
  if (!isStream(obj)) 
    stop("This is not a Stream object.")
  outputFolder <- suppressWarnings(normalizePath(outputFolder))
  if (!file.exists(outputFolder)) {
    message("The output folder does not exist. It will be created.")
    dir.create(outputFolder)
  }
  if (!is.data.frame(polRvis[[1]])) {
    message("This is not a data frame with the coordinates. Please create the polygon.")
    img<-obj$readNext()
    display(img)
    polRvis<-selectROI(img)
    }
    
  counter <- 0
  end <- Sys.time() + duration 
  d<-data.frame("frame"=0,"mx"=500,"my"=270)
  
  
  while (Sys.time() < end) {
    img <- obj$readNext()
    img<-changeColorSpace(img, "GRAY")
    
    if(savePNG==T){img$write(paste0(outputFolder, "/", counter, ".png"))}
    
    w1<-cloneImage(img)
    fillPoly(w1,polRvis[[1]])
      
    w2<-cloneImage(w1-img)
    contours <- findContours(w2>threshold)
    idxy<-group_by(contours$contours,id) %>% summarise(mx=mean(x),my=mean(y),frame=counter)
    idxy$d<-idxy$mx*idxy$my
      
    wx<-abs(idxy$mx-d$mx[(counter-1)])
    wy<-abs(idxy$my-d$my[(counter-1)])
    wxy<-wx*wy
    suppressWarnings(
      idxy2<-idxy[which(wxy==min(wxy)),]
    )
    
    d<-rbind(d,idxy2[,c(4,2,3)])
    counter <- counter + 1
    
    print(paste0("--------------------------------- tracking" ))

    }
  write.csv(d,paste0(outputFolder, "/trackId.csv",sep=""))
  release(obj)
  print(paste("images were taken at ",counter/duration," fps"))
  assign(x ="polygonR",value = polRvis,envir = .GlobalEnv)
}


