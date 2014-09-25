plotASetOfDatesValues=function(dates,...){#values will be generated using binomial
  values=rbinom(length(dates),size=1,prob=.5)
  plot(dates,values)
}

setWorkingDir<-function(){
  Sys.setenv(TZ='GMT')
  tryCatch({setwd("C:\\Users\\Van\\Google Drive\\rWorkspace\\data\\")},
           error=function(cond){ 
             setwd("C:\\Users\\mmoniruz\\Google Drive\\rWorkspace\\data\\")#lab
             #return (NA)
           },
           finally={}
  )
}

#it can take a set of check-in dates (interleaved by nCheck-ins) (e.g., cinput/auser-locFile)
#however, it ignores the nCheck-ins
#it generate all the dates between min and max date and draw whether 
#each date has a check-in or not (binary graph). Finally, draw a smooth
#fitting line

getDataFrameFromFile<-function(filename,eitherUserchOrSimulated){
  formats=c("%Y-%m-%d","%d-%b-%Y")
  whereDateAppears=c(2,4)
  separators=c("\t",",")
  
  currentFormat=formats[eitherUserchOrSimulated]
  currentAppearence=whereDateAppears[eitherUserchOrSimulated]
  currentSep=separators[eitherUserchOrSimulated]
  #cat("currentApprearence ",currentAppearence)#cat cannot handle date
  #cat("current sep ",currentSep)
  df<-read.table(filename,header=F,fill=T,sep=currentSep)
  print(head(df))
  #print(df[,currentAppearence])
  onlyDates<-strptime(df[,currentAppearence],format=currentFormat)
  head(onlyDates)
  
  mx=max(onlyDates)
  mn=min(onlyDates)
  #cat("max date ", mx)
  #cat("min date ",mn)
  nDaysBetMaxMin=as.integer(mx-mn)+1
  allDaysBetMaxMin<-seq(mn,by="24 hours",length.out=nDaysBetMaxMin)
  allDaysBetMaxMin
  datesFreq=data.frame(dates=allDaysBetMaxMin,freq=rep(0,nDaysBetMaxMin))
  
  for(i in 1:length(onlyDates)){
    #print(datesFreq[as.integer(onlyDates[i]-mn)+1,1])
    #print("vs")
    #print(onlyDates[i])
    if(datesFreq[as.integer(onlyDates[i]-mn)+1,1]!=onlyDates[i]){
      print("Hell! No")
      break
    }
    datesFreq[as.integer(onlyDates[i]-mn)+1,2]=1
  }
  return (datesFreq)
}

drawConvenienceBenchMark<-function(fileList){
  fileList="fileListInhistoryForConvenienceBenchMark.txt"
  library(ggplot2)
  setWorkingDir()
  allFile=read.table(fileList,header=F)
  head(allFile)
  k=1
  while(k<=nrow(allFile)-1){
    colours=c("black","blue")
    cat("k ",k)
  
    
    myPlot=qplot()
  
    for(i in 0:1){ #for some weird reasons, loop does not work
      print(paste("i ",i))
      fileToBeRead=paste("historyForConvenienceBenchMark\\",allFile[k+i,1],sep="")
      print(fileToBeRead)
      temp=read.table(fileToBeRead,header=F)
      #print(head(temp))
      datesFreq=getDataFrameFromFile(fileToBeRead,i+1)#1 represents a userch file, 2 represents a simulated file
      print(head(datesFreq))
      myPlot=myPlot+geom_smooth(data=data.frame(dates=datesFreq$dates,freq=datesFreq$freq,x=i+1),aes(dates,freq+(x[1]*.01)),se=F,col=colours[i+1])
      
      if(i==1){#then it is simulated history
        prevPath=getwd()
        setwd("C:\\Users\\Van\\Google Drive\\rWorkspace\\plotsForConvenienceBenchMark\\")
        png(paste(k,i,"conBenchMarkHist.png",sep=""))
        print(ggplot(datesFreq,aes(x=freq))+geom_histogram(binwidth=.05))
        dev.off()
        setwd(prevPath)
      }
    
    }
    #readline(paste(k," plot will be drawn now. Press Enter"))
    prevPath=getwd()
    setwd("C:\\Users\\Van\\Google Drive\\rWorkspace\\plotsForConvenienceBenchMark\\")
    png(paste(k,"conBenchMark.png",sep=""))
    print(myPlot)
    dev.off()
    setwd(prevPath)
    #readline(paste(k," plot is drawn now. Press Enter"))
  
    k=k+2
    }#end of while(k<nrow(allFile)-1)
}

drawConvenienceBenchMark("fileListInhistoryForConvenienceBenchMark.txt")
