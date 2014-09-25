getSamplesFromTruncatedNormal=function(sampleSize,mu,sdev){

  library(msm)
  set.seed(123497711)
  lowerPoint=mu
  sampledNums=as.integer(rtnorm(sampleSize,mean=mu,sd=sdev,upper=Inf,lower=lowerPoint)+.5)
  pValuesWithin3Sigma=(length(sampledNums[sampledNums<=(3*sdev)])/sampleSize)*100
  myHist=hist(sampledNums,labels=T,right=F,breaks=1:(sdev*6),col="cornflowerblue",xlab="Sampled integers")
  print(myHist)
  
  
  #write samples into file
  f=file("userSelectionNumberRtnorm.txt","w")
  write(sampledNums,f,sep="\n")
  close(f)

  #write parameters into file
  muSdev=data.frame(names=c("mu","sdev"),values=c(mu,sdev))
  write.table(muSdev,"muSdev.txt",sep=",")
  tempDf=read.csv("muSdev.txt")
  tempDf

}

mu=1;sdev=2;sampleSize=100000;
getSamplesFromTruncatedNormal(sampleSize,mu,sdev)