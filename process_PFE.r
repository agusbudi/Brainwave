rm(list=ls(all=TRUE))
annotname ="person1"
annotfolder =paste("PFE/",annotname,sep="")

position = c("TP9","TP10", "AF7", "AF8")
signal = c("delta","theta", "alpha", "beta","gamma")
songname = c("test1", "test2", "test3","test4","test5")

songnum= length(songname)

sink(paste(annotfolder,"/",annotname,"_result", ".csv", sep=""))
cat(paste("",signal,sep=","))
cat("\n")
cat("\n")

Results=matrix(NA, nrow = 7*songnum*4, ncol=5)
# Results[1,]=c("TP9",signal)
# Results[2,1]="min"
# Results[3,1]="Q1"
# Results[4,1]="Q2"
# Results[5,1]="Q3"
# Results[6,1]="max"
# Results[7,1]="mean"
# Results[8,1]="Std Dev"
minimaxi=matrix(0,nrow = 2, ncol = 5)
minimaxi[1,]=1000


k=0
j=1
for (j in 1:songnum){
  doc=read.table(paste(annotfolder,"/",songname[j],".csv", sep=""),header=TRUE, fill = TRUE,sep = ",", row.names = NULL)
  doc=as.matrix(doc)
  doc = doc[!is.na(doc[,2]),]
  linenumber = nrow(doc)
  for(infLoop in 1:ncol(doc)){
    doc[doc[,infLoop]== "-Inf", infLoop] <- -10
  }
  TP9=matrix(NA,nrow = linenumber, ncol = 6)
  TP9[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(TP9[1,1])
  
  TP9[1,1] = 0
  TP9[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  TP9[2:linenumber,1] = as.numeric(TP9[2:linenumber,1]) - firstnum
  
  TP9[,2]=as.numeric(doc[,2])
  TP9[,3]=as.numeric(doc[,6])
  TP9[,4]=as.numeric(doc[,10])
  TP9[,5]=as.numeric(doc[,14])
  TP9[,6]=as.numeric(doc[,18])
  
  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(TP9[,t+1])
    Results[2+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(TP9[,t+1])
    Results[6+idxRes,t]=mean(TP9[,t+1])
    Results[7+idxRes,t]=sd(TP9[,t+1])
  }
  k=k+1
  
  cat(paste("Left Ear_", annotname,"_",songname[j],"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")
  
  minval= min(TP9[,2:6])
  maxval= max(TP9[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_TP9", ".png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(TP9[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Left Ear Signal")
  lines(rep(0, max(TP9[,1])), type = "l", col = "black")
  lines(TP9[,2], type = "l", col = "red")
  lines(TP9[,3], type = "l", col = "blue")
  lines(TP9[,4], type = "l", col = "green")
  lines(TP9[,5], type = "l", col = "orange")
  lines(TP9[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()
  
  ##=============================================##
  AF7=matrix(NA,nrow = linenumber, ncol = 6)
  AF7[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(AF7[1,1])
  
  AF7[1,1] = 0
  AF7[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  AF7[2:linenumber,1] = as.numeric(AF7[2:linenumber,1]) - firstnum
  
  AF7[,2]=as.numeric(doc[,3])
  AF7[,3]=as.numeric(doc[,7])
  AF7[,4]=as.numeric(doc[,11])
  AF7[,5]=as.numeric(doc[,15])
  AF7[,6]=as.numeric(doc[,19])
  
  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(AF7[,t+1])
    Results[2+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(AF7[,t+1])
    Results[6+idxRes,t]=mean(AF7[,t+1])
    Results[7+idxRes,t]=sd(AF7[,t+1])
  }
  k=k+1
  cat(paste("Left Forehead_", annotname,"_",songname[j],"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")
  
  minval= min(AF7[,2:6])
  maxval= max(AF7[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_AF7", ".png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(AF7[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Left Forehead Signal")
  lines(rep(0, max(AF7[,1])), type = "l", col = "black")
  lines(AF7[,2], type = "l", col = "red")
  lines(AF7[,3], type = "l", col = "blue")
  lines(AF7[,4], type = "l", col = "green")
  lines(AF7[,5], type = "l", col = "orange")
  lines(AF7[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()
  
  ##=============================================##
  AF8=matrix(NA,nrow = linenumber, ncol = 6)
  AF8[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(AF8[1,1])
  AF8[1,1] = 0
  AF8[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  AF8[2:linenumber,1] = as.numeric(AF8[2:linenumber,1]) - firstnum
  
  AF8[,2]=as.numeric(doc[,4])
  AF8[,3]=as.numeric(doc[,8])
  AF8[,4]=as.numeric(doc[,12])
  AF8[,5]=as.numeric(doc[,16])
  AF8[,6]=as.numeric(doc[,20])
  
  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(AF8[,t+1])
    Results[2+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(AF8[,t+1])
    Results[6+idxRes,t]=mean(AF8[,t+1])
    Results[7+idxRes,t]=sd(AF8[,t+1])
  }
  k=k+1
  cat(paste("Right Forehead_", annotname,"_",songname[j],"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")
  
  minval= min(AF8[,2:6])
  maxval= max(AF8[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_AF8", ".png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(AF8[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Right Forehead Signal")
  lines(rep(0, max(AF8[,1])), type = "l", col = "black")
  lines(AF8[,2], type = "l", col = "red")
  lines(AF8[,3], type = "l", col = "blue")
  lines(AF8[,4], type = "l", col = "green")
  lines(AF8[,5], type = "l", col = "orange")
  lines(AF8[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()
  
  ##=====================================#
  TP10=matrix(NA,nrow = linenumber, ncol = 6)
  TP10[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
  firstnum = as.numeric(TP10[1,1])
  TP10[1,1] = 0
  TP10[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
  TP10[2:linenumber,1] = as.numeric(TP10[2:linenumber,1]) - firstnum
  
  TP10[,2]=as.numeric(doc[,5])
  TP10[,3]=as.numeric(doc[,9])
  TP10[,4]=as.numeric(doc[,13])
  TP10[,5]=as.numeric(doc[,17])
  TP10[,6]=as.numeric(doc[,21])
  
  idxRes=k*7
  for (t in 1:5){
    Results[1+idxRes,t]=min(TP10[,t+1])
    Results[2+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[2]
    Results[3+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[3]
    Results[4+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[4]
    Results[5+idxRes,t]=max(TP10[,t+1])
    Results[6+idxRes,t]=mean(TP10[,t+1])
    Results[7+idxRes,t]=sd(TP10[,t+1])
  }
  k=k+1
  cat(paste("Right Ear_", annotname,"_",songname[j],"\n", sep=""))
  cat("min,")
  cat(paste(as.character(Results[1+idxRes,]), collapse=","))
  cat("\n")
  cat("Q1,")
  cat(paste(as.character(Results[2+idxRes,]), collapse=","))
  cat("\n")
  cat("Q2,")
  cat(paste(as.character(Results[3+idxRes,]), collapse=","))
  cat("\n")
  cat("Q3,")
  cat(paste(as.character(Results[4+idxRes,]), collapse=","))
  cat("\n")
  cat("max,")
  cat(paste(as.character(Results[5+idxRes,]), collapse=","))
  cat("\n")
  cat("mean,")
  cat(paste(as.character(Results[6+idxRes,]), collapse=","))
  cat("\n")
  cat("Std Dev,")
  cat(paste(as.character(Results[7+idxRes,]), collapse=","))
  cat("\n\n")
  
  minval= min(TP10[,2:6])
  maxval= max(TP10[,2:6])
  # Give the chart file a name.
  png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_TP10", ".png", sep=""))
  # Plot the bar chart.
  plot(c(0,max(TP10[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Right Ear Signal")
  lines(rep(0, max(TP10[,1])), type = "l", col = "black")
  lines(TP10[,2], type = "l", col = "red")
  lines(TP10[,3], type = "l", col = "blue")
  lines(TP10[,4], type = "l", col = "green")
  lines(TP10[,5], type = "l", col = "orange")
  lines(TP10[,6], type = "l", col = "purple")
  legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
  # Save the file.
  dev.off()
  
  for(z in 1:5){
    if(min(TP9[,z+1]) < minimaxi[1,z] & min(TP9[,z+1])!="-Inf"  & min(TP9[,z+1])> -10 )
        minimaxi[1,z]=min(TP9[,z+1])
    if(max(TP9[,z+1]) > minimaxi[2,z])
      minimaxi[2,z]=max(TP9[,z+1])
    
    if(min(TP10[,z+1]) < minimaxi[1,z] & min(TP10[,z+1])!="-Inf"  & min(TP10[,z+1])> -10 )
      minimaxi[1,z]=min(TP10[,z+1])
    if(max(TP10[,z+1]) > minimaxi[2,z])
      minimaxi[2,z]=max(TP10[,z+1])
    
    if(min(AF7[,z+1]) < minimaxi[1,z] & min(AF7[,z+1])!="-Inf"  & min(AF7[,z+1])> -10 )
      minimaxi[1,z]=min(AF7[,z+1])
    if(max(AF7[,z+1]) > minimaxi[2,z])
      minimaxi[2,z]=max(AF7[,z+1])
    
    if(min(AF8[,z+1]) < minimaxi[1,z] & min(AF8[,z+1])!="-Inf"  & min(AF8[,z+1])> -10 )
      minimaxi[1,z]=min(AF8[,z+1])
    if(max(AF8[,z+1]) > minimaxi[2,z])
      minimaxi[2,z]=max(AF8[,z+1])
  }
}
#write.table(Results,paste(annotname,"_result", ".csv", sep=""), sep=",",row.names=FALSE,col.names=FALSE)
sink()
#file.show(paste(annotname,"_result", ".csv", sep=""))













###################################normalization and histogram############
i=1
j=1

sink(paste(annotfolder,"/",annotname,"_result", "-norm.csv", sep=""))
cat(paste("",signal,sep=","))
cat("\n")
cat("\n")

numBins=8
histoInfo=matrix(NA,nrow = songnum, ncol = numBins*4*songnum)
line=1
k=0
  for (j in 1:songnum){
    doc=read.table(paste(annotfolder,"/",songname[j],".csv", sep=""),header=TRUE, fill = TRUE,sep = ",", row.names = NULL)
    doc=as.matrix(doc)
    doc = doc[!is.na(doc[,2]),]
    linenumber = nrow(doc)
    for(infLoop in 1:ncol(doc)){
      doc[doc[,infLoop]== "-Inf", infLoop] <- -10
    }
    TP9=matrix(NA,nrow = linenumber, ncol = 6)
    TP9[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
    firstnum = as.numeric(TP9[1,1])
    
    TP9[1,1] = 0
    TP9[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
    TP9[2:linenumber,1] = as.numeric(TP9[2:linenumber,1]) - firstnum
    
    
    TP9[,2]=(as.numeric(doc[,2])-minimaxi[1,1])/((minimaxi[2,1]-minimaxi[1,1]))
    TP9[,3]=(as.numeric(doc[,6])-minimaxi[1,2])/((minimaxi[2,2]-minimaxi[1,2]))
    TP9[,4]=(as.numeric(doc[,10])-minimaxi[1,3])/((minimaxi[2,3]-minimaxi[1,3]))
    TP9[,5]=(as.numeric(doc[,14])-minimaxi[1,4])/((minimaxi[2,4]-minimaxi[1,4]))
    TP9[,6]=(as.numeric(doc[,18])-minimaxi[1,5])/((minimaxi[2,5]-minimaxi[1,5]))
    
    
    down=0
    deltaTop=1/numBins
    hi=1
    for(lop in 1:numBins){
      if(lop==numBins){
        histoInfo[line,hi]= length(TP9[TP9[,2]>= down & TP9[,2]<= (down+deltaTop),2])
        histoInfo[line,hi+8*1]= length(TP9[TP9[,3]>= down & TP9[,3]<= (down+deltaTop),3])
        histoInfo[line,hi+8*2]= length(TP9[TP9[,4]>= down & TP9[,4]<= (down+deltaTop),4])
        histoInfo[line,hi+8*3]= length(TP9[TP9[,5]>= down & TP9[,5]<= (down+deltaTop),5])
        histoInfo[line,hi+8*4]= length(TP9[TP9[,6]>= down & TP9[,6]<= (down+deltaTop),6])
      }else{
        histoInfo[line,hi]= length(TP9[TP9[,2]>= down & TP9[,2]< (down+deltaTop),2])
        histoInfo[line,hi+8*1]= length(TP9[TP9[,3]>= down & TP9[,3]< (down+deltaTop),3])
        histoInfo[line,hi+8*2]= length(TP9[TP9[,4]>= down & TP9[,4]< (down+deltaTop),4])
        histoInfo[line,hi+8*3]= length(TP9[TP9[,5]>= down & TP9[,5]< (down+deltaTop),5])
        histoInfo[line,hi+8*4]= length(TP9[TP9[,6]>= down & TP9[,6]< (down+deltaTop),6])

        down = down + deltaTop
      }
      hi=hi+1
    }
    
    idxRes=k*7
    for (t in 1:5){
      Results[1+idxRes,t]=min(TP9[,t+1])
      Results[2+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[2]
      Results[3+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[3]
      Results[4+idxRes,t]=quantile(TP9[,t+1], names=FALSE)[4]
      Results[5+idxRes,t]=max(TP9[,t+1])
      Results[6+idxRes,t]=mean(TP9[,t+1])
      Results[7+idxRes,t]=sd(TP9[,t+1])
    }
    k=k+1
    
    cat(paste("Left Ear_", annotname,"_",songname[j],"\n", sep=""))
    cat("min,")
    cat(paste(as.character(Results[1+idxRes,]), collapse=","))
    cat("\n")
    cat("Q1,")
    cat(paste(as.character(Results[2+idxRes,]), collapse=","))
    cat("\n")
    cat("Q2,")
    cat(paste(as.character(Results[3+idxRes,]), collapse=","))
    cat("\n")
    cat("Q3,")
    cat(paste(as.character(Results[4+idxRes,]), collapse=","))
    cat("\n")
    cat("max,")
    cat(paste(as.character(Results[5+idxRes,]), collapse=","))
    cat("\n")
    cat("mean,")
    cat(paste(as.character(Results[6+idxRes,]), collapse=","))
    cat("\n")
    cat("Std Dev,")
    cat(paste(as.character(Results[7+idxRes,]), collapse=","))
    cat("\n\n")
    
    minval= min(TP9[,2:6])
    maxval= max(TP9[,2:6])
    # Give the chart file a name.
    png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_TP9", "-norm.png", sep=""))
    # Plot the bar chart.
    plot(c(0,max(TP9[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Normalized Left Ear Signal")
    lines(rep(0, max(TP9[,1])), type = "l", col = "black")
    lines(TP9[,2], type = "l", col = "red")
    lines(TP9[,3], type = "l", col = "blue")
    lines(TP9[,4], type = "l", col = "green")
    lines(TP9[,5], type = "l", col = "orange")
    lines(TP9[,6], type = "l", col = "purple")
    legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
    # Save the file.
    dev.off()
    
    ##=============================================##
    AF7=matrix(NA,nrow = linenumber, ncol = 6)
    AF7[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
    firstnum = as.numeric(AF7[1,1])
    
    AF7[1,1] = 0
    AF7[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
    AF7[2:linenumber,1] = as.numeric(AF7[2:linenumber,1]) - firstnum
    
    AF7[,2]=(as.numeric(doc[,3])-minimaxi[1,1])/((minimaxi[2,1]-minimaxi[1,1]))
    AF7[,3]=(as.numeric(doc[,7])-minimaxi[1,2])/((minimaxi[2,2]-minimaxi[1,2]))
    AF7[,4]=(as.numeric(doc[,11])-minimaxi[1,3])/((minimaxi[2,3]-minimaxi[1,3]))
    AF7[,5]=(as.numeric(doc[,15])-minimaxi[1,4])/((minimaxi[2,4]-minimaxi[1,4]))
    AF7[,6]=(as.numeric(doc[,19])-minimaxi[1,5])/((minimaxi[2,5]-minimaxi[1,5]))
    
    down=0
    hi=1
    for(lop in 1:numBins){
      if(lop==numBins){
        histoInfo[line,hi+8*5]= length(AF7[AF7[,2]>= down & AF7[,2]<= (down+deltaTop),2])
        histoInfo[line,hi+8*6]= length(AF7[AF7[,3]>= down & AF7[,3]<= (down+deltaTop),3])
        histoInfo[line,hi+8*7]= length(AF7[AF7[,4]>= down & AF7[,4]<= (down+deltaTop),4])
        histoInfo[line,hi+8*8]= length(AF7[AF7[,5]>= down & AF7[,5]<= (down+deltaTop),5])
        histoInfo[line,hi+8*9]= length(AF7[AF7[,6]>= down & AF7[,6]<= (down+deltaTop),6])
      }else{
        histoInfo[line,hi+8*5]= length(AF7[AF7[,2]>= down & AF7[,2]< (down+deltaTop),2])
        histoInfo[line,hi+8*6]= length(AF7[AF7[,3]>= down & AF7[,3]< (down+deltaTop),3])
        histoInfo[line,hi+8*7]= length(AF7[AF7[,4]>= down & AF7[,4]< (down+deltaTop),4])
        histoInfo[line,hi+8*8]= length(AF7[AF7[,5]>= down & AF7[,5]< (down+deltaTop),5])
        histoInfo[line,hi+8*9]= length(AF7[AF7[,6]>= down & AF7[,6]< (down+deltaTop),6])
        
        down = down + deltaTop
      }
      hi=hi+1
    }
    
    idxRes=k*7
    for (t in 1:5){
      Results[1+idxRes,t]=min(AF7[,t+1])
      Results[2+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[2]
      Results[3+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[3]
      Results[4+idxRes,t]=quantile(AF7[,t+1], names=FALSE)[4]
      Results[5+idxRes,t]=max(AF7[,t+1])
      Results[6+idxRes,t]=mean(AF7[,t+1])
      Results[7+idxRes,t]=sd(AF7[,t+1])
    }
    k=k+1
    cat(paste("Left Forehead_", annotname,"_",songname[j],"\n", sep=""))
    cat("min,")
    cat(paste(as.character(Results[1+idxRes,]), collapse=","))
    cat("\n")
    cat("Q1,")
    cat(paste(as.character(Results[2+idxRes,]), collapse=","))
    cat("\n")
    cat("Q2,")
    cat(paste(as.character(Results[3+idxRes,]), collapse=","))
    cat("\n")
    cat("Q3,")
    cat(paste(as.character(Results[4+idxRes,]), collapse=","))
    cat("\n")
    cat("max,")
    cat(paste(as.character(Results[5+idxRes,]), collapse=","))
    cat("\n")
    cat("mean,")
    cat(paste(as.character(Results[6+idxRes,]), collapse=","))
    cat("\n")
    cat("Std Dev,")
    cat(paste(as.character(Results[7+idxRes,]), collapse=","))
    cat("\n\n")
    
    minval= min(AF7[,2:6])
    maxval= max(AF7[,2:6])
    # Give the chart file a name.
    png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_AF7", "-norm.png", sep=""))
    # Plot the bar chart.
    plot(c(0,max(AF7[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Normalized Left Forehead Signal")
    lines(rep(0, max(AF7[,1])), type = "l", col = "black")
    lines(AF7[,2], type = "l", col = "red")
    lines(AF7[,3], type = "l", col = "blue")
    lines(AF7[,4], type = "l", col = "green")
    lines(AF7[,5], type = "l", col = "orange")
    lines(AF7[,6], type = "l", col = "purple")
    legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
    # Save the file.
    dev.off()
    
    ##=============================================##
    AF8=matrix(NA,nrow = linenumber, ncol = 6)
    AF8[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
    firstnum = as.numeric(AF8[1,1])
    AF8[1,1] = 0
    AF8[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
    AF8[2:linenumber,1] = as.numeric(AF8[2:linenumber,1]) - firstnum
    
    AF8[,2]=(as.numeric(doc[,4])-minimaxi[1,1])/((minimaxi[2,1]-minimaxi[1,1]))
    AF8[,3]=(as.numeric(doc[,8])-minimaxi[1,2])/((minimaxi[2,2]-minimaxi[1,2]))
    AF8[,4]=(as.numeric(doc[,12])-minimaxi[1,3])/((minimaxi[2,3]-minimaxi[1,3]))
    AF8[,5]=(as.numeric(doc[,16])-minimaxi[1,4])/((minimaxi[2,4]-minimaxi[1,4]))
    AF8[,6]=(as.numeric(doc[,20])-minimaxi[1,5])/((minimaxi[2,5]-minimaxi[1,5]))
    
    down=0
    hi=1
    for(lop in 1:numBins){
      if(lop==numBins){
        histoInfo[line,hi+8*10]= length(AF8[AF8[,2]>= down & AF8[,2]<= (down+deltaTop),2])
        histoInfo[line,hi+8*11]= length(AF8[AF8[,3]>= down & AF8[,3]<= (down+deltaTop),3])
        histoInfo[line,hi+8*12]= length(AF8[AF8[,4]>= down & AF8[,4]<= (down+deltaTop),4])
        histoInfo[line,hi+8*13]= length(AF8[AF8[,5]>= down & AF8[,5]<= (down+deltaTop),5])
        histoInfo[line,hi+8*14]= length(AF8[AF8[,6]>= down & AF8[,6]<= (down+deltaTop),6])
      }else{
        histoInfo[line,hi+8*10]= length(AF8[AF8[,2]>= down & AF8[,2]< (down+deltaTop),2])
        histoInfo[line,hi+8*11]= length(AF8[AF8[,3]>= down & AF8[,3]< (down+deltaTop),3])
        histoInfo[line,hi+8*12]= length(AF8[AF8[,4]>= down & AF8[,4]< (down+deltaTop),4])
        histoInfo[line,hi+8*13]= length(AF8[AF8[,5]>= down & AF8[,5]< (down+deltaTop),5])
        histoInfo[line,hi+8*14]= length(AF8[AF8[,6]>= down & AF8[,6]< (down+deltaTop),6])
        
        down = down + deltaTop
      }
      hi=hi+1
    }
    
    
    idxRes=k*7
    for (t in 1:5){
      Results[1+idxRes,t]=min(AF8[,t+1])
      Results[2+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[2]
      Results[3+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[3]
      Results[4+idxRes,t]=quantile(AF8[,t+1], names=FALSE)[4]
      Results[5+idxRes,t]=max(AF8[,t+1])
      Results[6+idxRes,t]=mean(AF8[,t+1])
      Results[7+idxRes,t]=sd(AF8[,t+1])
    }
    k=k+1
    cat(paste("Right Forehead_", annotname,"_",songname[j],"\n", sep=""))
    cat("min,")
    cat(paste(as.character(Results[1+idxRes,]), collapse=","))
    cat("\n")
    cat("Q1,")
    cat(paste(as.character(Results[2+idxRes,]), collapse=","))
    cat("\n")
    cat("Q2,")
    cat(paste(as.character(Results[3+idxRes,]), collapse=","))
    cat("\n")
    cat("Q3,")
    cat(paste(as.character(Results[4+idxRes,]), collapse=","))
    cat("\n")
    cat("max,")
    cat(paste(as.character(Results[5+idxRes,]), collapse=","))
    cat("\n")
    cat("mean,")
    cat(paste(as.character(Results[6+idxRes,]), collapse=","))
    cat("\n")
    cat("Std Dev,")
    cat(paste(as.character(Results[7+idxRes,]), collapse=","))
    cat("\n\n")
    
    minval= min(AF8[,2:6])
    maxval= max(AF8[,2:6])
    # Give the chart file a name.
    png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_AF8", "-norm.png", sep=""))
    # Plot the bar chart.
    plot(c(0,max(AF8[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Normalized Right Forehead Signal")
    lines(rep(0, max(AF8[,1])), type = "l", col = "black")
    lines(AF8[,2], type = "l", col = "red")
    lines(AF8[,3], type = "l", col = "blue")
    lines(AF8[,4], type = "l", col = "green")
    lines(AF8[,5], type = "l", col = "orange")
    lines(AF8[,6], type = "l", col = "purple")
    legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
    # Save the file.
    dev.off()
    
    ##=====================================#
    TP10=matrix(NA,nrow = linenumber, ncol = 6)
    TP10[1,1] <- as.POSIXct(doc[1,1], format="%Y-%m-%d  %H:%M:%S")
    firstnum = as.numeric(TP10[1,1])
    TP10[1,1] = 0
    TP10[2:linenumber,1] <- as.POSIXct(doc[2:linenumber,1], format="%Y-%m-%d  %H:%M:%S")
    TP10[2:linenumber,1] = as.numeric(TP10[2:linenumber,1]) - firstnum
    
    TP10[,2]=(as.numeric(doc[,5])-minimaxi[1,1])/((minimaxi[2,1]-minimaxi[1,1]))
    TP10[,3]=(as.numeric(doc[,9])-minimaxi[1,2])/((minimaxi[2,2]-minimaxi[1,2]))
    TP10[,4]=(as.numeric(doc[,13])-minimaxi[1,3])/((minimaxi[2,3]-minimaxi[1,3]))
    TP10[,5]=(as.numeric(doc[,17])-minimaxi[1,4])/((minimaxi[2,4]-minimaxi[1,4]))
    TP10[,6]=(as.numeric(doc[,21])-minimaxi[1,5])/((minimaxi[2,5]-minimaxi[1,5]))
    
    down=0
    hi=1
    for(lop in 1:numBins){
      if(lop==numBins){
        histoInfo[line,hi+8*15]= length(TP10[TP10[,2]>= down & TP10[,2]<= (down+deltaTop),2])
        histoInfo[line,hi+8*16]= length(TP10[TP10[,3]>= down & TP10[,3]<= (down+deltaTop),3])
        histoInfo[line,hi+8*17]= length(TP10[TP10[,4]>= down & TP10[,4]<= (down+deltaTop),4])
        histoInfo[line,hi+8*18]= length(TP10[TP10[,5]>= down & TP10[,5]<= (down+deltaTop),5])
        histoInfo[line,hi+8*19]= length(TP10[TP10[,6]>= down & TP10[,6]<= (down+deltaTop),6])
      }else{
        histoInfo[line,hi+8*15]= length(TP10[TP10[,2]>= down & TP10[,2]< (down+deltaTop),2])
        histoInfo[line,hi+8*16]= length(TP10[TP10[,3]>= down & TP10[,3]< (down+deltaTop),3])
        histoInfo[line,hi+8*17]= length(TP10[TP10[,4]>= down & TP10[,4]< (down+deltaTop),4])
        histoInfo[line,hi+8*18]= length(TP10[TP10[,5]>= down & TP10[,5]< (down+deltaTop),5])
        histoInfo[line,hi+8*19]= length(TP10[TP10[,6]>= down & TP10[,6]< (down+deltaTop),6])
        
        down = down + deltaTop
      }
      hi=hi+1
    }
    
    idxRes=k*7
    for (t in 1:5){
      Results[1+idxRes,t]=min(TP10[,t+1])
      Results[2+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[2]
      Results[3+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[3]
      Results[4+idxRes,t]=quantile(TP10[,t+1], names=FALSE)[4]
      Results[5+idxRes,t]=max(TP10[,t+1])
      Results[6+idxRes,t]=mean(TP10[,t+1])
      Results[7+idxRes,t]=sd(TP10[,t+1])
    }
    k=k+1
    cat(paste("Right Ear_", annotname,"_",songname[j],"\n", sep=""))
    cat("min,")
    cat(paste(as.character(Results[1+idxRes,]), collapse=","))
    cat("\n")
    cat("Q1,")
    cat(paste(as.character(Results[2+idxRes,]), collapse=","))
    cat("\n")
    cat("Q2,")
    cat(paste(as.character(Results[3+idxRes,]), collapse=","))
    cat("\n")
    cat("Q3,")
    cat(paste(as.character(Results[4+idxRes,]), collapse=","))
    cat("\n")
    cat("max,")
    cat(paste(as.character(Results[5+idxRes,]), collapse=","))
    cat("\n")
    cat("mean,")
    cat(paste(as.character(Results[6+idxRes,]), collapse=","))
    cat("\n")
    cat("Std Dev,")
    cat(paste(as.character(Results[7+idxRes,]), collapse=","))
    cat("\n\n")
    
    minval= min(TP10[,2:6])
    maxval= max(TP10[,2:6])
    # Give the chart file a name.
    png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_TP10", "-norm.png", sep=""))
    # Plot the bar chart.
    plot(c(0,max(TP10[,1])),c(minval,maxval), type = "n", xlab = "Time in second(starting from 0 s)", ylab = "Signal rate", main = "Normalized Right Ear Signal")
    lines(rep(0, max(TP10[,1])), type = "l", col = "black")
    lines(TP10[,2], type = "l", col = "red")
    lines(TP10[,3], type = "l", col = "blue")
    lines(TP10[,4], type = "l", col = "green")
    lines(TP10[,5], type = "l", col = "orange")
    lines(TP10[,6], type = "l", col = "purple")
    legend('topright',horiz=TRUE, signal, lty=1, col=c('red', 'blue', 'green','orange','purple'), bty='n', cex=.75)
    # Save the file.
    
    area = "TP9"
    signallop=1
    for(lop in 1:19){
      if(lop==6){
        area="AF7"
        signallop=1
      } else if (lop==11){
        area="AF8"
        signallop=1
      } else if (lop==16){
        area="TP10"
        signallop=1
      }

      png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_", area ,"_", signal[signallop], "-histo-norm.png", sep=""))
      B = histoInfo[line,(8*(lop-1)+1):(8*(lop-1)+8)]
      names(B) <- 1:8
      mp <- barplot(B,main=paste(area, " - ", signal[signallop]), xlab="interval", ylab="the number of signal",
                    border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
      text(mp, B, labels = B, pos = 3)
      dev.off()
      
      signallop= signallop +1
    }

##draw for all histogram
    area = "TP9"
    signallop=1
    B2=NULL
    png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_", area ,"-histo-norm.png", sep=""), width=1280, height =480)
    for(lop in 1:20){
      if(lop==6){
        names(B2) <- c(' ','d','e','l','t','a',' ',' ',' ','t','h','e','t','a',' ',' ',' ','a','l','p','h','a',' ',' ',' ',' ','b','e','t','a',' ',' ',' ','g','a','m','m','a',' ',' ');
        mp <- barplot(B2,main="Left Ear Signal", xlab="interval", ylab="the number of signal",
                      border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
        text(mp, B2, labels = B2, pos = 3)
        abline(h=30, col=1)
        abline(v=0, col=1)
        abline(v=9.65, col=1)
        abline(v=9.65*2, col=1)
        abline(v=9.65*3, col=1)
        abline(v=9.65*4, col=1)
        abline(v=9.65*5, col=1)
        dev.off()
        B2=NULL
        area="AF7"
        signallop=1
        png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_", area ,"-histo-norm.png", sep=""), width=1280, height =480)
      } else if (lop==11){
        names(B2) <- c(' ','d','e','l','t','a',' ',' ',' ','t','h','e','t','a',' ',' ',' ','a','l','p','h','a',' ',' ',' ',' ','b','e','t','a',' ',' ',' ','g','a','m','m','a',' ',' ');
        mp <- barplot(B2,main="Left Forehead Signal", xlab="interval", ylab="the number of signal",
                      border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
        text(mp, B2, labels = B2, pos = 3)
        abline(h=30, col=1)
        abline(v=0, col=1)
        abline(v=9.65, col=1)
        abline(v=9.65*2, col=1)
        abline(v=9.65*3, col=1)
        abline(v=9.65*4, col=1)
        abline(v=9.65*5, col=1)
        dev.off()
        B2=NULL
        area="AF8"
        signallop=1
        png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_", area ,"-histo-norm.png", sep=""), width=1280, height =480)
      } else if (lop==16){
        names(B2) <- c(' ','d','e','l','t','a',' ',' ',' ','t','h','e','t','a',' ',' ',' ','a','l','p','h','a',' ',' ',' ',' ','b','e','t','a',' ',' ',' ','g','a','m','m','a',' ',' ');
        mp <- barplot(B2,main="Right Forehead Signal", xlab="interval (Delta-Theta-Alpha-Beta-Gamma)", ylab="the number of signal",
                      border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
        text(mp, B2, labels = B2, pos = 3)
        abline(h=30, col=1)
        abline(v=0, col=1)
        abline(v=9.65, col=1)
        abline(v=9.65*2, col=1)
        abline(v=9.65*3, col=1)
        abline(v=9.65*4, col=1)
        abline(v=9.65*5, col=1)
        dev.off()
        B2=NULL
        area="TP10"
        signallop=1
        png(file = paste(annotfolder,"/",annotname,"_",songname[j],"_", area ,"-histo-norm.png", sep=""), width=1280, height =480)
      }
      
      B = histoInfo[line,(8*(lop-1)+1):(8*(lop-1)+8)]
      B2 = c(B2, B)
      signallop= signallop +1
    }
    names(B2) <- c(' ','d','e','l','t','a',' ',' ',' ','t','h','e','t','a',' ',' ',' ','a','l','p','h','a',' ',' ',' ',' ','b','e','t','a',' ',' ',' ','g','a','m','m','a',' ',' ');
    mp <- barplot(B2,main="Right Ear Signal", xlab="interval", ylab="the number of signal",
                  border="red", density=c(10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80,10, 20, 30, 40, 50, 60, 70, 80), ylim = c(0,(linenumber+5)))
    text(mp, B2, labels = B2, pos = 3)
    abline(h=30, col=1)
    abline(v=0, col=1)
    abline(v=9.65, col=1)
    abline(v=9.65*2, col=1)
    abline(v=9.65*3, col=1)
    abline(v=9.65*4, col=1)
    abline(v=9.65*5, col=1)
    dev.off()
        
    line=line+1
    
  }

 write.table(histoInfo,paste(annotfolder,"/histoinfo.txt", sep=""),sep=",",row.names=FALSE,col.names=FALSE)
 histoInfoname= cbind(histoInfo,annotname)
 write.table(histoInfoname,paste(annotfolder,"/histoinfo.name.txt", sep=""),sep=",",row.names=FALSE,col.names=FALSE)
 
 sink()