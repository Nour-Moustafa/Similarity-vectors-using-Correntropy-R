makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i+2]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax),main="Attack DNS data",xlab="Feature vectors",ylab = "Values") }
    else { points(vectori, col=colouri,type="l") }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}



library(RColorBrewer)
nor_data=attack.dns[1:100,1:9]
par(mfrow=c(1,2)) 
names <- c("","","","","")
mylist <- list(nor_data[,1],nor_data[,2],nor_data[,3],nor_data[,4],nor_data[,5],nor_data[,6],nor_data[,7],nor_data[,8],nor_data[,9])
makeProfilePlot(mylist,names)



#nor_data=matrix(,nrow(or_data),ncol(or_data))
#for(y in 1:ncol(or_data))
#{
 # nor_data[,y] = (or_data[,y] - min(or_data[,y])) / (max(or_data[,y]) - min(or_data[,y]))
#}

