 setwd("C:/Users/Tigmanshu/Documents/Spatial Data Analytics")

# reading the csv file
 corrcsv<- read.csv(file="C:/Users/Tigmanshu/Documents/Spatial Data Analytics/ParticulateMatter.csv", header=TRUE, sep=",")
# creating the distance matrix which holds the distance bewttwen each pair of points 
 dismat<- matrix(,nrow=length(corrcsv$Lat),ncol = length(corrcsv$Lon))
 for (x in 1:nrow(dismat))
 {
     for (y in 1:ncol(dismat))
     { if (x !=y)
     {
         dissquare= ((corrcsv[x,4]-corrcsv[y,4])*(corrcsv[x,4]-corrcsv[y,4]))+((corrcsv[x,5]-corrcsv[y,5])*(corrcsv[x,5]-corrcsv[y,5]))
         dismat[x,y]=sqrt(dissquare)
     }
         if (x==y)
         {
             dismat[x,y]=0
         }
     }
 }
 View(dismat)
 mean(dismat)

 #the median is used here as the threhold to create the weight matrix.elements are either 0 if their  corresponding distance 
 #matrix element was greater than median value and 1 if their corresponding distance matrix element was less than the median value. 
 median(dismat)

 View(dismat)
 wmat<- matrix(,nrow=length(corrcsv$Lat),ncol = length(corrcsv$Lon))


 for (x in 1:nrow(dismat))
 {
     for (y in 1:ncol(dismat))
     {
         if (dismat[x,y] < 0.159 & x!=y)
         {
             wmat[x,y]=1
         }
         if (dismat[x,y] > 0.159)
         {
             wmat[x,y]=0
         }
     }
 }
 View(wmat)
 mean(corrcsv$PM25)
# the three below loops are used to calucalte differnt parts of Moran I expression
 
 sumInum=0
 for (x in 1:nrow(wmat))
 {
     for (y in 1:ncol(wmat))
     {
         sumInum=sumInum + wmat[x,y]*(corrcsv[x,3] -10.64048)*(corrcsv[y,3]-10.64048)
     }
 }
 sumIden1=0
 for (x in 1:nrow(wmat))
 {
     sumIden1= sumIden1 + (corrcsv[x,3]-10.64048)*(corrcsv[x,3]-10.64048)
 }
 sumIden2=0
 for (x in 1:nrow(wmat))
 {
     for (y in 1:ncol(wmat))
     {
         sumIden2=sumIden2 + wmat[x,y]
     }
 }
 # Moran I value calucalted
 I = (42 * sumInum)/(sumIden1*sumIden2)
 