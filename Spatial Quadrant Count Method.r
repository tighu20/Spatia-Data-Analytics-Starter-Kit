#Loading the dataset
oil_gas_dataset <- readOGR( "C:/Users/Tigmanshu/Documents/Spatial Data Analytics/OilGasLocationPA/OilGasLocationPA", "OilGasLocationPA")
#Getting coordinates from the dataset
coordiantes_dataset<-data.frame(coordinates(oil_gas_dataset))
#Converting locations into ppp format so it is useable by quadcount of spatstat
ppp_object <- ppp(coordiantes_dataset$coords.x1,coordiantes_dataset$coords.x2, c(-215840.44,256930.1), c(80496.98,364305.8))
#using quadcount of spasstat, I chose 70 as number of quadrat in one dimension and then calculated the number in other 
#dimesnion using the size of the entire study region											
quadrat_object = quadratcount(ppp_object, 70, 42)
# creating counter for storing new rows in dataframe of quad count dataframe
counter=0
result_dataframe <- data.frame()
#Function which will generate quadrats randomly using the runif funtion which will take min and max values of the 2 dimensions
# using the plot funtion , quadrats will be added when the funtion is called inside the for loop
plot(coordiantes_dataset, pch=20,col = "red", main ="Irregular quadrat")
legend(230000, 362000, legend=c("OilGasLocations", "Quadrats"),
       col=c("red", "black"), pch=c(20, 0), cex=0.8,
       title="Legend", text.font=4, bg=NA)
quadrat_count_dataframe <- function(dataset,nx,ny){
x_side=(max(dataset$coords.x1, na.rm=T)-min(dataset$coords.x1, na.rm=T))/nx
y_side=x_side
#70*42  
total_quarats = nx*ny
  
  for (x in 1:total_quarats){
    print(x)
    rx=runif(1, min(coordiantes_dataset$coords.x1, na.rm=T),max(coordiantes_dataset$coords.x1, na.rm=T))
    ry=runif(1, min(coordiantes_dataset$coords.x2, na.rm=T),max(coordiantes_dataset$coords.x2, na.rm=T))
    
    counter=0
	# subsetting those values which fall within the quadrant
    testloc=dataset[dataset$coords.x1 <= rx+x_side & dataset$coords.x1 >= rx & dataset$coords.x2 <= ry+y_side & dataset$coords.x2 >= ry, ] 
    counter=nrow(testloc)
   
  # creating the data frame containing the coordinates of endpoints of quadrat
  new_row  <- data.frame( xmin = rx,  xmax=rx+x_side, ymin=ry,ymax=ry+y_side,nEvents=count ) 
  result_dataframe  <- rbind( new_row, result_dataframe ) 
  
  
  # lines funtion will add the quadrat according to the random number generated
  lines(c(rx,rx+x_side),c(ry,ry))
  lines(c(rx,rx+x_side),c(ry+x_side,ry+x_side))
  lines(c(rx,rx),c(ry,ry+x_side))
  lines(c(rx+x_side,rx+x_side),c(ry,ry+x_side))    
  }
return (result_dataframe)  
}
# calling the above funtion and storing it in resdf
resdf=quadrat_count_dataframe(coordiantes_dataset,70,42)
dataframe1<- data.frame()
#calculating value of mean
mean<-nrow(oil_gas_dataset)/(2939)
# this funtion will calculate each row of table which is required of us for both the QCM and this will add each new row to dataframe
vmrcalc<-function(df){
  
  events<-unique(df['nEvents'])
  for(x in 1:nrow(events))
  
  {
    
    K<-events[x,]
    X<-length(which(df$nEvents == events[x,]))
    K_U<-K-mean
    K_U2<-K_U*K_U
    XK_U2<-K_U2*X
    new_row  <- data.frame( a = K,  b=X, c=K_U,d=K_U2,e=XK_U2) 
    dataframe1  <- rbind( new_row, dataframe1 ) 
    
  }
  
 # renaming the dataframes to meanigful names
  names(dataframe1)[1] <- "K"
  names(dataframe1)[2] <- "X"
  names(dataframe1)[3] <- "K-U"
  names(dataframe1)[4] <- "(K-μ)^2"
  names(dataframe1)[5] <- "X(K-μ)^2"
  return(dataframe1)
}
vmrdf<-vmrcalc(resdf)
variance<-sum(vmrdf$`X(K-µ)^2`)/2939
3 this will return the value for VMR for irregular quadrat
vmrValue=variance/mean

df2<- data.frame()


#funtion for calculating VMR for regular quadrat
vmr_cal_regular<-function(qd_obj){
# only concerned with the non zero values as far is table is concerned.  
  events=unique(qd_obj)
  for(itr in 1:length(unique(qd_obj))){
    
    K<-events[itr]
    X<-length(which(qd_obj == events[itr]))
    K_U<-K-mean
    K_U2<-K_U*K_U
    XK_U2<-K_U2*X
    new_row  <- data.frame( a = K,  b=X, c=K_U,d=K_U2,e=XK_U2 ) 
    df2  <- rbind( new_row, df2 ) 
    
  }
  # renaming to meanigful names
  names(df2)[1] <- "K"
  names(df2)[2] <- "X"
  names(df2)[3] <- "K-U"
  names(df2)[4] <- "(K-μ)^2"
  names(df2)[5] <- "X(K-μ)^2"
  return(df2)
}
vmrRegular.df<-vmrRegular(quadrat_object)
variance<-sum(vmrRegular.df$`X(K-µ)^2`)/2939
# this will return value for regular quadrat
vmrRegularValue=variance/mean

# setting values which will be used in plotting quadrats in Regular quadrats.
minx=min(coordiantes_dataset$coords.x1)
maxx=max(coordiantes_dataset$coords.x1)
miny=min(coordiantes_dataset$coords.x2)
maxy=max(coordiantes_dataset$coords.x2)

totx=maxx-minx
toty=maxy-miny

edgeX=(max(coordiantes_dataset$coords.x1, na.rm=T)-min(coordiantes_dataset$coords.x1, na.rm=T))/70
edgeY=toty/42
sx=edgeX
plot(coordiantes_dataset, pch=20,col = "red", main ="Regular quadrat")
legend(230000, 362000, legend=c("OilGasLocations", "Quadrats"),
       col=c("red", "black"), pch=c(20, 0), cex=0.8,
       title="Legend", text.font=4, bg=NA)