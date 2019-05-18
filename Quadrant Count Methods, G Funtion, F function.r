#Loading the dataset
pa_locs_dataset <- readOGR( "C:/Users/Tigmanshu/Documents/Spatial Data Analytics/PALocs", "PALocs")

#Getting coordinates from the dataset

coordiantes_dataset<-data.frame(coordinates(pa_locs_dataset))


#Converting locations into ppp format so it is useable by quadcount of spatstat


ppp_object <- ppp(coordiantes_dataset$coords.x1,coordiantes_dataset$coords.x2, c(-80.34917,-75.03389), c(39.85385,41.99139))


#using quadcount of spasstat, I chose 30 as number of quadrat in one dimension and then calculated the number in other 
#dimesnion using the size of the entire study region											
# creating counter for storing new rows in dataframe of quad count dataframe


counter=0
result_dataframe <- data.frame()



#Function which will generate quadrats randomly using the runif funtion which will take min and max values of the 2 dimensions
# using the plot funtion , quadrats will be added when the funtion is called inside the for loop

plot(coordiantes_dataset, pch=20,col = "red", main ="Irregular quadrat")



quadrat_count_dataframe <- function(dataset,nx,ny){
x_side=(max(dataset$coords.x1, na.rm=T)-min(dataset$coords.x1, na.rm=T))/nx
y_side=x_side
  
total_quarats = nx*ny
  
  for (x in 1:total_quarats){
    print(x)
    rx=runif(1, min(dataset$coords.x1, na.rm=T),max(dataset$coords.x1, na.rm=T))
    ry=runif(1, min(dataset$coords.x2, na.rm=T),max(dataset$coords.x2, na.rm=T))
    
    counter=0
	# subsetting those values which fall within the quadrant
    testloc=dataset[dataset$coords.x1 <= rx+x_side & dataset$coords.x1 >= rx & dataset$coords.x2 <= ry+y_side & dataset$coords.x2 >= ry, ] 
    counter=nrow(testloc)
   
  # creating the data frame containing the coordinates of endpoints of quadrat
  new_row  <- data.frame( xmin = rx,  xmax=rx+x_side, ymin=ry,ymax=ry+y_side,nEvents=counter ) 
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
resdf=quadrat_count_dataframe(coordiantes_dataset,30,12)
dataframe1<- data.frame()
#calculating value of mean
mean<-nrow(pa_locs_dataset)/(359)
# this funtion will calculate each row of table which is required of us for both the QCM and this will add each new row to dataframe
legend(-77, 42,legend=c("PALocations", "Quadrats"),
       col=c("red", "black"), pch=c(20, 0), cex=0.8,
       title="Legend", text.font=4, bg="white")

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

variance<-sum(vmrdf$`X(K-µ)^2`)/359

#this will return the value for VMR for irregular quadrat
vmrValue=variance/mean

# plotting the G AND F funtions for PA LOCATIONS File
x<-Gest(ppp_object)
plot(x)
y<-Fest(ppp_object)
plot(y)
