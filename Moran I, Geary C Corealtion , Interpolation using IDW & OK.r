# 1)

# (a)
#aghcouncil--> acc_data
#aghmunicipal--> am_data
acc_data <- readOGR(dsn="C:/Users/Tigmanshu/Documents/Spatial Data Analytics/AlleghenyCounty_Council/AlleghenyCounty_Council",
                  layer = "AlleghenyCounty_Council") 

am_data <- readOGR(dsn="C:/Users/Tigmanshu/Documents/Spatial Data Analytics/AlleghenyCounty_Municipal/AlleghenyCounty_Municipal",
                      layer = "AlleghenyCounty_Municipal") 

plot(acc_data, border = "orange")
title(main = "Allegheny County Council")

plot(am_data, border = "red")
title(main = "Allegheny County Municipality")

# Funtions for moran I and Geary S calculations


cal_GearysC <- function(input_dataframe, wmatrix, N)  {
  # convert matrix to data frame
  rook_coun_df <- data.frame(wmatrix)
  # W = sum of all the distances in the distance matrix
  W <- sum(colSums(rook_coun_df[,]))
  
  denomsum <- sum(input_dataframe$D2)
  
  sum <- 0
  
  
  for (row in 1:N) {
    rowval <- input_dataframe[row, "SHAPE_area"]
    for (column in 1:N) {
      
      w <- rook_coun_df[row, column]
      
      product1 <- rowval - input_dataframe[column, "SHAPE_area"]
      
      sum <- sum + (w*product1*product1) 
    }
  }
  return (((N-1)*sum)/(2*W*denomsum))
}

cal_MoransI <- function(input_dataframe, wmatrix, N)  {
  
  rook_coun_df <- data.frame(wmatrix)
  
  W <- sum(colSums(rook_coun_df[,]))
 
  denomsum <- sum(input_dataframe$D2)
  
  sum <- 0
  
  
  for (row in 1:N) {
    
    product1 <- input_dataframe[row, "D"]
    for (column in 1:N) {
      
      w <- rook_coun_df[row, column]
      
      product2 <- input_dataframe[column, "D"]
      
      sum <- sum + (w*product1*product2) 
    }
  }
  return ((N*sum)/(W*denomsum))
}


# Adjacency using rook council
rook_num1 <- poly2nb(acc_data, queen=FALSE)
# View(rook_num1)  

# Moran's I


rook_c.matrix <- matrix(ncol = nrow(acc_data), nrow = nrow(acc_data))

for (i in 1:nrow(acc_data))
{
  
  row_adaj_vec <- rook_num1[[i]]
  for (j in 1:nrow(acc_data))
    
	if(is.element(j, row_adaj_vec))
	{
      rook_c.matrix[i, j] <- c(1/length(row_adaj_vec))
    } 
	else 
	{
      rook_c.matrix[i, j] <- c(0)
    }
}
#valMean-->value_mean
# computing the mean of SHAPE_area attribute
value_mean <- mean(acc_data@data$SHAPE_area)

acc_data@data['D'] = acc_data@data$SHAPE_area - value_mean

acc_data@data['D2'] = acc_data@data$D^2

morans_value <- cal_MoransI(acc_data@data, rook_c.matrix, nrow(acc_data))
cat("Moran's I value is: ", moransvalue)
# Moran's I value is: 0.04388988

# cross checking using lib funtion
library(ape)
Moran.I(acc_data@data$SHAPE_area, rook_c.matrix)
#0.04388988(same as the computed Moran's I value above)

# Geary's C
rook_c.matrix <- matrix(ncol = nrow(acc_data), nrow = nrow(acc_data))

for (i in 1:nrow(acc_data)){
  row_adaj_vec <- rook_num1[[i]]
  for (j in 1:nrow(acc_data))
    if(is.element(j, row_adaj_vec)){
      rook_c.matrix[i, j] <- c(1/length(row_adaj_vec))
    } else {
      rook_c.matrix[i, j] <- c(0)
    }
}

gearys_value <- cal_GearysC(acc_data@data, rook_c.matrix, nrow(acc_data))

cat("Geary's C value is: ", gearys_value)
# Geary's C value is: 0.9270441 

# cross checking using lib funtion
geary.test(acc_data@data$SHAPE_area, mat2listw(rook_c.matrix, style="W"))
# 0.92704405(same as the computed Geary's C value above)


# Adjacency using queen council

q1 <- poly2nb(acc_data, queen=TRUE)
# View(q1) 

# Moran's I

queen_coun.matrix <- matrix(ncol = nrow(acc_data), nrow = nrow(acc_data))

for (i in 1:nrow(acc_data))
{
  row_adaj_vec <- q1[[i]]
  for (j in 1:nrow(acc_data))
    if(is.element(j, row_adaj_vec))
	{
      queen_coun.matrix[i, j] <- c(1/length(row_adaj_vec))
    } else 
	{
      queen_coun.matrix[i, j] <- c(0)
    }
}

# computing the mean of SHAPE_area attribute
value_mean <- mean(acc_data@data$SHAPE_area)

acc_data@data['D'] = acc_data@data$SHAPE_area - value_mean

acc_data@data['D2'] = acc_data@data$D^2

morans_value <- cal_MoransI(acc_data@data, queen_coun.matrix, n1)
cat("Moran's I value is: ", moransvalue)
# Moran's I value is: 0.04388988

# cross checking using lib funtion
library(ape)
Moran.I(acc_data@data$SHAPE_area, queen_coun.matrix)
#0.04388988(same as the computed Moran's I value above)

# Geary's C
queen_coun2.matrix <- matrix(ncol = nrow(acc_data), nrow = nrow(acc_data))

for (i in 1:nrow(acc_data))
{
  row_adaj_vec <- q1[[i]]
  for (j in 1:nrow(acc_data))
    if(is.element(j, row_adaj_vec))
	{
      queen_coun.matrix[i, j] <- c(1/length(row_adaj_vec))
    } else 
	{
      queen_coun.matrix[i, j] <- c(0)
    }
}

gearys_value <- cal_GearysC(acc_data@data, queen_coun.matrix, n1)

cat("Geary's C value is: ", gearys_value)
# Geary's C value is: 0.9270441 

# cross checking using lib funtion
geary.test(acc_data@data$SHAPE_area, mat2listw(queen_coun.matrix, style="W"))
# 0.92704405(same as the computed Geary's C value above)


# Adjacency using rook municipal
rook2 <- poly2nb(am_data, queen=FALSE)
# View(rook2)  

# Moran's I

rook_mun.matrix <- matrix(ncol = nrow(am_data), nrow = nrow(am_data))

for (i in 1:nrow(am_data){
  row_adaj_vec <- rook2[[i]]
  for (j in 1:nrow(am_data))
    if(is.element(j, row_adaj_vec)){
      rook_mun.matrix[i, j] <- c(1/length(row_adaj_vec))
    } else {
      rook_mun.matrix[i, j] <- c(0)
    }
}

# computing the mean of SHAPE_area attribute
val_Mean <- mean(am_data@data$SHAPE_area)

am_data@data['D'] = am_data@data$SHAPE_area - val_Mean

am_data@data['D2'] = am_data@data$D^2

morans_value <- cal_MoransI(am_data@data, rook_mun.matrix, nrow(am_data))
cat("Moran's I value is: ", moransvalue)
# Moran's I value is: -0.00132743

# cross checking using lib funtion
library(ape)
Moran.I(am_data@data$SHAPE_area, rook_mun.matrix)
# -0.00132743(same as the computed Moran's I value above)

# Geary's C
rook_mun2.matrix <- matrix(ncol = nrow(am_data), nrow = nrow(am_data))

for (i in 1:nrow(am_data)){
  row_adaj_vec <- rook2[[i]]
  for (j in 1:nrow(am_data))
    if(is.element(j, row_adaj_vec)){
      rook_mun.matrix[i, j] <- c(1/length(row_adaj_vec))
    } else {
      rook_mun.matrix[i, j] <- c(0)
    }
}

gearys_value <- cal_GearysC(am_data@data, rook_mun.matrix, nrow(am_data))

cat("Geary's C value is: ", gearys_value)
# Geary's C value is: 2.126325

# cross checking using lib funtion
geary.test(am_data@data$SHAPE_area, mat2listw(rook_mun.matrix, style="W"))
# 2.12632468(same as the computed Geary's C value above)

# Adjacency using Queen municipal
# Moran's I

queen_mun.matrix <- matrix(ncol = nrow(am_data), nrow = nrow(am_data))

for (i in 1:nrow(am_data)){
  row_adaj_vec <- queen2[[i]]
  for (j in 1:n2)
    if(is.element(j, row_adaj_vec)){
      queen_mun.matrix[i, j] <- c(1/length(row_adaj_vec))
    } else {
      queen_mun.matrix[i, j] <- c(0)
    }
}

# computing the mean of SHAPE_area attribute
val_Mean <- mean(am_data@data$SHAPE_area)
# creating a new column in Data Frame to store (x-mean(x))
am_data@data['D'] = am_data@data$SHAPE_area - val_Mean
# creating a new column in Data Frame to store square of (x-mean(x))
am_data@data['D2'] = am_data@data$D^2

morans_value <- cal_MoransI(am_data@data, queen_mun.matrix, n2)
cat("Moran's I value is: ", morans_value)
# Moran's I value is: 0.001530271 

# cross checking using lib funtion

Moran.I(am_data@data$SHAPE_area, queen_mun.matrix)
# 0.001530271(same as the computed Moran's I value above)

# Geary's C
queen_mun2.matrix <- matrix(ncol = nrow(am_data), nrow = nrow(am_data))

for (i in 1:nrow(am_data)){
  row_adaj_vec <- queen2[[i]]
  for (j in 1:nrow(am_data))
    if(is.element(j, row_adaj_vec)){
      queen_mun2.matrix[i, j] <- c(1/length(row_adaj_vec))
    } else {
      queen_mun2.matrix[i, j] <- c(0)
    }
}

gearys_value <- cal_GearysC(aghmunicipal@data, queen_mun2.matrix, nrow(am_data))

cat("Geary's C value is: ", gearys_value)
# Geary's C value is: 2.083058 

# cross checking using lib funtion
geary.test(am_data@data$SHAPE_area, mat2listw(queen_mun2.matrix, style="W"))
#2.08305758(same as the computed Geary's C value above)

#2)
crimePA <- readOGR( "C:/Users/Tigmanshu/Documents/Spatial Data Analytics/Crime_PA2002", "Crime_PA2002")
class(x = crimePA)
head(crimePA@data)
length(crimePA@data)

trueCentroids = gCentroid(crimePA,byid=TRUE)
plot(crimePA)
points(trueCentroids,pch=2)
head(trueCentroids)
X<-as.vector(trueCentroids$x)
Y<-as.vector(trueCentroids$y)




# 3)

library(rgeos)
library(gbutils)
library(sqldf)

pa_county <- readOGR(dsn="C:/Users/Tigmanshu/Documents/Spatial Data Analytics/PA_County_Select/PA_County_Select", layer = "PA_County_Select") 
numrows1 <- nrow(pa_county)
plot(pa_county, border = "red")
s_locations <- readOGR(dsn="C:/Users/Tigmanshu/Documents/Spatial Data Analytics/Ozone_Sensor_Locs/Ozone_Sensor_Locs",layer = "Ozone_Sensor_Locs") 
s_locationsok <- s_locations
numrows2 <- nrow(s_locations)
o_values <- read.delim("C:/Users/Tigmanshu/Documents/Spatial Data Analytics/Data/Data/Ozone_Value.dat",sep="|", head=FALSE) 
numrows3 <- nrow(o_values)
pac_centroid <- SpatialPointsDataFrame(gCentroid(pa_county, byid=TRUE),pa_county@data, match.ID=FALSE)
pactyctrdidwdf <- as.data.frame(pac_centroid@coords)

# (1) IDW

for (i in 1:numrows1) {
  # coordsmatrix <- pa_county@polygons[[i]]@Polygons[[1]]@coords
  # size <- nrow(coordsmatrix)
  long1 <- pac_centroid@coords[i,1]
  lat1 <- pac_centroid@coords[i,2]
  for(j in 1:numrows2) {
    long2 <- slocs@data[j,"long"]
    lat2 <- slocs@data[j,"lat"]
    slocs@data[j,'distance'] <- sqrt((long2-long1)^2+
                                          (lat2-lat1)^2)
  }

  
  s_locationsdata <- slocs@data
  
  loc_df <- sqldf(" SELECT *
            FROM s_locationsdata d1 JOIN o_values d2
            ON d1.id = d2.V3
            AND d2.V6=='OZONE' ")
  
  loc_df <- loc_df[order(loc_df["distance"]),] 
  
  loc_dfsub <- loc_df[1:5,c(1,10,9,30,22)]
  colnames(loc_dfsub) <- c("id","long","lat","value","d") 
  loc_dfsub['1/d'] <- 1/loc_dfsub['d']
  sum <- sum(loc_dfsub['1/d'])
  loc_dfsub['weight'] <- loc_dfsub['1/d']/sum
  loc_dfsub['weight*value'] <- loc_dfsub['weight']*loc_dfsub['value']
  
  pactyctrd_idw_df[i,'idwvalue'] <- sum(loc_dfsub['weight*value'])
}


# IDW plot 



qpal <- colorBin("Blue", pactyctrd_idw_df$idwvalue, bins=5)
leaflet(pa_county) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(pactyctrd_idw_df$idwvalue),
              label="OK", weight = 2) %>% 
  addLegend(values=~pactyctrd_idw_df$idwvalue,pal=qpal, title="ozone values")

# (2) OK

# c[1-exp(-d/a)]
c0 <- 5
a0 <- 3

pactyctrdokdf <- as.data.frame(pac_centroid@coords)

slocsokdata <- slocsok@data

slocsokdata <- sqldf(" SELECT *
            FROM slocsokdata d1 JOIN o_values d2
            ON d1.id = d2.V3
            AND d2.V6=='OZONE' ")

d <- dist(slocsokdata[,c("long", "lat")], method = "euclidean",
          upper = FALSE, diag = TRUE)

a <- c0*(1-exp(-d/a0))

amatrix <- as.matrix(a)
amatrix <- rbind(amatrix, c(1))
amatrix <- cbind(amatrix, c(1))
diag(amatrix) <- 0	
ainvmatrix <- solve(amatrix)

for (i in 1:numrows1) {
  long1 <- pac_centroid@coords[i,1]
  lat1 <- pac_centroid@coords[i,2]
  dmatrix <- matrix(nrow=numrows2, ncol=2)
  for(j in 1:numrows2) {
    long2 <- slocsok@data[j,"long"]
    lat2 <- slocsok@data[j,"lat"]
    dmatrix[j,1] <- slocsokdata[j,"id"]
    dmatrix[j,2] <- sqrt((long2-long1)^2+
                                            (lat2-lat1)^2)
  }
  dmatrix <- as.data.frame(dmatrix)
  dmatrixnew <- sqldf(" SELECT l1.id, l2.V2
            FROM locnvalueokdf l1 JOIN dmatrix l2
                      ON l1.id = l2.V1 ")
  dmatrixnew <- dmatrixnew[,2]
  dmatrixnew <- c0*(1-exp(-dmatrixnew/a0))
  bmatrix <- as.matrix(dmatrixnew)
  bmatrix <- rbind(bmatrix, c(1))
  wmatrix <- ainvmatrix %*% bmatrix
  for(k in 1:nrow(wmatrix)-1) {
    locnvalueokdf[k,'weight'] <- wmatrix[k, 1]
    #locnvalueokdf[k,'weight*value'] <- locnvalueokdf[k,'weight']*locnvalueokdf[k,'value']
  }
  locnvalueokdf['weight*value'] <- locnvalueokdf['weight'] * locnvalueokdf['value']
  pactyctrdokdf[i,'okvalue'] <- sum(locnvalueokdf['weight*value'])
}

# plot OK
qpal <- colorBin("Reds", pactyctrdokdf$okvalue, bins=10)
# qpal <- colorNumeric(palette = "Reds", domain = pactyctrdokdf$okvalue)
leaflet(pa_county) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(pactyctrdokdf$okvalue),
              label="OK", weight = 2) %>% 
  addLegend(values=~pactyctrdokdf$okvalue,pal=qpal, title="ozone values")


# library(gstat)
# library(lattice)
# 
# var.mod<-variogram(V8~1, data=slocsokdata,loc= ~long+lat,
#                    alpha=c(0,45,90,135))
# 
# plot(var.mod, identify=TRUE)



