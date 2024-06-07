#EDX Capstone Choose Your Own - CYO
#ATM clustering for definition of Field Engineers Team
#R versio 4.3.2
#Rstudio version 2023.12.0 Build 369
#Author: Fernando Athaide Mitidieri
##########################################################
# Load all required packages
# Note: this process could take a couple of minutes
##########################################################

#Verify if all packages are available, if not, they will be installed
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(gmapsdistance)) install.packages("gmapsdistance", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(geosphere)) install.packages("geosphere", repos = "http://cran.us.r-project.org")
if(!require(dbscan)) install.packages("dbscan", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearth)) install.packages("rnaturalearth", repos = "http://cran.us.r-project.org")
if(!require(chron)) install.packages("chron", repos = "http://cran.us.r-project.org")

#Once intalled the packages, we should load the libraries
library(ggplot2) #***
library(dplyr) #**** mutate
library(stringr) #***
library(gmapsdistance) ###
library(factoextra) # for elbow graph
library(geosphere) ## centroid
library(dbscan) ##dbscan
library(tidyverse) #***
library(caret) #*** loaded versi 
library(sf)
library(rnaturalearth)
library(chron)

##########################################################################################
##Functions session
##Definition of the functions used in the project
##########################################################################################

##########################################################################################
# Defining function to extract latitude and longitude of ATM table
#extract_coordenates(string_coordenate)
#Inputs:
#String with a longitude and latitude
#Outputs: 
#Numeric values of Long and Lat correspondent of string_coordenate
##########################################################################################
extract_coordenates <- function(string_coordenate) {
  # Remove 'POINT (' and ')' from string
  coordenates <- gsub("POINT \\(|\\)", "", string_coordenate)

  # Split string to obtain longitude e latitude
  parts <- strsplit(coordenates, " ")[[1]]

  # Convert each one in a numeric variable
  longitude <- as.numeric(parts[1])
  latitude <- as.numeric(parts[2])

  # Return A veCtor with longitude and latitude
  return(c(longitude, latitude))
}

##########################################################################################
# Defining function to calculate the distance between two coordenates. 
# Used a function reference of web to build this final function.
#haversine(point1, point2, km_hour)
#Inputs:
#point1 vector with a coordenate: long and Lat
#point2 vector with a coordenate: long and Lat
#km_hour a value indicating the average speed to be considered in the calculation of the 
#drivable time beteween point1 and point2
#Outputs:  is a dataframe
#Distance in meters
#Time in seconds
#Status of calculation
##########################################################################################
haversine <- function(point1, point2, km_hour) {
  R <- 6371  # Earth radius in kilometers
  lon1 <- point1[1]
  lat1 <- point1[2]
  
  lon2 <- point2[1]
  lat2 <- point2[2]
  
  lat1_rad <- deg2rad(lat1)
  lon1_rad <- deg2rad(lon1)
  lat2_rad <- deg2rad(lat2)
  lon2_rad <- deg2rad(lon2)
  
  dlat <- lat2_rad - lat1_rad
  dlon <- lon2_rad - lon1_rad
  
  a <- sin(dlat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  #For compatibility with Google API, distance it is given in meters
  distance <- round((R * c)*1000)
  
  #Calculates the average time to drive between these two points considering a rate of 50km per hour as 
  #an average.
  #For compatibility with Google API, time is given in seconds
  time <- (distance/(km_hour*1000))*60*60
  response <- data.frame(Distance = distance, Time = time, Status = "OK")
  return(response)
}

##########################################################################################
# Defining function to convert degree into radians
#deg2rad(deg)
#Inputs:
#A degree
#Outputs: 
#Radians
##########################################################################################
# Convert degree in radian
deg2rad <- function(deg) {
  return(deg * pi / 180)
}

##########################################################################################
#Function to calculate the gographical distance between two coordenates.
#distance_calculation(start_point,end_point)
#Inputs:
#Two geogrsphical points 
#Outputs: 
#Distance between the two points
##########################################################################################

distance_calculation <- function(start_point, end_point) {
  origin <- start_point
  destination <- end_point
  
  results <- haversine(origin, destination, 110)
  return(results)
}

##########################################################################################
#Some data to test and validate the functions that calculate the distances
##########################################################################################
#reference pints to test distance calculation function
buffalo <- c(-78.84327, 42.89434)
albany <- c(-73.74284, 42.66186)
#Test of function to calculate the geographical distance between two coordenate.
distance_calculation(buffalo, albany)

ponto1 <- c(-76.20540, 43.06096)
ponto2 <- c(-74.41121, 43.64788)
distance_calculation(ponto1, ponto2)

##########################################################################################
#Convert seconds to hh:mm formmat 
#sec2hour(seconds)
#Inputs:
#A periodo in seconds
#Outputs: 
#The inout in a format hh:mm
##########################################################################################
sec2hour <- function(seconds) {
  hour = floor(seconds/3600)
  seconds = seconds - (hour*3600)
  minute = floor(seconds/60)
  hourminute = paste(hour,minute)
  response = format(strptime(formatC(hourminute, width = 4, format = "d", flag = "0"), format="%H%M"), format = "%H:%M")
}

##########################################################################################
#Convert hours in minutes
#hour2min(hour, minute)
#Inputs:
#A period given by hour and minute
#Outputs: 
#The input in minutes
##########################################################################################
hour2min <- function(hour, minute) {
  resp <- (hour*60) + minute
  return(resp)
}
##########################################################################################
#Calculate the total number of engineer based on the ATMs in each cluster and rouding up
#calc_field_engineer(distribution)
#Inputs:
#A vector with the distribution of ATMs per cluster
#Outputs: 
#Total engineer needed. It is rounded up
##########################################################################################
calc_field_engineer <- function (distribution) {
  total <- numeric(length(distribution)) 
  j <- length(distribution)  
  for(i in 1:j){
  total[i] <- round(distribution[i]/60,0)  
  }
  return(sum(total))
}

####################################################################################################
####################################################################################################
#Calculate the max distance among the cluster and also the mas time among the clusters
#calc_maxdist_maxtime(ATM_list)
#Inputs:
#Dataframe with ATM localization and clusters
#J is the number of clusters
#Outputs: 
#Max Distance between ATMs and the centroid and also max time to drive from ATM to the centroid
####################################################################################################
####################################################################################################
calc_maxdist_maxtime <- function (ATM_list,m) {
#Create a sequence of K to run the loop. For this purpose we will start with 3 and try until 20 
#clusters.
seq_clusters <- seq(from = m, to = m, by = 1)
#Creating the table and the index to include the results of all process. This table will be used
#iin the report to justify the K chose.
general_resultsfc <-data.frame()
num_linha <-1 

  #Loop for calculate the distances between each ATM and its respectively cluster's centroid
  for(id_cluster in 1:m){
    
    #For process monitoring purpose
    # print(num_linha)
    # print(m)
    # print(id_cluster)
    
    #Needed for avoid a error in the comparison below
    j <- as.character(id_cluster) 
    k <- as.character(num_cluster)    
    #Filtering each cluster of a certain K and calculates the centroid of each
    Pontos_c <- ATM_list[,7:9] %>% filter(clusters == j)
    Pontos_c <- Pontos_c[,c("Longitude","Latitude")]
    centroid_c <- as.vector(geomean(Pontos_c))
    
    ##Function to consult google API to calculate the distance and time driving between each ATM and the
    ##centroid of the Cluster 1. Its takes time (several minutes per each cluster).
    resposta_cluster <- apply(Pontos_c, 1, distance_calculation, centroid_c)

    #Calculating the max, min and mean distance of all points and the centroid of cluster 2
    distances <- sapply(resposta_cluster, function(x) as.numeric(x[1]))
    max_distance_c <- round((max(distances)/1000),2)
    min_distance_c <- round((min(distances)/1000),2)
    mean_distance_c <- round((mean(distances)/1000),2)
    
    #Calculating the max, min and mean distance of all points and the centroid of cluster 2
    time <- sapply(resposta_cluster, function(x) as.numeric(x[2]))
    max_time_c <- sec2hour(max(time))
    min_time_c <- sec2hour(min(time))
    mean_time_c <- sec2hour(mean(time))
    
    #Storing results in a table to be used to take decision regarding the best K
    general_resultsfc[num_linha,1:8] <- data.frame(m,id_cluster,max_distance_c,min_distance_c,mean_distance_c, max_time_c,min_time_c, mean_time_c)
    num_linha <- num_linha+1
  }
  return(general_resultsfc)
}

################################################################################################
##Start pre-processing the data separating the coordenates in lat and long.
##used a user defined function
################################################################################################
###Read dataset from: 
###"https://data.ny.gov/api/views/ndex-ad5r/rows.csv"
### Data description
###Column Name	        / Description	                              / Type
###Name of Institution	/ Name of the institution that owns the ATM / Plain Text 
###Street Address	      / Street address of the ATM location        / Plain Text
###City	                / City in which the ATM is located          / Plain Text
###ZIP Code	            / ZIP code in which the ATM is located      / Number
###County	              / County in which the ATM is located        / Plain Text
###Georeference	        / Open Data                                 / Point

dl <- "data/rows.csv"
if(!file.exists(dl))
  download.file("https://data.ny.gov/api/views/ndex-ad5r/rows.csv", dl)
ATM <- read.csv(dl)

#Applying the function in each line of column Georeference and save the results in a 
#temporaly variable
coordenates_extracted <- t(sapply(ATM$Georeference, extract_coordenates))

#Converting the results for a dataframe format adding meaninful names 
#for the related columns 
coordenates_extracted <- as.data.frame(coordenates_extracted)
colnames(coordenates_extracted) <- c("Longitude", "Latitude")

#Adding the splitted coordenates to the original dataframe ATM
ATM$Longitude <- coordenates_extracted$Longitude
ATM$Latitude <- coordenates_extracted$Latitude

#Saving the preprocessed data to be used diuring the analysis phase
save(ATM, file="data/ATM.RData")

################################################################################################
##Continuing with the exploratory analysis the map of New York will be plotted with 
##all ATM locations included on it. 
################################################################################################
ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("New York"))

figure1 <- ggplot() + 
  geom_point(data =ATM, aes(x = Longitude, y = Latitude), size = 0.5) +
  geom_sf(data = ca_nv_map, fill = NA) +
  scale_shape_manual(values=seq(0,15)) +
  labs(title = 'ATM Bank Owned',
       subtitle = 'New York State',
       y = '', x = 'Fig. 1 ATM Geographical Distribution') +
  theme(legend.position = "none")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure1, file="figures/figure1.RData")
figure1
# dev.off()
# figure1

# Obter dados do estado de New York
ny_map <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "New York")

# Coordenadas para Albany, Buffalo, Rochester, New York City, e Syracuse
cities <- data.frame(
  name = c("Albany", "Buffalo", "Rochester", "New York", "Syracuse"),
  lat = c(42.6526, 42.8864, 43.1566, 40.7128, 43.0481),
  lon = c(-73.7562, -78.8784, -77.6088, -74.0060, -76.1474)
)

# Converter para um objeto sf
cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = st_crs(ny_map))

# Plotar o mapa do estado com as cidades e nomes das cidades
figure2 <- ggplot() +
  geom_sf(data = ny_map, fill = "gray80", color = "black") +
  geom_sf(data = cities_sf, color = "blue", size = 3, shape = 21, fill = "blue") +
  geom_text(data = cities, aes(x = lon, y = lat, label = name), nudge_y = 0.5, check_overlap = FALSE, color = "blue") +
  labs(title = 'Cities - New York State',
       y = '', x = 'Fig 2. Main cities of New York State') +
  theme(legend.position = "none")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure2, file="figures/figure2.RData")
figure2
# dev.off()
# figure2
################################################################################################
##First approach will be the algorithm K-means
##We should follow some steps to apply this algorithm
## 1) Define the initial K
## 2) Run the algorithm 
## 3) Evaluate the results
################################################################################################
#Set a seed for reproducibility
set.seed(1972) 

#Using the Elbow tehcnique to determine how many cluster should be optinal to be used with 
#K-means.
figure3 <- fviz_nbclust(ATM[, c("Longitude","Latitude")], kmeans, method="wss")
figure3 <- figure3 + theme(legend.position = "none") +
  labs(x = 'Fig 3. Optimal number of clusters') 


#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure3, file="figures/figure3.RData")
figure3
# dev.off()
# figure3

#Using the K calculated to check if this is the best K for this business case
k <- 2  # Number of clusters
clusters <- kmeans(ATM[, c("Longitude","Latitude")], centers = k)
ATM$clusters <- as.factor(clusters$cluster)  # Add cluster assignment to the dataframe

#Now lets distribute this points without cluster using K-means and the centroids calculated by 
#the DBScan algorithm, for this it is necessary calculate the "cenrtoids" of each cluster 
j <- max(as.numeric(ATM$clusters))
centroids <- data.frame()
for(i in 1:j){
  Pontos_c1 <- ATM[,7:9] %>% filter(clusters == as.character(i))
  Pontos_c1 <- Pontos_c1[,c("Longitude","Latitude")]
  centroids[i,1:2] <- as.matrix(geomean(Pontos_c1))
}

# Getting a little fancier with it by adding the state borders
ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("New York"))

figure4 <- ggplot() + 
  geom_point(data =ATM, aes(x = Longitude, y = Latitude, color = clusters, shape = clusters), size =1) +
  geom_point(data = clusters$centers, aes(x = Longitude, y = Latitude), size = 2) + 
  geom_point(data = centroids, aes(x = V1, y = V2), size = 2) + 
  geom_sf(data = ca_nv_map, fill = NA) +
  scale_shape_manual(values=seq(0,15)) +
  labs(title = 'K-means',
       y = '', x = 'Fig 4. K-means with K=2') +
  theme(legend.position = "none")
#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure4, file="figures/figure4.RData")
figure4
# dev.off()
# figure4

#Let's calculate de max, min and average distance between the ATMs and the centroid of both cluster 

#Now calc the centroid of each cluster
# Calculando o centroide esférico do cluster 1
Pontos_c1 <- ATM[,7:9] %>% filter(clusters == '1')
Pontos_c1 <- Pontos_c1[,c("Longitude","Latitude")]
centroid_c1 <- as.vector(geomean(Pontos_c1))

##Function to consult google API to calculate the distance and time driving between each ATM and the
##centroid of the Cluster 1. Its takes time (several minutes approx 18 minutes).
start_time <- Sys.time()
resposta_cluster1 <- apply(Pontos_c1, 1, distance_calculation, centroid_c1)
end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken

#Calculating the max, min and mean distances of all points and the centroid of cluster 1
distances1 <- sapply(resposta_cluster1, function(x) as.numeric(x[1]))
max_distance_c1 <- round((max(distances1)/1000),2)
min_distance_c1 <- round((min(distances1)/1000),2)
mean_distance_c1 <- round((mean(distances1)/1000),2)

#Calculating the max, min and mean times of all points and the centroid of cluster 2
time1 <- sapply(resposta_cluster1, function(x) as.numeric(x[2]))
max_time_c1 <- sec2hour(max(time1))
min_time_c1 <- sec2hour(min(time1))
mean_time_c1 <- sec2hour(mean(time1))

#Now calc the centroid of each cluster
# Calculando o centroide esférico do cluster 2
Pontos_c2 <- ATM[,7:9] %>% filter(clusters == '2')
Pontos_c2 <- Pontos_c2[,c("Longitude","Latitude")]
centroid_c2 <- as.vector(geomean(Pontos_c2))
# Imprimindo o centroide
# print(centroid_c2)
# print(Pontos_c2)

##Function to consult google API to calculate the distance and time driving between each ATM and the
##centroid of the Cluster 2. Its takes time (several minutes approx 51 minutes).
start_time <- Sys.time()
resposta_cluster2 <- apply(Pontos_c2, 1, distance_calculation, centroid_c2)
#resposta_cluster2 <- sapply(start_point_c2$coordenate, distance_calculation, centroid_c2$coordenate, simplify =FALSE)
end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken

#Calculating the max, min and mean distances of all points and the centroid of cluster 2
distances2 <- sapply(resposta_cluster2, function(x) as.numeric(x[1]))
max_distance_c2 <- round((max(distances2)/1000),2)
min_distance_c2 <- round((min(distances2)/1000),2)
mean_distance_c2 <- round((mean(distances2)/1000),2)

#Calculating the max, min and mean times of all points and the centroid of cluster 2
time2 <- sapply(resposta_cluster2, function(x) as.numeric(x[2]))
max_time_c2 <- sec2hour(max(time2))
min_time_c2 <- sec2hour(min(time2))
mean_time_c2 <- sec2hour(mean(time2))

results_table <- data.frame(
 "Max Distance" = c(max_distance_c1, max_distance_c2),
 "Min Distance" = c(min_distance_c1, min_distance_c2),
 "Mean Distance" = c(mean_distance_c1,mean_distance_c2), 
 "Max Time" = c(max_time_c1, max_time_c2),
 "Min Time" = c(min_time_c1, min_time_c2),
 "Mean Time" = c(mean_time_c1,mean_time_c2) 
 )

rownames(results_table) <- c("Cluster1", "Cluster2")
#Because it took so long to calculate, has been saved the summary of results data 
#to be used during the analysis phase.
save(results_table, file="data/results_table.RData")

#Crating a table with results for comparison purpose among all methods
results_comparison <- data.frame(matrix(nrow=5,ncol=5))
colnames(results_comparison) <- c("Max distance", "Max time","Variance of # ATMs per Cluster","Amplitude of # ATMs per Cluster", "# of Field Engineers")
rownames(results_comparison) <- c("Kmeans best K=2", "Kmeans calculated K=6", "Kmeans Extra Feature K=6", "Dbscan and Kmeans K=6", "Dbscan, Extra Feature and Kmeans k=6")

#Results of Kmeans k=2
results_comparison$`Max distance`[1] <- max(max_distance_c1,max_distance_c2)
results_comparison$`Max time`[1] <- sec2hour(max(max(time1),max(time2)))
results_comparison$`Variance of # ATMs per Cluster`[1] <- var(clusters$size)
results_comparison$`Amplitude of # ATMs per Cluster`[1] <- max(clusters$size)-min(clusters$size)
results_comparison$`# of Field Engineers`[1] <- calc_field_engineer(clusters$size)

#Because it took so long to calculate, has been saved the summary of results data 
#to be used during the analysis phase.
save(results_comparison, file="data/results_comparison.RData")

######################################################################################################
# For this particular problem there are some aspects that we should be consider to support 
# the decision regarding the number of clusters. 
# Normally a working day of a field engineer has 11 hours, which includes an hour interval 
# for lunch. Typically an ATM call takes 45 minutes and one field engineer can perform 4 calls a day.
# Considering these hyperparameters, so, in a normal working day, a field engineer will 
# spend 3 hours in front of an ATM, 1 hour having lunch, and 7 hour traveling from one ATM 
# to another and in his commute. Which means that he has approx an 1:30 hour on average to travel from 
# one point to another.
# This will be one of our criteria to evaluate the results: maximum of 1 hour of travel time 
# between the center of the clusters and the ATMs.
#This will be one of the criteria to evaluate the results: an average maximum of 1:30 hour travel time 
#between the center of the clusters and the ATMs.
#Other criteria will be defined as an average of maximum distance of 250 km between the ATM and the center 
#of the respective cluster.
######################################################################################################

######################################################################################################
######################################################################################################
#Now will be tested different K to find out that one that outcome the best results according with the 
#considerations above
#Will run a loop for from k=3 to 20 to find out the K that all clusters have max distance between the 
#ATM and the centroid less than 250km and drivable time less than 90 kinutes. 
#When this happens, than the loop with be finished and the K that results this condition should be 
#used to calculate the quantity of field engineers.
######################################################################################################
######################################################################################################

#Create a sequence of K to run the loop. For this purpose we will start with 3 and try until 20 
#clusters.
seq_clusters <- seq(from = 3, to = 20, by = 1)
#Creating the table and the index to include the results of all process. This table will be used
#iin the report to justify the K chose.
general_results <-data.frame()
num_linha <-1 
#This line is only to measure the time of processing all Ks.
start_totaltime <- Sys.time()

#Loop for calculate a sequence of K and verify if the max distance is < than 250km.
#If a specific K has all clusters with max distance between the ATM and the centroid 
#of each cluster less than 150km this will be the K chosen.
for(num_cluster in seq_clusters){

# #each loop calculater the Kmeans with a different K.
  ATM_aux <- ATM
  set.seed(1972) 
  clusters <- kmeans(ATM_aux[, c("Longitude","Latitude")], centers = num_cluster)
  ATM_aux$clusters <- as.factor(clusters$cluster)  # Add cluster assignment to the dataframe

#control variable for the second loop.
  total_cluster <- max(as.numeric(ATM_aux$clusters))

  #Loop for calculate the distances between eacxh ATM and its respectively cluster's centroid
  for(id_cluster in 1:total_cluster){
    
#For process monitoring purpose
  print(num_linha)
  print(num_cluster)
  print(id_cluster)

  #Needed for avoid a error in the comparison below
  j <- as.character(id_cluster) 
  k <- as.character(num_cluster)    
  #Filtering each cluster of a certain K and calculates the centroid of each
  Pontos_c <- ATM_aux[,7:9] %>% filter(clusters == j)
  Pontos_c <- Pontos_c[,c("Longitude","Latitude")]
  centroid_c <- as.vector(geomean(Pontos_c))

  ##Function to consult google API to calculate the distance and time driving between each ATM and the
  ##centroid of the Cluster 1. Its takes time (several minutes per each cluster).
  start_time <- Sys.time()
  resposta_cluster <- apply(Pontos_c, 1, distance_calculation, centroid_c)
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  print(time_taken)
  
  #Calculating the max, min and mean distance of all points and the centroid of cluster 2
  distances <- sapply(resposta_cluster, function(x) as.numeric(x[1]))
  max_distance_c <- round((max(distances)/1000),2)
  min_distance_c <- round((min(distances)/1000),2)
  mean_distance_c <- round((mean(distances)/1000),2)
  
  #Calculating the max, min and mean distance of all points and the centroid of cluster 2
  time <- sapply(resposta_cluster, function(x) as.numeric(x[2]))
  max_time_c <- sec2hour(max(time))
  min_time_c <- sec2hour(min(time))
  mean_time_c <- sec2hour(mean(time))
  
  #Storing results in a table to be used to take decision regarding the best K
  general_results[num_linha,1:8] <- data.frame(num_cluster,id_cluster,max_distance_c,min_distance_c,mean_distance_c, max_time_c,min_time_c, mean_time_c)
  num_linha <- num_linha+1
  }

  resp <- filter(general_results, num_cluster == k) ##ignorar NA
  
  distance_test <- mean(resp$max_distance_c)
  time_aux <- as.POSIXct(resp$max_time_c, format="%H:%M")
  # Extrair horas e minutos
  hours <- as.numeric(format(time_aux, "%H"))
  minutes <- as.numeric(format(time_aux, "%M"))
  
  minute_aux <- sapply(hours, hour2min, minutes, simplify = TRUE)
  minutes_test <- mean(minute_aux)
  
  if((distance_test < 250) & (minutes_test < 90))
  {
   print("Less than 250 km and less than 90 min.")
     break
  }
  }

#Calculating the total time consumed by the for loop
end_totaltime <- Sys.time()
totaltime_taken <- end_totaltime - start_totaltime
print(totaltime_taken)

#Saved to be used in the report to show the results.
save(general_results, file="data/general_results.RData")

######################################################################################################
######################################################################################################
#After analyzing the results of the previous process, the best K that also results the max distance 
#between the ATMs and the respective centroid to be less than 250 km and the max travel time less than 
#90 minutes is the K=6.
#So, lets recalculate and plot the data in a map for visualization purpose, identifying the centroides
# of each cluster
######################################################################################################
######################################################################################################

#Saving the best number of clusters generated by the loop above
kluster <- as.numeric(k) 

#Calculate the custers.
#Set the seed to repetibility purposes
set.seed(1972) 
clusters <- kmeans(ATM[, c("Longitude","Latitude")], centers = kluster)
ATM$clusters <- as.factor(clusters$cluster)  # Add cluster assignment to the dataframe

# Adding the state borders to buils the map
ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("New York"))

# Plot the data with the clusters and centroids
figure5 <- ggplot() + 
  geom_point(data =ATM, aes(x = Longitude, y = Latitude, colour = clusters, shape = clusters), size =1) +
  geom_point(data = clusters$centers, aes(x = Longitude, y = Latitude), size = 2) + 
  geom_sf(data = ca_nv_map, fill = NA) +
  scale_shape_manual(values=seq(0,15)) +
  labs(title = 'K-means',
       y = '', x = 'Fig 5. K-means with K=6') +
  theme(legend.position = "none")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure5, file="figures/figure5.RData")
figure5

#Results of Kmeans k=2
results_comparison$`Max distance`[2] <- max(general_results$max_distance_c)
results_comparison$`Max time`[2] <- max(general_results$max_time_c)
results_comparison$`Variance of # ATMs per Cluster`[2] <- var(clusters$size)
results_comparison$`Amplitude of # ATMs per Cluster`[2] <- max(clusters$size)-min(clusters$size)
results_comparison$`# of Field Engineers`[2] <- calc_field_engineer(clusters$size)

#Because it took so long to calculate, has been saved the summary of results data 
#to be used during the analysis phase.
save(results_comparison, file="data/results_comparison.RData")

######################################################################################################
######################################################################################################
#Now will be included two new dimensions in the dataset. Will be used two geographical reference point 
#to calculate the distances for all those ATMs to these point and these distances will be used as 
#additional dimensions in dataset
######################################################################################################
######################################################################################################

#Preparing the data to calculate the distances among all ATMs and TWO reference points. 
#Has been tested one reference point, but two generated best results. 
Pontos_c <- ATM[,7:8]
#Refenrence point 1 - Chosen directly from the map
ponto_reference1 <- c(-79.73701, 41.99031)
#Refenrence point 2 - Chosen directly from the map
ponto_reference2 <- c(-73.36494, 44.98635)

##Function to calculate the distance and time driving between each ATM and the
##reference point #1. Its takes time to run.
start_time <- Sys.time()
resposta_ref1 <- apply(Pontos_c, 1, distance_calculation, ponto_reference1)
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

ref_distances1 <- sapply(resposta_ref1, function(x) as.numeric(x[1]))
ATM1 <- ATM[-9]
ATM1$Reference1 <- round((ref_distances1/1000),2)

##Function to calculate the distance and time driving between each ATM and the
##reference point #2. Its takes time to run.
start_time <- Sys.time()
resposta_ref2 <- apply(Pontos_c, 1, distance_calculation, ponto_reference2)
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

ref_distances2 <- sapply(resposta_ref2, function(x) as.numeric(x[1]))
ATM1$Reference2 <- round((ref_distances2/1000),2)

#Calculate the custers.
#Set the seed to repetibility purposes
set.seed(1972) 
k <- 6  # Best clustert number resulted from our latest analysis
clusters <- kmeans(ATM1[, c("Longitude","Latitude","Reference1","Reference2")], centers = k)
ATM1$clusters <- as.factor(clusters$cluster)  # Add cluster assignment to the dataframe

# Adding the state borders
ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("New York"))
# Plot the data with the cluster and data
figure6 <- ggplot() + 
  geom_point(data =ATM1, aes(x = Longitude, y = Latitude, color = clusters, shape = clusters), size =1) +
  geom_point(data = clusters$centers, aes(x = Longitude, y = Latitude), size = 2) + 
  geom_sf(data = ca_nv_map, fill = NA) +
  scale_shape_manual(values=seq(0,15)) +
  labs(title = 'K-means and extra feature',
       y = '', x = 'Fig 6. K-means with K=6') +
   theme(legend.position = "none")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure6, file="figures/figure6.RData")
figure6

# Reorder columns for call function
ATM1 <- ATM1[, c(1,2,3,4,5,6,7,8,11,9,10)]
resp <- calc_maxdist_maxtime(ATM1,6)

#Results
results_comparison$`Max distance`[3] <- max(resp$max_distance_c)
results_comparison$`Max time`[3] <- max(resp$max_time_c)
results_comparison$`Variance of # ATMs per Cluster`[3] <- var(clusters$size)
results_comparison$`Amplitude of # ATMs per Cluster`[3] <- max(clusters$size)-min(clusters$size)
results_comparison$`# of Field Engineers`[3] <- calc_field_engineer(clusters$size)


#Because it took so long to calculate, has been saved the summary of results data 
#to be used during the analysis phase.
save(results_comparison, file="data/results_comparison.RData")


#####################################################################################################
#The next session willl apply the algorithm DBScan to verify if we can obtain a best result than 
#K-Means. 
#####################################################################################################
#Erasing the results from last experiment and copying the data to a new dataset to be used with the 
#DBsacn algorithm 
ATM_aux <- ATM[-9]

#Set seed for reproducibility
set.seed(1972)  
Dbscan_cl <- dbscan(ATM_aux[, c("Longitude","Latitude")], eps = 0.25, MinPts =75)
ATM_aux$clusters <- as.factor(Dbscan_cl$cluster)  # Add cluster assignment to the dataframe

#Lerts calculate the centroid of each cluster fot plotting purposes
j <- max(Dbscan_cl$cluster)
centroids <- data.frame()
for(i in 1:j){
  Pontos_c1 <- ATM_aux[,7:9] %>% filter(clusters == as.character(i))
  Pontos_c1 <- Pontos_c1[,c("Longitude","Latitude")]
  centroids[i,1:2] <- as.matrix(geomean(Pontos_c1))
}

# Getting a little fancier with it by adding the state borders
ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("New York"))

figure7 <- ggplot() + 
  geom_point(data =ATM_aux, aes(x = Longitude, y = Latitude, color = clusters, shape = clusters), size =1) +
  geom_point(data = centroids, aes(x = V1, y = V2), size = 2) +
  geom_sf(data = ca_nv_map, fill = NA) +
  scale_shape_manual(values=seq(0,15)) +
  labs(title = 'DBScan',
       y = '', x = 'Fig 7. Clustering with DBScan') +
 theme(legend.position = "none")  

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure7, file="figures/figure7.RData")
figure7


#####################################################################################################
#After see that DBScan resulted in a plenty of ATMs not assigned for any cluster, 
#let's experiment combine this results with a K-means 
#####################################################################################################
#Erasing the results from last experiment and copying the data to a new dataset to be used with the 
#DBsacn algorithm 
ATM_aux <- ATM[-9]

#Set seed for reproducibility
set.seed(1972)  
#First run the Dbscan and generate the clusters 
Dbscan_cl <- dbscan(ATM_aux[, c("Longitude","Latitude")], eps = 0.25, MinPts = 75)
ATM_aux$clusters <- as.factor(Dbscan_cl$cluster)  # Add cluster assignment to the dataframe

#####################################################################################################
#Points with no cluster assigned are assigned by Kmeans
#Selecting only the ATMs not assigned for any cluster, it means cluster 0
Pontos_c <- ATM_aux %>% filter(clusters == '0')
Pontos_c <- Pontos_c[,7:8]

#Now lets distribute this points without cluster using K-means and the centroids calculated by 
#the DBScan algorithm, for this it is necessary calculate the "cenrtoids" of each cluster 
j <- max(Dbscan_cl$cluster)
centroids <- data.frame()

for(i in 1:j){
  Pontos_c1 <- ATM_aux[,7:9] %>% filter(clusters == as.character(i))
  Pontos_c1 <- Pontos_c1[,c("Longitude","Latitude")]
  centroids[i,1:2] <- as.matrix(geomean(Pontos_c1))
}

set.seed(1972) 
k <- max(Dbscan_cl$cluster) # Best clustert number resulted from our latest analysis
clusters <- kmeans(Pontos_c, centers = centroids)
Pontos_c$clusters <- as.factor(clusters$cluster)  # Add cluster assignment to the dataframe

k <- as.integer(1)
j <- length(ATM_aux$clusters)

for(i in 1:j){
  if((as.numeric(ATM_aux[i,7]) == as.numeric(Pontos_c[k,1])) & (as.numeric(ATM_aux[i,8]) == as.numeric(Pontos_c[k,2])))
  {
    ATM_aux[i,9] <- Pontos_c[k,3]
    k <- as.integer(k+1)
    if(k>length(Pontos_c$clusters)) break
  }
}
#####################################################################################################

# Getting a little fancier with it by adding the state borders
ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("New York"))
figure8 <- ggplot() + 
  geom_point(data =ATM_aux, aes(x = Longitude, y = Latitude, color = clusters, shape = clusters), size =1) +
  geom_point(data = centroids, aes(x = V1, y = V2), size = 2) + 
  geom_sf(data = ca_nv_map, fill = NA) +
  scale_shape_manual(values=seq(0,15)) +
  labs(title = 'DBScan and Kmeans',
       y = '', x = 'Fig 8. Clustering with DBScan and Kmeans') +
  theme(legend.position = "none")
sumary_dbscan_extra <- summary(ATM1$clusters)

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure8, file="figures/figure8.RData")
figure8

#Calc results for comparison
resp <- calc_maxdist_maxtime(ATM_aux,6)

#Results
results_comparison$`Max distance`[4] <- max(resp$max_distance_c)
results_comparison$`Max time`[4] <- max(resp$max_time_c)
results_comparison$`Variance of # ATMs per Cluster`[4] <- var(clusters$size)
results_comparison$`Amplitude of # ATMs per Cluster`[4] <- max(clusters$size)-min(clusters$size)

clusters_size <- summary(ATM_aux$clusters)
clusters_size <- clusters_size[-1]
results_comparison$`# of Field Engineers`[4] <- calc_field_engineer(clusters_size)

#Because it took so long to calculate, has been saved the summary of results data 
#to be used during the analysis phase.
save(results_comparison, file="data/results_comparison.RData")



#####################################################################################################
#After see that DBScan resulted in a plenty of ATMs not assigned for any cluster, 
#let's experiment include an additional dimension into our data, let's calculate the distance for 
#each ATM from a specific reference point in the New York State and let's evaluate if we will 
#have a best result for our clusters.
#####################################################################################################
ATM1 <- ATM[-9]
#Preparing the data to calculate the distances among all ATMs and TWO reference pointS that is
#Had has tested one reference point, but two generated best results. 
Pontos_c <- ATM1[,7:8]
#Refenrence point 1
ponto_reference1 <- c(-79.73701, 41.99031)
#Refenrence point 2
ponto_reference2 <- c(-73.36494, 44.98635)

##Function to calculate the distance and time driving between each ATM and the
##centroid of the Clusters. Its takes time (several minutes per each cluster).
start_time <- Sys.time()
resposta_ref1 <- apply(Pontos_c, 1, distance_calculation, ponto_reference1)
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

ref_distances1 <- sapply(resposta_ref1, function(x) as.numeric(x[1]))
ATM1$Reference1 <- round((ref_distances1/1000),2)

##Function to calculate the distance and time driving between each ATM and the
##centroid of the Clusters. Its takes time (several minutes per each cluster).
start_time <- Sys.time()
resposta_ref2 <- apply(Pontos_c, 1, distance_calculation, ponto_reference2)
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

ref_distances2 <- sapply(resposta_ref2, function(x) as.numeric(x[1]))
ATM1$Reference2 <- round((ref_distances2/1000),2)

#Set seed for reproducibility
set.seed(1972)                                                         
Dbscan_cl <- dbscan(ATM1[, c("Longitude","Latitude","Reference1","Reference2")], eps = 8, MinPts = 80)
ATM1$clusters <- as.factor(Dbscan_cl$cluster)  # Add cluster assignment to the dataframe

#Now lets distribute this points without cluster using K-means and the centroids calculated by 
#the DBScan algorithm, for this it is necessary calculate the "cenrtoids" of each cluster 
j <- max(Dbscan_cl$cluster)
centroids <- data.frame()
for(i in 1:j){
  Pontos_c1 <- ATM1[,7:11] %>% filter(clusters == as.character(i))
  Pontos_c1 <- Pontos_c1[,c("Longitude","Latitude")]
  centroids[i,1:2] <- as.matrix(geomean(Pontos_c1))
}

# Getting a little fancier with it by adding the state borders
ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("New York"))
figure9 <- ggplot() + 
  geom_point(data =ATM1, aes(x = Longitude, y = Latitude, color = clusters, shape = clusters), size =1) +
  geom_point(data = centroids, aes(x = V1, y = V2), size = 2) + 
  geom_sf(data = ca_nv_map, fill = NA) +
  scale_shape_manual(values=seq(0,15)) +
  labs(title = 'DBScan and extra feature',
       y = '', x = 'Fig 9. Clustering with DBScan') +
  theme(legend.position = "none")
sumary_dbscan_extra <- summary(ATM1$clusters)

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure9, file="figures/figure9.RData")
figure9

#####################################################################################################
#Finally lets try combine both algorithms to reach a better outcome
#First will be run the DBScan with the parameters used in the session b3
#####################################################################################################
ATM1 <- ATM[-9]
#Preparing the data to calculate the distances among all ATMs and TWO reference pointS that is
#Had has tested one reference point, but two generated best results. 
Pontos_c <- ATM1[,7:8]
#Refenrence point 1
ponto_reference1 <- c(-79.73701, 41.99031)
#Refenrence point 2
ponto_reference2 <- c(-73.36494, 44.98635)

##Function to calculate the distance and time driving between each ATM and the
##centroid of the Clusters. Its takes time (several minutes per each cluster).
start_time <- Sys.time()
resposta_ref1 <- apply(Pontos_c, 1, distance_calculation, ponto_reference1)
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

ref_distances1 <- sapply(resposta_ref1, function(x) as.numeric(x[1]))
ATM1$Reference1 <- round((ref_distances1/1000),2)

##Function to calculate the distance and time driving between each ATM and the
##centroid of the Clusters. Its takes time (several minutes per each cluster).
start_time <- Sys.time()
resposta_ref2 <- apply(Pontos_c, 1, distance_calculation, ponto_reference2)
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

ref_distances2 <- sapply(resposta_ref2, function(x) as.numeric(x[1]))
ATM1$Reference2 <- round((ref_distances2/1000),2)

#Set seed for reproducibility
set.seed(1972)                                                         
Dbscan_cl <- dbscan(ATM1[, c("Longitude","Latitude","Reference1","Reference2")], eps = 8, MinPts = 80)
ATM1$clusters <- as.factor(Dbscan_cl$cluster)  # Add cluster assignment to the dataframe


#####################################################################################################
#Points with no cluster assigned are assigned by Kmeans
#Selecting only the ATMs not assigned for any cluster, it means cluster 0
Pontos_c <- ATM1 %>% filter(clusters == '0')
Pontos_c <- Pontos_c[,7:8]

#Now lets distribute this points without cluster using K-means and the centroids calculated by 
#the DBScan algorithm, for this it is necessary calculate the "cenrtoids" of each cluster 
j <- max(Dbscan_cl$cluster)
centroids <- data.frame()

for(i in 1:j){
  Pontos_c1 <- ATM1[,7:11] %>% filter(clusters == as.character(i))
  Pontos_c1 <- Pontos_c1[,c("Longitude","Latitude")]
  centroids[i,1:2] <- as.matrix(geomean(Pontos_c1))
}

set.seed(1972) 
k <- max(Dbscan_cl$cluster) # Best clustert number resulted from our latest analysis
clusters <- kmeans(Pontos_c, centers = centroids)
Pontos_c$clusters <- as.factor(clusters$cluster)  # Add cluster assignment to the dataframe

k <- as.integer(1)
j <- length(ATM1$clusters)

for(i in 1:j){
  if((as.numeric(ATM1[i,7]) == as.numeric(Pontos_c[k,1])) & (as.numeric(ATM_aux[i,8]) == as.numeric(Pontos_c[k,2])))
  {
    ATM1[i,11] <- Pontos_c[k,3]
    k <- as.integer(k+1)
    if(k>length(Pontos_c$clusters)) break
  }
}
#####################################################################################################
# Getting a little fancier with it by adding the state borders
ca_nv_map <- rnaturalearth::ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(name %in% c("New York"))
figure10 <- ggplot() + 
  geom_point(data =ATM1, aes(x = Longitude, y = Latitude, color = clusters, shape = clusters), size =1) +
  geom_point(data = centroids, aes(x = V1, y = V2), size = 2) + 
  geom_sf(data = ca_nv_map, fill = NA) +
  scale_shape_manual(values=seq(0,15)) +
  labs(title = 'DBScan, extra feature and Kmeans',
       y = '', x = 'Fig 10. Clustering with DBScan and Kmeans') +
  theme(legend.position = "none")

#Saving plot to be used in the report. Will be loaded into the RMD
#file. 
save(figure10, file="figures/figure10.RData")
figure10

# Reorder columns for call function
ATM1 <- ATM1[, c(1,2,3,4,5,6,7,8,11,9,10)]
resp <- calc_maxdist_maxtime(ATM1,6)

#Results
results_comparison$`Max distance`[5] <- max(resp$max_distance_c)
results_comparison$`Max time`[5] <- max(resp$max_time_c)
results_comparison$`Variance of # ATMs per Cluster`[5] <- var(clusters$size)
results_comparison$`Amplitude of # ATMs per Cluster`[5] <- max(clusters$size)-min(clusters$size)

clusters_size <- summary(ATM1$clusters)
clusters_size <- clusters_size[-1]
results_comparison$`# of Field Engineers`[5] <- calc_field_engineer(clusters_size)

#Because it took so long to calculate, has been saved the summary of results data 
#to be used during the analysis phase.
save(results_comparison, file="data/results_comparison.RData")

echo "# EDX_CYO" >> README.md
git init
git add README.md
git commit -m "first commit"
git branch -M main
git remote add origin https://github.com/fmitidieri/EDX_CYO.git
git push -u origin main


