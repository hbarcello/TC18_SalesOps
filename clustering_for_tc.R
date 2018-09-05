###########################################
# Code for postal code clustering 
# Author : Hunter Barcello
# Email : hbarcello@tableau.com
# Git : hbarcello
# Last Edit Date : 09-05-2018
# Created For : Tableau Territory Alignment
###########################################

#Before running this for the first time you may need to install the "flexclust" package for R

#Input file **must** have following fields (all case sensitive) or the script will fail
# "State"
# "Key"
# "CentroidX" (Latitude)
# "CentroidY" (Longitude)
# "Index" 


library("flexclust")


unit_cluster <- function(file_location, number_territories, output_location, seed=1980, from_prep=TRUE){

#Set random seed for repeatable results
set.seed(seed)  

#Prep CSVs currently using some tab delimiting
if(from_prep==TRUE){  
main.df <- read.csv(file_location, sep="\t")
}
  else{main.df <- read.csv(file_location)}


#Guess territory target size by using Index : Territory ratio
target.mean <- sum(main.df$Index)/number_territories

#target cluster size (sets size as a proporation of target.mean)
target.cluster.size = .15

#For units that have more than 70% of our target index, don't include in clustering process
main.df <- main.df[main.df$Index < target.mean*.70,]

#Create List of all the unique states
state_list <- unique(main.df$State)

#Creating temporary vectors that we will use inside the loop for each state
x_temp <- numeric()
y_temp <- numeric()
State <- character()
Cluster <- character()
key_temp <- character()
index_temp <- numeric()
output.df <- data.frame(CentroidX = double(), CentroidY = double(), 
                        State = character(), Key = character(), 
                        Index = numeric())
Cluster <- character(0)


#Iterate through each unique state
for(s in state_list){
  #Select only records with same state
  temp.df <- subset(main.df, State==s, select=c(CentroidX, CentroidY, Key, 
                                                Index))
  
  #calculate average index of a postal code in the state
  state.mean <- mean(temp.df$Index)
  
  #Some guesses at what an appropriate number of clusters might be
  #Always have at least 2 clusters in a State for some flexibility
  num.clust <- round(nrow(temp.df)/((target.mean*target.cluster.size)/state.mean))
  if(num.clust < 2){ num.clust=2 } else {num.clust}
  
  #Don't love this piece, but create a vector representing which state this is
  #Only doing this so we can keep the information intact when we write the data
  #if we are running this inside another application
  State <- c(State, rep(s, times=nrow(temp.df)))
  
  #Store key and x,y coords in this vector to add back in at the end
  x_temp <- c(x_temp, temp.df$CentroidX)
  y_temp <- c(y_temp, temp.df$CentroidY)
  key_temp <- c(key_temp, temp.df$Key)
  index_temp <- c(index_temp, temp.df$Index)
  
  temp.df$CentroidX <- as.numeric(temp.df$CentroidX)
  temp.df$CentroidY <- as.numeric(temp.df$CentroidY)
  
  kmedclus <- stepFlexclust(subset(temp.df, select=c(CentroidX, CentroidY)), k=num.clust, nrep=(10), FUN=kcca, family=kccaFamily("kmedians"))
  temp.df$cluster_assign_kmed <- paste(s,predict(kmedclus, subset(temp.df, select=c(CentroidX, CentroidY))))
  
  #Create information on clusters and their values
  cluster.agg.value <- aggregate(Index ~ cluster_assign_kmed, data=temp.df, sum)
  
  #Remove Values that are lower than 95% target Index for a territory
  #First - create vector with cluster names to remove
  clusters.to.break <- cluster.agg.value[as.numeric(cluster.agg.value$Index) > as.numeric(target.mean), ]
  
  
  
  if(nrow(clusters.to.break) > 0){
    
    
    #Create new data frame to cluster again
    to.recluster <- temp.df[temp.df$cluster_assign_kmed %in% clusters.to.break$cluster_assign_kmed, ]
    
    
    num.clust2 <- min(round(sum(to.recluster$Index)/(as.numeric(target.mean)*.15)), nrow(to.recluster)/2) #Calculate New Number of Clusters for Secondary Stage
    if(num.clust2 < 2){ num.clust2 <- 2} #Just to make sure we don't get that cluster less than 2 error
    
    kmedclus2 <- stepFlexclust(to.recluster[,1:2], k=num.clust2, nrep=(5), FUN=kcca, family=kccaFamily("kmedians"))
    to.recluster$new.clust <- paste("Round2", s, predict(kmedclus2, to.recluster[,1:2]))
    temp.df$cluster_assign_kmed[temp.df$cluster_assign_kmed %in% clusters.to.break$cluster_assign_kmed] <- to.recluster$new.clust
    
    
  }
  output.df <- rbind(output.df, data.frame(temp.df$CentroidX, 
                                           temp.df$CentroidY, 
                                           rep(s, times=nrow(temp.df)),
                                           temp.df$Key, temp.df$Index, 
                                           temp.df$cluster_assign_kmed))
}

colnames(output.df) <- c("CentroidX", "CentroidY", "State", "Key", "Index", "Cluster")


  {
  write.csv(output.df, output_location)

  }

}
