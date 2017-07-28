#Load the dataset 
movies <- read.csv("D:/Dropbox/Capston/Data/ml-latest-small/movies.csv", header = TRUE, stringsAsFactors = FALSE)

ratings <- read.csv("D:/Dropbox/Capston/Data/ml-latest-small/ratings.csv", header = TRUE, stringsAsFactors = FALSE)

# 

# Prepare the data by spliting the dataset
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:7)


#Create a movie genres matrix  
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")

#create a empty matrix
genre_matrix <- matrix(0,9126,18) 
#first row is set to be the genre list, column names is set be genre list
genre_matrix[1,] <- genre_list 
colnames(genre_matrix) <- genre_list 

for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe, remove first row (the genre list), convert from characters to integers
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE)
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
}

#convert the ratings into a binary format, ratings of 4 & 5 are 1, representing likes, and ratings of 1&2&3 are -1, representing dislikes
binaryratings <- ratings
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}

#Convert the binaryratings matrix to the format we want, where rows are are movieIds, cols are userIds
binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
#remove movieIds col
binaryratings2 = binaryratings2[,-1] 

movieIds <- length(unique(movies$movieId)) 
movieIds  #9125

ratingmovieIds <- length(unique(ratings$movieId)) 
ratingmovieIds #9066

#We notice that the ratings dataset has less movies than the ones in movies dataset, so we need to remove rows that are not rated from movies dataset
movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL

genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

#Obtain the dot product for User Profiles
result = matrix(0,18,671)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}

#Convert the user profile to Binary scale, positive values were set to 1 to represent likes, negative values were set to 0 to represent dislikes.
for (i in 1:nrow(result)){
  if (result[i] < 0){
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}


# Now we can measure the similarity between user profiles and the movie genre matrix. Since a user probably like similiar movies.

#First user's profile
result2 <- result[1,] 
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) 

#Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Cosine")
sim_results <- as.data.frame(as.matrix(sim_results[1:9066]))
rows <- which(sim_results == min(sim_results))

#Recommended movies
movies[rows,2]


#Calculate Cosine distance between user profile and all movies
sim_results2 <- dist(sim_mat, method = "Jaccard")
sim_results2 <- as.data.frame(as.matrix(sim_results2[1:9066]))
rows <- which(sim_results2 == min(sim_results2))
#Recommended movies
movies[rows,2]
