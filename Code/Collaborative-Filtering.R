# Load dataset and view the data
movies <-
  read.csv("D:/Dropbox/Capston/Data/ml-latest-small/movies.csv", header = TRUE, stringsAsFactors = FALSE)

ratings<-
  read.csv("D:/Dropbox/Capston/Data/ml-latest-small/ratings.csv", header = TRUE, stringsAsFactors = FALSE)

options(useFancyQuotes = FALSE) 

str(movies)
str(ratings)

library(reshape2)
library(recommenderlab)

#Create ratings matrix. Rows = userId, Columns = movieId
ratings <-
  dcast(ratings,
        userId ~ movieId,
        value.var = "rating",
        na.rm = FALSE)

#remove userIds from ratings
ratings <- as.matrix(ratings[, -1]) 


#Create a recommenderlab sparse matrix
ratings <- as(ratings, 'realRatingMatrix')
 ratings

#Create a RecommenderLab Evaluation Scheme, 
#Splitting the ratings into a Training set and a Test set:
#train_proportion is set to be 0.5
#number of given ratings per test user is set to be 10
#Goodratings is >= 5.0

evaluation_scheme <- evaluationScheme(
  ratings, 
  method='split',
  train=0.6,
  k=1,
  goodRating=5,
  given=10)

ratings_train <- getData(evaluation_scheme, 'train')
ratings_train

ratings_test_known <- getData(evaluation_scheme, 'known')
ratings_test_known

ratings_test_unknown <- getData(evaluation_scheme, 'unknown')
ratings_test_unknown


# Popularity-Based Recommender

popular_rec <- Recommender(
  data=ratings_train,
  method='POPULAR')


#Obtain top 10 recommendations for 1st user in dataset
popular_rec_pred <-
  predict(popular_rec, ratings[1], n = 10) 

#convert recommenderlab object to readable list
popular_rec_list <-
  as(popular_rec_pred, "list") 

#Obtain Top-10 recommendations and display it
popular_rec_result <- matrix(0, 10)
for (i in c(1:10)) {
  popular_rec_result[i] <- as.integer(popular_rec_list[[1]][i])
}
popular_rec_result <- as.data.frame(movies[popular_rec_result, 2])
colnames(popular_rec_result) <- list("Top-10 Movies-Popular")

popular_rec_result


# User-Based Collaborative-Filtering Recommender


#Compare algorithms with different parameter, finding the one with the best result
algorithms_UBCF <- list(
  "user-based CF1" = list(name = "UBCF", param = list(method = "Cosine", nn =
                                                       10)),
  "user-based CF2" = list(name = "UBCF", param = list(method = "Cosine", nn =
                                                       30)),
  "user-based CF3" = list(name = "UBCF", param = list(method = "Cosine", nn =
                                                       50)),
  "user-based CF4" = list(name = "UBCF", param = list(method = "Pearson", nn =
                                                       10)),
  "user-based CF5" = list(name = "UBCF", param = list(method = "Pearson", nn =
                                                       30)),
  "user-based CF6" = list(name = "UBCF", param = list(method = "Pearson", nn =
                                                       50))
)

#n=c denote top-N
evaluation_results_UBCF <-
  evaluate(evaluation_scheme, algorithms_UBCF, n = c(1, 3, 5, 10, 15, 20)) 

#plot the avged ROC
plot(evaluation_results_UBCF, legend = "topleft") 
#plot the avged prec/rec
plot(evaluation_results_UBCF, "prec/rec") 

# As we can see from the resulted figure, CF3 is the best, thus we will use it in the following section

# User-Based Collaborative Filtering
# normalizing by subtracting average rating per user
# use Pearson correlation
# number of Nearest Neighbors for calibration

user_based_cofi_rec <- Recommender(
  data=ratings_train,
  method='UBCF',           
  parameter=list(
    normalize='center',    
    method='Cosine',      
    nn=50                  
  ))


#Obtain top 10 recommendations for 1st user in dataset
user_based_cofi_rec_pred  <-
  predict(user_based_cofi_rec, ratings[1], n = 10) 

#convert recommenderlab object to readable list
user_based_cofi_rec_list <-
  as(user_based_cofi_rec_pred, "list") 

#Obtain Top-10 recommendations and display the result
user_based_cofi_rec_result <- matrix(0, 10)
for (i in c(1:10)) {
  user_based_cofi_rec_result[i] <- as.integer(user_based_cofi_rec_list[[1]][i])
}
user_based_cofi_rec_result <- as.data.frame(movies[user_based_cofi_rec_result, 2])
colnames(user_based_cofi_rec_result) <- list("Top-10 Movies-UBCF")

user_based_cofi_rec_result


# Latent-Factor Collaborative Filtering Recommender

#Compare algorithms with different parameter, finding the one with the best result
algorithms_SVD <- list(
  "latent_factor CF1" = list(name = "SVD", param = list(k = 10,maxiter = 60)),
  "latent_factor CF2" = list(name = "SVD", param = list(k = 20, maxiter = 60)),
  "latent_factor CF3" = list(name = "SVD", param = list(k = 10, maxiter = 80)),
  "latent_factor CF4" = list(name = "SVD", param = list(k = 20, maxiter = 80)),
  "latent_factor CF5" = list(name = "SVD", param = list(k = 10, maxiter = 100)),
  "latent_factor CF6" = list(name = "SVD", param = list(k = 20, maxiter = 100))
)

#n=c denote top-N
evaluation_results_SVD <-
  evaluate(evaluation_scheme, algorithms_SVD, n = c(1, 3, 5, 10, 15, 20)) 

#plot the avged ROC
plot(evaluation_results_SVD, legend = "bottomright") 

#plot the avged prec/rec
plot(evaluation_results_SVD, "prec/rec") 

# As we can see from the result, CF5 is the best, thus we will use its parameter in the following section

#Latent-Factor Collaborative Filtering Recommender with matrix factorization by Singular-Value Decomposition (SVD)
# number of latent factors is set to be 10
# normalize is "center", which means subtracting average rating per user

  latent_factor_cofi_rec <- Recommender(
    data=ratings_train,
    method='SVD',            
    parameter=list(
      k = 10,        
      normalize ='center'   
    ))
  
#Obtain top 10 recommendations for 1st user in dataset
  user_based_cofi_rec_pred  <-
    predict(user_based_cofi_rec, ratings[1], n = 10) 
  
#convert recommenderlab object to readable list
  user_based_cofi_rec_list <-
    as(user_based_cofi_rec_pred, "list") 
  
#Obtain Top-10 recommendations
  user_based_cofi_rec_result <- matrix(0, 10)
  for (i in c(1:10)) {
    user_based_cofi_rec_result[i] <- as.integer(user_based_cofi_rec_list[[1]][i])
  }
  user_based_cofi_rec_result <- as.data.frame(movies[user_based_cofi_rec_result, 2])
  colnames(user_based_cofi_rec_result) <- list("Top-10 Movies-SVD")
  
  user_based_cofi_rec_result
  

  
#Compare the five recommender algorithms for the top-N list
   algorithms <- list(
     "random items" = list(name = "RANDOM", param = NULL),
     "popular items" = list(name = "POPULAR", param = NULL),
     "user-based CF" = list(name = "UBCF", param = list(method = "Cosine", nn =50)),
     "Latent CF" = list(name = "SVD", param = list(k = 10, maxiter = 100))
   )
   
#n=c denote top-N   
   evaluation_results <-
     evaluate(evaluation_scheme, algorithms, n = c(1, 3, 5, 10, 15, 20)) 
   
#get results for all runs of 'random items'
eval_results <- getConfusionMatrix(evaluation_results[[1]])

#alternatively, get avged result for 'random items'
avg(evaluation_results[[1]])
   
#plot the avged ROC   
plot(evaluation_results, legend = "topleft") 
   
#plot the avged prec/rec
plot(evaluation_results, "prec/rec") 
   
# Make predictions on the Test dataset for the ratings and evaluate their 
  
# Popularity-Based Recommender 
   popular_rec_pred_rating <- predict(
     popular_rec,
     ratings_test_known,
     type='ratings')
   
   popular_rec_pred_acc_rating <- calcPredictionAccuracy(
     popular_rec_pred_rating,
     ratings_test_unknown)
   
   popular_rec_pred_acc_rating
   

# User-Based Collaborative-Filtering Recommender   
   user_based_cofi_rec_pred_rating <- predict(
     user_based_cofi_rec,
     ratings_test_known,
     type='ratings')
   
   user_based_cofi_rec_pred_acc_rating <- calcPredictionAccuracy(
     user_based_cofi_rec_pred_rating,
     ratings_test_unknown)
   
   user_based_cofi_rec_pred_acc_rating
   

# Latent-Factor Collaborative-Filtering Recommender   
    latent_factor_cofi_rec_pred_rating <- predict(
      latent_factor_cofi_rec,
      ratings_test_known,
      type='ratings')
   
    latent_factor_cofi_red_pred_acc_rating <- calcPredictionAccuracy(
      latent_factor_cofi_rec_pred_rating,
      ratings_test_unknown)
   
    latent_factor_cofi_red_pred_acc_rating
   
#Comparison of RMSE, MSE, and MAE for recommender methods for the evaluation scheme.
   eval_ratings_results <- evaluate(evaluation_scheme, algorithms, type = "ratings")
   plot(eval_ratings_results, legend="topright")
   

   