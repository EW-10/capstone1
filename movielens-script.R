##############
# PLEASE NOTE: 
##############

# Running this whole R script took 12 minutes on my computer, with the browser closed.

# In the immediately following comments section, I present the code along with a step-by-step/line-by-line
# explanation of the code.

# Please note, that I appreciate that one would not normally  comment line by line as I have done below,
# but, for purposes of this project I believe this is a clearer demonstration of my understanding of what
# each line of code does.

#
#
#
#

# Following the comments section, I present the executable code by itself.


##############
##############

#
#
#

###################
# Comments Section
###################

#
#
#
#

# Please make sure you are in the directory where you want the files to be saved to.

# getwd() # To make sure you are in the directory where you want the files to be saved to.



# The following code that is used to create the edx and validation sets was provided as part of this project, and 
# was not written by me.

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# library(tidyverse)
# library(caret)
# library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# dl <- tempfile()
# download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                 col.names = c("userId", "movieId", "rating", "timestamp"))

# movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
# colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))


# movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
# set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
# test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
# edx <- movielens[-test_index,]
# temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
# validation <- temp %>% 
#  semi_join(edx, by = "movieId") %>%
#  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
# removed <- anti_join(temp, validation)
# edx <- rbind(edx, removed)

# rm(dl, ratings, movies, test_index, temp, movielens, removed)

# This is the end of the code that was provided as part of this project.
# From now on, the code that follows was written by me.

# I will save the datasets as created above. 
# The following lines of code are optional.

# getwd() # To make sure I am in the directory where I want my objects to be saved.

# Working directory is "capstone". (The full path is "~/projects/capstone").

# setwd("movielens/rda")

# getwd()

# If you would like to save the edx and validation sets for later use, you can use the following lines
# of code to save the edx and validation sets for later use.

# save(edx, file = "edx.rda")

# save(validation, file = "validation.rda")

##################################################################################################
# The edx and validation datasets were created above with the code that was provided as part of 
# this project. I included above the code that was used to generate these datasets.
##################################################################################################

#
#
#
#
#

# I now present the code that generates my predicted movie ratings and RMSE score. 

#
#
#
#

###################################################################
# STEP 1: Creation of train and test sets based on the edx dataset.
###################################################################

#####################################################################################################
# The following code is used to create the train and test sets that I use based on the edx dataset.
#####################################################################################################

# Loading/installing a library that is used in this project.

# if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# library(lubridate)

# The following library is used as well.

# if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

# library(ggthemes)

# If the edx dataset has been removed and has not been loaded yet, use the following code.

# getwd() # To make sure you are in the directory where you previously saved the edx dataset ("edx.rda").

# load("edx.rda")

# I now create the train and test sets that I use based on the edx dataset.

# First, I will set the seed so that the results are reproducible.

# The following line of code should be used if using R 3.6 or later. If using R 3.5 or earlier, use `set.seed(1)`.

# set.seed(2, sample.kind="Rounding")

# Defining a vector y, upon which the data partitioning will take place.

# y <- edx$rating

# Create a test_index to index the rows that will be included in the test set.
# The test set will include 20% of the data (p = 0.2).

# test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

# Create train and test sets based on the test_index as created above.

# train_set <- edx[-test_index,]
# test_set <- edx[test_index]

# Create a temporary file that will enable to keep the rows that are removed from the test set and to add them back
# into the train set.

# temp <- test_set

# Removing rows from the test set that their "movieId" is not included in the train set.

# test_set <- test_set %>%
#  semi_join(train_set, by = "movieId")

# Removing rows from the test set that their "userId" is not included in the train set.

# test_set <- test_set %>%
#  semi_join(train_set, by = "userId")

# Keeping the rows that were removed from the test set above.

# removed <- anti_join(temp, test_set)

# Adding the rows that were removed from the test set above back into the train set.

# train_set <- train_set %>%
#  rbind(removed)

# Removing the temporary files - temp, removed.

# rm(temp, removed)

# Removing the edx dataset if it was saved above in order to free space.

# rm(edx)

# Saving the datasets that were created above, train_set and test_set, for later use as rda files.
# This line is optional.

# save(train_set, file = "train_set.rda")
# save(test_set, file = "test_set.rda")

# The following two lines should only be used if train_set and test_set have been removed and have not 
# been loaded yet.

# load("test_set.rda")
# load("train_set.rda")

########################
# STEP 2: RMSE function
########################

######################################################################################
# Writing an RMSE function to be used for testing the model for movie recommendations.
######################################################################################

# The RMSE function is written below compares a vector of predicted ratings to the actual ratings
# for these movie-user combinations. The function used to estimate the error of the model is the
# RMSE function - or Root Mean Squared Error - sqrt(mean((predicted - actual)^2)).

# RMSE <- function(predicted, actual){
#  sqrt(mean((predicted - actual)^2))
# }

###########################################################
# STEP 3: Creation of cross-validation train and test sets.
###########################################################

# Now I will carve out a cross-validation set based on the train set. This set will be used in order to select
# the optimal (or best) lambda for the model. The parameter lambda is used for regularizing the effects used
# in the model.

# The cross-validation datasets that are based on the train set are created so that the test set will not be used
# to select the parameter lambda, in order not to overtrain the model on the test set.
# The book by Prof. Irizarry states that - "If I train an algorithm on the same dataset that I use to compute
# the apparent error, I might be overtraining. In general, when I do this, the apparent error will be an
# underestimate of the true error.” 

# For this reason, I created the following cross-validation sets that are carved out of the train set
# in order to run cross-validation and select the optimal (best) lambda for the model.

# head(train_set)

# Use the following lines of code to re-load the train set, if you have saved and removed it.

# getwd() # To make sure you are in the directory where the relevant files are located.

# load("train_set.rda") # Loading the train set.

# I am going to save now the rating column of the train set to an object named "y" in order to 
# run the data partitioning on it.

# y <- train_set$rating

# I am going to set the seed using set.seed() in order to make sure that the data partitioning and the results
# are reproducible. The following line of code should be used if using R 3.6 or later. 
# If using R 3.5 or earlier, use `set.seed(1)`.

# set.seed(3, sample.kind = "Rounding")

# I now create the test index, with 20% of the data from the train set included in the cv (cross-validation)
# test set.

# test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

# Now I create the test and train cv datasets to be used for cross-validation of the parameter lambda for my model.

# test_cv_set <- train_set[test_index,]

# train_cv_set <- train_set[-test_index,]

# Now, I will make sure that the cross-validation test set will contain only users and movies that are included
# in the cross-validation train set.

# I will create a temporary "temp" object to store the cross-validation test dataset before I will remove
# any rows from it.

# temp <- test_cv_set

# Now I will remove rows from the cross-validation test set for users and for movies that are not included in the
# cross-validation train set.

# test_cv_set <- test_cv_set %>%
#  semi_join(train_cv_set, by = "movieId") %>%
#  semi_join(train_cv_set, by = "userId")

# I will keep the rows that were removed in a temporary object called "removed".

# removed <- anti_join(temp, test_cv_set)

# This is the number of rows that were removed from the cross-validation test set.

# nrow(removed)

# I will also make sure that the same number of rows that were removed from the test set are now included in 
# the object "removed".

# identical(nrow(removed), nrow(temp) - nrow(test_cv_set))

# Now I will add the rows that were removed from the cross-validation test set back into the cross-validation
# train set.

# train_cv_set <- train_cv_set %>%
#  rbind(removed)

# Now I will make sure that the cross-validation train and test set I created contain together the same number 
# of rows that are in the original train set that they were created from.

# identical(nrow(train_set), nrow(train_cv_set) + nrow(test_cv_set))

# Now I will remove the temporary files "removed" and "temp".

# rm(temp, removed)

# It is also possible to remove the train set to free space. If the train set has not been saved before,
# you should save it before you remove it.
# The following two lines of code are optional.

# save(train_set, file = "train_set.rda")
# rm(train_set)

#############################################################################
# STEP 4: Cross-Validation to select the best (optimal) lambda for the model.
#############################################################################

# I will now start the cross-validation to select the best (optimal) lambda for my model.

# First, I will create a variable mu - a variable that will store the overall mean rating for all users and for 
# all movies for the cross-validation train set ("train_cv_set").

# mu <- mean(train_cv_set$rating)

# This is the resulting mu.

# mu

##########################################################
# Wrangling the cross-validation train and test sets.
##########################################################

# I will now wrangle the cross-validation train and test sets. First, I will remove the title column since I do
# not use the title as part of my analysis.

# train_cv_set <- train_cv_set %>%
#  select(-title)

# test_cv_set <- test_cv_set %>%
#  select(-title)

# I am now going to wrangle the cross-validation train and test sets in order to include further columns representing
# a time variable that are all based on the original "timestamp" column. 
# The following columns will be added to both the cv train and the cv test datasets: columns of date, week, day and
# year.

# The date column is created with the function "as_datetime()" applied to the "timstamp" column.

# The week column is created with the function "round_date(.., unit = "week") applied to the "date" column.

# The day column represents the day of the year. It is created with the function yday() applied to the "date" column.

# The year column is created with the function year() applied to the "date" column.

# train_cv_set <- train_cv_set %>%
#  mutate(date = as_datetime(timestamp)) %>%
#  mutate(week = round_date(date, unit = "week")) %>%
#  mutate(day = yday(date)) %>%
#  mutate(year = year(date))

# test_cv_set <- test_cv_set %>%
#  mutate(date = as_datetime(timestamp)) %>%
#  mutate(week = round_date(date, unit = "week")) %>%
#  mutate(day = yday(date)) %>%
#  mutate(year = year(date))

#####################################################################
# Cross-Validation to select the best (optimal) lambda for the model.
#####################################################################

# I will now run cross-validation on the cross-validation train and test sets as created above
# in order to select the best lambda for the model.

# Below I define the vector of lambdas on which I will run the cross-validation to select the best lambda for the
# model.

# lambdas <- seq(0, 10, 0.25)

# I will now run cross-validation to select the best lambda in order to regularize all of the effects that
# are included in the model. 

# Note the definition of each one of the effects as included in this model below.

# Also note that for simplicity, when I define each one of the effects, the variable name does not include
# the letter "r" for "regularized". For simplicity, I omitted the letter "r" for "regularization" from all
# of the variable names. However, please note that all of the variables (effects) included in the model
# are regularized, as you can see below. 

# Also note that I am cross-validating to select the optimal (best) lambda to be used for all of the effects.
# The same lambda will be used to regularize all of the effects included in the model. At previous models I 
# tried to select a different lambda for each effect, but in my experience this did not improve much the RMSE
# on the test set, therefore I decided in my final model to select one lambda for all of the effects, as you
# can see below.

# Also, please note that if you choose to run the code below, it is heavy and might take some time to run.
# You might want to consider saving some of the datasets created above and removing them in order to free
# space before running it. I include here the code for saving and removing datasets if you wish to do that
# before running the cross-validation as it appears below.

# saving and removing datasets to free space.

# getwd() # To make sure you are in the directory where you want the files to be saved.

# save(edx, file = "edx.rda")
# rm(edx)

# save(train_set, file = "train_set.rda")
# rm(train_set)

# save(test_set, file = "test_set.rda")
# rm(test_set)

# rm(y)

# Note: The following cross-validation took 5 minutes to run on my computer.

# results <- sapply(lambdas, function(l){
  
#  reg_movie_effect <- train_cv_set %>%
#    group_by(movieId) %>%
#    summarize(b_i = sum(rating - mu)/(n() + l))
  
#  train_cv_set <- train_cv_set %>%
#    left_join(reg_movie_effect, by = "movieId")
  
#  reg_user_effect <- train_cv_set %>%
#    group_by(userId) %>%
#    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  
#  train_cv_set <- train_cv_set %>%
#    left_join(reg_user_effect, by = "userId")
  
#  reg_genre_effect <- train_cv_set %>% 
#    group_by(genres) %>%
#    summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l))
  
#  train_cv_set <- train_cv_set %>%
#    left_join(reg_genre_effect, by = "genres")
  
#  reg_week_effect <- train_cv_set %>%
#    group_by(week) %>%
#    summarize(b_w = sum(rating - mu - b_i - b_u - b_g)/(n() + l))
  
#  train_cv_set <- train_cv_set %>%
#    left_join(reg_week_effect, by = "week")
  
#  reg_day_effect <- train_cv_set %>%
#    group_by(day) %>%
#    summarize(b_d = sum(rating - mu - b_i - b_u - b_g - b_w)/(n() + l))
  
#  train_cv_set <- train_cv_set %>%
#    left_join(reg_day_effect, by = "day")
  
#  reg_year_effect <- train_cv_set %>%
#    group_by(year) %>%
#    summarize(b_y = sum(rating - mu - b_i - b_u - b_g - b_w)/(n() + l))
  
#  train_cv_set <- train_cv_set %>%
#    left_join(reg_year_effect, by = "year")

#  test_cv_set <- test_cv_set %>%
#    left_join(reg_movie_effect, by = "movieId") %>%
#    left_join(reg_user_effect, by = "userId") %>%
#    left_join(reg_genre_effect, by = "genres") %>%
#    left_join(reg_week_effect, by = "week") %>%
#    left_join(reg_day_effect, by = "day") %>%
#    left_join(reg_year_effect, by = "year")
  
#  test_cv_set <- test_cv_set %>%
#    mutate(predicted = mu + b_i + b_u + b_g + b_w + b_d + b_y)
  
#  rmse <- RMSE(test_cv_set$predicted, test_cv_set$rating)

#  rmse
#})

# min_rmse_cv <- min(results)

# min_rmse_cv

# best_lambda <- lambdas[which.min(results)]

# best_lambda

# The best lambda obtained for the model for regularizing all of the effects is best_lambda = 5.

# I will present a graph showing the effect of lambda on the resultant RMSE as computed on the test cv
# data set.

# df <- data.frame(Lambda = lambdas, RMSE = results)

# plot_lambda_rmse <- df %>% ggplot(aes(Lambda, RMSE)) + 
#  geom_point(size = 4, col = "maroon3") +
#  geom_point(aes(x = best_lambda, y = min_rmse_cv), size = 4, col = "royalblue3") +
#  ggtitle("Effects of the Lambda on the RMSE") +
#  theme_igray()

# plot_lambda_rmse

# This plot is presented as part of my Rmd report. If you wish, you can save it with this line of code below.

# ggsave("plot_lambda_rmse.png")

##########################################################
# STEP 5: Testing the RMSE for this model on the test set.
##########################################################

# Please note the test set has not been used so far and has not been used to select the best lambda for 
# the model.

# First, I am going to fit the model on the whole train set, and then test the RMSE on the test set.

# If the train and test sets have been saved and removed, you need to re-load them.

# load("train_set.rda")
# load("test_set.rda")

# I save the train and test sets under the names train_s and test_s ("s" for "small") and then wrangle them,
# first to remove the column "title" that I do not use in the analysis.

# train_s <- train_set %>%
#  select(-title)

# test_s <- test_set %>%
#  select(-title)

# It is an option to remove the original train and test sets now to clear space. 
# If they have not been saved before, they should be saved before they are removed.

# save(train_set, file = "train_set.rda")
# save(test_set, file = "test_set.rda")

# Now it is optional to remove the train and test sets.

# rm(train_set, test_set)

# I will now wrangle the train and test sets (train_s and test_s) to include the following columns - 
# date, week, day and year, as defined above.

# train_s <- train_s %>%
#  mutate(date = as_datetime(timestamp)) %>%
#  mutate(week = round_date(date, unit = "week")) %>%
#  mutate(day = yday(date)) %>% 
#  mutate(year = year(date))

# test_s <- test_s %>%
#  mutate(date = as_datetime(timestamp)) %>%
#  mutate(week = round_date(date, unit = "week")) %>%
#  mutate(day = yday(date)) %>%
#  mutate(year = year(date))

# I will now redefine mu - this time as the overall mean rating for all movies and for all users in the train set.

# mu <- mean(train_s$rating)

# mu

# I will now compute the effects included in the model using the best lambda that was selected above
# for all of the effects. 


# The effects that are included in the final model are: 

## Movie effect (b_i)

## User effect (b_u)

## Genre combination effect (b_g)

### and three Time effects, as follows.

## Week by week effect over the whole time period (b_w)

## Day of the year effect (b_d)

## Year effect (b_y)


# Note again that the letter "r" (which stands for regularized") from all these effects' variable names for
# simplicity reasons. Please also note that all the effects below are regularized, using the lambda
# that was selected above. In the code below I simply call these effects "effects"
# without mentioning the word "regularized" for simplicity.


# reg_movie_effect <- train_s %>%
#  group_by(movieId) %>%
#  summarize(b_i = sum(rating - mu)/(n() + best_lambda))

# train_s <- train_s %>%
#  left_join(reg_movie_effect, by = "movieId")

# reg_user_effect <- train_s %>%
#  group_by(userId) %>%
#  summarize(b_u = sum(rating - mu - b_i)/(n() + best_lambda))

# train_s <- train_s %>%
#  left_join(reg_user_effect, by = "userId")

# reg_genre_effect <- train_s %>% 
#  group_by(genres) %>%
#  summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + best_lambda))

# train_s <- train_s %>%
#  left_join(reg_genre_effect, by = "genres")

# reg_week_effect <- train_s %>%
#  group_by(week) %>%
#  summarize(b_w = sum(rating - mu - b_i - b_u - b_g)/(n() + best_lambda))

# train_s <- train_s %>%
#  left_join(reg_week_effect, by = "week")

# reg_day_effect <- train_s %>%
#  group_by(day) %>%
#  summarize(b_d = sum(rating - mu - b_i - b_u - b_g - b_w)/(n() + best_lambda))

# train_s <- train_s %>%
#  left_join(reg_day_effect, by = "day")

# reg_year_effect <- train_s %>%
#  group_by(year) %>%
#  summarize(b_y = sum(rating - mu - b_i - b_u - b_g - b_w - b_d)/(n() + best_lambda))

#  train_s <- train_s %>%
#    left_join(reg_year_effect, by = "year")

# I will now join into the test set all of the effects as computed above based on the train set, in order
# to make the predictions for the test set using the model I built.

# test_s <- test_s %>%
#  left_join(reg_movie_effect, by = "movieId") %>%
#  left_join(reg_user_effect, by = "userId") %>%
#  left_join(reg_genre_effect, by = "genres") %>%
#  left_join(reg_week_effect, by = "week") %>%
#  left_join(reg_day_effect, by = "day") %>%
#  left_join(reg_year_effect, by = "year")

# I  will now make predictions for the test set based on my model.

# test_s <- test_s %>%
#  mutate(predicted = mu + b_i + b_u + b_g + b_w + b_d + b_y)

# I will now compute the RMSE of my model for the test set.

# rmse_test_set <- RMSE(test_s$predicted, test_s$rating)

# rmse_test_set

# The RMSE obtained with my model on the test set is 0.8647502.

###############################################
# STEP 6: Fitting the model on the edx dataset.
###############################################

# I will now fit my model on the whole of the edx dataset, before testing its RMSE on the validation
# set.

# I will save the edx dataset under the name "edx_s". Then the edx set itself can be saved and removed
# to free space.

# If the edx set was saved and removed before, you will now need to re-load it.

# load("edx.rda")

# edx_s <- edx

# If edx set has not been saved before, you might want to save it before you remove it to free space.

# save(edx, file = "edx.rda")

# This line is optional. You can remove the edx dataset now to free space.

# rm(edx)

# Now I will wrangle edx_s which is based on the edx dataset.

# edx_s <- edx_s %>%
#  select(-title)

# edx_s <- edx_s %>%
#  mutate(date = as_datetime(timestamp)) %>%
#  mutate(week = round_date(date, unit = "week")) %>%
#  mutate(day = yday(date)) %>%
#  mutate(year = year(date))

# I will now fit the model on the whole of the edx dataset. I will then test it on the validation set.

# First, I redefine mu as the overall mean rating for all movies and all ratings in the edx dataset.

# mu <- mean(edx_s$rating)

# mu

# I will now compute the effects that are included in the model using the best lambda as
# selected above. The effects will now be computed using the whole of the edx dataset.

# reg_movie_effect <- edx_s %>%
#  group_by(movieId) %>%
#  summarize(b_i = sum(rating - mu)/(n() + best_lambda))

# edx_s <- edx_s %>%
#  left_join(reg_movie_effect, by = "movieId")

# reg_user_effect <- edx_s %>%
#  group_by(userId) %>%
#  summarize(b_u = sum(rating - mu - b_i)/(n() + best_lambda))

# edx_s <- edx_s %>%
#  left_join(reg_user_effect, by = "userId")

# reg_genre_effect <- edx_s %>% 
#  group_by(genres) %>%
#  summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + best_lambda))

# edx_s <- edx_s %>%
#  left_join(reg_genre_effect, by = "genres")

# reg_week_effect <- edx_s %>%
#  group_by(week) %>%
#  summarize(b_w = sum(rating - mu - b_i - b_u - b_g)/(n() + best_lambda))

# edx_s <- edx_s %>%
#  left_join(reg_week_effect, by = "week")

# reg_day_effect <- edx_s %>%
#  group_by(day) %>%
#  summarize(b_d = sum(rating - mu - b_i - b_u - b_g - b_w)/(n() + best_lambda))

# edx_s <- edx_s %>%
#  left_join(reg_day_effect, by = "day")

# reg_year_effect <- edx_s %>%
#  group_by(year) %>%
#  summarize(b_y = sum(rating - mu - b_i - b_u - b_g - b_w - b_d)/(n() + best_lambda))

#  edx_s <- edx_s %>%
#    left_join(reg_year_effect, by = "year")

#####################################################################
# STEP 7: I will now test the RMSE of my model on the validation set.
#####################################################################

# I will now test the final model on the validation set.

# If the validation set has been saved and removed before in order to free space, you will need to re-load it now.

# load("validation.rda")

# I will wrangle the validation set in order to remove the column "title", and also in order to include the columns
# date, week, day and year, as defined above. The validation set will be saved under the name "validation_s"
# ("s" for short) before I will do any wrangling on it.

# validation_s <- validation

# This step is optional. You can save and remove the validation set. You should save it before removing it
# if you have not saved it before.

# save(validation, file = "validation.rda")

# It is now optional to remove the validation set in order to free space.

# rm(validation)

# validation_s <- validation_s %>%
#  select(-title)

# validation_s <- validation_s %>%
#  mutate(date = as_datetime(timestamp)) %>%
#  mutate(week = round_date(date, unit = "week")) %>%
#  mutate(day = yday(date)) %>%
#  mutate(year = year(date))

# I will now join into the validation set the regularized effects as computed above using the edx dataset.

# validation_s <- validation_s %>%
#  left_join(reg_movie_effect, by = "movieId") %>%
#  left_join(reg_user_effect, by = "userId") %>%
#  left_join(reg_genre_effect, by = "genres") %>%
#  left_join(reg_week_effect, by = "week") %>%
#  left_join(reg_day_effect, by = "day") %>%
#  left_join(reg_year_effect, by = "year")

# I will now make predictions for the validation set.

# validation_s <- validation_s %>%
#  mutate(predicted = mu + b_i + b_u + b_g + b_w + b_d + b_y)

# The predicted movie ratings for the validation set are in the column validation_s$predicted.

# I will now compute the RMSE as a result of applying my final model to the validation set.

# rmse_final_model <- RMSE(validation_s$predicted, validation_s$rating)

# rmse_final_model

# The RMSE obtained for the validation set as a result of the final model is 0.8642704.

# tibble("RMSE final model" = rmse_final_model) %>%
#  knitr::kable()


#############################
# Saving and removing objects
#############################


# The following lines of code are optional. 

# Please note that the following objects are used as part of the Rmd file, so if you would like to run 
# the Rmd file yourself, you will need to save these objects first.

# getwd() # To make sure you are in the directory where you want the files to be saved.

# save(edx, file = "edx.rda")

# save(edx_s, file = "edx_s.rda")

# save(train_set, file = "train_set.rda")

# save(RMSE, file = "RMSE.rda")

# save(min_rmse_cv, file = "min_rmse_cv.rda")

# save(lambdas, file = "lambdas.rda")

# These objects can now be removed.

# rm(edx, edx_s, train_set, RMSE, min_rmse_cv, lambdas)


# The goal of this project was to create predicted ratings for the validation set and obtain RMSE for my model.
# It is possible to save the wrangled validation set validation_s that now contains the column "predicted" 
# for my predicted ratings.


# Saving the "validation_s" object which includes my predictions for the validation set.

# save(validation_s, file = "validation_s.rda")

# This object can now be removed.

# rm(validation_s)


# It is possible to save the "df" object, in order to reproduce the RMSE vs Lambda plot at a later stage.
# The following line of code is optional.

# save(df, file = "df.rda")

# This object can be now removed.

# rm(df)


# The best lambda obtained using cross-validation with my model can be saved for future reference.

# save(best_lambda, file = "best_lambda.rda")

# rm(best_lambda)


# mu, which is computed as the overall mean rating for all movies and for all users on the whole edx dataset
# can be saved for future reference and removed as well.

# save(mu, file = "mu.rda")

# rm(mu)


# Again, the effects computed using my model on the edx dataset can be saved for later use.
# The code to save and remove the effects computed appears below. This is optional.

# save(reg_day_effect, file = "reg_day_effect.rda")

# rm(reg_day_effect)

# save(reg_genre_effect, file = "reg_genre_effect.rda")

# rm(reg_genre_effect)

# save(reg_movie_effect, file = "reg_movie_effect.rda")

# rm(reg_movie_effect)

# save(reg_user_effect, file = "reg_user_effect.rda")

# rm(reg_user_effect)

# save(reg_week_effect, file = "reg_week_effect.rda")

# rm(reg_week_effect)

# save(reg_year_effect, file = "reg_year_effect.rda")

# rm(reg_year_effect)


# If the train_set has not been saved, you can save it now.

# save(train_set, file = "train_set.rda")

# rm(train_set)


# Saving the final RMSE obtained for the validation set.

# save(rmse_final_model, file = "rmse_final_model.rda")

# Before removing the RMSE for the final model, I will present it again, as this is the final result of this project.

# tibble("RMSE final model" = rmse_final_model) %>%
#  knitr::kable()

# rm(rmse_final_model)

# The other objects can now be removed. 

# Please note that if you have already removed some of these objects, you might get a warning when you run the 
# line of code below.

# rm(edx, edx_s, plot_lambda_rmse, test_cv_set, test_index, test_s, test_set, train_cv_set, train_s, train_set, 
# validation, validation_s, lambdas, min_rmse_cv, results, rmse_test_set, y)

# Alternatively, if you are sure that you have saved all the objects that you are interested in for future referenece,
# you can remove all objects. This is optional.

# rm(list = ls())

# This is the end of the R script for this project.

#######***########
#######***########

#
#
#
#
#
#


##########################
# Executable Code Section
##########################


# The R script code with less comments follows, in order to make its review easier.

# This code is executable.


###################
# Program Overview
###################

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# This code was provided as part of this project.

# The code that follows below was written by me.


###################################################################
# STEP 1: Creation of train and test sets based on the edx dataset.
###################################################################

########################
# STEP 2: RMSE function
########################

###########################################################
# STEP 3: Creation of cross-validation train and test sets.
###########################################################

#############################################################################
# STEP 4: Cross-Validation to select the best (optimal) lambda for the model.
#############################################################################

##########################################################
# STEP 5: Testing the RMSE for this model on the test set.
##########################################################

###############################################
# STEP 6: Fitting the model on the edx dataset.
###############################################

#####################################################################
# STEP 7: I will now test the RMSE of my model on the validation set.
#####################################################################


#############################
# Saving and removing objects
#############################

#
#
#
#
#
#


############################
# Now - the executable code
############################



# Please make sure you are in the directory where you want the files to be saved to.

# getwd()



# The following code that is used to create the edx and validation sets was provided as part of this project, and 
# was not written by me.


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# This is the end of the code that was provided as part of this project.
# From now on, the code that follows was written by me.

# I will save the datasets as created above.

# The following lines of code are optional.

# To find out my working directory.
# getwd()

# Working directory is "capstone". (The full path is "~/projects/capstone").

# setwd("movielens/rda")
# getwd()

# If you would like to save the edx and validation sets for later use, you can use the following lines
# of code to save the edx and validation sets for later use.

# save(edx, file = "edx.rda")
# save(validation, file = "validation.rda")

# The code above described was used to create the edx and validation datasets using the code provided
# as part of this project.

# I now present the code that generates my predicted movie ratings and RMSE score. 

##################################################################################################
# The edx and validation datasets were created above with the code that was provided as part of 
# this project. I included above the code that was used to generate these datasets.
##################################################################################################

###################################################################
# STEP 1: Creation of train and test sets based on the edx dataset.
###################################################################

#####################################################################################################
# The following code is used to create the train and test sets that I use based on the edx dataset.
#####################################################################################################

# Loading/installing a library.

if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(lubridate)

# Loading/installing another library.

if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

library(ggthemes)

# If the edx dataset has been removed and has not been loaded yet, use the following code.

# getwd()

# load("edx.rda")

# The following line of code should be used if using R 3.6 or later. If using R 3.5 or earlier, use `set.seed(1)`.

set.seed(1, sample.kind="Rounding")

y <- edx$rating

# The test set will include 20% of the data (p = 0.2).

test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train_set <- edx[-test_index,]
test_set <- edx[test_index]

temp <- test_set

test_set <- test_set %>%
  semi_join(train_set, by = "movieId")

test_set <- test_set %>%
  semi_join(train_set, by = "userId")

removed <- anti_join(temp, test_set)

train_set <- train_set %>%
  rbind(removed)

rm(temp, removed)

# save(train_set, file = "train_set.rda")
# save(test_set, file = "test_set.rda")

# load("test_set.rda")
# load("train_set.rda")

########################
# STEP 2: RMSE function
########################

######################################################################################
# Writing an RMSE function to be used for testing the model for movie recommendations.
######################################################################################

RMSE <- function(predicted, actual){
  sqrt(mean((predicted - actual)^2))
}

###########################################################
# STEP 3: Creation of cross-validation train and test sets.
###########################################################

# in order to run cross-validation and select the optimal (best) lambda for the model.

# head(train_set)

# Use the following lines of code to re-load the train set, if you have saved and removed it.

# getwd()

# load("train_set.rda")

y <- train_set$rating

# The following line of code should be used if using R 3.6 or later. 
# If using R 3.5 or earlier, use `set.seed(1)`.

set.seed(1, sample.kind = "Rounding")

# I now create the test index, with 20% of the data from the train set included in the cv (cross-validation) test set.

test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

# Now I create the test and train cv datasets to be used for cross-validation of the parameter lambda for my model.

test_cv_set <- train_set[test_index,]

train_cv_set <- train_set[-test_index,]

temp <- test_cv_set

test_cv_set <- test_cv_set %>%
  semi_join(train_cv_set, by = "movieId") %>%
  semi_join(train_cv_set, by = "userId")

removed <- anti_join(temp, test_cv_set)

nrow(removed)

identical(nrow(removed), nrow(temp) - nrow(test_cv_set))

train_cv_set <- train_cv_set %>%
  rbind(removed)

identical(nrow(train_set), nrow(train_cv_set) + nrow(test_cv_set))

rm(temp, removed)

# The following two lines of code are optional.

# save(train_set, file = "train_set.rda")
# rm(train_set)

#############################################################################
# STEP 4: Cross-Validation to select the best (optimal) lambda for the model.
#############################################################################

# First, I will create a variable mu - a variable that will store the overall mean rating for all users and for 
# all movies for the cross-validation train set ("train_cv_set").

mu <- mean(train_cv_set$rating)

mu

##########################################################
# Wrangling the cross-validation train and test sets.
##########################################################

train_cv_set <- train_cv_set %>%
  select(-title)

test_cv_set <- test_cv_set %>%
  select(-title)

train_cv_set <- train_cv_set %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  mutate(day = yday(date)) %>%
  mutate(year = year(date))

test_cv_set <- test_cv_set %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  mutate(day = yday(date)) %>%
  mutate(year = year(date))

#####################################################################
# Cross-Validation to select the best (optimal) lambda for the model.
#####################################################################

# I will now run cross-validation on the cross-validation train and test sets as created above
# in order to select the best lambda for the model.

lambdas <- seq(0, 10, 0.25)

# Note the definition of each one of the effects as included in this model below.

# saving and removing datasets to free space.

# getwd()

# save(edx, file = "edx.rda")
# rm(edx)

# save(train_set, file = "train_set.rda")
# rm(train_set)

# save(test_set, file = "test_set.rda")
# rm(test_set)

# rm(y)

# Note: The following cross-validation took 5 minutes to run on my computer.

results <- sapply(lambdas, function(l){
  
  reg_movie_effect <- train_cv_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  train_cv_set <- train_cv_set %>%
    left_join(reg_movie_effect, by = "movieId")
  
  reg_user_effect <- train_cv_set %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  
  train_cv_set <- train_cv_set %>%
    left_join(reg_user_effect, by = "userId")
  
  reg_genre_effect <- train_cv_set %>% 
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l))
  
  train_cv_set <- train_cv_set %>%
    left_join(reg_genre_effect, by = "genres")
  
  reg_week_effect <- train_cv_set %>%
    group_by(week) %>%
    summarize(b_w = sum(rating - mu - b_i - b_u - b_g)/(n() + l))
  
  train_cv_set <- train_cv_set %>%
    left_join(reg_week_effect, by = "week")
  
  reg_day_effect <- train_cv_set %>%
    group_by(day) %>%
    summarize(b_d = sum(rating - mu - b_i - b_u - b_g - b_w)/(n() + l))
  
  train_cv_set <- train_cv_set %>%
    left_join(reg_day_effect, by = "day")
  
  reg_year_effect <- train_cv_set %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u - b_g - b_w)/(n() + l))
  
  train_cv_set <- train_cv_set %>%
    left_join(reg_year_effect, by = "year")
  
  test_cv_set <- test_cv_set %>%
    left_join(reg_movie_effect, by = "movieId") %>%
    left_join(reg_user_effect, by = "userId") %>%
    left_join(reg_genre_effect, by = "genres") %>%
    left_join(reg_week_effect, by = "week") %>%
    left_join(reg_day_effect, by = "day") %>%
    left_join(reg_year_effect, by = "year")
  
  test_cv_set <- test_cv_set %>%
    mutate(predicted = mu + b_i + b_u + b_g + b_w + b_d + b_y)
  
  rmse <- RMSE(test_cv_set$predicted, test_cv_set$rating)
  
  rmse
})

min_rmse_cv <- min(results)

min_rmse_cv

best_lambda <- lambdas[which.min(results)]

best_lambda

# The best lambda obtained for the model for regularizing all of the effects is best_lambda = 5.

df <- data.frame(Lambda = lambdas, RMSE = results)

plot_lambda_rmse <- df %>% ggplot(aes(Lambda, RMSE)) + 
  geom_point(size = 4, col = "maroon3") +
  geom_point(aes(x = best_lambda, y = min_rmse_cv), size = 4, col = "royalblue3") +
  ggtitle("Effects of Lambda on RMSE") +
  theme_pander()

plot_lambda_rmse

# You can save this plot (it is used in the Rmd report) with this line of code below.

# ggsave("plot_lambda_rmse.png")

##########################################################
# STEP 5: Testing the RMSE for this model on the test set.
##########################################################

# If the train and test sets have been saved and removed, you need to re-load them.

# load("train_set.rda")
# load("test_set.rda")

train_s <- train_set %>%
  select(-title)

test_s <- test_set %>%
  select(-title)

# It is an option to remove the original train and test sets now to clear space. 
# If they have not been saved before, they should be saved before they are removed.

# save(train_set, file = "train_set.rda")
# save(test_set, file = "test_set.rda")

# Now it is optional to remove the train and test sets.

# rm(train_set, test_set)

train_s <- train_s %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  mutate(day = yday(date)) %>% 
  mutate(year = year(date))

test_s <- test_s %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  mutate(day = yday(date)) %>%
  mutate(year = year(date))

mu <- mean(train_s$rating)

mu

# The effects that are included in the final model are: 

## Movie effect (b_i)

## User effect (b_u)

## Genre combination effect (b_g)

### and three Time effects, as follows.

## Week by week effect over the whole time period (b_w)

## Day of the year effect (b_d)

## Year effect (b_y)

reg_movie_effect <- train_s %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + best_lambda))

train_s <- train_s %>%
  left_join(reg_movie_effect, by = "movieId")

reg_user_effect <- train_s %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + best_lambda))

train_s <- train_s %>%
  left_join(reg_user_effect, by = "userId")

reg_genre_effect <- train_s %>% 
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + best_lambda))

train_s <- train_s %>%
  left_join(reg_genre_effect, by = "genres")

reg_week_effect <- train_s %>%
  group_by(week) %>%
  summarize(b_w = sum(rating - mu - b_i - b_u - b_g)/(n() + best_lambda))

train_s <- train_s %>%
  left_join(reg_week_effect, by = "week")

reg_day_effect <- train_s %>%
  group_by(day) %>%
  summarize(b_d = sum(rating - mu - b_i - b_u - b_g - b_w)/(n() + best_lambda))

train_s <- train_s %>%
  left_join(reg_day_effect, by = "day")

reg_year_effect <- train_s %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u - b_g - b_w - b_d)/(n() + best_lambda))

train_s <- train_s %>%
  left_join(reg_year_effect, by = "year")

test_s <- test_s %>%
  left_join(reg_movie_effect, by = "movieId") %>%
  left_join(reg_user_effect, by = "userId") %>%
  left_join(reg_genre_effect, by = "genres") %>%
  left_join(reg_week_effect, by = "week") %>%
  left_join(reg_day_effect, by = "day") %>%
  left_join(reg_year_effect, by = "year")

test_s <- test_s %>%
  mutate(predicted = mu + b_i + b_u + b_g + b_w + b_d + b_y)

rmse_test_set <- RMSE(test_s$predicted, test_s$rating)

rmse_test_set

# The RMSE obtained with my model on the test set is 0.8647502.

###############################################
# STEP 6: Fitting the model on the edx dataset.
###############################################

# If the edx set was saved and removed before, you will now need to re-load it.

# load("edx.rda")

edx_s <- edx

# save(edx, file = "edx.rda")

# This line is optional. You can remove the edx dataset now to free space.

# rm(edx)

edx_s <- edx_s %>%
  select(-title)

edx_s <- edx_s %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  mutate(day = yday(date)) %>%
  mutate(year = year(date))

# I will now fit the model on the whole of the edx dataset. I will then test it on the validation set.

mu <- mean(edx_s$rating)

mu

# I will now compute the effects that are included in the model using the best lambda as
# selected above.

reg_movie_effect <- edx_s %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + best_lambda))

edx_s <- edx_s %>%
  left_join(reg_movie_effect, by = "movieId")

reg_user_effect <- edx_s %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + best_lambda))

edx_s <- edx_s %>%
  left_join(reg_user_effect, by = "userId")

reg_genre_effect <- edx_s %>% 
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + best_lambda))

edx_s <- edx_s %>%
  left_join(reg_genre_effect, by = "genres")

reg_week_effect <- edx_s %>%
  group_by(week) %>%
  summarize(b_w = sum(rating - mu - b_i - b_u - b_g)/(n() + best_lambda))

edx_s <- edx_s %>%
  left_join(reg_week_effect, by = "week")

reg_day_effect <- edx_s %>%
  group_by(day) %>%
  summarize(b_d = sum(rating - mu - b_i - b_u - b_g - b_w)/(n() + best_lambda))

edx_s <- edx_s %>%
  left_join(reg_day_effect, by = "day")

reg_year_effect <- edx_s %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u - b_g - b_w - b_d)/(n() + best_lambda))

edx_s <- edx_s %>%
  left_join(reg_year_effect, by = "year")

#####################################################################
# STEP 7: I will now test the RMSE of my model on the validation set.
#####################################################################

# If the validation set has been saved and removed before in order to free space, you will need to re-load it now.

# load("validation.rda")

validation_s <- validation

# This step is optional. You can save and remove the validation set. You should save it before removing it
# if you have not saved it before.

# save(validation, file = "validation.rda")

# It is now optional to remove the validation set in order to free space.

# rm(validation)

validation_s <- validation_s %>%
  select(-title)

validation_s <- validation_s %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  mutate(day = yday(date)) %>%
  mutate(year = year(date))

validation_s <- validation_s %>%
  left_join(reg_movie_effect, by = "movieId") %>%
  left_join(reg_user_effect, by = "userId") %>%
  left_join(reg_genre_effect, by = "genres") %>%
  left_join(reg_week_effect, by = "week") %>%
  left_join(reg_day_effect, by = "day") %>%
  left_join(reg_year_effect, by = "year")

validation_s <- validation_s %>%
  mutate(predicted = mu + b_i + b_u + b_g + b_w + b_d + b_y)

# The predicted movie ratings for the validation set are in the column validation_s$predicted.

rmse_final_model <- RMSE(validation_s$predicted, validation_s$rating)

rmse_final_model

# The RMSE obtained for the validation set as a result of the final model is 0.8642704.

tibble("RMSE final model" = rmse_final_model) %>%
  knitr::kable()


#############################
# Saving and removing objects
#############################

# The following lines of code are optional. Please note that some of the following objects are used as part of the Rmd file,
# so if you would like to run the Rmd file yourself, you will need to save these objects first.

# getwd() # To make sure you are in the directory where you want the files to be saved.

# setwd("./rdas") # If you have a sub-directory of the working directory named "rdas" where you want the files to be saved.

# You would have to had created this directory beforehand.


# save(edx, file = "edx.rda")

# save(edx_s, file = "edx_s.rda")

# save(RMSE, file = "RMSE.rda")



# save(min_rmse_cv, file = "min_rmse_cv.rda")


# save(lambdas, file = "lambdas.rda")


# save(train_set, file = "train_set.rda")


# These objects can now be removed.

# rm(edx, edx_s, train_set, RMSE, min_rmse_cv, lambdas)


# Saving the "validation_s" object which includes my predictions for the validation set.

# save(validation_s, file = "validation_s.rda")

# This object can now be removed.

# rm(validation_s)


# Saving the "df" object which contains the results of the cross-
# validation process.

# save(df, file = "df.rda")

# This object can be now removed.

# rm(df)


# Saving the best_lambda obtained as a result of the cross-validation.

# save(best_lambda, file = "best_lambda.rda")

# rm(best_lambda)


# Saving mu, the overall mean rating obtained for the edx dataset.

# save(mu, file = "mu.rda")

# rm(mu)


# Saving the effects obtained for the edx dataset.

# save(reg_day_effect, file = "reg_day_effect.rda")

# rm(reg_day_effect)

# save(reg_genre_effect, file = "reg_genre_effect.rda")

# rm(reg_genre_effect)

# save(reg_movie_effect, file = "reg_movie_effect.rda")

# rm(reg_movie_effect)

# save(reg_user_effect, file = "reg_user_effect.rda")

# rm(reg_user_effect)

# save(reg_week_effect, file = "reg_week_effect.rda")

# rm(reg_week_effect)

# save(reg_year_effect, file = "reg_year_effect.rda")

# rm(reg_year_effect)


# If the train_set has not been saved, you can save it now.

# save(train_set, file = "train_set.rda")

# rm(train_set)


# Saving the final RMSE obtained for the validation set.

# save(rmse_final_model, file = "rmse_final_model.rda")

# Before removing the RMSE for the final model, I will present it again, as this is the final result of this project.

tibble("RMSE final model" = rmse_final_model) %>%
  knitr::kable()

# rm(rmse_final_model)

# The other objects can now be removed. Please note that if you have already removed some of these objects, you might get a warning when you run the 
# line of code below.

# rm(edx, edx_s, plot_lambda_rmse, test_cv_set, test_index, test_s, test_set, train_cv_set, train_s, train_set, 
# validation, validation_s, lambdas, min_rmse_cv, results, rmse_test_set, y)

# Alternatively, if you are sure that you have saved all the objects that you are interested in for future reference,
# you can remove all objects.

# rm(list = ls())

# This is the end of the R script for this project.
 
#######***########
#######***########