#################################################################
# Create edx set, validation set with code provided from HarvardX
#################################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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

#cleanup memory
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#quick check for size and structure of created datasets
dim(edx)
head(edx)
dim(validation)
head(validation)



#add date/increments to edx
library(lubridate)
edx <- edx %>% 
  transform(date = as.Date(as.POSIXlt(timestamp, origin = "1970-01-01", 
                                      format ="%Y-%m-%d"), format = "%Y-%m-%d")) %>%
  mutate (year = format(as.Date(date), "%Y"), 
          year_month = format(as.Date(date), "%Y-%m"), 
          month = format(as.Date(date), "%m"), 
          week = round_date(date, unit = "week"))

#check edx for missing values
any(is.na(edx))


#check that the number of movies, users, and dates are similar to Grouplens information
#number of distinct movies and users
c(unique_movies = n_distinct(edx$movieId), unique_users = n_distinct(edx$userId), total_combination = n_distinct(edx$movieId)*n_distinct(edx$userId))

#ratings range and date range
c(min_rating = min(edx$rating), max_rating = max(edx$rating))
c(min_date = min(edx$date), max_date = max(edx$date))



###Exploration and statistics###

#summary of all ratings
ratings <- edx$rating
hist(ratings)

mean(ratings)


###Movies summary###

#histogram of number(n) of reviews per movie (log10 scale) 
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ggtitle("Reviews per Movie (log10 scale)")


#average rating by movie 
movie_avgs <- edx %>% 
  group_by(title) %>%
  summarize(count = n(), avg_rating = mean(rating))

movie_avgs %>% 
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  ggtitle("Movie Average Rating") +
  xlim(0, 5)

#standard deviation within movie
movie_sds <- edx %>% 
  group_by(title) %>%
  summarize(count = n(), sd_rating = sd(rating))

movie_sds %>% 
  ggplot(aes(sd_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  ggtitle("Within Movie Standard Deviation") +
  xlim(0, 5)


#visualization example: movie to movie variation is higher than within movie
set.seed(1, sample.kind = "Rounding")
sample_12 <- edx[sample(nrow(edx), 12), ]

edx %>% filter(movieId %in% sample_12$movieId) %>%
  ggplot(aes(title, rating)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=60, hjust=1))+
  ggtitle("Movie to movie variation is greater than within move") 

#Trend of rating vs number of times rated(count)
movie_avgs %>% 
  ggplot(aes(count, avg_rating)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Movie Average Rating vs Count of Ratings per Movie")

#top 20 most rated movies
edx %>% 
  group_by(title) %>%
  summarize(n = n(), avg_rating = mean(rating)) %>%
  top_n(20, n) %>%
  arrange(desc(n)) %>%
  knitr::kable()


###Summary: Users, Genres, Week of Rating###

#average rating by user
library(gridExtra)

u1 <- edx %>% 
  group_by(userId) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  ggtitle("User Average Rating") +
  xlim(0, 5)

#standard deviation within user

u2 <- edx %>% 
  group_by(userId) %>%
  summarize(count = n(), sd_rating = sd(rating)) %>%
  ggplot(aes(sd_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  ggtitle("Standard Deviation Within User") +
  xlim(0, 5)

grid.arrange(u1, u2, ncol = 2)

#genres avgs

g1 <- edx %>% 
  group_by(genres) %>% 
  summarize(count = n(), avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  ggtitle("Genres Average Rating") +
  xlim(0, 5)

#sd within genre

g2 <- edx %>% 
  group_by(genres) %>% 
  summarize(count = n(), sd_rating = sd(rating)) %>%
  ggplot(aes(sd_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  ggtitle("Standard Deviation Within Genre") +
  xlim(0, 5)

grid.arrange(g1, g2, ncol = 2)

#week to week avgs

w1 <- edx %>% 
  group_by(week) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  ggtitle("Weekly Average Rating") +
  xlim(0, 5)

#within week

w2 <- edx %>% 
  group_by(week) %>%
  summarize(count = n(), sd_rating = sd(rating)) %>%
  ggplot(aes(sd_rating)) + 
  geom_histogram(bins = 30, color = "black") + 
  ggtitle("Standard Deviation Within Week") +
  xlim(0, 5)

grid.arrange(w1, w2, ncol = 2)


#histogram of number(n) of reviews per user (log10 scale) 
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Reviews per User (log10 scale)")


#number of reviews per grouped genres
genre_counts <- edx %>% 
  group_by(genres) %>% 
  summarize(count = n())
summary(genre_counts)



#Note that the genres column often inclues more than one genre separated by "|".
head(edx$genres)



###Analyze Time Factor###

#trend by year with points representing year_month average
edx%>% 
  group_by(year, month) %>%
  summarize(avg_rating_per_month = mean(rating)) %>%
  ggplot(aes(year, avg_rating_per_month)) +
  geom_boxplot()+
  ggtitle("Each Box Includes 12 Monthly Averages")

#Ratings in 1995
count_1995 <- edx %>%
  filter(year == "1995")
cat("The number of ratings in 1995 was", nrow(count_1995))


#week over week average ratings
edx %>% 
  group_by(week) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(week, rating)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.05) +
  ggtitle("Ratings by Weekly Average") +
  ylim(1.5, 5)

#day over day average ratings  
edx %>% 
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.02) +
  ggtitle("Ratings by Daily Average") +
  ylim(1.5, 5)

#ratings by month
edx%>% 
  group_by(year_month, month) %>%
  summarize(avg_rating_by_year = mean(rating)) %>%
  ggplot(aes(month, avg_rating_by_year)) +
  geom_boxplot() +
  ggtitle("Ratings Binned All Years by Month")



#average rating as a function of each movie's rating count per year (rate)
ratings_per_year <- edx%>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(as.numeric(year)),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  arrange(desc(rate))
ratings_per_year %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()
  



###Building the model###

#break edx into train set and test set
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#define the function RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#most simple model
#predict the average
mu <- mean(train_set$rating)
mu

naive_rmse <- RMSE(test_set$rating, mu)

#create dataframe to store results
rmse_results <- data_frame(Method = "Train/Test: Just the average", RMSE = naive_rmse)

rmse_results %>% knitr::kable()

###Movie effect###

#calculate and show distribution for movie effect b_i
b_i <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#visualize movie effect
b_i %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#predict ratings with the additional of movie effect to the model
predicted_ratings <- mu + test_set %>% 
  left_join(b_i, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Train/Test: Movie Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()

###User effect###

#User distribution summary.  Note 1st-3rd quartile is 3.35-3.91
user_summary <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating))
summary(user_summary)

#add_user effect to model
b_u <- train_set %>% 
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Train/Test: Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

###Genres effect###

#add genre effect to model, leaving genres grouped
b_g <- train_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Train/Test: Movie + User+ Genre Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()


###Regularization with movie, user, genre###

#find the lambda the minimizes RMSE
lambdas <- seq(0, 10, 0.25)
rmses_g <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_g <- train_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    left_join(b_g, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u + b_g) %>% 
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses_g)

lambda_i_u_g <- lambdas[which.min(rmses_g)]

#the value of lambda that minimizes the RMSE
print(lambda_i_u_g)

#RMSE for Regularization with movie+user+genre
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Train/Test: Regularized Movie + User + Genre Effect Model",  
                                     RMSE = min(rmses_g)))
rmse_results %>% knitr::kable()


###Visualization examples of impact of regularization 

#Plot impact of regularization on movie effect
l <- lambda_i_u_g
movie_reg <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l), n_i = n()) 

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) 

data_frame(original_b_i = movie_avgs$b_i, 
           regularlized_b_i = movie_reg$b_i, 
           n = movie_reg$n_i) %>%
  ggplot(aes(original_b_i, regularlized_b_i, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.4) +
  ggtitle("Regularization of Movie Effect")

#Plot impact of regularization on genres effect

reg_b_i <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l))
reg_b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))
reg_b_g <- train_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l), n_g = n())

data_frame(original_b_g = b_g$b_g, 
           regularized_b_g = reg_b_g$b_g, 
           n = reg_b_g$n_g) %>%
  ggplot(aes(original_b_g, regularized_b_g, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.4) +
  ggtitle("Regularization of Genres Effect")


###Re-evaluate the rate factor

#check for effect of rate once other effects accounted for
#build joined_train table, including rate, and compute residuals

movie_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating-mu)/(n() + l)) 

user_reg <-  train_set%>%
  left_join(movie_reg) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating-mu-b_i)/(n() + l)) 

genre_reg <-  train_set%>%
  left_join(movie_reg) %>%
  left_join(user_reg) %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating-mu-b_i-b_u)/(n() + l)) 


train_set_rate <-train_set %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(as.numeric(year)),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  select(movieId, rate)

#join effects to test set and calculate residuals
joined_test <- test_set %>% 
  left_join(movie_reg, by='movieId') %>% 
  left_join(user_reg, by='userId') %>% 
  left_join(genre_reg, by='genres') %>%
  left_join(train_set_rate, by="movieId") %>%
  replace_na(list(b_i=0, b_u=0, b_g=0)) %>%
  mutate(residuals = rating - mu - b_i - b_u - b_g)

#rate no longer explains variation after other effects (movies) are accounted for
joined_test %>%
  group_by(movieId) %>%
  summarize(avg_residuals = mean(residuals), rate = mean(rate)) %>%
  ggplot(aes(rate, avg_residuals)) +
  geom_point() +
  geom_smooth()


### FINAL EVALUATION: use edx to predict validation set ratings ###

mu <- mean(edx$rating)
lambda <- 4.75 

movie_reg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating-mu)/(n() + lambda))

user_reg <-  edx %>%
  left_join(movie_reg) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating-mu-b_i)/(n() + lambda))

genre_reg <-  edx %>%
  left_join(movie_reg) %>%
  left_join(user_reg) %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating-mu-b_i-b_u)/(n() + lambda))

joined_table <- validation %>% 
  left_join(movie_reg, by='movieId') %>% 
  left_join(user_reg, by='userId') %>% 
  left_join(genre_reg, by='genres') %>%
  replace_na(list(b_i=0, b_u=0, b_g=0)) %>%
  mutate(residuals = rating - mu - b_i - b_u - b_g, predicted = mu + b_i + b_u + b_g)


validation_rmse <- RMSE(joined_table$predicted, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Validation Final: Regularized Movie + User + Genre Effect Model",  
                                     RMSE = validation_rmse))
rmse_results %>% knitr::kable()

#The validation RMSE is 0.8644514 (below our target of 0.86490)


#How accurately do we predict whether someone likes (thumbs up) or doesn't like (thumbs down)?
predicted_recommend <- ifelse(joined_table$predicted >= mean(edx$rating), "thumbs up", "thumbs down")
actual <- ifelse(validation$rating >= mean(validation$rating), "thumbs up", "thumbs down")

confusionMatrix(as.factor(predicted_recommend), as.factor(actual), positive = "thumbs up")
