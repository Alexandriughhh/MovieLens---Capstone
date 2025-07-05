# Capstone 

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)

# MovieLens 10M dataset:
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, files = ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, files = movies_file)

## Data Wrangling

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Load required packages
library(tidyverse)
library(caret)

# Ensure movielens dataset is loaded
# (Assuming you've already created movielens with left_join earlier)

# Set seed for reproducibility
set.seed(1)

# Create test index (10% for final holdout set)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

# Split into edx and temporary holdout
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

# Keep only users and movies in holdout that also appear in edx
final_holdout_test <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add any removed rows back into edx
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)


###########################################################################################################################
 # Quiz
###########################################################################################################################

# How many zeros and threes were given as ratings?
zeros <- sum(edx$rating == 0)
threes <- sum(edx$rating == 3)

ratingCount <- lapply(seq(0,5,0.5), function(n)
  sum(edx$rating == n)
) %>% 
  setNames(as.character(seq(0,5,0.5)))

edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()

# How many different movies are in the edx dataset?
uniqueMovies <- length(unique(edx$movieId))
n_distinct(edx$movieId)

# How many different users are in the edx dataset?
distinctUsers <- n_distinct(edx$userId)

# How many movie ratings are in each of the following genres?
genreList <- c('Drama', 'Comedy', 'Thriller', 'Romance')
genreCounts <- sapply(genreList, function(g){
  edx %>% filter(str_detect(genres, g)) %>% tally()
})

edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n())

# Which movie has the most ratings?
numRatings <- edx %>% group_by(movieId) %>% 
  summarize(numRatings = n(), movieTitle = first(title)) %>%
  arrange(desc(numRatings)) %>%
  top_n(10, numRatings)

# What are the 5 most given ratings?
numRatings <- edx %>% group_by(rating) %>% 
  summarize(number = n())

numRatings %>% top_n(5) %>% arrange(desc(number))

# In general, half star rating are less common than whole star ratings? True/False

numRatings %>%
  mutate(halfStar = rating %% 1 == 0.5) %>%
  group_by(halfStar) %>%
  summarize(number = sum(number))

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

###########################################################################################################################
# Exploratory Data Analysis 
###########################################################################################################################

#edx structure
str(edx)

# UserId: Find users who rate movies on more than one day 

library(dplyr)
library(lubridate)
library(ggplot2)

# Add a date column
edx <- edx %>%
  mutate(review_day = as_date(as_datetime(timestamp)))

# Count number of users who rated more than once
user_active_days <- edx %>%
  group_by(userId) %>%
  summarize(num_days = n_distinct(review_day), .groups = "drop")

multi_day_users <- user_active_days %>% filter(num_days > 1) %>% nrow()
total_users <- nrow(user_active_days)

# Plot
user_active_days %>%
  ggplot(aes(x = num_days)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", boundary = 0.5, closed = "left") +
  scale_x_continuous(limits = c(1, 30), breaks = seq(1, 30, 2)) +
  scale_y_log10() +
  labs(
    title = "Distribution of Distinct Rating Days per User",
    subtitle = paste("Out of", total_users, "users,", multi_day_users, "rated on more than one day"),
    x = "Number of Distinct Days a User Rated",
    y = "Number of Users (log scale)",
    caption = "Users with more than 30 active days excluded from this plot for readability"
  ) +
  theme_minimal()


summary(user_active_days$num_days)

# MovieId - Popularity and Variability of Ratings

library(dplyr)
library(lubridate)
library(ggplot2)

# Summarize rating count and variability per movie
movie_stats <- edx %>%
  group_by(movieId, title) %>%
  summarize(
    avg_rating = mean(rating),
    num_ratings = n(),
    sd_rating = sd(rating),
    .groups = "drop"
  )

# Top 10 most rated movies
top_movies <- movie_stats %>%
  arrange(desc(num_ratings)) %>%
  slice_head(n = 10)

# Top 10 most controversial (highest standard deviation)
controversial_movies <- movie_stats %>%
  filter(num_ratings >= 50) %>%  # exclude rare cases
  arrange(desc(sd_rating)) %>%
  slice_head(n = 10)

# Plot: Ratings vs Rating Count
movie_stats %>%
  ggplot(aes(x = num_ratings, y = avg_rating)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Average Rating vs. Number of Ratings per Movie",
    x = "Number of Ratings",
    y = "Average Rating"
  ) +
  theme_minimal()

# Timestamp

edx <- edx %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))

# Load libraries if not already loaded
library(dplyr)
library(lubridate)
library(ggplot2)

# Create a day-of-week column and count ratings per weekday
edx %>%
  mutate(weekday = wday(review_date)) %>%
  count(weekday) %>%
  ggplot(aes(x = factor(weekday), y = n)) +  # Convert weekday to factor so it plots correctly
  geom_col(fill = "steelblue") +
  labs(
    title = "Most Common Day to Submit a Rating",
    x = "Day of the Week (1=Sunday)",
    y = "Number of Ratings",
    caption = "Source: edx dataset"
  ) +
  theme_minimal()


#Goal:
#Top 10 movies with the worst average ratings
#Top 10 movies with average ratings closest to the dataset’s mean

library(dplyr)

# Step 1: Calculate average rating per movie
movie_avg_by_title <- edx %>%
  group_by(title) %>%
  summarize(avg_rating = mean(rating), rating_count = n()) %>%
  ungroup()

# Step 2: Filter to only movies with a decent number of ratings (optional)
movie_avg_filtered <- movie_avg_by_title %>%
  filter(rating_count >= 50)  # Adjust as needed

# Step 3: Top 10 worst-rated movies
worst_movies <- movie_avg_filtered %>%
  arrange(avg_rating) %>%
  slice_head(n = 10)

# Step 4: Find the dataset-wide average rating
overall_mean <- mean(edx$rating)

# Step 5: Top 10 movies closest to the average rating
average_movies <- movie_avg_filtered %>%
  mutate(diff_from_mean = abs(avg_rating - overall_mean)) %>%
  arrange(diff_from_mean) %>%
  slice_head(n = 10)

# View the results
worst_movies
average_movies


#Sequels better than the original

library(dplyr)
library(stringr)

# Step 1: Calculate average rating by title
movie_ratings <- edx %>%
  group_by(title) %>%
  summarize(avg_rating = mean(rating), n = n()) %>%
  ungroup()

# Step 2: Flag sequels based on common naming patterns
movie_ratings <- movie_ratings %>%
  mutate(
    is_sequel = str_detect(title, regex("2|II|Part 2|Two", ignore_case = TRUE)),
    base_title = str_replace(title, regex("[:\\-]?\\s*(2|II|Part 2|Two).*", ignore_case = TRUE), "")
  )

# Step 3: Find matching original/sequel title pairs by base title
sequel_pairs <- movie_ratings %>%
  filter(is_sequel) %>%
  select(base_title, sequel_title = title)

# View the first 10 matches
head(sequel_pairs, 10)



library(dplyr)
library(stringr)

# Step 1: Calculate average rating by title
movie_ratings <- edx %>%
  group_by(title) %>%
  summarize(avg_rating = mean(rating), n = n()) %>%
  ungroup()

# Step 2: Identify sequels (Types 1–4)
movie_ratings <- movie_ratings %>%
  mutate(
    is_sequel = str_detect(title, regex(
      # Roman numerals, numbers, "Part" notation, colon subtitles
      " II| III| IV| [2-5]\\b|Part [2-5]|:\\s.+", 
      ignore_case = TRUE
    )),
    
    # Exclude "Not to Do" cases: exact title match + just a year in ()
    is_not_sequel = str_detect(title, "\\(\\d{4}\\)") & 
      !str_detect(title, regex(" II| III| IV| [2-5]\\b|Part [2-5]|:\\s.+", ignore_case = TRUE)),
    
    # Final sequel flag
    is_true_sequel = is_sequel & !is_not_sequel
  )

# Step 3: Extract base titles for Types 1–4
movie_ratings <- movie_ratings %>%
  mutate(
    base_title = case_when(
      str_detect(title, regex(" II| III| IV", ignore_case = TRUE)) ~ str_replace(title, regex(" II| III| IV.*", ignore_case = TRUE), ""),
      str_detect(title, regex(" [2-5]\\b", ignore_case = TRUE)) ~ str_replace(title, regex(" [2-5]\\b.*", ignore_case = TRUE), ""),
      str_detect(title, regex("Part [2-5]", ignore_case = TRUE)) ~ str_replace(title, regex(" Part [2-5].*", ignore_case = TRUE), ""),
      str_detect(title, regex(":\\s.+", ignore_case = TRUE)) ~ str_replace(title, regex(":\\s.*", ignore_case = TRUE), ""),
      TRUE ~ NA_character_
    )
  )

## Step 4 Clean whitespace from base_title before selecting
sequel_pairs <- movie_ratings %>%
  filter(is_true_sequel, !is.na(base_title)) %>%
  mutate(base_title = str_trim(base_title)) %>%
  select(base_title, sequel_title = title)

# View first few pairs
head(sequel_pairs, 10)



# ratings for sequel_pairs
library(dplyr)

# Step 1: Calculate average ratings for all titles
title_ratings <- edx %>%
  group_by(title) %>%
  summarize(
    avg_rating = mean(rating),
    num_ratings = n(),
    .groups = "drop"
  )

# Step 2: Join ratings to base and sequel titles
sequel_comparison <- sequel_pairs %>%
  left_join(title_ratings, by = c("base_title" = "title")) %>%
  rename(base_avg_rating = avg_rating, base_num_ratings = num_ratings) %>%
  left_join(title_ratings, by = c("sequel_title" = "title")) %>%
  rename(sequel_avg_rating = avg_rating, sequel_num_ratings = num_ratings)

# Step 3: Compare ratings
sequel_comparison <- sequel_comparison %>%
  mutate(
    sequel_better = sequel_avg_rating > base_avg_rating,
    rating_difference = round(sequel_avg_rating - base_avg_rating, 2)
  )

# Step 4: View results
head(sequel_comparison, 10)
#Sequels that outperformed the original
sequel_comparison %>%
  count(sequel_better)
#sort by largest improvement
sequel_comparison %>%
  arrange(desc(rating_difference)) %>%
  head(10)
#sort by biggest disappointment
sequel_comparison %>%
  arrange(desc(rating_difference)) %>%
  head(10)


# Rating Trends Over Time (Monthly)
library(dplyr)
library(tidyverse)
library(caret)

edx %>%
  mutate(review_month = floor_date(as_datetime(timestamp), unit = "month")) %>%
  group_by(review_month) %>%
  summarize(avg_rating = mean(rating), .groups = "drop") %>%
  ggplot(aes(x = review_month, y = avg_rating)) +
  geom_line(color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkorange") +
  labs(
    title = "Average Movie Ratings Over Time (Monthly)",
    x = "Month",
    y = "Average Rating"
  ) +
  theme_minimal()

# Rating Trends by Genre Over Time (Yearly)

edx %>%
  mutate(year = year(as_datetime(timestamp))) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(year, genres) %>%
  filter(n() > 500) %>%
  summarize(avg_rating = mean(rating), .groups = "drop") %>%
  ggplot(aes(x = year, y = avg_rating, color = genres)) +
  geom_line(size = 1) +
  labs(
    title = "Average Ratings by Genre Over Time",
    x = "Year",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

###########################################################################################################################
# Methods - Load Data & Prepare Train/Test Sets
###########################################################################################################################

library(tidyverse)
library(caret)

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

final_holdout_test <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)


# Estimate Effects: RMSE Function
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

###########################################################################################################################
# Methods - Develop, Train, and Test Algorithm
###########################################################################################################################

# Model 1: Naive(global average)
mu_hat <- mean(edx$rating)
naive_rmse <- RMSE(final_holdout_test$rating, mu_hat)


# Model 2: Movie Effect
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat), groups = "drop")

predicted_ratings_movie <- final_holdout_test %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)

rmse_movie <- RMSE(final_holdout_test$rating, predicted_ratings_movie)

# Model 3: Movie + User Effect
user_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i), .groups = "drop")

combined_effects <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(combined = b_i + b_u)

predicted_ratings_user <- final_holdout_test %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

rmse_user <- RMSE(final_holdout_test$rating, predicted_ratings_user)

# Model 4: Timestamp Effect
edx <- edx %>%
  mutate(date = as_datetime(timestamp),
         rating_week = round_date(date, unit = "week"))

user_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i), .groups = "drop")

time_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(rating_week) %>%
  summarize(b_t = mean(rating - mu_hat - b_i - b_u))

# Model 5: Genre Effect
genre_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))


# Regularization and Lambda Tuning

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(lambda) {
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + lambda))
  
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))
  
  predicted_ratings <- final_holdout_test %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(final_holdout_test$rating, predicted_ratings))
})

# Find best lambda
best_lambda <- lambdas[which.min(rmses)]

# Summary Table of RMSEs
rmse_results <- tibble(
  method = c("Just the average", "Movie Effect", "Movie + User Effect", "Regularized Model"),
  RMSE = c(naive_rmse, rmse_movie, rmse_user, min(rmses))
)

###########################################################################################################################
# Methods - Match the structure of final_holdout_test to edx
###########################################################################################################################

# Recalculate the baseline mu.
mu <- mean(edx$rating)

# Recalculate each effect in correct order.
# Movie Effect (b_i_reg)
b_i_reg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + best_lambda), .groups = "drop")

# User effect (b_u_reg)
b_u_reg <- edx %>%
  left_join(b_i_reg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i) / (n() + best_lambda), .groups = "drop")

# Predict using regularized effects
predicted_ratings_reg <- final_holdout_test %>%
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_regularized <- RMSE(final_holdout_test$rating, predicted_ratings_reg)

# Match the structure of final_holdout_test to edx

library(lubridate)
library(tidyverse)
library(dplyr)

# Add date-related columns to final_holdout_test
final_holdout_test <- final_holdout_test %>%
  mutate(
    release_year = as.numeric(str_extract(title, "\\((\\d{4})\\)")),
    date = as_datetime(timestamp),
    rating_week = round_date(date, unit = "week")
  )

# Predictions and RMSEs
predicted_ratings <- final_holdout_test %>%
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_regularized <- RMSE(final_holdout_test$rating, predicted_ratings)

## Regularization Scatterplot

qplot(lambdas, rmses, geom = "line") +
  geom_vline(xintercept = best_lambda, linetype = "dashed", color = "red") +
  labs(title = "RMSE vs. Lambda (Regularization)",
       x = "Lambda",
       y = "RMSE")


###########################################################################################################################
# Results
###########################################################################################################################
library(dplyr)
library(tidyverse)
library(caret)

# Updated RMSE Summary Table
rmse_results <- tibble(
  method = c("Just the average", "Movie Effect", "Movie + User Effect", "Regularized Model"),
  RMSE = c(naive_rmse, rmse_movie, rmse_user, rmse_regularized)
)

# Distribution of Model Effects
# Movie Effect distribution
ggplot(movie_avgs, aes(x = b_i)) +
  geom_histogram(bins = 40, fill = "dodgerblue") +
  labs(title = "Distribution of Movie Effects", x = "Movie Effect (b_i)", y = "Frequency")

# User Effect distribution
ggplot(user_avgs, aes(x = b_u)) +
  geom_histogram(bins = 40, fill = "orchid") +
  labs(title = "Distribution of User Effects", x = "User Effect (b_u)", y = "Frequency")

# Movie + User Effect combined
print(colnames(edx))

user_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i), .groups = "drop")


combined_effects <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(combined = b_i + b_u)

#Plot Combined
combined_effects %>%
  ggplot(aes(x = combined)) +
  geom_histogram(binwidth = 0.05, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribution of Combined Effects (Movie + User)",
    x = "b_i + b_u",
    y = "Count"
  ) +
  theme_minimal()

# Simple Baseline Comparison
library(tibble)
library(knitr)

table1 <- tibble(
  Method = c("Project Objective", "Simple Average", "Movie Effect"),
  RMSE = c(0.86490, naive_rmse, rmse_movie),
  Difference = c(0, naive_rmse - 0.86490, rmse_movie - 0.86490)
)

kable(table1, caption = "Comparison of RMSE: Simple Average vs Movie Effect")

# Adding User Effects
table2 <- tibble(
  Method = c("Project Objective", "Simple Average", "Movie Effect", "Movie + User Effect"),
  RMSE = c(0.86490, naive_rmse, rmse_movie, rmse_user),
  Difference = c(0, naive_rmse - 0.86490, rmse_movie - 0.86490, rmse_user - 0.86490)
)

kable(table2, caption = "Comparison of RMSE: Adding User Effects")

# Final model with Regularization
table3 <- tibble(
  Method = c("Project Objective", "Simple Average", "Movie Effect", "Movie + User Effect", "Regularized Model"),
  RMSE = c(0.86490, naive_rmse, rmse_movie, rmse_user, rmse_regularized),
  Difference = c(0, naive_rmse - 0.86490, rmse_movie - 0.86490, rmse_user - 0.86490, rmse_regularized - 0.86490)
)

kable(table3, caption = "Comparison of RMSE: Final Regularized Model")


