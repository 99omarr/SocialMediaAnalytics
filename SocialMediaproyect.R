install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
install.packages("purrr")
library(purrr)
install.packages("lubridate")
library(lubridate)
install.packages("stringr")
library(stringr)
install.packages("tidytext")
library(tidytext)
install.packages("tidyr")
library(tidyr)
library(dplyr)
install.packages("psych")
library(psych)
install.packages("caret")
library(caret)
library(stringi)
install.packages("car")
library(car)
install.packages("randomForest")
library(randomForest)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("partykit")
library(partykit)

data <- read.csv("C:/Users/ACER/Downloads/instagram_data.csv")

names(data)
head(data)

summary(data)

missing_values <- is.na(data)

missing_counts <- colSums(missing_values)

print(missing_counts)

character_columns <- sapply(data, is.character)
missing_values <- sapply(data[character_columns], function(x) sum(is.na(x) | x == ""))
print(missing_values)

column_name <- "owner_username"
rows_with_empty_values <- data[data[[column_name]] == "", ]
print(rows_with_empty_values)

rows_to_delete <- c(11661, 11692)
clean_data <- data[-rows_to_delete, ]

character_columns <- sapply(clean_data, is.character)
missing_values <- sapply(clean_data[character_columns], function(x) sum(is.na(x) | x == ""))
print(missing_values)

column_name2 <- "imageUrl"
rows_with_empty_values2 <- clean_data[clean_data[[column_name2]] == "", ]
print(rows_with_empty_values2)

row_to_delete2 <- 11662
clean_data2 <- clean_data[-row_to_delete2, ]

rm(clean_data2)
rm(row_to_delete2)

row_to_delete2 <- 11661
clean_data2 <- clean_data[-row_to_delete2, ]

clean_data <- clean_data2

rm(clean_data2)

str(clean_data)

clean_data$comments <- as.numeric(clean_data$comments)
clean_data$likes <- as.numeric(clean_data$likes)

summary(clean_data)

clean_data_sorted <- clean_data[order(-clean_data$followers), ]
print(head(clean_data_sorted,15))

clean_data$created_at <- as.POSIXct(clean_data$created_at, origin = "1970-01-01", tz="UTC")

summary(clean_data)

##boxplot likes
ggplot(clean_data, aes(x = likes)) +
  geom_histogram(binwidth = 1000000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Likes",
       x = "Number of Likes",
       y = "Frequency")

ggplot(clean_data, aes(x = likes)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  scale_x_continuous(trans = "log10", breaks = trans_breaks("log10", function(x) 10^x),
                     labels = scales::comma) +
  labs(title = "Distribution of Likes (Log Scale)",
       x = "Number of Likes (Log Scale)",
       y = "Frequency")


#To exclude extreme values from the boxplot, you can first identify the cutoff points for what you consider to be extreme values, and then filter the data to exclude observations outside of those cutoff points. Here's how you can do it in R:


# Define cutoff points for extreme values (e.g., values above the 99th percentile)
cutoff <- quantile(clean_data$likes, 0.90)

# Filter the data to exclude extreme values
clean_data_filtered <- clean_data[clean_data$likes <= cutoff, ]

# Create a boxplot of the filtered data
ggplot(clean_data_filtered, aes(x = "", y = likes)) +
  geom_boxplot() +
  labs(title = "Boxplot of Likes (Excluding Extreme Values)",
       x = "",
       y = "Likes")
       
# Create a density plot of the likes variable
ggplot(clean_data, aes(x = likes)) +
  geom_density(fill = "skyblue", color = "blue") +
  labs(title = "Density Plot of Likes",
       x = "Likes",
       y = "Density")
       
# Apply logarithmic transformation to the likes variable
clean_data$log_likes <- log(clean_data$likes)

# Create a boxplot of the transformed variable
ggplot(clean_data, aes(x = "", y = log_likes)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-transformed Likes",
       x = "",
       y = "Log-transformed Likes")
       
# Create a density plot of the log_likes variable
ggplot(clean_data, aes(x = log_likes)) +
  geom_density(fill = "skyblue", color = "blue") +
  labs(title = "Density Plot of Log-transformed Likes",
       x = "Log-transformed Likes",
       y = "Density")
       
ggplot(clean_data, aes(x = followers)) +
  geom_histogram(binwidth = 10000, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Followers",
       x = "Followers Count",
       y = "Frequency")
       
# Trim extreme values
clean_data_trimmed <- clean_data[clean_data$followers < 1000000 & clean_data$following < 1000, ]

# Create histograms for trimmed data
ggplot(clean_data_trimmed, aes(x = followers)) +
  geom_histogram(binwidth = 10000, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Followers (Trimmed)",
       x = "Followers Count",
       y = "Frequency")

ggplot(clean_data_trimmed, aes(x = following)) +
  geom_histogram(binwidth = 100, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Following (Trimmed)",
       x = "Following Count",
       y = "Frequency")
       
# Create a scatter plot of likes against followers
ggplot(clean_data, aes(x = followers, y = likes)) +
  geom_point() +
  labs(title = "Scatter Plot of Likes vs Followers",
       x = "Followers",
       y = "Likes")
       
# Calculate correlation coefficients between likes, followers, and following
correlation_matrix <- cor(clean_data[, c("likes", "followers", "following")])

# Print correlation matrix
print(correlation_matrix)

# Remove rows with missing values in any of the columns
#clean_data <- na.omit(clean_data)

# Recalculate correlation coefficients
correlation_matrix <- cor(clean_data[, c("likes", "followers", "following")])

# Print correlation matrix
print(correlation_matrix)

clean_data$shortcode <- stri_trans_nfd(clean_data$shortcode)
clean_data$shortcode <- gsub("\\s+", "", clean_data$shortcode)

duplicated_rows <- duplicated(clean_data)

# Print the indices of duplicate rows
print(which(duplicated_rows))

# Alternatively, you can also filter the duplicate rows directly
duplicate_data <- clean_data[duplicated_rows, ]

# If you want to remove the duplicate rows from the dataset
clean_data_no_duplicates <- clean_data[!duplicated_rows, ]

# Print the new dataset without duplicates
print(clean_data_no_duplicates)

rows_with_na <- clean_data_no_duplicates %>%
  filter(rowSums(is.na(.)) > 0)

# Display the rows with NAs
print(rows_with_na)

common_owner_ids <- intersect(clean_data_no_duplicates$owner_id, rows_with_na$owner_id)

if (length(common_owner_ids) > 0) {
  print(paste("There are", length(common_owner_ids), "common owner IDs in the dataframes."))
  print("Common owner IDs:")
  print(common_owner_ids)
} else {
  print("There are no common owner IDs in the dataframes.")
}

# print(rows_with_na_followers)
#--------------------------------------------------------------------------------
#  if (row$likes < 100) {
    #   return(1302439)  # Impute 10,000 followers, adjust as needed
    #  } else if (row$likes >= 10000 & row$likes < 50000) {
    # Rule for likes between 1000 and 10000
    #   return(3069241)  # Impute 10,000 followers, adjust as needed
    #    # Rule for likes between 1000 and 10000
    #   return(7020770)  # Impute 10,000 followers, adjust as needed
    #   } else if (row$likes >= 300000 & row$likes < 500000) {
    # Rule for likes between 1000 and 10000
    #   return(12690586)  # Impute 10,000 followers, adjust as needed
    #  } else if (row$likes >= 500000 & row$likes < 750000) {
    # Rule for likes between 1000 and 10000
    #     return(22165865)  # Impute 10,000 followers, adjust as needed
    #  } else {
    #    # Rule for likes greater than or equal to 10000
    #     return(117474511)  # Impute 50,000 followers, adjust as needed
    #   }
  # }

# Apply the function to impute missing values in 'followers'
# clean_data_no_duplicates$followers <- clean_data_no_duplicates$followers %>%
#  if_else(is.na(.), map_dbl(clean_data_no_duplicates, impute_followers), .)

# Now, 'instagram_data' has imputed missing values in 'followers' based on rules and 'likes' values

instagram_data <- clean_data_no_duplicates

data_with_complete <- instagram_data %>%
  filter(!is.na(followers) & !is.na(likes))

# Step 2: Fit a linear regression model
# Followers as the dependent variable, likes as the independent variable
model <- lm(followers ~ likes, data = data_with_complete)

# Step 3: Predict missing values
# Filter rows with missing values in 'followers' and complete 'likes' data
data_with_missing_followers <- instagram_data %>%
  filter(is.na(followers) & !is.na(likes))

# Predict missing values using the model
predicted_followers <- predict(model, data_with_missing_followers)

# Step 4: Impute missing values
# Assign predicted values to the missing values in the original dataframe
instagram_data$followers[is.na(instagram_data$followers) & !is.na(instagram_data$likes)] <- predicted_followers



# Now, 'instagram_data' has imputed missing values in 'followers' based on the linear regression model

# Note: If there are still remaining missing values, further methods or models may be needed to handle them.

# Calculate the median of the 'following' column excluding missing values
median_following <- median(instagram_data$following, na.rm = TRUE)

# Impute missing values in 'following' with the median
instagram_data$following[is.na(instagram_data$following)] <- median_following

# Define the function to cap outliers
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

# Apply capping to 'likes' and 'following'
instagram_data$likes <- cap_outliers(instagram_data$likes)
instagram_data$followers <- cap_outliers(instagram_data$followers)

# Verify the results
summary(instagram_data$likes)
summary(instagram_data$followers)

# creación de variable likes/followers

instagram_data$likes_to_followers <- instagram_data$likes / instagram_data$followers

# creación de variable likes/following

instagram_data$likes_to_following <- instagram_data$likes / instagram_data$following

# creación de variable followers/following

instagram_data$followers_to_following <- instagram_data$followers / instagram_data$following

# Diferencia likes and follwers

instagram_data$likes_minus_followers <- instagram_data$likes - instagram_data$followers

# Diferencia entre following y followers

instagram_data$followers_minus_following <- instagram_data$followers - instagram_data$following

# Ratio likes y comments

instagram_data$comments_to_likes <- instagram_data$comments / instagram_data$likes

# cantidad de likes promedio por usuario:

mean_likes_per_owner <- instagram_data %>%
  group_by(owner_id) 
  
# Cantidad de followers promedio por usuario
  
median_followers_per_owner <- instagram_data %>%
group_by(owner_id) %>%
summarize(median_followers = median(followers, na.rm = TRUE))

# Cantidad de comments promedio por usuario

median_comments_per_owner <- instagram_data %>%
group_by(owner_id) %>%
summarize(median_comments = median(comments, na.rm = TRUE))

# Cantidad de likes por usuario

total_likes_per_owner <- instagram_data %>%
group_by(owner_id) %>%
summarize(total_likes = sum(likes, na.rm = TRUE))

# Cantidad de comments por usuario

total_comments_per_owner <- instagram_data %>%
  group_by(owner_id) %>%
  summarize(total_comments = sum(comments, na.rm = TRUE))

# Cantidad de posts por usuario

post_count_per_owner <- instagram_data %>%
group_by(owner_id) %>%
summarize(post_count = n())

# Cantidad máxima de likes por usuario

max_likes_per_owner <- instagram_data %>%
group_by(owner_id) %>%
summarize(max_likes = max(likes, na.rm = TRUE))

# Cantidad mínima de likes por usuario

min_followers_per_owner <- instagram_data %>%
group_by(owner_id) %>%
summarize(min_followers = min(followers, na.rm = TRUE))

# Desviación standar de likes por usuario:

stddev_likes_per_owner <- instagram_data %>%
group_by(owner_id) %>%
summarize(stddev_likes = sd(likes, na.rm = TRUE))

# Creación columna año
instagram_data$year <- year(instagram_data$created_at)

# Creación columna mes
instagram_data$month <- month(instagram_data$created_at)

# Creación columna día (número)
instagram_data$day <- day(instagram_data$created_at)

# Creación columna día (día de la semana) (1 = lunes, 7 = domingo)
instagram_data$day_of_week <- wday(instagram_data$created_at, week_start = 1)

# Creación columna hora
instagram_data$hour <- hour(instagram_data$created_at)

# Creación variable binaria para determinar si es fin de semana el día de la publicación
instagram_data$weekend <- ifelse(instagram_data$day_of_week %in% c(6, 7), 1, 0)

# Clasificación (night, morning, afternoon, evening)

instagram_data$time_of_day <- case_when(
  instagram_data$hour >= 0 & instagram_data$hour < 6 ~ "night",
  instagram_data$hour >= 6 & instagram_data$hour < 12 ~ "morning",
  instagram_data$hour >= 12 & instagram_data$hour < 18 ~ "afternoon",
  instagram_data$hour >= 18 & instagram_data$hour < 24 ~ "evening",
  TRUE ~ NA_character_
)
# Creación variable para cantidad de letras del texto

instagram_data$text_length <- str_length(instagram_data$caption)

# Creación variable para cantidad de palabras del texto
instagram_data$word_count <- str_count(instagram_data$caption, "\\w+")

#Hashtag
instagram_data$contains_keyword <- str_detect(instagram_data$caption, "hashtag")

# Análisis de sentimiento de la variable caption
instagram_data_sentiment <- instagram_data %>%
  unnest_tokens(word, caption) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  dplyr::count(owner_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0))

# Define the order of the categories in the 'time_of_day' column
time_of_day_order <- c("morning", "afternoon", "evening", "night")

# Convert the 'time_of_day' column to a factor with the specified order
instagram_data$time_of_day <- factor(instagram_data$time_of_day, levels = time_of_day_order, ordered = TRUE)

# Apply label encoding to the 'time_of_day' column
instagram_data$time_of_day_encoded <- as.integer(instagram_data$time_of_day)

instagram_data$time_of_day_encoded <- NULL

instagram_data <- instagram_data %>%
  mutate(
    time_of_day_afternoon = ifelse(time_of_day == "afternoon", 1, 0),
    time_of_day_night = ifelse(time_of_day == "night", 1, 0),
    time_of_day_evening = ifelse(time_of_day == "evening", 1, 0),
    time_of_day_morning = ifelse(time_of_day == "morning", 1, 0)
  )

## binning
bin_limits <- seq(min(instagram_data$followers), max(instagram_data$followers), length.out = 6)

# Perform equal width binning
instagram_data$followers_bin <- cut(instagram_data$followers, breaks = bin_limits, labels = FALSE)

instagram_data <- instagram_data %>% select(-followers_bin)

num_bins <- 5

instagram_data <- instagram_data %>%
  mutate(followers_bin = cut(followers, breaks = num_bins))
instagram_data <- instagram_data %>% select(-followers_bin)

describe(instagram_data)
glimpse(instagram_data)



#tablas de frecuencias

ggplot(instagram_data, aes(x = likes, y = followers)) +
  geom_point()

ggplot(instagram_data, aes(x = log_likes, y = followers)) +
  geom_point()

hour_frequency <- table(instagram_data$hour)

# Print the frequency table
print(hour_frequency)


day_frequency <- table(instagram_data$day_of_week)

# Print the frequency table
print(day_frequency)

correl <- instagram_data %>% select(-owner_id) %>% select_if(is.numeric) %>%
  cor(y = instagram_data$likes, use = "pairwise.complete.obs", method = "spearman") %>% abs() %>% round(2)

correl <- correl[order(-correl[, 1]),]
correl[-1]

par(las=2)
barplot(correl[-1], horiz = FALSE, cex.names = 0.5, col= "red")


# Check for infinite values in the column log_likes
inf_indices <- is.infinite(instagram_data$log_likes)

# Replace infinite values with 0
instagram_data$log_likes[inf_indices] <- 0

# Check for infinite values in the column comments_to_likes
inf_indicescom<- is.infinite(instagram_data$comments_to_likes)

# Replace infinite values with 0
instagram_data$comments_to_likes[inf_indicescom] <- 0

# Replace NaN values in the column 'likes_to_following' with a specific value (e.g., 0)
instagram_data$likes_to_following[is.nan(instagram_data$likes_to_following)] <- 0

# Replace inf values with 'likes' values
instagram_data$likes_to_following[is.infinite(instagram_data$likes_to_following)] <- instagram_data$likes[is.infinite(instagram_data$likes_to_following)]

instagram_data$followers_to_following[is.infinite(instagram_data$followers_to_following)] <- instagram_data$followers[is.infinite(instagram_data$followers_to_following)]

# Replace inf values with zero in 'comments_to_like'
instagram_data$comments_to_likes[is.infinite(instagram_data$comments_to_likes)] <- 0

# Replace NaN with 0 in the column comments_to_likes
instagram_data$comments_to_likes <- ifelse(is.nan(instagram_data$comments_to_likes), 0, instagram_data$comments_to_likes)

instagram_data <- instagram_data %>%
  mutate(is_video_numeric = ifelse(is_video == "True", 1, 0))

instagram_data <- instagram_data %>%
  mutate(multiple_images_numeric = ifelse(multiple_images == "True", 1, 0))

instagram_data <- instagram_data %>%
  mutate(contains_keyword = as.numeric(contains_keyword))

# Create new features directly in train data

instagram_data$comments_followers_interaction <- instagram_data$comments * instagram_data$followers
instagram_data$comments_followers_ratio <- instagram_data$comments / (instagram_data$followers + 1)  # To avoid division by zero
instagram_data$followers_squared <- instagram_data$followers^2
instagram_data$log_followers <- log(instagram_data$followers + 1)
instagram_data$day_of_month <- as.integer(format(instagram_data$created_at, "%d"))
instagram_data$month <- as.integer(format(instagram_data$created_at, "%m"))
instagram_data$year <- as.integer(format(instagram_data$created_at, "%Y"))
instagram_data$hour <- as.integer(format(instagram_data$created_at, "%H"))
instagram_data$weekly_likes <- ave(instagram_data$likes, instagram_data$week, FUN = sum)
instagram_data <- instagram_data %>%
  arrange(created_at) %>%
  mutate(lag_likes = lag(likes, n = 1))

# Replace NA values with zero in 'lag_likes'
instagram_data$lag_likes[is.na(instagram_data$lag_likes)] <- 0

selected_data <- instagram_data %>%
  select(comments, likes, followers, following, likes_to_followers, likes_to_following, followers_to_following, likes_minus_followers, 
         followers_minus_following, comments_to_likes, year, month, day, day_of_week, hour, weekend, text_length, word_count, contains_keyword,
         time_of_day_afternoon, time_of_day_night, time_of_day_evening, time_of_day_morning, is_video_numeric, multiple_images_numeric,
         comments_followers_interaction, comments_followers_ratio, followers_squared, log_followers, day_of_month, weekly_likes, lag_likes)

correlations <- cor(instagram_data %>% select(likes, likes_to_followers, likes_to_following, followers_to_following, likes_minus_followers, followers_minus_following, comments_to_likes), use = "complete.obs")

print(correlations)

# Remove collinear variables
selected_data2 <- selected_data %>% 
  select(-followers_minus_following, 
         -time_of_day_morning, 
         -day_of_month, 
         -weekly_likes,
         -likes_minus_followers,
         -text_length,
         -followers,
         -time_of_day_evening,
         -comments_followers_interaction,
         -day,
         -day_of_week,
         -contains_keyword,
         -multiple_images_numeric,
         -following)

# Fit a linear model for VIF calculation
vif_model <- lm(likes ~ ., data = selected_data2)

# Calculate VIF
vif_values <- vif(vif_model)

# Print VIF values
print(vif_values)

alias(vif_model)

# Split data 
set.seed(123)  
trainIndex <- createDataPartition(selected_data2$likes, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train_data <- selected_data2[trainIndex, ]
test_data <- selected_data2[-trainIndex, ]

# Train the linear regression model

lm_model <- train(likes ~ ., data = train_data, method = "lm")
summary(lm_model)



# Make predictions on test data
lm_predictions <- predict(lm_model, newdata = test_data)

# Evaluate model performance
lm_rmse <- sqrt(mean((lm_predictions - test_data$likes)^2))
lm_rmse  # Print RMSE (Root Mean Squared Error)


par(mfrow = c(2, 2))
plot(lm_rmse)

set.seed(123)
cv_control <- trainControl(method = "cv", number = 10)
cv_model <- train(likes ~ ., data = train_data, method = "lm", trControl = cv_control)
print(cv_model)

# Make predictions on the test data
test_predictions <- predict(cv_model, newdata = test_data)

# Actual values
actual_values <- test_data$likes

# Calculate RMSE
rmse <- sqrt(mean((test_predictions - actual_values)^2))

# Calculate MAE
mae <- mean(abs(test_predictions - actual_values))

# Calculate R-squared
rss <- sum((test_predictions - actual_values)^2)
tss <- sum((actual_values - mean(actual_values))^2)
rsq <- 1 - rss/tss

# Print the metrics
cat("RMSE: ", rmse, "\n")
cat("MAE: ", mae, "\n")
cat("R-squared: ", rsq, "\n")

# Plot predicted vs actual values
plot(actual_values, test_predictions, 
     main = "Predicted vs Actual Values",
     xlab = "Actual Values", ylab = "Predicted Values",
     pch = 19, col = "blue")

# Add a line for perfect predictions
abline(a = 0, b = 1, col = "red")


### RF method
rf_model <- train(likes ~ ., data = train_data, method = "rf")

print(rf_model)
predictions_rf <- predict(rf_model, newdata = test_data)
postResample(predictions_rf, test_data$likes)

summary(rf_model)

single_tree <- randomForest::getTree(rf_model$finalModel, k = 1, labelVar = TRUE)

# Convert the extracted tree to a data frame
tree_df <- as.data.frame(single_tree)

# Convert the data frame to a party object for plotting
party_tree <- as.party(rpart::rpart(likes ~ ., data = train_data))

# Plot the tree
plot(party_tree, main = "Single Tree from Random Forest")


