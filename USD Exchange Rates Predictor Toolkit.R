#importing a libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(neuralnet)

#loading the data
exchange_usd <-
  read_excel(
    file_path <-
      "F:/IIT/2nd Year/Sem 2/ML/CW/ExchangeUSD .xlsx"
  )


#changing col names
colnames(exchange_usd) <- c("Date", "Wdy", "USD/EUR" )
summary(exchange_usd)
#limiting for 3rd column
exchange_usd <- exchange_usd[, 3]


#FIRST TIME LAG (t-1)
USD_current <- lag(exchange_usd, 1)
USD_output <- exchange_usd
#combining the cols
tdelay1 <- bind_cols(USD_current, USD_output)
#removing NA values
tdelay1 <-
  tdelay1[complete.cases(tdelay1), ]

#SECOND TIME LAGS (t-2)
USD_before <- lag(exchange_usd, 2)
USD_output <- exchange_usd
#combining the cols
tdelay2 <- bind_cols(USD_before, USD_current, USD_output)
#removing NA values
tdelay2 <-
  tdelay2[complete.cases(tdelay2), ]

#THIRD TIME LAGS (t-3)
USD_before2 <- lag(exchange_usd, 3)
USD_output <- exchange_usd
tdelay3 <-
  bind_cols(USD_before2, USD_before, USD_current, USD_output)
tdelay3 <-
  tdelay3[complete.cases(tdelay3), ]

#FOURTH TIME LAGS (t-4)
USD_before3 <- lag(exchange_usd, 4)
USD_output <- exchange_usd
tdelay4 <-
  bind_cols(USD_before3, USD_before2, USD_before, USD_current, USD_output)
tdelay4 <-
  tdelay4[complete.cases(tdelay4), ]

#5th TIME LAGS (t-5)
USD_before4 <- lag(exchange_usd, 5)
USD_output <- exchange_usd
# Assuming you want to include the original data frame as the first column in the bind_cols() function call
tdelay5 <-
  bind_cols(exchange_usd,
            USD_before4,
            USD_before3,
            USD_before2,
            USD_before,
            USD_current,
            USD_output)

tdelay5 <-
  tdelay5[complete.cases(tdelay5), ]




#6thTIME LAGS (t-5)
USD_before5 <- lag(exchange_usd, 6)
USD_output <- exchange_usd
tdelay6 <-
  bind_cols(USD_before5,
            USD_before4,
            USD_before3,
            USD_before2,
            USD_before,
            USD_current,
            USD_output)
tdelay6 <-
  tdelay6[complete.cases(tdelay6), ]



#SEVENTH TIME LAGS (t-7)
USD_before6 <- lag(exchange_usd, 7)
USD_output <- exchange_usd

tdelay7 <-
  bind_cols(
    USD_before6,
    USD_before5,
    USD_before4,
    USD_before3,
    USD_before2,
    USD_before,
    USD_current,
    USD_output
  )
tdelay7 <-
  tdelay7[complete.cases(tdelay7), ]

#part c
#min and max values to use when denormalizing
min_value1 <- min(tdelay1)
max_value1 <- max(tdelay1)
min_value2 <- min(tdelay2)
max_value2 <- max(tdelay2)
min_value3 <- min(tdelay3)
max_value3 <- max(tdelay3)
min_value4 <- min(tdelay4)
max_value4 <- max(tdelay4)
min_value7 <- min(tdelay7)
max_value7 <- max(tdelay7)
#normalizing function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#side by side matrix normalization
par(mfrow = c(1, 2))
#list of matrix to pass through the loop
matrix_list <-
  list(
    tdelay1,
    tdelay2,
    tdelay3,
    tdelay4,
    tdelay7
  )
#for loop to get the box plots before and after normalization
for (i in 1:length(matrix_list)) {
  boxplot(matrix_list[[i]])
  matrix_list[[i]] <-
    as.data.frame(apply(matrix_list[[i]], 2, normalize))
  print(paste0("Summary for tdelay
", i, ":"))
  print(summary(matrix_list[[i]]))
  boxplot(matrix_list[[i]])
}
#give back the column names
colnames(tdelay1) <- c("USD_current", "USD_output")
colnames(tdelay2) <-
  c("USD_before", "USD_current", "USD_output")
colnames(tdelay3) <-
  c("USD_before2", "USD_before", "USD_current", "USD_output")
colnames(tdelay4) <-
  c("USD_before3",
    "USD_before2",
    "USD_before",
    "USD_current",
    "USD_output")
colnames(tdelay7) <-
  c(
    "USD_before6",
    "USD_before5",
    "USD_before4",
    "USD_before3",
    "USD_before2",
    "USD_before",
    "USD_current",
    "USD_output"
  )
#part d
#unnormalizing function
unnormalize <- function(x, min, max) {
  return((max - min) * x + min)
}

#RELU ACTIVATION FUNCTION
relu <- function(x)
  ifelse(x > 0, x, 0)
## FOR 1 INPUT: tdelay1
#set seed for reproducibility
set.seed(123)
#get number of rows
n_rows <- nrow(tdelay1)
#get index of rows
train_index <- sample(seq_len(n_rows), size = 400, replace = FALSE)
test_index <- setdiff(seq_len(n_rows), train_index)
#split train-test dataset
train_data <- tdelay1[train_index,]
test_data <- tdelay1[test_index,]
#CREATE 3 SEPERATE NEURAL NETWORKS
#set seed for reproducibility
set.seed(123)
#train the neural network 1
nn1 <- neuralnet(
  USD_output ~ USD_current,
  data = train_data,
  hidden = c(3, 5),
  act.fct = tanh,
  
  linear.output = TRUE
)
#plot NN1
plot(nn1)
#train the neural network 2
nn2 <- neuralnet(
  USD_output ~ USD_current,
  data = train_data,
  hidden = 7,
  act.fct = "logistic",
  linear.output = FALSE
)
#plot the neural network
plot(nn2)
#train the neural network 3
nn3 <- neuralnet(
  USD_output ~ USD_current,
  data = train_data,
  hidden = c(4, 9),
  act.fct = relu,
  linear.output = TRUE
)
#plot the neural network 3
plot(nn3)
#extract actual values from test data
actual_values <- test_data$USD_output
actual_values <-
  unnormalize(actual_values, min = min_value1, max = max_value1)

#list to store all the results
results <- list()
#loop through the three nns
for (i in 1:3) {
  nn_name <- paste0("nn", i)
  
  #prediction
  nn_predictions <- predict(get(nn_name), test_data)
  nn_predictions <-
    unnormalize(nn_predictions, min = min_value1, max = max_value1)
  
  #RMSE
  rmse <- sqrt(mean((nn_predictions - actual_values) ^ 2))
  
  #MAE
  mae <- mean(abs(nn_predictions - actual_values))
  
  #MAPE
  mape <-
    mean(abs((actual_values - nn_predictions) / actual_values)) * 100
  
  #sMAPE
  smape <-
    mean(2 * abs(nn_predictions - actual_values) / (abs(nn_predictions) + abs(actual_values))) *
    100
  
  
  #save the results
  results[[i]] <-
    list(
      nn = nn_name,
      rmse = rmse,
      mae = mae,
      mape = mape,
      smape = smape
    )
  
  #display the result
  cat("Results for neural network ", nn_name, "\n")
  cat(paste0("RMSE: ", round(rmse, 2), "\n"))
  cat(paste0("MAE: ", round(mae, 2), "\n"))
  cat(paste0("MAPE: ", round(mape, 2), "%\n"))
  cat(paste0("sMAPE: ", round(smape, 2), "%\n\n"))
}
# FOR 2 INPUT: tdelay2

#set seed for reproducibility
set.seed(123)
#get number of rows
n_rows <- nrow(tdelay2)
#get index of rows for training and testing
train_index <- sample(seq_len(n_rows), size = 400, replace = FALSE)
test_index <- setdiff(seq_len(n_rows), train_index)
#split to train and test data set
train_data <- tdelay2[train_index,]
test_data <- tdelay2[test_index,]
#######CREATE 3 SEPERATE NEURAL NETWORKS###############
#set seed for reproducibility
set.seed(123)

#train the neural network 4
nn4 <-
  neuralnet(
    USD_output ~ USD_current + USD_before,
    data = train_data,
    hidden = c(9, 2),
    act.fct = tanh,
    linear.output = FALSE
  )
#plot the neural network 4
plot(nn4)

#train the neural network 5
nn5 <-
  neuralnet(
    USD_output ~ USD_before + USD_current,
    data = train_data,
    hidden = c(3, 8) ,
    act.fct = "logistic",
    linear.output = TRUE
  )
#plot the neural network 5
plot(nn5)

#train the neural network 6
nn6 <-
  neuralnet(
    USD_output ~ USD_before + USD_current,
    data = train_data,
    hidden = 5,
    act.fct = relu,
    linear.output = TRUE
  )

#plot the neural network 6
plot(nn6)
#extract actual values from test data
actual_values <- test_data$USD_output
actual_values <-
  unnormalize(actual_values, min = min_value2, max = max_value2)
#loop through the three neural networks
for (i in 4:6) {
  nn_name <- paste0("nn", i)
  
  #prediction
  nn_predictions <- predict(get(nn_name), test_data)
  nn_predictions <-
    unnormalize(nn_predictions, min = min_value2, max = max_value2)
  
  #calculate RMSE
  rmse <- sqrt(mean((nn_predictions - actual_values) ^ 2))
  
  #calculate MAE
  mae <- mean(abs(nn_predictions - actual_values))
  
  #calculate MAPE
  mape <-
    mean(abs((actual_values - nn_predictions) / actual_values)) * 100
  
  #calculate sMAPE
  smape <-
    mean(2 * abs(nn_predictions - actual_values) / (abs(nn_predictions) + abs(actual_values))) *
    100
  
  
  #save results
  results[[i]] <-
    list(
      nn = nn_name,
      rmse = rmse,
      mae = mae,
      mape = mape,
      smape = smape
    )
  
  #display results
  cat("Results for neural network ", nn_name, "\n")
  cat(paste0("RMSE: ", round(rmse, 2), "\n"))
  cat(paste0("MAE: ", round(mae, 2), "\n"))
  cat(paste0("MAPE: ", round(mape, 2), "%\n"))
  cat(paste0("sMAPE: ", round(smape, 2), "%\n\n"))
}
# FOR 3 INPUT: tdelay3

#set seed for reproducibility
set.seed(123)
#getting the number of rows in dataset
n_rows <- nrow(tdelay3)
#get index of rows for training and testing
train_index <- sample(seq_len(n_rows), size = 400, replace = FALSE)
test_index <- setdiff(seq_len(n_rows), train_index)
#train test data split
train_data <- tdelay3[train_index,]
test_data <- tdelay3[test_index,]
#######CREATE 3 SEPERATE NEURAL NETWORKS###############
#set seed for reproducibility
set.seed(123)

#train the neural network 7
nn7 <-
  neuralnet(
    USD_output ~ USD_before2 + USD_before + USD_current,
    data = train_data,
    hidden = 6,
    act.fct = tanh,
    linear.output = TRUE
  )
#plot the neural network 7
plot(nn7)
#train the neural network 8
nn8 <-
  neuralnet(
    USD_output ~ USD_before2 + USD_before + USD_current,
    data = train_data,
    hidden = c(2, 5),
    act.fct = "logistic",
    linear.output = TRUE
  )
#plot the neural network 8
plot(nn8)




#train the neural network 9
nn9 <-
  neuralnet(
    USD_output ~ USD_before2 + USD_before + USD_current,
    data = train_data,
    hidden = 7,
    act.fct = "tanh",
    linear.output = FALSE
  )
#plot the neural network 9
plot(nn9)

#extract actual values from test data and unnormalizing
actual_values <- test_data$USD_output
actual_values <-
  unnormalize(actual_values, min = min_value3, max = max_value3)
#lloop through the three neural networks
for (i in 7:9) {
  nn_name <- paste0("nn", i)
  
  #predicting and unnormalizing
  nn_predictions <- predict(get(nn_name), test_data)
  nn_predictions <-
    unnormalize(nn_predictions, min = min_value3, max = max_value3)
  
  #calculate RMSE
  rmse <- sqrt(mean((nn_predictions - actual_values) ^ 2))
  
  #calculate MAE
  mae <- mean(abs(nn_predictions - actual_values))
  
  #calculate MAPE
  mape <-
    mean(abs((actual_values - nn_predictions) / actual_values)) * 100
  
  #calculate sMAPE
  smape <-
    mean(2 * abs(nn_predictions - actual_values) / (abs(nn_predictions) + abs(actual_values))) *
    100
  
  #save to list
  results[[i]] <-
    list(
      nn = nn_name,
      rmse = rmse,
      mae = mae,
      mape = mape,
      smape = smape
    )
  
  
  #dissplay results
  cat("Results for neural network ", nn_name, "\n")
  cat(paste0("RMSE: ", round(rmse, 2), "\n"))
  cat(paste0("MAE: ", round(mae, 2), "\n"))
  cat(paste0("MAPE: ", round(mape, 2), "%\n"))
  cat(paste0("sMAPE: ", round(smape, 2), "%\n\n"))
}
# FOR 4 INPUT: tdelay4

#set seed for reproducibility
set.seed(123)
#get number of rows in dataset
n_rows <- nrow(tdelay4)
#get index of rows for training and testing
train_index <- sample(seq_len(n_rows), size = 400, replace = FALSE)
test_index <- setdiff(seq_len(n_rows), train_index)
#train test data split
train_data <- tdelay4[train_index,]
test_data <- tdelay4[test_index,]
#######CREATE 3 SEPERATE NEURAL NETWORKS###############
#set seed for reproducibility
set.seed(123)
#train the neural network 10

nn10 <-
  neuralnet(
    USD_output ~ USD_before3 + USD_before2 + USD_before + USD_current,
    data = train_data,
    hidden = c(4, 6),
    act.fct = tanh,
    linear.output = TRUE
  )
#plot the neural network 10
plot(nn10)
#train the neural network 11
nn11 <-
  neuralnet(
    USD_output ~ USD_before3 + USD_before2 + USD_before + USD_current,
    data = train_data,
    hidden = 9,
    act.fct = "logistic",
    linear.output = TRUE
  )
#plot the neural network 11
plot(nn11)
#train the neural network 12
nn12 <-
  neuralnet(
    USD_output ~ USD_before3 + USD_before2 + USD_before + USD_current,
    data = train_data,
    hidden = 6,
    act.fct = relu,
    linear.output = TRUE
  )
#plot the neural network 12
plot(nn12)
#extract actual values from test data and unnormalizing

actual_values <- test_data$USD_output
actual_values <-
  unnormalize(actual_values, min = min_value4, max = max_value4)
#loop through the three neural networks
for (i in 10:12) {
  nn_name <- paste0("nn", i)
  
  #predicting and unnormalizing
  nn_predictions <- predict(get(nn_name), test_data)
  nn_predictions <-
    unnormalize(nn_predictions, min = min_value4, max = max_value4)
  
  #calculate RMSE
  rmse <- sqrt(mean((nn_predictions - actual_values) ^ 2))
  
  #calculate MAE
  mae <- mean(abs(nn_predictions - actual_values))
  
  #calculate MAPE
  mape <-
    mean(abs((actual_values - nn_predictions) / actual_values)) * 100
  
  #calculate sMAPE
  smape <-
    mean(2 * abs(nn_predictions - actual_values) / (abs(nn_predictions) + abs(actual_values))) *
    100
  
  #saving to the list
  results[[i]] <-
    list(
      nn = nn_name,
      rmse = rmse,
      mae = mae,
      mape = mape,
      smape = smape
    )
  
  #display results
  cat("Results for neural network ", nn_name, "\n")
  
  cat(paste0("RMSE: ", round(rmse, 2), "\n"))
  cat(paste0("MAE: ", round(mae, 2), "\n"))
  cat(paste0("MAPE: ", round(mape, 2), "%\n"))
  cat(paste0("sMAPE: ", round(smape, 2), "%\n\n"))
}
# FOR 7 INPUT: tdelay7

#set seed for reproducibility
set.seed(123)
#get number of rows in dataset
n_rows <- nrow(tdelay7)
#get index of rows for training and testing
train_index <- sample(seq_len(n_rows), size = 400, replace = FALSE)
test_index <- setdiff(seq_len(n_rows), train_index)
#train-test split
train_data <- tdelay7[train_index,]
test_data <- tdelay7[test_index,]
#CREATE 3 SEPERATE NEURAL NETWORKS##
#set seed for reproducibility
set.seed(123)
#train the neural network 13
nn13 <- neuralnet(
  USD_output ~
    USD_before6 + USD_before5 + USD_before4 + USD_before3 +
    USD_before2 + USD_before + USD_current,
  data =
    train_data,
  
  hidden = c(9, 3),
  act.fct = tanh,
  linear.output = TRUE
)
#plot the neural network 13
plot(nn13)
#train the neural network 14
nn14 <- neuralnet(
  USD_output ~
    USD_before6 + USD_before5 + USD_before4 + USD_before3 +
    USD_before2 + USD_before + USD_current,
  data =
    train_data,
  hidden = c(9, 3),
  act.fct = "logistic",
  linear.output = TRUE
)
#pplot the neural network 14
plot(nn14)
#train the neural network 15
nn15 <- neuralnet(
  USD_output ~
    USD_before6 + USD_before5 + USD_before4 + USD_before3 +
    USD_before2 + USD_before + USD_current,
  data =
    train_data,
  hidden = c(9, 3),
  act.fct = "tanh",
  linear.output = TRUE
)
#plot the neural network 15
plot(nn15)

#extract actual values from test data and unnormalizing
actual_values <- test_data$USD_output
actual_values <-
  unnormalize(actual_values, min = min_value7, max = max_value7)
#loop through the three neural networks
for (i in 13:15) {
  nn_name <- paste0("nn", i)
  
  #predicting and unnormalizing
  nn_predictions <- predict(get(nn_name), test_data)
  nn_predictions <-
    unnormalize(nn_predictions, min = min_value7, max = max_value7)
  
  #calculate RMSE
  rmse <- sqrt(mean((nn_predictions - actual_values) ^ 2))
  
  #calculate MAE
  mae <- mean(abs(nn_predictions - actual_values))
  
  #calculate MAPE
  mape <-
    mean(abs((actual_values - nn_predictions) / actual_values)) * 100
  
  #calculate sMAPE
  smape <-
    mean(2 * abs(nn_predictions - actual_values) / (abs(nn_predictions) + abs(actual_values))) *
    100
  
  #store the results in the list
  results[[i]] <-
    list(
      nn = nn_name,
      rmse = rmse,
      mae = mae,
      mape = mape,
      smape = smape
    )
  
  
  #print the results
  cat("Results for neural network ", nn_name, "\n")
  cat(paste0("RMSE: ", round(rmse, 2), "\n"))
  cat(paste0("MAE: ", round(mae, 2), "\n"))
  cat(paste0("MAPE: ", round(mape, 2), "%\n"))
  cat(paste0("sMAPE: ", round(smape, 2), "%\n\n"))
}

#Part h
#Scatter Plot
# Take the RMSE values out of the list of results.
rmse_values <- sapply(results, function(res)
  res$rmse)

# Find which MLP network index has the lowest RMSE.
best_mlp_index <- which.min(rmse_values)

# Get the most effective MLP network
best_mlp <- get(paste0("nn", best_mlp_index))

# Predictions for the top MLP network
best_model_pred <- predict(best_mlp, test_data)
best_model_pred <-
  denormalize(best_model_pred, min = min_value1, max = max_value1)

# Actual values
output_denorm <-
  denormalize(test_data$USD_output, min = min_value1, max = max_value1)

# Create a scatter plot
plot(
  output_denorm,
  best_model_pred,
  col = "blue",
  pch = 16,
  xlab = "Actual Values",
  ylab = "Predicted Values",
  main = "Actual vs Predicted Values"
)

# Add a diagonal line for reference
abline(0, 1, col = "red")


legend("bottomright",
       legend = "Ideal Fit",
       col = "red",
       lty = 1)

# Add a regression line (optional)
# abline(lm(best_model_pred ~ output_denorm), col = "green")

#Line chart
# Extract RMSE values from the results list
rmse_values <- sapply(results, function(res)
  res$rmse)

# Find the index of the MLP network with the lowest RMSE
best_mlp_index <- which.min(rmse_values)

# Retrieve the best MLP network
best_mlp <- get(paste0("nn", best_mlp_index))

# Predictions of the best MLP network
best_model_pred <- predict(best_mlp, test_data)
best_model_pred <-
  denormalize(best_model_pred, min = min_value1, max = max_value1)

# Actual values
output_denorm <-
  denormalize(test_data$USD_output, min = min_value1, max = max_value1)

# Create a line chart
plot(
  output_denorm,
  type = "l",
  col = "blue",
  ylim = range(c(output_denorm, best_model_pred)),
  xlab = "Time",
  ylab = "USDvsEUR Rate",
  main = "Predictions vs. Actual Values"
)
lines(best_model_pred, col = "red")
legend(
  "topright",
  legend = c("Actual", "Predicted"),
  col = c("blue", "red"),
  lty = 1
)