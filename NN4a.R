#Use proper library
library(neuralnet)

#Import the data and clean it of Na's. Convert neg,pos to 0,1.
aps_failure_data <- read.csv("aps_failure_training_set.csv", na.strings = c("","na"))
aps_failure_data_clean <- na.omit(aps_failure_data)
aps_failure_data_clean$class <- ifelse(aps_failure_data_clean$class == "pos", 1, 0)
aps_failure_data_clean$class <- as.numeric(aps_failure_data_clean$class)
aps_failure_data_clean$aa_000 <- as.numeric(aps_failure_data_clean$aa_000)
aps_failure_data_clean$ac_000 <- as.numeric(aps_failure_data_clean$ac_000)

#Define training and testing data
training_data <- aps_failure_data_clean[1:295, c("aa_000", "ac_000", "class")]
testing_data <- aps_failure_data_clean[296:590, c("aa_000","ac_000", "class")]


# Creates the neural net with the training data
neural_net4a <- neuralnet(class ~ aa_000 + ac_000, data = training_data,
                          hidden = 5, linear.output = FALSE, algorithm="rprop+")


# Initialize variables to track errors
training_errors <- rep(0, 10)
testing_errors <- rep(0, 10)


# Loop over the number of epochs to collect errors
for (i in 1:10) {
  # Compute errors on training data
  net_result_train <- compute(neural_net4a, training_data[, 1:2])
  training_errors[i] <- sum((net_result_train$net.result[, 1] > 0.5) 
                            != training_data[, "class"])
  
  
  # Compute errors on testing data
  net_result_test <- compute(neural_net4a, testing_data[, 1:2])
  testing_errors[i] <- sum((net_result_test$net.result[, 1] > 0.5) 
                           != testing_data[, "class"])
  
  
  # Update the neural network with training data
  neural_net4a <- update(neural_net4a, training_data[, 1:2], 
                         training_data[, "class"])
}

#Plots the neural net
par(mfrow = c(2,1))
plot(neural_net4a,main = "Panel 1")


# Plots the training and testing errors over the epochs
plot(training_errors, type = "l", col = "blue", xlab = "# of Epochs", 
     ylab = "# of Errors", ylim = c(0, max(training_errors, testing_errors)),main = "Panel 2")
lines(testing_errors, type = "l", col = "red")
legend("bottomright", legend = c("Training Errors", "Testing Errors"), 
       col = c("blue", "red"), lty = 1)


# To answer 4b we will need to adjust the number of hidden layers (this increases complexity) and track what changes it makes.
