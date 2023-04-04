library(neuralnet)
aps_failure_data <- read.csv("aps_failure_training_set.csv")
aps_failure_data <- na.omit(aps_failure_data)
aps_failure_data$aa_000 <- na.omit(aps_failure_data$aa_000)
aps_failure_data$ac_000 <- na.omit(aps_failure_data$ac_000)
aps_failure_data$class <- ifelse(aps_failure_data$class == "pos", 1, 0)
aps_failure_data$class <- as.numeric(aps_failure_data$class)
aps_failure_data$aa_000 <- as.numeric(aps_failure_data$aa_000)
aps_failure_data$ac_000 <- as.numeric(aps_failure_data$ac_000)

training_data <- aps_failure_data[, c("aa_000", "ac_000", "class")]
testing_data <- aps_failure_data[, c("aa_000","ac_000", "class")]
#Fit Neural Network
neural_net4a <- neuralnet(class ~ aa_000 + ac_000, data = training_data,
                        hidden = 5, act.fct ="tanh", linear.output = FALSE, 
                        algorithm="rprop+", stepmax = 0.001)
plot(neural_net4a)
test_net4a <- compute(neural_net4a,testing_data[,"aa000","ac000"])

evaluate <- 1 -mean((test_net4a$net.result -testing_data$aa_000)^2)


