library(readxl)
library(neuralnet)
library(Metrics)
library(e1071)
library(tseries)
library(openxlsx)

data <- read_excel('Irish_data.xlsx')
data$MW_diff <- c(0, diff(data$MW, 1))
set.seed(01112001)

#train test split
train_unscaled <- data[1:17,]
test_unscaled <- data[18:21,]

#looking for correlation
cor(data[,2:7])

mean(data$MW)

plot(x=data$year, y=data$MW, type='b' ,xlab='Year', ylab='Waste (kg/capita)', 
     xaxt = "n", cex.lab=1.2)
axis(1, at = data$year, labels = FALSE, tcl = -0.5) #tick marks for every year
text(
  x=data$year[data$year%%2==0],      #positions for labels
  y=538,                             #slightly below the axis
  labels=data$year[data$year%%2==0], #adding the labels
  srt=45,                            #rotation angle in degrees
  adj=0.5,                           #alignment (0.5=centered)
  xpd=TRUE,                          #allow drawing outside the plot area
  cex=0.9                            #size of the text
)

#scaling
train_scaled <- as.data.frame(scale(train_unscaled))
train_mean <- sapply(train_unscaled, mean)
train_sd <- sapply(train_unscaled, sd)
test_scaled <- as.data.frame(scale(test_unscaled, center=train_mean, 
                                   scale=train_sd))

#fitting SVM ----
svm_model <- svm(MW~GEC+GDP+population+unemployment+C02+inflation, 
                 data=train_scaled, type='eps-regression', kernal='radial')

predictions_svm <- predict(svm_model, train_scaled)

predictions_unscaled_svm <- predictions_svm*sd(train_unscaled$MW)+mean(train_unscaled$MW)
residuals_svm <- train_unscaled$MW-predictions_unscaled_svm
plot(predictions_unscaled_svm, residuals_svm, xlab='Fitted values (kg/capita)', 
      ylab='Residuals (kg/capita)', xaxt="n", yaxt="n", cex.lab=1.2)
axis(1, at = seq(575, 800, by = 25))
axis(2, at = seq(-60, 40, by = 20))

# Compute R^2
SSE <- sum((train_unscaled$MW-predictions_unscaled_svm)^2)
SST <- sum((train_unscaled$MW-mean(train_unscaled$MW))^2)
R2 <- 1-SSE/SST
cat("R^2:", R2, "\n") #very high

rmse(train_unscaled$MW, predictions_unscaled_svm)
mae(train_unscaled$MW, predictions_unscaled_svm)

plot(train_unscaled$MW, type='l')
lines(predictions_unscaled_svm, col='red')

#test set
predictions_test_svm <- predict(svm_model, test_scaled)
actual_test <- test_scaled$MW

predictions_unscaled_test_svm <- predictions_test_svm*sd(train_unscaled$MW)+mean(train_unscaled$MW)
residuals_test_svm <- test_unscaled$MW-predictions_unscaled_test_svm
plot(predictions_unscaled_test_svm, residuals_test_svm, xlab='Fitted', 
     ylab='Residuals', main = 'Fitted vs Residuals - Test (svm)')

# Compute R^2
SSE <- sum((test_unscaled$MW-predictions_unscaled_test_svm)^2)
SST <- sum((test_unscaled$MW-mean(test_unscaled$MW))^2)
R2 <- 1-SSE/SST
cat("R^2:", R2, "\n")

rmse(test_unscaled$MW,predictions_unscaled_test_svm)
mae(test_unscaled$MW,predictions_unscaled_test_svm)

plot(test_unscaled$MW, type='l')
lines(predictions_unscaled_test_svm, col='red')

#mean baseline model----

#training set
mean_baseline_train <- rep(mean(train_unscaled$MW), nrow(train_unscaled))

#train metrics
SSE <- sum((train_unscaled$MW - mean_baseline_train)^2)
SST <- sum((train_unscaled$MW - mean(train_unscaled$MW))^2)
R2 <- 1 - SSE/SST
cat("R^2:", R2, "\n")
cat("RMSE:", rmse(train_unscaled$MW, mean_baseline_train), "\n")
cat("MAE:", mae(train_unscaled$MW, mean_baseline_train), "\n\n")

#test set
mean_baseline_test <- rep(mean(train_unscaled$MW), nrow(test_unscaled))

#test metrics
SSE <- sum((test_unscaled$MW - mean_baseline_test)^2)
SST <- sum((test_unscaled$MW - mean(test_unscaled$MW))^2)
R2 <- 1 - SSE/SST
cat("R^2:", R2, "\n")
cat("RMSE:", rmse(test_unscaled$MW, mean_baseline_test), "\n")
cat("MAE:", mae(test_unscaled$MW, mean_baseline_test), "\n")





# ---- Naive Baseline ----
#train - predict y_{t-1}
naive_baseline_train <- c(NA, head(train_unscaled$MW, -1))

#test - last value from training, then rolling forward
naive_baseline_test <- c(tail(train_unscaled$MW, 1), head(test_unscaled$MW, -1))

# Train metrics (excluding first NA)
SSE <- sum((train_unscaled$MW[-1] - naive_baseline_train[-1])^2)
SST <- sum((train_unscaled$MW[-1] - mean(train_unscaled$MW[-1]))^2)
R2 <- 1 - SSE/SST
cat("R^2:", R2, "\n")
cat("RMSE:", rmse(train_unscaled$MW[-1], naive_baseline_train[-1]), "\n")
cat("MAE:", mae(train_unscaled$MW[-1], naive_baseline_train[-1]), "\n\n")

# Test metrics
SSE <- sum((test_unscaled$MW - naive_baseline_test)^2)
SST <- sum((test_unscaled$MW - mean(test_unscaled$MW))^2)
R2 <- 1 - SSE/SST
cat("R^2:", R2, "\n")
cat("RMSE:", rmse(test_unscaled$MW, naive_baseline_test), "\n")
cat("MAE:", mae(test_unscaled$MW, naive_baseline_test), "\n")

