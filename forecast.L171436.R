# Split the data in training and test set --------------------------------------

train.L171436 <- window(ts.demand.L171436, end = c(2014, 5))
test.L171436 <- window(ts.demand.L171436, start = c(2014,6))


# Fit models -------------------------------------------------------------------

# fit1.L171436 <- ets(train.L171436, model = "AAM", restrict = FALSE)
fit2.L171436 <- ets(train.L171436, model = "AAM", restrict = FALSE, 
                    alpha = 0.2, beta = 0.1, gamma = 0.1)
fit3.L171436 <- ets(train.L171436, model = "AAM", restrict = FALSE, 
                    alpha = 0.3, beta = 0.1, gamma = 0.1)
fit4.L171436 <- ets(train.L171436, model = "AAM", restrict = FALSE, 
                    alpha = 0.37, beta = 0.1, gamma = 0.15)


# Forecast the models fitted ---------------------------------------------------

# forecast1.L171436 <- forecast(fit1.L171436, h = 12)
forecast2.L171436 <- forecast(fit2.L171436, h = 12)
forecast3.L171436 <- forecast(fit3.L171436, h = 12)
forecast4.L171436 <- forecast(fit4.L171436, h = 12)


# Compare models ---------------------------------------------------------------

# acc1.L171436 <- accuracy(forecast1.L171436, test.L171436)
acc2.L171436 <- accuracy(forecast2.L171436, test.L171436)
acc3.L171436 <- accuracy(forecast3.L171436, test.L171436)
acc4.L171436 <- accuracy(forecast4.L171436, test.L171436)


# Also check the summary() for the fitted models for AIC etc 


# Calculate accuracy and service level for the most promising ------------------

a4 <- demand.L171436[25:36,2]

## The accuracy is 1 - [sum(abs(demand - forecast))/sum(demand)]

acc4.2 <- 1 - abs(sum(a4 - forecast2.L171436$mean))/sum(a4)
acc4.3 <- 1 - abs(sum(a4 - forecast3.L171436$mean))/sum(a4)
acc4.4 <- 1 - abs(sum(a4 - forecast4.L171436$mean))/sum(a4)

## For the service level i suppose 0 safety stock to see how the model performs.
## It is 1 - [(the demand I could NOT cover)/sum(demand)] - I add the positive 
## numbers from a4 - forecast2.L171436$mean.

sl4.2 <- 1 - (5+53)/sum(a4)
sl4.3 <- 1 - (21+73+3)/sum(a4)
sl4.4 <- 1 - (23+3+77+3)/sum(a4)

## Accuracy is a good indicator, but I need a high SL. SL comes first.


# Select the best model --------------------------------------------------------

## In this case either fit3 or fit4 will do, since both the accuracy and the SL 
## are almost identical.

fit.L171436 <- fit4.L171436
fcast.L171436 <- forecast4.L171436


# Fit model to all data --------------------------------------------------------

fitall.L171436 <- ets(ts.demand.L171436, model = "AAM", restrict = FALSE, 
                      alpha = 0.37, beta = 0.1, gamma = 0.15)
fcastall.L171436 <- forecast(fitall.L171436, h = 12)
fcastcomp.L171436 <- forecast(ets(train.L171436, model = fitall.L171436, 
                                  restrict = FALSE, use.initial.values = TRUE), 
                              h = 14)
plot(fcastall.L171436, xlab = "Χρόνος", ylab = "Πωλήσεις", main = "L171436")


# Calculate CV -----------------------------------------------------------------

cv.L171436 <- sqrt(fitall.L171436$sigma2) / mean(fitall.L171436$fitted)


