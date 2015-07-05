# Split the data in training and test set --------------------------------------

train.T19044 <- window(ts.demand.T19044, end = c(2014, 5))
test.T19044 <- window(ts.demand.T19044, start = c(2014, 6))


# Fit models -------------------------------------------------------------------

# fit1.T19044 <- ets(train.T19044, model = "AAM", restrict = FALSE)
fit2.T19044 <- ets(train.T19044, model = "AAM", restrict = FALSE, alpha = 0.2, 
                   beta = 0.1, gamma = 0.1)
fit3.T19044 <- ets(train.T19044, model = "AAM", restrict = FALSE, alpha = 0.1, 
                   beta = 0.05, gamma = 0.05)
fit4.T19044 <- ets(train.T19044, model = "AAM", restrict = FALSE, alpha = 0.25, 
                   beta = 0.005, gamma = 0.005)


# Forecast the models fitted ---------------------------------------------------

# forecast1.T19044 <- forecast(fit1.T19044, h = 12)
forecast2.T19044 <- forecast(fit2.T19044, h = 12)
forecast3.T19044 <- forecast(fit3.T19044, h = 12)
forecast4.T19044 <- forecast(fit4.T19044, h = 12)


# Compare models ---------------------------------------------------------------

# acc1.T19044 <- accuracy(forecast1.T19044, test.T19044)
acc2.T19044 <- accuracy(forecast2.T19044, test.T19044)
acc3.T19044 <- accuracy(forecast3.T19044, test.T19044)
acc4.T19044 <- accuracy(forecast4.T19044, test.T19044)


# Also check the summary() for the fitted models for AIC etc 


# Calculate accuracy and service level for the most promising ------------------

a3 <- demand.T19044[26:37,2]

## The accuracy is 1 - [sum(abs(demand - forecast))/sum(demand)]

acc3.2 <- 1 - abs(sum(a3 - forecast2.T19044$mean))/sum(a3)
acc3.3 <- 1 - abs(sum(a3 - forecast3.T19044$mean))/sum(a3)
acc3.4 <- 1 - abs(sum(a3 - forecast4.T19044$mean))/sum(a3)

## For the service level i suppose 0 safety stock to see how the model performs.
## It is 1 - [(the demand I could NOT cover)/sum(demand)] - I add the positive 
## numbers from a2 - forecast2.T19044$mean.

sl3.2 <- 1 - (20+55+13)/sum(a3)
sl3.3 <- 1 - (35+69+102+110+64+69+73)/sum(a3)
sl3.4 <- 1 - (27)/sum(a3)

## Accuracy is a good indicator, but I need a high SL. SL comes first.


# Select the best model --------------------------------------------------------

## In this case either fit3 or fit4 will do, since both the accuracy and the SL 
## are almost identical.

fit.T19044 <- fit3.T19044
fcast.T19044 <- forecast3.T19044


# Fit model to all data --------------------------------------------------------

fitall.T19044 <- ets(ts.demand.T19044, model = "AAM", restrict = FALSE, 
                     alpha = 0.1, beta = 0.05, gamma = 0.05)
fcastall.T19044 <- forecast(fitall.T19044, h = 12)

plot(fcastall.T19044, xlab = "Χρόνος", ylab = "Πωλήσεις", main = "T19044")


# Calculate CV -----------------------------------------------------------------

cv.T19044 <- sqrt(fitall.T19044$sigma2) / mean(fitall.T19044$fitted)


# Inventory control model ------------------------------------------------------

fcastcomp.T19044 <- forecast(ets(train.T19044, model = fitall.T19044, 
                                  restrict = FALSE, use.initial.values = TRUE), 
                              h = 14)


y3 <- ceiling(fcastcomp.T19044$mean)
sigma3 <- sqrt(fitall.T19044$sigma2)
z <- qnorm(.97)

s3 <- ceiling(sigma3 * z + y3)

# Add zeros to make ts the same length ---
s3 <- ts(c(0, 0, s3), start = c(2014, 4), frequency = 12)


ip3 <- ts(c(1712, 1447, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
          start = c(2014, 4), frequency = 12)

order3 <- ts(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), start = c(2014, 4), 
             frequency = 12)

sales3 <- window(ts.demand.T19044, start = c(2014, 4))


for (i in 1:14) {
  if (i > 2) {
    ip3[i] <- ip3[i-1] + order3[i-2] - sales3[i]
    if (ip3[i] < 0) ip3[i] <- 0
  }
  order3[i] <- s3[i+2] - ip3[i]
  if (order3[i] < 0) order3[i] <- 0
}

# Lost sales %
abs(77+0-576 + 0+347-364 + 17+313-420)/sum(window(sales3, start = c(2014, 6)))

# Total cost
9*3.3 + sum(window(ip3, start = c(2014, 6)))*0.06*4.48 + 
  sum(window(order3, start = c(2014, 6)))*4.48



# AGROTECH's policy ---

fagro.T19044 <- ts(c(0, 0, ceiling(1.3 * window(ts.demand.T19044, 
                                                 start = c(2013, 6), 
                                                 end = c(2014, 7)))), 
                    start = c(2014, 4), frequency = 12)



ip3agro <- ts(c(1712, 1447, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
              start = c(2014, 4), frequency = 12)

order3agro <- ts(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                 start = c(2014, 4), frequency = 12)

for (i in 1:14) {
  if (i > 2) {
    ip3agro[i] <- ip3agro[i-1] + order3agro[i-2] - sales3[i]
    if (ip3agro[i] < 0) ip3agro[i] <- 0
  }
  order3agro[i] <- fagro.T19044[i+2] - ip3agro[i]
  if (order3agro[i] < 0) order3agro[i] <- 0
}

# Lost sales %
abs(77+0-576 + 0+297-364 + 156+53-311)/sum(window(sales3, start = c(2014, 6)))

# Total cost
9*3.3 + sum(window(ip3agro, start = c(2014, 6)))*0.06*4.48 + 
  sum(window(order3agro, start = c(2014, 6)))*4.48


