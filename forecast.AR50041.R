# Split the data in training and test set --------------------------------------

train.AR50041 <- window(ts.demand.AR50041, end = c(2014, 5))
test.AR50041 <- window(ts.demand.AR50041, start = c(2014, 6))


# Fit models -------------------------------------------------------------------

# fit1.AR50041 <- ets(train.AR50041, model = "AAM", restrict = FALSE)
fit2.AR50041 <- ets(train.AR50041, model = "AAM", restrict = FALSE, 
                    alpha = 0.2, beta = 0.1, gamma = 0.1)
fit3.AR50041 <- ets(train.AR50041, model = "AAM", restrict = FALSE, 
                    alpha = 0.2, beta = 0.02, gamma = 0.001)
fit4.AR50041 <- ets(train.AR50041, model = "AAM", restrict = FALSE, 
                    alpha = 0.18, beta = 0.02, gamma = 0.001)


# Forecast the models fitted ---------------------------------------------------

# forecast1.AR50041 <- forecast(fit1.AR50041, h = 12)
forecast2.AR50041 <- forecast(fit2.AR50041, h = 12)
forecast3.AR50041 <- forecast(fit3.AR50041, h = 12)
forecast4.AR50041 <- forecast(fit4.AR50041, h = 12)


# Compare models ---------------------------------------------------------------

# acc1.AR50041 <- accuracy(forecast1.AR50041, test.AR50041)
acc2.AR50041 <- accuracy(forecast2.AR50041, test.AR50041)
acc3.AR50041 <- accuracy(forecast3.AR50041, test.AR50041)
acc4.AR50041 <- accuracy(forecast4.AR50041, test.AR50041)


# Also check the summary() for the fitted models for AIC etc 


# Calculate accuracy and service level for the most promising ------------------

a2 <- demand.AR50041[26:37,2]

## The accuracy is 1 - [sum(abs(demand - forecast))/sum(demand)]

acc2.2 <- 1 - abs(sum(a2 - forecast2.AR50041$mean))/sum(a2)
acc2.3 <- 1 - abs(sum(a2 - forecast3.AR50041$mean))/sum(a2)
acc2.4 <- 1 - abs(sum(a2 - forecast4.AR50041$mean))/sum(a2)

## For the service level i suppose 0 safety stock to see how the model performs.
## It is 1 - [(the demand I could NOT cover)/sum(demand)] - I add the positive 
## numbers from a2 - forecast2.AR50041$mean.

sl2.2 <- 1 - (38+17+90+145+19+38+65)/sum(a2)
sl2.3 <- 1 - (20+5+84+151+13+43+75)/sum(a2)
sl2.4 <- 1 - (17+89+148+12+43+76)/sum(a2)

## Accuracy is a good indicator, but I need a high SL. SL comes first.


# Select the best model --------------------------------------------------------

## In this case either fit3 or fit4 will do, since both the accuracy and the SL 
## are almost identical.

fit.AR50041 <- fit4.AR50041
fcast.AR50041 <- forecast4.AR50041


# Fit model to all data --------------------------------------------------------

fitall.AR50041 <- ets(ts.demand.AR50041, model = "AAM", restrict = FALSE, 
                      alpha = 0.18, beta = 0.02, gamma = 0.001)
fcastall.AR50041 <- forecast(fitall.AR50041, h = 12)

plot(fcastall.AR50041, xlab = "Χρόνος", ylab = "Πωλήσεις", main = "AR50041")


# Calculate CV -----------------------------------------------------------------

cv.AR50041 <- sqrt(fitall.AR50041$sigma2) / mean(fitall.AR50041$fitted)


# Inventory control model ------------------------------------------------------

fcastcomp.AR50041 <- forecast(ets(train.AR50041, model = fitall.AR50041, 
                                  restrict = FALSE, use.initial.values = TRUE), 
                              h = 14)

y2 <- ceiling(fcastcomp.AR50041$mean)
sigma2 <- sqrt(fitall.AR50041$sigma2)
z <- qnorm(.97)

s2 <- ceiling(sigma2 * z + y2)

# Add zeros to make ts the same length ---
s2 <- ts(c(0, 0, s2), start = c(2014, 4), frequency = 12)

# ip2 <- ts(c(2963, 2822, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
#           start = c(2014, 4), frequency = 12)
ip2 <- ts(c(100, 150, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
          start = c(2014, 4), frequency = 12)

order2 <- ts(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), start = c(2014, 4), 
             frequency = 12)

sales2 <- window(ts.demand.AR50041, start = c(2014, 4))


for (i in 1:14) {
  if (i > 2) {
    ip2[i] <- ip2[i-1] + order2[i-2] - sales2[i]
    if (ip2[i] < 0) ip2[i] <- 0
  }
  order2[i] <- s2[i+2] - ip2[i]
  if (order2[i] < 0) order2[i] <- 0
}

# Lost sales % for (100, 150, ...)
abs((50+51-211))/sum(window(sales2, start = c(2014, 6)))


# Total cost
12*3.3 + sum(window(ip2, start = c(2014, 6)))*0.06*10.26 + 
  sum(window(order2, start = c(2014, 6)))*10.26



# AGROTECH's policy ---

fagro.AR50041 <- ts(c(0, 0, ceiling(1.3 * window(ts.demand.AR50041, 
                                                 start = c(2013, 6), 
                                                 end = c(2014, 7)))), 
                    start = c(2014, 4), frequency = 12)


# ip2agro <- ts(c(2963, 2822, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
#           start = c(2014, 4), frequency = 12)
ip2agro <- ts(c(100, 150, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
              start = c(2014, 4), frequency = 12)

order2agro <- ts(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                 start = c(2014, 4), frequency = 12)

for (i in 1:14) {
  if (i > 2) {
    ip2agro[i] <- ip2agro[i-1] + order2agro[i-2] - sales2[i]
    if (ip2agro[i] < 0) ip2agro[i] <- 0
  }
  order2agro[i] <- fagro.AR50041[i+2] - ip2agro[i]
  if (order2agro[i] < 0) order2agro[i] <- 0
}

# Lost sales % for (100, 150, ...) 
abs(97+0-98 + 0+66-122 + 237+0-247 + 0+0-31 + 38+0-211) / 
  sum(window(sales2, start = c(2014, 6)))


# Total cost
9*3.3 + sum(window(ip2agro, start = c(2014, 6)))*0.06*10.26 + 
  sum(window(order2agro, start = c(2014, 6)))*10.26



