# Split the data in training and test set --------------------------------------

`train.YU81824-025` <- window(`ts.demand.YU81824-025`, end = c(2014, 5))
`test.YU81824-025` <- window(`ts.demand.YU81824-025`, start = c(2014, 6))


# Fit models -------------------------------------------------------------------

`fit1.YU81824-025` <- ets(`train.YU81824-025`, model = "AAM", restrict = FALSE)
# `fit2.YU81824-025` <- ets(`train.YU81824-025`, model = "AAM", restrict = FALSE, 
#                           alpha = 0.2, beta = 0.1, gamma = 0.1)
`fit3.YU81824-025` <- ets(`train.YU81824-025`, model = "AAM", restrict = FALSE, 
                          alpha = 0.1, beta = 0.05, gamma = 0.1)
# `fit4.YU81824-025` <- ets(`train.YU81824-025`, model = "MAM")
# `fit5.YU81824-025` <- ets(`train.YU81824-025`, model = "MAM", alpha = 0.2, 
#                        beta = 0.1, gamma = 0.1)
# `fit6.YU81824-025` <- ets(`train.YU81824-025`, model = "MAM", alpha = 0.1, 
#                          beta = 0.05, gamma = 0.1)
`fit7.YU81824-025` <- ets(`train.YU81824-025`, model = "AAM", restrict = FALSE, 
                          alpha = 0.02, beta = 0.01, gamma = 0.01)


# Forecast the models fitted ---------------------------------------------------

`forecast1.YU81824-025` <- forecast(`fit1.YU81824-025`, h = 12)
# `forecast2.YU81824-025` <- forecast(`fit2.YU81824-025`, h = 12)
`forecast3.YU81824-025` <- forecast(`fit3.YU81824-025`, h = 12)
# `forecast4.YU81824-025` <- forecast(`fit4.YU81824-025`, h = 12)
# `forecast5.YU81824-025` <- forecast(`fit5.YU81824-025`, h = 12)
# `forecast6.YU81824-025` <- forecast(`fit6.YU81824-025`, h = 12)
`forecast7.YU81824-025` <- forecast(`fit7.YU81824-025`, h = 12)


# Compare models ---------------------------------------------------------------

`acc1.YU81824-025` <- accuracy(`forecast1.YU81824-025`, `test.YU81824-025`)
# `acc2.YU81824-025` <- accuracy(`forecast2.YU81824-025`, `test.YU81824-025`)
`acc3.YU81824-025` <- accuracy(`forecast3.YU81824-025`, `test.YU81824-025`)
# `acc4.YU81824-025` <- accuracy(`forecast4.YU81824-025`, `test.YU81824-025`)
# `acc5.YU81824-025` <- accuracy(`forecast5.YU81824-025`, `test.YU81824-025`)
# `acc6.YU81824-025` <- accuracy(`forecast6.YU81824-025`, `test.YU81824-025`)
`acc7.YU81824-025` <- accuracy(`forecast7.YU81824-025`, `test.YU81824-025`)

# Also check the summary() for the fitted models for AIC etc 


# Calculate accuracy and service level for the most promising ------------------

a1 <- `demand.YU81824-025`[23:34,2]

## The accuracy is 1 - [sum(abs(demand - forecast))/sum(demand)]

acc1.1 <- 1 - abs(sum(a1 - `forecast1.YU81824-025`$mean))/sum(a1)
acc1.3 <- 1 - abs(sum(a1 - `forecast3.YU81824-025`$mean))/sum(a1)
acc1.7 <- 1 - abs(sum(a1 - `forecast7.YU81824-025`$mean))/sum(a1)

## For the service level i suppose 0 safety stock to see how the model performs.
## It is 1 - [(the demand I could NOT cover)/sum(demand)] - I add the positive 
## numbers from a1 - `forecast1.YU81824-025`$mean.

sl1.1 <- 1 - (32+130+60+119+53+136)/sum(a1)
sl1.3 <- 1 - (66+60+53+8+123)/sum(a1)
sl1.7 <- 1 - (82+71+43+124)/sum(a1)

## Accuracy is a good indicator, but I need a high SL. SL comes first.


# Select the best model --------------------------------------------------------
`fit.YU81824-025` <- `fit3.YU81824-025`
`fcast.YU81824-025` <- `forecast3.YU81824-025`


# Fit model to all data --------------------------------------------------------

`fitall.YU81824-025` <- ets(`ts.demand.YU81824-025`, model = "AAM", 
                            restrict = FALSE, alpha = 0.1, beta = 0.05, 
                            gamma = 0.1)
`fcastall.YU81824-025` <- forecast(`fitall.YU81824-025`, h = 12)
plot(`fcastall.YU81824-025`, xlab = "Χρόνος", ylab = "Πωλήσεις", 
     main = "YU81824-025")



# Calculate CV -----------------------------------------------------------------

`cv.YU81824-025` <- sqrt(`fitall.YU81824-025`$sigma2) / 
  mean(`fitall.YU81824-025`$fitted)


# Inventory control model ------------------------------------------------------

`fcastcomp.YU81824-025` <- forecast(ets(`train.YU81824-025`, 
                                        model = `fitall.YU81824-025`, 
                                        restrict = FALSE, 
                                        use.initial.values = TRUE), h = 14)

y1 <- ceiling(`fcastcomp.YU81824-025`$mean)
sigma1 <- sqrt(`fitall.YU81824-025`$sigma2)
z <- qnorm(.97)

s1 <- ceiling(sigma1 * z + y1)

# Add zeros to make ts the same length ---
s1 <- ts(c(0, 0, s1), start = c(2014, 4), frequency = 12)

ip1 <- ts(c(257, 534, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), start = c(2014, 4), 
          frequency = 12)

order1 <- ts(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), start = c(2014, 4), 
             frequency = 12)

sales1 <- window(`ts.demand.YU81824-025`, start = c(2014, 4))


for (i in 1:14) {
  if (i > 2) {
    ip1[i] <- ip1[i-1] + order1[i-2] - sales1[i]
    if (ip1[i] < 0) ip1[i] <- 0
  }
  order1[i] <- s1[i+2] - ip1[i]
  if (order1[i] < 0) order1[i] <- 0
}

# Lost sales %
abs(93+0-196)/sum(window(sales1, start = c(2014, 6)))

# Total cost
11*3.3 + sum(window(ip1, start = c(2014, 6)))*0.06*76.5 + 
  sum(window(order1, start = c(2014, 6)))*76.5



# AGROTECH's policy ---

`fagro.YU81824-025` <- ts(c(0, 0, ceiling(1.3 * window(`ts.demand.YU81824-025`, 
                                           start = c(2013, 6), 
                                           end = c(2014, 7)))), 
                    start = c(2014, 4), frequency = 12)


ip1agro <- ts(c(257, 534, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
              start = c(2014, 4), frequency = 12)

order1agro <- ts(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                 start = c(2014, 4), frequency = 12)


for (i in 1:14) {
  if (i > 2) {
    ip1agro[i] <- ip1agro[i-1] + order1agro[i-2] - sales1[i]
    if (ip1agro[i] < 0) ip1agro[i] <- 0
  }
  order1agro[i] <- `fagro.YU81824-025`[i+2] - ip1agro[i]
  if (order1agro[i] < 0) order1agro[i] <- 0
}

# Lost sales % 
abs(11+0-196 + 0+408-439 + 0+540-551 + 32+87-332) / 
  sum(window(sales1, start = c(2014, 6)))

# Total cost
11*3.3 + sum(window(ip1agro, start = c(2014, 6)))*0.06*76.5 + 
  sum(window(order1agro, start = c(2014, 6)))*76.5


