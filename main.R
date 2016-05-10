# Load packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(xlsx)
library(tidyr)
library(lubridate)
library(ggfortify)
library(forecast)


# Load data --------------------------------------------------------------------
## All the files to be read are expected to be in the working directory.

skus.initial <- read.csv("AgrotechSKUs.csv", strip.white = TRUE)
skus.initial <- transform(skus.initial, SKU = as.character(SKU), 
                          Description = as.character(Description), 
                          CriticalCode = factor(CriticalCode), 
                          CriticalCodeDescription = 
                            as.character(CriticalCodeDescription), 
                          CommodityCodeDescription = 
                            as.character(CommodityCodeDescription), 
                          InventoryClassCodeDescription = 
                            as.character(InventoryClassCodeDescription))


# Transform data ---------------------------------------------------------------

skus <- tbl_df(skus.initial) %>% 
	select(SKU, InventoryClassCode, CriticalCode, CommodityCode, 
	       DNPTimiAgoras, TimiLianikis, PosotitaPoliseon) %>% 
	rename(sku = SKU, inventory.class = InventoryClassCode, 
	       critical.code = CriticalCode, commodity.code = CommodityCode, 
	       timi.agoras = DNPTimiAgoras, timi.polisis = TimiLianikis, 
	       zitisi = PosotitaPoliseon) %>% 
	mutate(kerdos = timi.polisis - timi.agoras, 
	       tziros = zitisi * timi.polisis) %>% 
	arrange(desc(tziros)) %>% 
  mutate(cum.tziros = cumsum(tziros), perc.tziros = tziros / sum(tziros), 
         cum.perc.tziros = cum.tziros / sum(tziros))


# ABC analysis -----------------------------------------------------------------

type.a <- skus %>% 
  filter(cum.perc.tziros <= 0.8)

type.b <- skus %>% 
  filter(cum.perc.tziros > 0.8, cum.perc.tziros <= 0.95)

type.c <- skus %>% 
  filter(cum.perc.tziros > 0.95)


## ABC plot ---

num.sku <- vector(mode = "numeric", length = nrow(skus))

for (i in seq_along(skus$sku)) {
  num.sku[i] <- i
}

cum.num.sku <- num.sku/nrow(skus)

skus <- skus %>% 
  mutate(cum.num.sku = cum.num.sku)

abc.plot <- ggplot(skus, aes(x = skus$cum.num.sku*100, 
                             y = skus$cum.perc.tziros*100))

abc.plot + geom_line() + xlab("Αθροιστικό ποσοστό % κωδικών") + 
  ylab("Αθροιστικό ποσοστό % ετήσιας αξίας αποθεμάτων") + 
  annotate("rect", xmin = 0, xmax = 1293/22068*100, ymin = 0, ymax = 80, 
           alpha = .2) + 
  annotate("rect", xmin = 0, xmax = (1293+2391)/22068*100, ymin = 0, 
           ymax = 95, alpha = .2) + 
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, alpha = .2) + 
  annotate("text", x = (1293/22068)/2*100, y = 80/2, label = "Κωδικοί A (1,293)", 
           angle = 90) + 
  annotate("text", x = (1293/22068+(2391/22068)/2)*100, y = 95/2, 
           label = "Κωδικοί B (2,391)", angle = 90) + 
  annotate("text", x = (2391/22068+(1-(2391/22068))/2)*100, y = 50, 
           label = "Κωδικοί C (18,384)") + 
  scale_y_continuous(breaks = c(0, 80, 95, 100)) + 
  scale_x_continuous(breaks = round(c(0, 1293/22068*100, (1293+2391)/22068*100, 
                                      100), 2))


# Select SKUs ------------------------------------------------------------------

skus.suggested <- skus %>% 
  select(sku, inventory.class, critical.code, commodity.code, timi.agoras, 
         timi.polisis, zitisi, kerdos, tziros, perc.tziros) %>% 
  filter(sku == "RE509208" | sku == "RE58028" | sku == "AR70107" | 
           sku == "AL39473" | sku == "L171436" | sku == "Z13000" | 
           sku == "L113016" | sku == "AH209911" | sku == "N116690" | 
           sku == "CC39857" | sku == "T19044" | sku == "AL118572" | 
           sku == "RE55051" | sku == "14M7303" | sku == "AL174533" | 
           sku == "L113583" | sku == "AL78004" | sku == "N190332" | 
           sku == "H153329" | sku == "RE562705")

skus.selected <- skus %>% 
  select(sku, inventory.class, critical.code, commodity.code, timi.agoras, 
         timi.polisis, zitisi, kerdos, tziros, perc.tziros) %>% 
  filter(sku == "T19044" | sku == "L113016" | sku == "CC39857" | 
           sku == "L171436" | sku == "YU81824-025" | sku == "AR50041" | 
           sku == "RE546781" | sku == "N193500" | sku == "YU83070-200" | 
           sku == "RE509208")


# Load data for selected SKUs --------------------------------------------------

for (i in seq_along(skus.selected$sku)) {
  assign(paste("demand", skus.selected$sku[i], sep = "."), 
         read.xlsx("SelectedData.xlsx", sheetName = skus.selected$sku[i], 
                   colIndex = c(4, 5, 6)))
}


# Transform the data for selected SKUs -----------------------------------------
## If run more than once, the demand.* variables will break, because 
## operations run on the pasting and date parsing commands won't work as 
## expected.

## YU81824-025 ---

`demand.YU81824-025` <- unite(`demand.YU81824-025`, date, fiscalyear, 
                              fiscalmonth, sep = "-")
`demand.YU81824-025`$date <- paste(`demand.YU81824-025`$date, "1", sep = "-")
`demand.YU81824-025`$date <- ymd(`demand.YU81824-025`$date, 
                                 tz = "Europe/Athens")
`demand.YU81824-025` <- transform(`demand.YU81824-025`, 
                                  quantity = as.integer(quantity))


## AR50041 ---

demand.AR50041 <- unite(demand.AR50041, date, fiscalyear, fiscalmonth, 
                       sep = "-")
demand.AR50041$date <- paste(demand.AR50041$date, "1", sep = "-")
demand.AR50041$date <- ymd(demand.AR50041$date, tz = "Europe/Athens")
demand.AR50041 <- transform(demand.AR50041, quantity = as.integer(quantity))


## RE546781 ---

demand.RE546781 <- unite(demand.RE546781, date, fiscalyear, fiscalmonth, 
                       sep = "-")
demand.RE546781$date <- paste(demand.RE546781$date, "1", sep = "-")
demand.RE546781$date <- ymd(demand.RE546781$date, tz = "Europe/Athens")
demand.RE546781 <- transform(demand.RE546781, quantity = as.integer(quantity))


## T19044 ---

demand.T19044 <- unite(demand.T19044, date, fiscalyear, fiscalmonth, 
                       sep = "-")
demand.T19044$date <- paste(demand.T19044$date, "1", sep = "-")
demand.T19044$date <- ymd(demand.T19044$date, tz = "Europe/Athens")
demand.T19044 <- transform(demand.T19044, quantity = as.integer(quantity))


## N193500 ---

demand.N193500 <- unite(demand.N193500, date, fiscalyear, fiscalmonth, 
                       sep = "-")
demand.N193500$date <- paste(demand.N193500$date, "1", sep = "-")
demand.N193500$date <- ymd(demand.N193500$date, tz = "Europe/Athens")
demand.N193500 <- transform(demand.N193500, quantity = as.integer(quantity))


## YU83070-200 ---

`demand.YU83070-200` <- unite(`demand.YU83070-200`, date, fiscalyear, 
                              fiscalmonth, sep = "-")
`demand.YU83070-200`$date <- paste(`demand.YU83070-200`$date, "1", sep = "-")
`demand.YU83070-200`$date <- ymd(`demand.YU83070-200`$date, 
                                 tz = "Europe/Athens")
`demand.YU83070-200` <- transform(`demand.YU83070-200`, 
                                  quantity = as.integer(quantity))


## RE509208 ---

demand.RE509208 <- unite(demand.RE509208, date, fiscalyear, fiscalmonth, 
                       sep = "-")
demand.RE509208$date <- paste(demand.RE509208$date, "1", sep = "-")
demand.RE509208$date <- ymd(demand.RE509208$date, tz = "Europe/Athens")
demand.RE509208 <- transform(demand.RE509208, quantity = as.integer(quantity))


## L113016 ---

demand.L113016 <- unite(demand.L113016, date, fiscalyear, fiscalmonth, 
                       sep = "-")
demand.L113016$date <- paste(demand.L113016$date, "1", sep = "-")
demand.L113016$date <- ymd(demand.L113016$date, tz = "Europe/Athens")
demand.L113016 <- transform(demand.L113016, quantity = as.integer(quantity))


## CC39857 ---

demand.CC39857 <- unite(demand.CC39857, date, fiscalyear, fiscalmonth, 
                       sep = "-")
demand.CC39857$date <- paste(demand.CC39857$date, "1", sep = "-")
demand.CC39857$date <- ymd(demand.CC39857$date, tz = "Europe/Athens")
demand.CC39857 <- transform(demand.CC39857, quantity = as.integer(quantity))


## L171436 ---

demand.L171436 <- unite(demand.L171436, date, fiscalyear, fiscalmonth, 
                       sep = "-")
demand.L171436$date <- paste(demand.L171436$date, "1", sep = "-")
demand.L171436$date <- ymd(demand.L171436$date, tz = "Europe/Athens")
demand.L171436 <- transform(demand.L171436, quantity = as.integer(quantity))


# Create time series -----------------------------------------------------------

## YU81824-025 ---

`ts.demand.YU81824-025` <- ts(`demand.YU81824-025`$quantity, 
                              start = c(2012, 8), end = c(2015, 5), 
                              frequency = 12)

`plot.YU81824-025` <- autoplot(`ts.demand.YU81824-025`)
`plot.YU81824-025` + ggtitle("YU81824-025") + xlab("Χρόνος") + ylab("Πωλήσεις")


## AR50041 ---

ts.demand.AR50041 <- ts(demand.AR50041$quantity, start = c(2012, 5), 
                        end = c(2015, 5), frequency = 12)

plot.AR50041 <- autoplot(ts.demand.AR50041)
plot.AR50041 + ggtitle("AR50041") + xlab("Χρόνος") + ylab("Πωλήσεις")


## RE546781 ---

ts.demand.RE546781 <- ts(demand.RE546781$quantity, start = c(2012, 10), 
                         end = c(2015, 5), frequency = 12)

plot.RE546781 <- autoplot(ts.demand.RE546781)
plot.RE546781 + ggtitle("RE546781") + xlab("Time") + ylab("Sales")


## T19044 ---

ts.demand.T19044 <- ts(demand.T19044$quantity, start = c(2012, 5), 
                       end = c(2015, 5), frequency = 12)

plot.T19044 <- autoplot(ts.demand.T19044)
plot.T19044 + ggtitle("T19044") + xlab("Χρόνος") + ylab("Πωλήσεις")


## N193500 ---

ts.demand.N193500 <- ts(demand.N193500$quantity, start = c(2012, 8), 
                        end = c(2015, 5), frequency = 12)

plot.N193500 <- autoplot(ts.demand.N193500)
plot.N193500 + ggtitle("N193500") + xlab("Time") + ylab("Sales")


## YU83070-200 ---

`ts.demand.YU83070-200` <- ts(`demand.YU83070-200`$quantity, 
                              start = c(2012, 9), end = c(2015, 5), 
                              frequency = 12)

`plot.YU83070-200` <- autoplot(`ts.demand.YU83070-200`)
`plot.YU83070-200` + ggtitle("YU83070-200") + xlab("Time") + ylab("Sales")


## RE509208 ---

ts.demand.RE509208 <- ts(demand.RE509208$quantity, start = c(2012, 5), 
                         end = c(2015, 5), frequency = 12)

plot.RE509208 <- autoplot(ts.demand.RE509208)
plot.RE509208 + ggtitle("RE509208") + xlab("Time") + ylab("Sales")


## L113016 ---

ts.demand.L113016 <- ts(demand.L113016$quantity, start = c(2012, 8), 
                        end = c(2015, 5), frequency = 12)

plot.L113016 <- autoplot(ts.demand.L113016)
plot.L113016 + ggtitle("L113016") + xlab("Time") + ylab("Sales")


## CC39857 ---

ts.demand.CC39857 <- ts(demand.CC39857$quantity, start = c(2013, 4), 
                        end = c(2015, 5), frequency = 12)

plot.CC39857 <- autoplot(ts.demand.CC39857)
plot.CC39857 + ggtitle("CC39857") + xlab("Time") + ylab("Sales")


## L171436 ---

ts.demand.L171436 <- ts(demand.L171436$quantity, start = c(2012, 6), 
                        end = c(2015, 5), frequency = 12)

plot.L171436 <- autoplot(ts.demand.L171436)
plot.L171436 + ggtitle("L171436") + xlab("Χρόνος") + ylab("Πωλήσεις")


# Final SKU selection ----------------------------------------------------------

skus.final <- skus.selected %>% 
  filter(sku == "T19044" | sku == "L171436" | sku == "YU81824-025" | 
           sku == "AR50041")



