# Read and subset the data

skusInitial <- read.csv("AgrotechSKUs.csv", row.names = 1)
skus <- skusInitial[, c(2, 6, 10, 11)]
skus <- transform(skus, CriticalCode = factor(CriticalCode))


# Define easier-to-use variables

critCode <- skus$CriticalCode
invClass <- skus$InventoryClassCode
timi <- skus$TimiLianikis
zitisi <- skus$PosotitaPoliseon

tziros <- timi*zitisi

skus <- cbind(skus, tziros)

tziros <- skus$tziros


# Sort the data frame


