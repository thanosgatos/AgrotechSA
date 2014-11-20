# Reading data and forming the needed table

skusInitial <- read.csv("AgrotechSKUs.csv", row.names = 1)
skus <- skusInitial[, c(2, 6, 10, 11)]
skus <- transform(skus, CriticalCode = factor(CriticalCode))

critCode <- skus$CriticalCode
invClass <- skus$InventoryClassCode
timi <- skus$TimiLianikis
zitisi <- skus$PosotitaPoliseon

tziros <- timi*zitisi

skus <- cbind(skus, tziros)


