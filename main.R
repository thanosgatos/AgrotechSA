# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)


# Load data --------------------------------------------------------------------

skus.initial <- read.csv("AgrotechSKUs.csv")
skus.initial <- transform(skus.initial, CriticalCode = factor(CriticalCode))


# Transform data ---------------------------------------------------------------

skus <- tbl_df(skus.initial) %>%
  select(SKU, InventoryClassCode, CriticalCode, CommodityCode, DNPTimiAgoras, 
         TimiLianikis, PosotitaPoliseon) %>%
  rename(sku = SKU, inventory.class = InventoryClassCode, 
         critical.code = CriticalCode, commodity.code = CommodityCode, 
         timi.agoras = DNPTimiAgoras, timi.polisis = TimiLianikis, 
         zitisi = PosotitaPoliseon) %>%
  mutate(kerdos = timi.polisis - timi.agoras, tziros = zitisi * timi.polisis)
