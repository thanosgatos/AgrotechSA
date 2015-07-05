?class.first <- skus %>%
  group_by(inventory.class) %>%
  # arrange(desc(tziros), desc(kerdos)) %>%
  slice(1)

class.first.zitisi <- skus %>% 
  group_by(inventory.class) %>% 
  arrange(desc(zitisi)) %>% 
  slice(1)

skus %>% 
  group_by(inventory.class) %>% 
  summarise(n = n(), pososto.tzirou = sum(perc.tziros))

type.a %>% 
  group_by(inventory.class) %>% 
  summarise(n = n(), pososto.tzirou = sum(perc.tziros))

type.b %>% 
  group_by(inventory.class) %>% 
  summarise(n = n(), pososto.tzirou = sum(perc.tziros))

type.c %>% 
  group_by(inventory.class) %>% 
  summarise(n = n(), pososto.tzirou = sum(perc.tziros))

type.a %>% 
  group_by(inventory.class) %>% 
  summarise(n = n(), synolo.tzirou = sum(tziros), 
            pososto.tzirou = sum(perc.tziros)) %>% 
  arrange(desc(synolo.tzirou))

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
           sku == "RE509208") %>% 
  mutate(perc.tziros = round(perc.tziros, digits = 4))

for (i in seq_along(skus.selected$sku)) {
  assign(paste("data", skus.selected$sku[i], sep = "."), 
         read.xlsx("SelectedData.xlsx", sheetName = skus.selected$sku[i]))
}





