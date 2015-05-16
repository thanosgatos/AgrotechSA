ab.by.commodity %>%
  summarize(n = n()) %>%
  arrange(desc(n))

comm.447 <- class.ab %>%
  filter(commodity.code == "447") %>%
  arrange(desc(tziros))

class.first <- skus %>%
  group_by(inventory.class) %>%
  arrange(desc(tziros), desc(kerdos)) %>%
  slice(1)



skus2 <- skus %>%
  select(sku, inventory.class, timi.polisis, zitisi, tziros) %>%
  arrange(desc(tziros)) %>%
  mutate(perc.tziros = tziros / sum(tziros), 
         cum.perc.tziros = cumsum(perc.tziros))

skus3 <- filter(skus2, cum.perc.tziros < 0.8)

q80 <- quantile(skus$tziros, probs = 0.8)
class.alpha <- filter(skus, tziros > q80)


