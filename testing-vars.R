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

