# Read-in the obesity data and save
library(tidyverse)

datapath <- here::here("data-raw", "processed", "obesity-combo.csv")
ob <- readr::read_csv(datapath) %>%
  filter(region != "Aggregates") %>%
  arrange(country, sex, -year) %>%
  group_by(country, sex) %>%
  mutate(flag_smoke = if_else(is.na(smoke), "missing", "observed")) %>%
  fill(smoke, .direction = "updown") %>%
  ungroup()
usethis::use_data(ob, overwrite = TRUE)

# Read-in country dictionary and save
cypath <- here::here("data-raw", "processed", "country-ids.csv")
cydict <- readr::read_csv(cypath)
usethis::use_data(ob, overwrite = TRUE)
