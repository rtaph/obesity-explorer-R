# Read-in the obesity data and save
datapath <- here::here("data-raw", "processed", "obesity-combo.csv")
ob <- readr::read_csv(datapath) %>%
  dplyr::filter(region != "Aggregates")
usethis::use_data(ob, overwrite = TRUE)

# Read-in country dictionary and save
cypath <- here::here("data-raw", "processed", "country-ids.csv")
cydict <- readr::read_csv(cypath)
usethis::use_data(ob, overwrite = TRUE)
