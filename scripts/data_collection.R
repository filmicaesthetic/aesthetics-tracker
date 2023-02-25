# load libs

library(dplyr)
library(stringr)
library(tidyr)
library(rvest)
library(purrr)

source("scripts/web_scraping_functions.R")

# scrape aesthetics data from aesthetics wiki
aes_raw <- get_aesthetics_list()

write.csv(aes_raw, paste0("data/aeslist/",Sys.Date(),"_aestheticslist.csv"), row.names = FALSE)

# get depop results for each aesthetic
res <- get_depop_results(aes_raw)

# join results to df
aes_df <- aes_raw |>
  left_join(res, by = "aesthetic")

# add date column
aes_df$date <- as.character(Sys.Date())

# save as csv
write.csv(aes_df |> select(-aes_link),paste0('data/',Sys.Date(),'_aesthetics','.csv'))

# get top 100
aes_100 <- aes_df |>
  arrange(-depop_results) |>
  head(100) |>
  select(-depop_results)

# get etsy results
etsy <- get_etsy_results(aes_100)

# add date
etsy_df <- etsy |>
  mutate(date = as.character(Sys.Date()))

# save to csv
write.csv(etsy_df, paste0("data/",Sys.Date(),"_etsyresults.csv"))
