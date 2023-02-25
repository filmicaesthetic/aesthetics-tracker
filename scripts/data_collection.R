# load libs

library(dplyr)
library(stringr)
library(tidyr)
library(rvest)
library(purrr)

source("scripts/web_scraping_functions.R")

# scrape aesthetics data from aesthetics wiki
aes_raw <- get_aesthetics_list()

write.csv(paste0("data/aeslist/",Sys.Date(),"_aestheticslist.csv"), row.names = FALSE)

# get depop results for each aesthetic
res <- get_depop_results(aes_raw)

# join results to df
aes_df <- aes_raw |>
  left_join(res, by = "aesthetic")

# add date column
aes_df$date <- as.character(Sys.Date())

# save as csv
write.csv(aes_df |> select(-aes_link),paste0('data/',Sys.Date(),'_aesthetics','.csv'))
