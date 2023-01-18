#load libs

library(dplyr)
library(stringr)
library(tidyr)
library(rvest)
library(DBI)
library(duckdb)
library(purrr)

source("scripts/web_scraping_functions.R")
source("scripts/database_functions.R")

# scrape aesthetics data from aesthetics wiki
aes_raw <- get_aesthetics_list()

# update aesthetics table

# connect to duckdb
con <- dbConnect(duckdb(), 
                 dbdir="aesthetics_tracker.duckdb", 
                 read_only=FALSE)

# insert any new aesthetics into database
add_new_aesthetics(aes_raw)

# get depop results for each aesthetic
res <- get_depop_results(aes_raw)

# join results to df
aes_df <- aes_raw |>
  left_join(res, by = "aesthetic")

# add date column
aes_df$date <- as.character(Sys.Date())

# save as csv
write.csv(aes_df |> select(-aes_link),paste0('data/',Sys.Date(),'_aesthetics','.csv'))

# add to db

# get aes_id
aes_ids <- dbGetQuery(con, "SELECT aesthetic, aes_id FROM aesthetics")

# format df to match aes_depopresults table
aes_db <- aes_df |>
  left_join(aes_ids, by = "aesthetic") |>
  select(aes_id, depop_results, date)

# remove any other entries from this date to avoid duplicates if process has already been run
dbSendStatement(con, "DELETE FROM aes_depopresults WHERE date = CURRENT_DATE")

# append new data to table
dbAppendTable(con, "aes_depopresults", aes_db)

# disconnect from db
dbDisconnect(con, shutdown=TRUE)

