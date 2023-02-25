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
aes_raw <- read.csv(paste0("data/aeslist/",Sys.Date(),"_aestheticslist.csv"))

# update aesthetics table

# connect to duckdb - reboot if duckdb package updated
tryCatch(con <- dbConnect(duckdb(), 
                          dbdir="aesthetics_tracker.duckdb", 
                          read_only=FALSE),
         error = function(e){
           
           tryCatch(file.remove("aesthetics_tracker.duckdb"))
           
           source("scripts/duckdb_setup.R")
           
           con <- dbConnect(duckdb(), 
                            dbdir="aesthetics_tracker.duckdb", 
                            read_only=FALSE)
           
         })


# insert any new aesthetics into database
add_new_aesthetics(aes_raw)

# read depop results csv
aes_df <- read.csv(paste0('data/',Sys.Date(),'_aesthetics','.csv')) |> select(-X)

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

