# load packages
library(DBI)
library(duckdb)
library(dplyr)
library(purrr)
library(rvest)
library(stringr)
library(purrr)
library(tidyr)

source("scripts/web_scraping_functions.R")

## CREATE AESTHETIC LIST

# get aesthetic names and page urls
aesthetics_list <- get_aesthetics_list()

# add aesthetic IDs
aesthetics_list <- aesthetics_list |>
  ungroup() |>
  mutate(aes_id = row_number()) |>
  select(aes_id, aesthetic, aes_link)

# create duckdb
con <- dbConnect(duckdb(), 
                 dbdir="aesthetics_tracker.duckdb", 
                 read_only=FALSE)

# add aesthetics table to db
dbWriteTable(con, "aesthetics", aesthetics_list, overwrite = TRUE)


## DEPOP RESULTS TABLE

file_list <- paste0("data/", list.files("data")[grepl("_aesthetics.csv", list.files("data"))])

all_df <- readr::read_csv(file_list)

# add aes_id to raw data
all_df <- all_df |>
  left_join(aesthetics_list, by = "aesthetic")

# select relevant columns for db
all_df_db <- all_df |>
  select(aes_id, depop_results, date)

# add aes_depopresults table to db
dbWriteTable(con, "aes_depopresults", all_df_db, overwrite = TRUE)

## AESTHETICS WIKI DETAILS

# get images 

# get images from aesthetic pages
aes_wiki <- aesthetics_list |>
  rowwise() |>
  mutate(main_img = possibly(get_main_img, otherwise = NA_real_)(aes_link))

# add aesthetics table to db
dbWriteTable(con, "aes_wiki_img", aes_wiki, overwrite = TRUE)


## INFO BOX DATA

# get info box from aesthetic pages
aes_info <- aesthetics_list |>
  rowwise() |>
  mutate(info_box = list(possibly(get_info_box, otherwise = NA_real_)(aes_link)))

info_box_test <- data.frame()

for (i in 1:nrow(aes_info)) {
  
  if (is.na(aes_info$info_box[i]) == FALSE) {
    
    if (nrow(aes_info$info_box[i][[1]])>0) {
    
    df_it <- aes_info$info_box[i][[1]]
    df_it$aesthetic <- aes_info$aesthetic[i]
    
    info_box_test <- rbind(info_box_test, df_it)
    
    }
    
  }
  
}

info_box_raw <- info_box_test |>
  left_join(aesthetics_list, "aesthetic") |>
  select(aes_id, headers, values)

# add aesthetics table to db
dbWriteTable(con, "aes_info_box", info_box_raw, overwrite = TRUE)


## related aesthetics

rel_aes <- info_box_raw |>
  filter(headers == "Related aesthetics") |>
  select(aes_id, related_aesthetics = values)

related_aesthetics <- rel_aes |>
  mutate(related_aesthetics = gsub("\n\n\n", "\n", related_aesthetics)) |>
  mutate(related_aesthetics = gsub("\n", ", ", related_aesthetics)) |>
  mutate(related_aesthetics = str_split(related_aesthetics, ", ")) |>
  unnest(related_aesthetics)

# add aesthetics table to db
dbWriteTable(con, "aes_related_aesthetics", related_aesthetics, overwrite = TRUE)

ins_df <- data.frame(aesthetic = c("test"),
                     type = c("test"),
                     description = c("test"),
                     img = c("test"),
                     url = c("test"),
                     date = c("test"))

# create blank aes_textinsights table
dbCreateTable(con, "aes_textinsights", ins_df)
