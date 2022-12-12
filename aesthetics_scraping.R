#load libs

library(dplyr)
library(stringr)
library(tidyr)
library(rvest)

# url of aesthetics wiki list of aesthetics page
url <- 'https://aesthetics.fandom.com/wiki/List_of_Aesthetics'

# set url connection
url <-  url(url, "rb")

# extract all tables on page
url_html <- url |>
  read_html() |>
  html_table()

# close connection
close(url)

# set up blank vector
vc <- c()

# save all table columns to vc
for (i in (1:length(url_html))) {
  
  vc_it <- c(as.character(unlist(as.data.frame(url_html[[i]]), use.names=FALSE)))
  
  vc <- c(vc, vc_it)
  
}

# convert to data frame
aes_df <- data.frame(aesthetic = c(vc))

# split entries into individual rows
aes_df <- aes_df |>
  mutate(aesthetic = str_split(vc, "\n")) |>
  unnest(cols = c(aesthetic))

# function to look up search results on depop
depop_results <- function(aurl) {
  
  url_it <- paste0("https://www.depop.com/search/?q=","%27",gsub(" ", "%20", aurl),"%27aesthetic")
  
  res <- url_it |>
    read_html() |>
    html_element(".Container-sc-__sc-1t5af73-0.Searchstyles__StyledContainer-sc-__sc-1ep2n60-0.ujpvx.PrZuM") |>
    html_node("b") |>
    html_text()
  
  res <- as.numeric(res)
  
  return(res)
  
}

# set up blank vector
res <- data.frame()

# collect search results
for (i in 1:10) {
  
  res_it <- data.frame(aesthetic = c(aes_df$aesthetic[i]),
                       depop_results = c(depop_results(aes_df$aesthetic[i])))
  
  res <- rbind(res, res_it)
  
  Sys.sleep(0.4)
  
}

# join results to df
aes_df <- aes_df |>
  left_join(res, by = "aesthetic")
# add date column
aes_df$date <- as.character(Sys.Date())

# save as csv
write.csv(aes_df,paste0('data/',Sys.Date(),'_aesthetics','.csv'))
