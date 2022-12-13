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
  unnest(cols = c(aesthetic)) |>
  filter(aesthetic != "")


# function to return negative keyword sring to avoid including other aesthetics
minus_words <- function(x) {
  
  # split input into individual words
  words <- str_split(x, " ")[[1]]
  # set up blank list
  aes_sel <- c()
  # for each word, find matches in aesthetics list and add a negative keyword for each additional word used
  for (i in 1:length(words)) {
    
    aes_sel_it <- aes_df$aesthetic[grepl(tolower(words[i]), tolower(aes_df$aesthetic))]
    
    aes_sel_it <- aes_sel_it[aes_sel_it != x]
    
    aes_sel_it <- do.call(paste, c(as.list(aes_sel_it), sep = " "))
    
    if (length(aes_sel_it > 0)) {
      
      aes_sel_split <- str_split(aes_sel_it, " ")[[1]]
      
    } else {
      aes_sel_split <- NULL
    }
    
    aes_sel <- c(aes_sel, aes_sel_split)
    
  }
  # remove input words and duplicates
  aes_sel <- aes_sel[!(aes_sel %in% words)]
  aes_sel <- unique(aes_sel)
  # paste into string for search
  list_x <- do.call(paste, c(as.list(aes_sel), sep = "%20-"))
  # add - to start
  list_fin <- paste0("-", list_x)
  
  return(list_fin)
  
}

# function to look up search results on depop
depop_results <- function(aurl) {
  
  # get negative keyword string
  neg_key <- minus_words(aurl)
  # create url string
  url_it <- paste0("https://www.depop.com/search/?q=","%27",gsub(" ", "%20", aurl),"%27%20aesthetic%20",neg_key)
  # get search results value
  res <- url_it |>
    read_html() |>
    html_element(".Container-sc-__sc-1t5af73-0.Searchstyles__StyledContainer-sc-__sc-1ep2n60-0.ujpvx.PrZuM") |>
    html_node("b") |>
    html_text()
  # convert to numeric
  res <- as.numeric(res)
  
  return(res)
  
}

# set up blank vector
res <- data.frame()

# collect search results
for (i in 1:nrow(aes_df)) {
  
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
