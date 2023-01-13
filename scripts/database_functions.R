## database functions

# insert new aesthetics to aesthetics table 
add_new_aesthetics <- function(aes_df) {
  
  # get current list of aesthetics from database
  aes_db <- dbGetQuery(con, "SELECT aes_id, aesthetic FROM aesthetics")
  
  # get list of any aesthetics not in the database
  new_aes <- aes_df[!(aes_df$aesthetic %in% aes_db$aesthetic),]
  
  
  if (nrow(new_aes) > 0) {
    
    df <- data.frame(aesthetic = new_aes$aesthetic,
                     aes_link = new_aes$aes_link) |>
      mutate(aes_id = row_number() + max(aes_db$aes_id)) |>
      select(aes_id, aesthetic, aes_link) |>
      rowwise() |>
      mutate(main_img = possibly(get_main_img, otherwise = NA_real_)(aes_link),
             info_box = list(possibly(get_info_box, otherwise = NA_real_)(aes_link)))
    
    # create info box df
    info_box_raw <- data.frame()
    
    for (i in 1:nrow(df)) {
      
      if (is.na(df$info_box[i]) == FALSE) {
        
        if (nrow(df$info_box[i][[1]])>0) {
          
          df_it <- df$info_box[i][[1]]
          df_it$aesthetic <- df$aesthetic[i]
          
          info_box_raw <- rbind(info_box_test, df_it)
          
        }
        
      }
      
    }
    
    info_box_df <- info_box_raw |>
      left_join(aesthetics_list, "aesthetic") |>
      select(aes_id, headers, values)
    
    if (nrow(info_box_df) > 0) {
      
      # append info box
      dbAppendTable(con, "aes_info_box", info_box_df)
      
    }
    
    for (i in 1:nrow(df)) {
      
      dbSendStatement(con, paste0("INSERT INTO aesthetics (aes_id, aesthetic, aes_link) VALUES ('",df$aes_id[i],"', '",df$aesthetic[i],"', '",df$aes_link[i],"');"))
      dbSendStatement(con, paste0("INSERT INTO aes_wiki_img (aes_id, aesthetic, aes_link, main_img) VALUES ('",df$aes_id[i],"', '",df$aesthetic[i],"', '",df$aes_link[i],"', '",df$main_img[i],");"))
      
    }
    
  }
  
}

# function to return negative keyword string to avoid including other aesthetics
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

# get search results value function - handling errors
get_res <- function(url) {
  
  tryCatch( { res <- url |>
    read_html() |>
    html_element(".Container-sc-__sc-1t5af73-0.Searchstyles__StyledContainer-sc-__sc-1ep2n60-0.ujpvx.PrZuM") |>
    html_node("b") |>
    html_text() }
    , error = function(e) {res <<- NA})
  
  return(res)
  
}

# function to look up search results on depop
depop_results <- function(aurl) {
  
  # replace & with %26 to search correctly
  aurl <- gsub("&", "%26", aurl)
  # get negative keyword string
  neg_key <- minus_words(aurl)
  # create url string
  url_it <- paste0("https://www.depop.com/search/?q=","%27",gsub(" ", "%20", aurl),"%27%20aesthetic%20",neg_key)
  # get search results
  res <- get_res(url_it)
  # convert to numeric
  res <- as.numeric(res)
  
  return(res)
  
}