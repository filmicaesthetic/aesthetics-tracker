## web scraping functions

get_aesthetics_list <- function() {
  
  base_url <- "https://aesthetics.fandom.com/wiki/List_of_Aesthetics"
  
  # get aesthetics list from aesthetics wiki
  # extract all tables on page
  aes_urls <- base_url |>
    read_html() |>
    html_nodes("ul") |>
    html_nodes("li") |>
    html_nodes("a") |>
    html_attr("href")
  
  aes_names <- base_url |>
    read_html() |>
    html_nodes("ul") |>
    html_nodes("li") |>
    html_nodes("a") |>
    html_text2()
  
  raw_df <- data.frame(aesthetic = aes_names,
                       aes_link = aes_urls)
  
  start <- str_which(aes_names, "Single-Subject Aesthetics") + 1
  end <- str_which(aes_names, "Categories") - 1
  
  aes_df <- raw_df[start:end,]
  
  aes_df <- aes_df |>
    rowwise() |>
    mutate(aes_link = ifelse(substr(aes_link, 1, 4) == "http", aes_link, paste0("https://aesthetics.fandom.com", aes_link))) |>
    filter(aesthetic != "")
  
  return(aes_df)
  
}

# function to look up search results on depop
depop_results <- function(aurl, aes_df) {
  
  # replace & with %26 to search correctly
  aurl <- gsub("&", "%26", aurl)
  # get negative keyword string
  neg_key <- minus_words(aurl, aes_df)
  # create url string
  url_it <- paste0("https://www.depop.com/search/?q=","%27",gsub(" ", "%20", aurl),"%27%20aesthetic%20",neg_key)
  # get search results
  res <- get_res(url_it)
  # convert to numeric
  res <- as.numeric(res)
  
  return(res)
  
}

# function to return negative keyword string to avoid including other aesthetics
minus_words <- function(x, aes_df) {
  
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

get_depop_results <- function(aes_df) {
  
  # set up blank vector
  res <- data.frame()
  
  # collect search results
  for (i in 1:nrow(aes_df)) {
    
    res_it <- data.frame(aesthetic = c(aes_df$aesthetic[i]),
                         depop_results = c(depop_results(aes_df$aesthetic[i], aes_df)))
    
    res <- rbind(res, res_it)
    
    Sys.sleep(0.4)
    
  }
  
  return(res)
  
}

# function to extract main image from wiki page
get_main_img <- function(url) {
  
  # read html - if 404 then return NA
  img_html <- tryCatch(read_html(url),
                       error=function(cond) {
                         return(NA)
                       }) 
  
  # if NA, return NA
  if (is.na(img_html) == TRUE) {
    
    img <- NA
    # otherwise process image path  
  } else {
    
    img <- img_html |>
      html_nodes(".pi-image img") |>
      html_attr("src")
    
    img <- img[1]
    
    img <- substr(img, 1, str_locate(img, "\\/revision")[1]-1)
    
  }
  # return value
  return(img)
  
}

# function to extract main info box from wiki page
get_info_box <- function(url) {
  
  # read html - if 404 then return NA
  url_html <- tryCatch(read_html(url),
                       error=function(cond) {
                         return(NA)
                       }) 
  
  # if NA, return NA
  if (is.na(url_html) == TRUE) {
    
    df <- NA
    
    # otherwise process info box dataframe  
  } else {
    
    headers <- url_html |> 
      html_nodes(".pi-data") |>
      html_nodes("h3") |>
      html_text2()
    
    values <- url_html |> 
      html_nodes(".pi-data") |>
      html_nodes(".pi-data-value.pi-font") |>
      html_text2()
    
    df <- data.frame(headers = headers,
                     values = values)
    
  }
  
  return(df)
  
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
