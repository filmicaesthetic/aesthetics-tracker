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

get_depop_results <- function(aes_df) {
  
  # set up blank vector
  res <- data.frame()
  
  # collect search results
  for (i in 1:nrow(aes_df)) {
    
    res_it <- data.frame(aesthetic = c(aes_df$aesthetic[i]),
                         depop_results = c(depop_results(aes_df$aesthetic[i])))
    
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
