
# function to look up search results on etsy
etsy_results <- function(aurl, aes_df) {
  
  # replace & with %26 to search correctly
  aurl <- gsub("&", "%26", aurl)
  # get negative keyword string
  neg_key <- minus_words(aurl, aes_df)
  # create url string
  url_it <- paste0("https://www.etsy.com/search/?q=","%27",gsub(" ", "%20", aurl),"%27%20aesthetic%20decor%20",neg_key)
  # get search results value
  res <- url_it |>
    read_html() |>
    html_element(".wt-text-right-xs.wt-text-left-md.wt-pr-md-0.wt-pr-xs-1") |>
    html_node("span") |>
    html_text()
  # convert to numeric
  res <- as.numeric(gsub("[^0-9]", "", res))
  
  return(res)
  
}

etsy_results("Cottagecore", aes_df)

# set up blank vector
etsy_res <- data.frame()

# collect search results
for (i in 1:nrow(aes_df)) {
  
  etsy_res_it <- data.frame(aesthetic = c(aes_df$aesthetic[i]),
                       etsy_results = c(etsy_results(aes_df$aesthetic[i])))
  
  etsy_res <- rbind(etsy_res, etsy_res_it)
  
  Sys.sleep(0.4)
  
}