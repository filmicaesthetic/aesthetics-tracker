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
          
          info_box_raw <- rbind(info_box_raw, df_it)
          
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