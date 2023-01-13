## create feed of interesting insights

library(dplyr)
library(DBI)
library(duckdb)

# create duckdb
con <- dbConnect(duckdb(),
                 dbdir="aesthetics_tracker.duckdb",
                 read_only=FALSE)

# get today's data
today <- dbGetQuery(con, "SELECT aesthetics.aesthetic, 
                    COALESCE(depop_results, 0) as cw_results,
                    ROW_NUMBER() OVER (ORDER BY cw_results DESC) AS rank_cw,
                    aesthetics.aes_link,
                    aes_wiki_img.main_img
                    FROM aes_depopresults
                    LEFT JOIN aesthetics ON aes_depopresults.aes_id=aesthetics.aes_id
                    LEFT JOIN aes_wiki_img ON aes_depopresults.aes_id=aes_wiki_img.aes_id
                    WHERE date = current_date")

# get last week's data
lw <- dbGetQuery(con, "SELECT aesthetic, 
                    COALESCE(depop_results, 0) as lw_results,
                    ROW_NUMBER() OVER (ORDER BY lw_results DESC) AS rank_lw,
                    FROM aes_depopresults
                    LEFT JOIN aesthetics ON aes_depopresults.aes_id=aesthetics.aes_id
                    WHERE date = CURRENT_DATE - INTERVAL 7 DAY")

# combine and add metrics
lw_comp <- today |>
  left_join(lw, by = "aesthetic") |>
  mutate(diff = cw_results - lw_results) |>
  mutate(diff_perc = diff / lw_results) |>
  mutate(tbl_mvmt = rank_lw - rank_cw) |> 
  arrange(-diff_perc)

## biggest gains

big_gains <- lw_comp |>
  filter(cw_results > 50, diff_perc > 0.2) |>
  rowwise() |>
  mutate(description = ifelse(diff_perc > 10, paste0("Huge spike in results for ",aesthetic," aesthetic in the last 7 days", ifelse(tbl_mvmt > 0, paste0(", moving up ", floor(tbl_mvmt), " places in the aesthetics popularity table."), ".")),
                              paste0("Results for ",aesthetic," aesthetic increased by ",scales::percent(diff_perc, 1), " in the last 7 days", ifelse(tbl_mvmt > 0, paste0(", moving up ", floor(tbl_mvmt), " places in the aesthetics popularity table."), ".")))) |>
  mutate(pubDate = Sys.Date()) |>
  mutate(type = "biggest_gains") |>
  select(aesthetic, type, description, img=main_img, url=aes_link)

## biggest losses

big_loss <- lw_comp |>
  arrange(diff_perc) |>
  filter(cw_results > 50, diff_perc < -0.1) |>
  rowwise() |>
  mutate(description = ifelse(diff_perc < -0.5, paste0("Huge drop in results for ",aesthetic," aesthetic in the last 7 days", ifelse(tbl_mvmt < 0, paste0(", moving down ", floor(abs(tbl_mvmt)), " places in the aesthetics popularity table."), ".")),
                              paste0("Results for ",aesthetic," aesthetic decreased by ",scales::percent(abs(diff_perc), 1), " in the last 7 days", ifelse(tbl_mvmt > 0, paste0(", moving down ", floor(abs(tbl_mvmt)), " places in the aesthetics popularity table."), ".")))) |>
  mutate(pubDate = Sys.Date()) |>
  mutate(type = "biggest_loss") |>
  select(aesthetic, type, description, img=main_img, url=aes_link)


text_insights <- rbind(big_gains, big_loss) 

text_insights$date <- as.character(Sys.Date())

# add to db
dbAppendTable(con, "aes_textinsights", text_insights)

# disconnect from db
dbDisconnect(con, shutdown=TRUE)

## movement in top 20
# list_overtaken <- function(df, start_rank, end_rank) {
#   
#   aests <- df$aesthetic[df$rank_lw > start_rank & df$rank_lw <= end_rank]
#   print(aests)
#   if (length(aests) == 1) {
#     res <- aests
#   } else if (length(aests) == 2) {
#     res <- paste(aests, sep = " & ")
#   } else {
#     last <- aests[length(aests)]
#     aests_fil <- aests[1:(length(aests)-1)]
#     
#     res <- paste0(paste(aests_fil, sep = ", "), " & ", last)
#   }
#   
#   return(res)
# }
# 
# lw_comp |>
#   filter(rank_cw <= 20 | rank_lw <= 20) |>
#   arrange(rank_cw) |>
#   filter(rank_lw != rank_cw) |>
#   rowwise() |>
#   mutate(description = ifelse(tbl_mvmt > 0, paste0(aesthetic, "has moved up", 
#                                                     ifelse(tbl_mvmt == 1, "1 place", paste0(tbl_mvmt, "places")), 
#                                                     "in the aesthetic popularity table, overtaking", list_overtaken(lw_comp, as.numeric(rank_lw), as.numeric(rank_cw))),""))

## aesthetics going from 0 to some results
# need to make a cumulative total for this to say they've had their "first" results.

# from_zero <- dbGetQuery(con, "SELECT aesthetic, 
#                     COALESCE(depop_results, 0) as cw_results,
#                     ROW_NUMBER() OVER (ORDER BY cw_results DESC) AS rank_cw,
#                     SUM (CASE WHEN date = CURRENT_DATE THEN depop_results ELSE 0 END) = SUM(cw_results) AS first_check
#                     FROM aes_depopresults
#                     LEFT JOIN aesthetics ON aes_depopresults.aes_id=aesthetics.aes_id
#                     WHERE first_check = TRUE")
# 
# from_zero <- lw_comp$aesthetic[lw_comp$diff > 0 & lw_comp$lw_results == 0]
# from_zero_last <- from_zero[length(from_zero)]
# from_zero <- from_zero[1:(length(from_zero)-1)]
# 
# from_zero_text <- paste0(paste(from_zero, sep = ", "), " & ", from_zero_last, " aesthetics have ")