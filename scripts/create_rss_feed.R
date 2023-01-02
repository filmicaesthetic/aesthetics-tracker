## create feed of interesting insights

library(dplyr)

today <- read.csv(paste0("data/",Sys.Date(),"_aesthetics.csv")) |>
  mutate(depop_results = ifelse(is.na(depop_results), 0, depop_results)) |>
  select(aesthetic, cw_results = depop_results) |>
  mutate(rank_cw = rank(-cw_results))
lw <- read.csv(paste0("data/",Sys.Date() - 7,"_aesthetics.csv")) |>
  mutate(depop_results = ifelse(is.na(depop_results), 0, depop_results)) |>
  select(aesthetic, lw_results = depop_results) |>
  mutate(rank_lw = rank(-lw_results))

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
  select(aesthetic, description)

## biggest losses

big_loss <- lw_comp |>
  arrange(diff_perc) |>
  filter(cw_results > 50, diff_perc < -0.05) |>
  rowwise() |>
  mutate(description = ifelse(diff_perc < -0.5, paste0("Huge drop in results for ",aesthetic," aesthetic in the last 7 days", ifelse(tbl_mvmt < 0, paste0(", moving down ", floor(abs(tbl_mvmt)), " places in the aesthetics popularity table."), ".")),
                              paste0("Results for ",aesthetic," aesthetic decreased by ",scales::percent(abs(diff_perc), 1), " in the last 7 days", ifelse(tbl_mvmt > 0, paste0(", moving down ", floor(abs(tbl_mvmt)), " places in the aesthetics popularity table."), ".")))) |>
  mutate(pubDate = Sys.Date()) |>
  mutate(type = "biggest_loss") |>
  select(aesthetic, description)



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

# from_zero <- lw_comp$aesthetic[lw_comp$diff > 0 & lw_comp$lw_results == 0]
# from_zero_last <- from_zero[length(from_zero)]
# from_zero <- from_zero[1:(length(from_zero)-1)]
# 
# from_zero_text <- paste0(paste(from_zero, sep = ", "), " & ", from_zero_last, " aesthetics have ")