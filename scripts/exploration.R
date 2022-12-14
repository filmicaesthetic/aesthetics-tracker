
pacman::p_load(dplyr, ggplot2, tidyr, showtext, formattable)

files <- list.files("data")
files <- files[grepl("aesthetics", files) == TRUE]

df <- data.frame()

for (i in 1:length(files)) {
  
  df_it <- read.csv(paste0("data/",files[i]))
  
  df <- rbind(df, df_it)
  
}

df_fl <- df |>
  select(-X) |>
  replace_na(replace = list(depop_results = 0)) |>
  filter(date %in% c(min(date), max(date))) |>
  pivot_wider(id_cols = c(aesthetic), names_from = "date", values_from = "depop_results")

colnames(df_fl) <- c("aesthetic", "min_date", "max_date")

df_summ <- df_fl |>
  mutate(diff = max_date - min_date,
         diff_perc = diff / min_date) |>
  filter(aesthetic != "") |>
  arrange(-max_date) |>
  mutate(col_group = ifelse(diff_perc >= 0.03, "pos", ifelse(diff_perc > -0.03, "mid", "neg"))) |>
  mutate(rank = floor(rank(-max_date)))

library(plotly)

pal <- c("pos" = customGreen,
         "mid" = "#ffffff",
         "neg" = customRed)

shape_pal <- c("pos" = 2,
               "mid" = 1,
               "neg" = 6)

df_summ |>
  head(50) |>
  ggplot(aes(y = reorder(aesthetic, max_date, sum))) +
  geom_col(aes(x = 1), fill = "#f0e9e9", width = 0.8) +
  geom_segment(aes(x = 0.7, xend = 0.9, yend = aesthetic, color = col_group), size = 3.6) +
  geom_point(aes(x = 0.72, shape = col_group), color = "#1d1d1d", stroke = 1, size =2) +
  geom_text(aes(x = 0.1, label = toupper(aesthetic)), family = "Poppins", size = 3.6, hjust = 0, vjust = 0.5) +
  geom_text(aes(x = 0.05, label = rank), size = 2.5) +
  geom_text(aes(x = 0.8, label = scales::percent(diff_perc, 0.1)), hjust = 0.5, size = 3) +
  scale_shape_manual(values = shape_pal) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  labs(title = "Top Aesthetics by Depop search results") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())


df_summ |>
  head(25) |>
  ggplot(aes(x = reorder(aesthetic, max_date, sum), y = max_date)) +
  geom_col(color = "#1d1d1d") +
  labs("Top Aesthetics by Depop Search Results",
       x = "Aesthetic",
       y = "Depop Search Results") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")





df_gt <- df_summ |>
  select(-diff)

formattable(df_gt)

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

Test <- formatter("span",
                  style = x ~ style(display = "block", font.weight = "bold",color = "black","border-radius" = "4px",
                                    "padding-right" = "4px",
                                    "background-color" = ifelse(x <= -0.1, "#d15656", ifelse(x > -0.1 & x <= -0.02, "#ff7f7f", ifelse(x > -0.02 & x <= 0.02, "white", ifelse(x > 0.02 & x <= 0.1, "#DeF7E9",ifelse(x > 10, "#71CA97",NA)))))),
                  x ~ percent(x))

formattable(df_gt, align =c("l","c","c","c"), list(
  `Aesthetic` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `diff_perc`= Test
))

diff_df <- df |>
  filter(aesthetic != "") |>
  group_by(aesthetic) |>
  mutate(diff = last(depop_results) - first(depop_results),
         diff_perc = diff / first(depop_results)) |>
  filter(depop_results > 100) |>
  arrange(-abs(diff_perc))

diff_df
