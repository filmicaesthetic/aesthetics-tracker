
library(dplyr)
library(ggplot2)

files <- list.files("data")
files <- files[grepl("aesthetics", files) == TRUE]

df <- data.frame()

for (i in length(files)) {
  
  df_it <- read.csv(paste0("data/",files[i]))
  
  df <- rbind(df, df_it)
  
}

df |>
  filter(aesthetic != "") |>
  arrange(-depop_results) |>
  head(25) |>
  ggplot(aes(x = reorder(aesthetic, depop_results, sum), y = depop_results)) +
  geom_col(color = "#1d1d1d") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")
