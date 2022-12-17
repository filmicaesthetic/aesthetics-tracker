## get images for aesthetics

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
  html_nodes("table") |>
  html_nodes("tbody") |>
  html_nodes("tr") |>
  html_nodes("td") |>
  html_nodes("ul") |>
  html_nodes("li") |>
  html_nodes("a") |>
  html_attr("href")

# close connection
close(url)