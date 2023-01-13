
pacman::p_load(igraph, networkD3, GGally, network, sna, intergraph, htmltools)

aes <- dbGetQuery(con, "SELECT * FROM aesthetics")
rel <- dbGetQuery(con, "SELECT * FROM aes_related_aesthetics")
rel_id <- aes |> select(related_aesthetics = aesthetic, rel_id = aes_id)

depop <- read.csv("data/2023-01-04_aesthetics.csv") |>
  select(aesthetic, depop_results) |>
  mutate(depop_results = ifelse(is.na(depop_results), 0, depop_results)) |>
  mutate(group = 1) |>
  mutate(depop_results = depop_results ^ (1 / 1.5))



aes_related <- aes |>
  left_join(rel, by = "aes_id") |>
  left_join(rel_id, by = "related_aesthetics") |>
  select(from = aes_id, to = rel_id) |>
  filter(to %in% from) |>
  mutate(value = 1,
         from = from - 1,
         to = to - 1)

net <- graph_from_data_frame(d=aes_related, vertices=depop, directed=T) 

ggnet2(net)

simpleNetwork(aes_related, zoom = TRUE, opacity = 0.95, linkColour = "#d1d1d1", nodeColour = "#cc0070")


browsable(
  tagList(
    tags$head(
      tags$style('
        body{background-color: #DAE3F9 !important}
        .nodetext{fill: #000000}
        .legend text{fill: #FF0000}
      ')
    ),
    forceNetwork(Links = aes_related, Nodes = depop, 
                 Source = "from", Target = "to",
                 Value = "value", NodeID = "aesthetic",
                 Nodesize = "depop_results", width = "100%", height = "1024px",
                 zoom = TRUE, fontSize = 42, linkColour = "#d1d1d1",
                 Group = "group", opacity = 0.95)
  )
)





related_aesthetics |>
  group_by(aes_id) |>
  summarise(n = n()) |>
  arrange(-n)



# Load data
data(MisLinks)
data(MisNodes)

ml <- MisLinks
mn <- MisNodes

# Plot
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)