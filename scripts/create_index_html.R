
library(stringr)
library(dplyr)
library(rvest)

header_code <- '<!DOCTYPE HTML>
<!--
	Multiverse by HTML5 UP
	html5up.net | @ajlkn
	Free for personal and commercial use under the CCA 3.0 license (html5up.net/license)
-->
<html>
	<head>
	  	<!-- HTML Meta Tags -->
		<title>Trending Aesthetics - Top 100</title>
		<meta name="description" content="Hottest 100 aesthetics right now.">
                <meta charset="utf-8" />
		<meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=no" />
		<link rel="stylesheet" href="assets/css/main.css" />
		<noscript><link rel="stylesheet" href="assets/css/noscript.css" /></noscript>

		<!-- Facebook Meta Tags -->
		<meta property="og:url" content="https://filmicaesthetic.github.io">
		<meta property="og:type" content="website">
		<meta property="og:title" content="Trending Aesthetics - Top 100">
		<meta property="og:description" content="Hottest 100 aesthetics right now.">

		<!-- Twitter Meta Tags -->
		<meta name="twitter:card" content="summary_large_image">
		<meta property="twitter:domain" content="filmicaesthetic.github.io">
		<meta property="twitter:url" content="https://filmicaesthetic.github.io">
		<meta name="twitter:title" content="Trending Aesthetics - Top 100">
		<meta name="twitter:description" content="Hottest 100 aesthetics right now.">

    <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Rubik%20Mono%20One">

	</head>
	<body class="is-preload">

		<!-- Wrapper -->
			<div id="wrapper">

				<!-- Header -->
					<header id="header">
						<h1><a href="index.html"><strong>Trending Aesthetics</strong><br>Top 100</a></h1>
						<nav>
							<ul>
								<li><a href="#footer" class="icon solid fa-info-circle">Huh?</a></li>
		</ul>
		  </nav>
		  </header>
		  
		  <!-- Main -->
		  <div id="main">
'

footer_code <- '</div>

				<!-- Footer -->
					<footer id="footer" class="panel">
						<div class="inner split">
							<div>
								<section>
									<h2>Trending Aesthetics</h2>
									<p>We look on Depop for all aesthetics listed in the Aesthetics Wiki and rank them based on results.</p>
  </section>
  <section>
  <h2>Follow me on ...</h2>
  <ul class="icons">
    <li><a href="https://www.twitter.com/trendingaesthetics" class="icon brands fa-twitter"><span class="label">Twitter</span></a></li>
        </ul>
        </section>
        <p class="copyright">
          &copy; Trending Aesthetics. Design: <a href="http://html5up.net">HTML5 UP</a>.
          </p>
            </div>
            <div>
            
                                  </div>
                                  </div>
                                  </footer>
                                  
                                  </div>
                                  
                                  <!-- Scripts -->
                                  <script src="assets/js/jquery.min.js"></script>
                                    <script src="assets/js/jquery.poptrox.min.js"></script>
                                      <script src="assets/js/browser.min.js"></script>
                                        <script src="assets/js/breakpoints.min.js"></script>
                                          <script src="assets/js/util.js"></script>
                                            <script src="assets/js/main.js"></script>
                                              
                                              </body>
                                              </html>'


tbl <- read.csv("data/current_league_table.csv") |>
  head(100)

yday_tbl <- read.csv("data/prev_league_table.csv") |>
  head(100) |>
  mutate(yday_rank = row_number()) |>
  select(aesthetic, yday_rank)

# function to extract main image from wiki page
get_main_img <- function(url) {
  
  img <- url |>
    read_html() |>
    html_nodes(".pi-image img") |>
    html_attr("src")
  
  img <- img[1]
  
  img <- substr(img, 1, str_locate(img, "\\/revision")[1]-1)
  
  return(img)
  
}



# create wiki url and extract images
tbl_get <- tbl |>
  mutate(aes_wiki_link = paste0("https://aesthetics.fandom.com/wiki/",gsub(" ", "_", gsub("2-Tone", "2 Tone", aesthetic)))) |>
  mutate(rank = row_number()) |>
  rowwise() |>
  mutate(main_img = get_main_img(aes_wiki_link)) |>
  mutate(main_img = ifelse(is.na(main_img) == TRUE, "https://www.dontcrampmystyle.co.uk/wp-content/uploads/2014/05/Light_Pastel_Purple_429585_i0-1.png", main_img))

tbl_ext <- tbl_get |>
  left_join(yday_tbl, by = "aesthetic") |>
  mutate(change = yday_rank - rank) |>
  mutate(change_lbl = ifelse(is.na(change), "new", ifelse(change==0, "none", ifelse(change>0, "up", "down")))) |>
  mutate(change_html = ifelse(change_lbl == "new", 
                              '<h4>NEW</h4>',
                              ifelse(change_lbl == "none",
                                     '',
                                     ifelse(change_lbl == "up",
                                            paste0('<h5>',change,'<strong>▲</strong></h5>'),
                                            paste0('<h6>',abs(change),'<strong>▼</strong></h6>'))))) |>
  select(-change, -change_lbl)



code_list <- list(header_code)

for (i in 1:100) {
  
  change_html <- tbl_ext$change_html[i]
  aes_rank <- tbl_ext$rank[i]
  aes_name <- tbl_ext$aesthetic[i]
  main_img <- tbl_ext$main_img[i]
  depop_results_link <- paste0("https://www.depop.com/search/?q=%22",gsub(" ", "%20", aes_name),"%22+aesthetic")
  
  art <- paste0(' <!-- ',aes_name,' -->
						<article class="thumb">
							<a href="',main_img,'" class="image"><img src="',main_img,'" alt="',aes_name,' header image" /></a>
							<h2>',aes_name,'</h2>
						  <h3>',aes_rank,'</h3>',
						  change_html,'
							<a href = "',depop_results_link,'"><button>Shop ',aes_name,' on Depop</button></a></p>
						</article>
       ')
  
  assign(paste0("art_",i), art)
  
  code_list <- c(code_list, art)
  
}

code_list <- c(code_list, footer_code)

code <- paste0(code_list)

write.table(code, 
            file='index.html', 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)

