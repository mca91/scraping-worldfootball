

all_spielbericht_urls <- function(
  spielplaene, 
  sleep = 1, 
  base_url = "https://www.weltfussball.de", 
  dir = getwd(),
  save_result = TRUE
  ) {
  
  matchdays <- 1:length(spielplaene) 
  league_names <- names(spielplaene)
  names(matchdays) <- league_names
  
  spielbericht_urls <- map(
    matchdays, 
    function(x) map(
      1:length(spielplaene[[x]]),
      ~ {
        cat("getting spielbericht URLs for '", league_names[x] , "'", "matchday", .x, "\n") 
        get_spielbericht_urls(spielplaene[[x]][.x], sleep = sleep) %>% 
        paste0(base_url, .) 
      }
    )
  )
  
  assign("spielbericht_urls", spielbericht_urls, envir = globalenv())
  
  if(save_result) saveRDS(object = spielbericht_urls, file = paste(dir, "spielbericht_urls.RDS", sep = "/"))
  
}  


get_all_match_data <- function(
  spielbericht_urls,
  subset = NULL,
  sleep = 1, 
  output = "assign",
  path = paste0(getwd(), "match_data.Rds")
  ) {
  
  if(!is.null(subset)) spielbericht_urls <- spielbericht_urls[subset]
  
  league_names <- names(spielbericht_urls) 
  
  # scrape match data
  match_data <- 
    map(
      1:length(spielbericht_urls),
      function(x) map(
        1:length(spielbericht_urls[[x]] %>% unlist),
        ~ {
          urls <- spielbericht_urls[[x]] %>% unlist
          cat("Getting match data for '", league_names[x], "' ", ": match ", .x, "\n", sep = " ")
          get_match_data(urls[.x], sleep = sleep) 
        }
      ) %>% reduce(rbind)
      
    )
  
  if(output == "assign") {
    assign("match_data", match_data, envir = globalenv())
  } else if(output == "save") {
    saveRDS(object = match_data, file = paste0(path, ".RDS"))
  } else {
    return(match_data)    
  }
  
}



##########################################################################################
 
all_spielplaene <- list(
  "A League (Australia)" = c(
    paste0(base_url, "/spielplan/aus-a-league-", "2019-2020", "-spieltag/", 1:29, "/"),
    paste0(base_url, "/spielplan/aus-a-league-", "2020-2021", "-spieltag/", 1:24, "/")    
  ),
  "Kategoria Superiore (Albania)" = c(
    paste0(base_url, "/spielplan/alb-kategoria-superiore-", "2019-2020", "-spieltag/", 1:36, "/"),
    paste0(base_url, "/spielplan/alb-kategoria-superiore-", "2020-2021", "-spieltag/", 1:36, "/")
  ),
  "1. Bundesliga (Austria)" = c(
    paste0(base_url, "/spielplan/aut-bundesliga-", "2019-2020", "-spieltag/", 1:22, "/"),
    paste0(base_url, "/spielplan/aut-bundesliga-", "2020-2021", "-spieltag/", 1:22, "/")
  ),
  "2. Liga (Austria)" = c(
    paste0(base_url, "/spielplan/aut-2-liga-", "2019-2020", "-spieltag/", 1:30, "/"),
    paste0(base_url, "/spielplan/aut-2-liga-", "2020-2021", "-spieltag/", 1:30, "/")
  ),
  "Primera Division - Apertura (Costa Rica)" = c(
    paste0(base_url, "/spielplan/crc-primera-division-", "2019-2020-apertura", "-spieltag/", 1:22, "/"),
    paste0(base_url, "/spielplan/crc-primera-division-", "2020-2021", "-spieltag/", 1:16, "/")
  ),
  "Primera Division - Clausura (Costa Rica)" = c(
    paste0(base_url, "/spielplan/crc-primera-division-", "2019-2020-clausura", "-spieltag/", 1:22, "/"),
    paste0(base_url, "/spielplan/crc-primera-division-", "2020-2021-clausura", "-spieltag/", 1:22, "/")
  ),
  "Superligaen (Denmark)" = c(
    paste0(base_url, "/spielplan/den-superliga-", "2019-2020", "-spieltag/", 1:26, "/"),
    paste0(base_url, "/spielplan/den-superliga-", "2020-2021", "-spieltag/", 1:22, "/")
  ),
  "Premier League (England)" = c(
    paste0(base_url, "/spielplan/eng-premier-league-", "2019-2020", "-spieltag/", 1:38, "/"),
    paste0(base_url, "/spielplan/eng-premier-league-", "2020-2021", "-spieltag/", 1:38, "/")
  ),
  "Championship (England)" = c(
    paste0(base_url, "/spielplan/eng-championship-", "2019-2020", "-spieltag/", 1:46, "/"),
    paste0(base_url, "/spielplan/eng-championship-", "2020-2021", "-spieltag/", 1:46, "/")
  ),
  "1. Bundesliga (Germany)" = c(
    paste0(base_url, "/spielplan/bundesliga-", "2019-2020", "-spieltag/", 1:34, "/"),
    paste0(base_url, "/spielplan/bundesliga-", "2020-2021", "-spieltag/", 1:34, "/")
  ),
  "2. Bundesliga (Germany)" = c(
    paste0(base_url, "/spielplan/2-bundesliga-", "2019-2020", "-spieltag/", 1:34, "/"),
    paste0(base_url, "/spielplan/2-bundesliga-", "2020-2021", "-spieltag/", 1:34, "/")
  ),
  "3. Liga (Germany)" = c(
    paste0(base_url, "/spielplan/3-liga-", "2019-2020", "-spieltag/", 1:38, "/"),
    paste0(base_url, "/spielplan/3-liga-", "2020-2021", "-spieltag/", 1:38, "/")
  ),
  "Super League (Greece)" = c(
    paste0(base_url, "/spielplan/gre-super-league-", "2019-2020", "-spieltag/", 1:26, "/"),
    paste0(base_url, "/spielplan/gre-super-league-", "2020-2021", "-spieltag/", 1:26, "/")
  ),
  "OTP Bank Liga (Hungary)" = c(
    paste0(base_url, "/spielplan/hun-nb-i-", "2019-2020", "-spieltag/", 1:33, "/"),
    paste0(base_url, "/spielplan/hun-nb-i-", "2020-2021", "-spieltag/", 1:33, "/")
  ),
  "Serie A (Italy)" = c(
    paste0(base_url, "/spielplan/ita-serie-a-", "2019-2020", "-spieltag/", 1:38, "/"),
    paste0(base_url, "/spielplan/ita-serie-a-", "2020-2021", "-spieltag/", 1:38, "/")
  ),
  "Serie B (Italy)" = c(
    paste0(base_url, "/spielplan/ita-serie-b-", "2019-2020", "-spieltag/", 1:38, "/"),
    paste0(base_url, "/spielplan/ita-serie-b-", "2020-2021", "-spieltag/", 1:38, "/")
  ),
  "Ekstraklasa (Poland)" = c(
    paste0(base_url, "/spielplan/pol-ekstraklasa-", "2019-2020", "-spieltag/", 1:30, "/"),
    paste0(base_url, "/spielplan/ita-ekstraklasa-", "2020-2021", "-spieltag/", 1:30, "/")
  ),
  "Primera Liga (Portugal)" = c(
    paste0(base_url, "/spielplan/por-primeira-liga-", "2019-2020", "-spieltag/", 1:34, "/"),
    paste0(base_url, "/spielplan/por-primeira-liga-", "2020-2021", "-spieltag/", 1:34, "/")
  ),
  "Liga 1 (Romania)" = c(
    paste0(base_url, "/spielplan/rou-liga-1-", "2019-2020", "-spieltag/", 1:30, "/"),
    paste0(base_url, "/spielplan/rou-liga-1-", "2020-2021", "-spieltag/", 1:30, "/")
  ),
  "SuperLiga (Serbia)" = c(
    paste0(base_url, "/spielplan/srb-super-liga-", "2019-2020", "-spieltag/", 1:30, "/"),
    paste0(base_url, "/spielplan/srb-super-liga-", "2020-2021", "-spieltag/", 1:30, "/")
  ),
  "PrvaLiga (Slovenia)" = c(
    paste0(base_url, "/spielplan/svn-prvaliga-", "2019-2020", "-spieltag/", 1:36, "/"),
    paste0(base_url, "/spielplan/svn-prvaliga-", "2020-2021", "-spieltag/", 1:36, "/")
  ),
  "La Liga (Spain)" = c(
    paste0(base_url, "/spielplan/esp-primera-division-", "2019-2020", "-spieltag/", 1:38, "/"),
    paste0(base_url, "/spielplan/esp-primera-division-", "2020-2021", "-spieltag/", 1:38, "/")
  ),
  "Segunda Division (Spain)" = c(
    paste0(base_url, "/spielplan/esp-segunda-division-", "2019-2020", "-spieltag/", 1:42, "/"),
    paste0(base_url, "/spielplan/esp-segunda-division-", "2020-2021", "-spieltag/", 1:42, "/")
  ),
  "Premyer Liga (Ukraine)" = c(
    paste0(base_url, "/spielplan/ukr-premyer-liga-", "2019-2020", "-spieltag/", 1:22, "/"),
    paste0(base_url, "/spielplan/ukr-premyer-liga-", "2020-2021", "-spieltag/", 1:26, "/")
  )
  
)

# scrape / generate spielbericht urls
all_spielbericht_urls(spielplaene = all_spielplaene)

# scrape and generate match data

# list for gathering outcomes
scrapes <- list()

scrapes$`AUS-ALeague` <- get_all_match_data(spielbericht_urls, subset = "A League (Australia)", output = "return")
scrapes$`ALB-KategoriaSuperiore` <- get_all_match_data(spielbericht_urls, subset = "Kategoria Superiore (Albania)", output = "return")
scrapes$`AUT-Bundesliga` <- get_all_match_data(spielbericht_urls, subset = "1. Bundesliga (Austria)", output = "return")
scrapes$`AUT-Liga2` <- get_all_match_data(spielbericht_urls, subset = "2. Liga (Austria)", output = "return")

scrapes$`CRC-PrimeraDivision` <- get_all_match_data(spielbericht_urls, subset = "Primera Division - Apertura (Costa Rica)", output = "return")
scrapes$`CRC-PrimeraDivisionClausura` <- get_all_match_data(spielbericht_urls, subset = "Primera Division - Clausura (Costa Rica)", output = "return")
scrapes$`DEN-Superligaen` <- get_all_match_data(spielbericht_urls, subset = "Superligaen (Denmark)", output = "return")
scrapes$`ENG-PremierLeague` <- get_all_match_data(spielbericht_urls, subset = "Premier League (England)", output = "return")
scrapes$`ENG-Championship` <- get_all_match_data(spielbericht_urls, subset = "Championship (England)", output = "return")
scrapes$`GER-Bundesliga1` <- get_all_match_data(spielbericht_urls, subset = "1. Bundesliga (Germany)", output = "return")
