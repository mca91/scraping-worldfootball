
library(rvest)
library(tidyverse)
library(lubridate)

# I use German UFT-8 locale for Linux-based OS and "German_Germany.1252" on Windows.
# !!Note that using the system default may fail ( check Sys.getlocale("LC_TIME") )
# because match dates / times are in German format and we need a corresponding locale for lubridate
# to parse correctly to date_time format!!

# Here I set the locale globally. Other option is to provide a locale
# to the match data scraping functions.
Sys.setlocale(locale = "de_DE.UTF-8") 


############################################ functions ############################################

# function for scraping match report _URLs_ for provided seasons.
all_spielbericht_urls <- function(
  spielplaene,  # needs to be a named nested list, see body
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


# function for scraping results of _a single match_
get_match_data <- function(
  link, 
  sleep = 1, 
  locale = "de_DE.UTF-8"
  ) {
  
  # pause
  Sys.sleep(sleep)
  
  # parse match result page
  page <- read_html(link)
  
  # home cards
  home_cards <- page %>% html_elements("table .standard_tabelle") %>% .[1] %>% html_elements("tr > td > img") %>% html_attr("alt")
  
  # away cards
  away_cards <- page %>% html_elements("table .standard_tabelle") %>% .[2] %>% html_elements("tr > td > img") %>% html_attr("alt")
  
  # obtain and return tibble with interesting variables
  tibble(
    date = page %>% html_element("th:nth-child(2)") %>% html_text2() %>% str_replace("\\n", " ") %>% parse_date_time(orders = "%A, %d. %B %Y %H:%M", locale = locale, tz = "Europe/Berlin"),  
    league = str_extract(link, pattern = "(?<=spielbericht/).+(?=-\\d{4}-\\d{4})"),
    season = str_extract(link, pattern = "\\d{4}-\\d{4}"),
    matchday = page %>% html_elements("select") %>% .[3] %>% html_elements("option[selected='selected']") %>% html_text() %>% parse_number(),
    home = page %>% html_node("th:nth-child(1) a") %>% html_text(),
    away = page %>% html_node("th+ th a") %>% html_text(),
    outcome = page %>% html_node(".resultat") %>% html_text(),
    ref = page %>% html_node("tr:nth-child(3) .dunkel~ .dunkel+ .dunkel") %>% html_text(),
    att = page %>% html_node("br+ .standard_tabelle tr:nth-child(2) .dunkel~ .dunkel+ .dunkel") %>% html_text() %>% str_remove(pattern = "\\.") %>% str_trim(),
    yellow_home = sum(home_cards == "gelb"),
    yellow_away = sum(away_cards == "gelb"),
    red_home = sum(home_cards == "rot"),
    red_away = sum(away_cards == "rot"),
    redyellow_home = sum(home_cards == "gelb-rot"),
    redyellow_away = sum(away_cards == "gelb-rot")
  ) %>% 
    mutate(
      outcome = str_trim(outcome),
      ref = str_remove(ref, pattern = "\\s\\(.+")
    ) %>%
    separate(outcome, into = c("goals_home", "goals_away"), convert = T)
  
}

# function for scraping results for multiple matches
get_all_match_data <- function(
  spielbericht_urls, # needs to be a named nested list, see body
  subset = NULL,
  sleep = 1, 
  output = "assign",
  locale =  "de_DE.UTF-8",
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
          get_match_data(urls[.x], sleep = sleep, locale) 
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
    paste0(base_url, "/spielplan/pol-ekstraklasa-", "2020-2021", "-spieltag/", 1:30, "/")
  ),
  "Primera Liga (Portugal)" = c(
    paste0(base_url, "/spielplan/por-primeira-liga-", "2019-2020", "-spieltag/", 1:34, "/"),
    paste0(base_url, "/spielplan/por-primeira-liga-", "2020-2021", "-spieltag/", 1:34, "/")
  ),
  "Liga 1 (Romania)" = c(
    paste0(base_url, "/spielplan/rou-liga-1-", "2019-2020", "-spieltag/", 1:26, "/"),
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
# all_spielbericht_urls(spielplaene = all_spielplaene)
spielbericht_urls <- readRDS("spielbericht_urls.RDS")

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
scrapes$`GER-Bundesliga2` <- get_all_match_data(spielbericht_urls, subset = "2. Bundesliga (Germany)", output = "return")
scrapes$`GER-3Liga` <- get_all_match_data(spielbericht_urls, subset = "3. Liga (Germany)", output = "return")
scrapes$`GRE-SuperLeague` <-  get_all_match_data(spielbericht_urls, subset = "Super League (Greece)", output = "return")
scrapes$`HUN-OTBPankLiga` <-  get_all_match_data(spielbericht_urls, subset = "OTP Bank Liga (Hungary)", output = "return")

scrapes$`ITA-SerieA` <-  get_all_match_data(spielbericht_urls, subset = "Serie A (Italy)", output = "return")
scrapes$`ITA-SerieB` <-  get_all_match_data(spielbericht_urls, subset = "Serie B (Italy)", output = "return")
scrapes$`POL-Ekstraklasa` <-  get_all_match_data(spielbericht_urls, subset = "Ekstraklasa (Poland)" , output = "return")
scrapes$`POR-PrimeiraLiga` <-  get_all_match_data(spielbericht_urls, subset = "Primera Liga (Portugal)" , output = "return")
scrapes$`ROU-Liga1` <-  get_all_match_data(spielbericht_urls, subset = "Liga 1 (Romania)", output = "return")

scrapes$`SRB-SuperLiga`  <-  get_all_match_data(spielbericht_urls, subset = "SuperLiga (Serbia)" , output = "return")
scrapes$`SVN-PrvaLiga` <-  get_all_match_data(spielbericht_urls, subset = "PrvaLiga (Slovenia)" , output = "return")
scrapes$`ESP-LaLiga` <-  get_all_match_data(spielbericht_urls, subset = "La Liga (Spain)", output = "return")
scrapes$`ESP-SegundaDivision` <- get_all_match_data(spielbericht_urls, subset = "Segunda Division (Spain)", output = "return")
scrapes$`UKR-PremyerLiga` <- get_all_match_data(spielbericht_urls, subset = "Premyer Liga (Ukraine)", output = "return")

# save raw results
saveRDS(scrapes, file = "all_matches.RDS")

# join results
wfb_data <- map(scrapes, ~ .[[1]]) %>% reduce(rbind)
saveRDS(wfb_data, file = "wfb_data.RDS")


