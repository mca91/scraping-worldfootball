
library(rvest)
library(tidyverse)
library(lubridate)

# data from weltfussball.de
base_url <- "https://www.weltfussball.de"



###### Functions ######

# function for scraping spielplan URLs given matchday URL
get_spielbericht_urls <- function(x, sleep = 1) {
  Sys.sleep(sleep)
  read_html(x) %>% html_nodes(".standard_tabelle td:nth-child(6) a") %>% html_attr("href")
} 

# function for scraping results of a single match
get_match_data <- function(link, 
                           sleep = 1, 
                           locale = Sys.getlocale("LC_TIME")) # for system locale. I use  "German_Germany.1252" on Windows.
  {
  
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

###################################################################################################################

# list for gathering outcomes
scrapes <- list()

############################################ A-League (Australia) #################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/aus-a-league-", "2019-2020", "-spieltag/", 1:26, "/"),
  paste0(base_url, "/spielplan/aus-a-league-", "2020-2021", "-spieltag/", 1:26, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 

# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)


scrapes$`AUS-ALeague` <- match_data

############################################ Kategoria Superiore (Albania) ########################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/alb-kategoria-superiore-", "2019-2020", "-spieltag/", 1:36, "/"),
  paste0(base_url, "/spielplan/alb-kategoria-superiore-", "2020-2021", "-spieltag/", 1:36, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`ALB-KategoriaSuperiore` <- match_data





############################################ 1. Bundesliga (Austria) ########################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/aut-bundesliga-", "2019-2020", "-spieltag/", 1:22, "/"),
  paste0(base_url, "/spielplan/aut-bundesliga-", "2020-2021", "-spieltag/", 1:22, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`AUT-Bundesliga` <- match_data


############################################ 2. Liga (Austria) ########################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/aut-2-liga-", "2019-2020", "-spieltag/", 1:30, "/"),
  paste0(base_url, "/spielplan/aut-2-liga-", "2020-2021", "-spieltag/", 1:30, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`AUT-Liga2` <- match_data


############################################ Primera Division (Costa Rica) ########################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/crc-primera-division-", "2019-2020", "-spieltag/", 1:30, "/"),
  paste0(base_url, "/spielplan/crc-primera-division-", "2020-2021", "-spieltag/", 1:30, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`CRC-PrimeraDivision` <- match_data



############################################ Superligaen (Denmark) ########################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/den-superliga-", "2019-2020", "-spieltag/", 1:26, "/"),
  paste0(base_url, "/spielplan/den-superliga-", "2020-2021", "-spieltag/", 1:22, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`DEN-Superligaen` <- match_data


############################################ Premier League (England) ########################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/eng-premier-league-", "2019-2020", "-spieltag/", 1:38, "/"),
  paste0(base_url, "/spielplan/eng-premier-league-", "2020-2021", "-spieltag/", 1:38, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`ENG-PremierLeague` <- match_data


############################################ Championship (England) ########################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/eng-championship-", "2019-2020", "-spieltag/", 1:46, "/"),
  paste0(base_url, "/spielplan/eng-championship-", "2020-2021", "-spieltag/", 1:46, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`ENG-Championship` <- match_data




############################################ 1. Bundesliga (Germany) ######################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/bundesliga-", "2019-2020", "-spieltag/", 1:34, "/"),
  paste0(base_url, "/spielplan/bundesliga-", "2020-2021", "-spieltag/", 1:34, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`GER-Bundesliga1` <- match_data



############################################ 2. Bundesliga (Germany) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/2-bundesliga-", "2019-2020", "-spieltag/", 1:34, "/"),
  paste0(base_url, "/spielplan/2-bundesliga-", "2020-2021", "-spieltag/", 1:34, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`GER-Bundesliga2` <- match_data


############################################ 3. Liga (Germany) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- c(
  paste0(base_url, "/spielplan/3-liga-", "2019-2020", "-spieltag/", 1:38, "/"),
  paste0(base_url, "/spielplan/3-liga-", "2020-2021", "-spieltag/", 1:38, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`GER-3Liga` <- match_data



############################################ Super League (Greece) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/gre-super-league-", "2019-2020", "-spieltag/", 1:26, "/"),
  paste0(base_url, "/spielplan/gre-super-league-", "2020-2021", "-spieltag/", 1:26, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`GRE-SuperLeague` <- match_data



############################################ OTP Bank Liga (Hungary) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/hun-nb-i-", "2019-2020", "-spieltag/", 1:33, "/"),
  paste0(base_url, "/spielplan/hun-nb-i-", "2020-2021", "-spieltag/", 1:33, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`HUN-OTBPankLiga` <- match_data



############################################ Serie A (Italy) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/ita-serie-a-", "2019-2020", "-spieltag/", 1:38, "/"),
  paste0(base_url, "/spielplan/ita-serie-a-", "2020-2021", "-spieltag/", 1:38, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`ITA-SerieA` <- match_data



############################################ Serie B (Italy) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/ita-serie-b-", "2019-2020", "-spieltag/", 1:38, "/"),
  paste0(base_url, "/spielplan/ita-serie-b-", "2020-2021", "-spieltag/", 1:38, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`ITA-SerieB` <- match_data



############################################ Ekstraklasa (Poland) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/pol-ekstraklasa-", "2019-2020", "-spieltag/", 1:30, "/"),
  paste0(base_url, "/spielplan/ita-ekstraklasa-", "2020-2021", "-spieltag/", 1:30, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x])
  }
) %>%
  reduce(rbind)

scrapes$`POL-Ekstraklasa` <- match_data



############################################ Primera Liga (Portugal) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/por-primeira-liga-", "2019-2020", "-spieltag/", 1:34, "/"),
  paste0(base_url, "/spielplan/por-primeira-liga-", "2020-2021", "-spieltag/", 1:34, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`POR-PrimeiraLiga` <- match_data



############################################ Liga 1 (Romania) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/rou-liga-1-", "2019-2020", "-spieltag/", 1:30, "/"),
  paste0(base_url, "/spielplan/rou-liga-1-", "2020-2021", "-spieltag/", 1:30, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`ROU-Liga1` <- match_data




############################################ SuperLiga (Serbia) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/srb-super-liga-", "2019-2020", "-spieltag/", 1:30, "/"),
  paste0(base_url, "/spielplan/srb-super-liga-", "2020-2021", "-spieltag/", 1:30, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`SRB-SuperLiga` <- match_data



############################################ PrvaLiga (Slovenia) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/svn-prvaliga-", "2019-2020", "-spieltag/", 1:36, "/"),
  paste0(base_url, "/spielplan/svn-prvaliga-", "2020-2021", "-spieltag/", 1:36, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`SVN-PrvaLiga` <- match_data




############################################ La Liga (Spain) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/esp-primera-division-", "2019-2020", "-spieltag/", 1:38, "/"),
  paste0(base_url, "/spielplan/esp-primera-division-", "2020-2021", "-spieltag/", 1:38, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`ESP-LaLiga` <- match_data



############################################ Segunda Division (Spain) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/esp-segunda-division-", "2019-2020", "-spieltag/", 1:42, "/"),
  paste0(base_url, "/spielplan/esp-segunda-division-", "2020-2021", "-spieltag/", 1:42, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`ESP-SegundaDivision` <- match_data



############################################ Premyer Liga (Ukraine) ####################################################

# generate urls for 19/20, 20/21 spielplaene
spielplaene <- rbind(
  paste0(base_url, "/spielplan/ukr-premyer-liga-", "2019-2020", "-spieltag/", 1:22, "/"),
  paste0(base_url, "/spielplan/ukr-premyer-liga-", "2020-2021", "-spieltag/", 1:26, "/")
)

# scrape spielbericht urls
spielbericht_urls <- map(
  1:length(spielplaene), 
  function(x) {
    cat("getting spielbericht URLs for matchday", x, "\n")
    get_spielbericht_urls(spielplaene[x])
  }
) %>%
  unlist() %>% 
  paste0(base_url, .) 


# scrape match data
match_data <- map(
  1:length(spielbericht_urls), 
  function(x) {
    cat("getting match data for match", x, "\n")
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`UKR-PremyerLiga` <- match_data
