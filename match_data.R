
library(rvest)
library(tidyverse)

# data from weltfussball.de
base_url <- "https://www.weltfussball.de"



###### Functions ######

# function for scraping spielplan URLs given matchday URL
get_spielbericht_urls <- function(x, sleep = 1) {
  Sys.sleep(sleep)
  read_html(x) %>% html_nodes(".standard_tabelle td:nth-child(6) a") %>% html_attr("href")
} 

# function for scraping results of a single match
get_match_data <- function(link, sleep = 1) {
  
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
    date = page %>% html_element("th:nth-child(2)") %>% html_text2() %>% str_replace("\\n", " ") %>% as.POSIXct(format = "%A, %d. %B %Y %H:%M", tz = "Europe/Berlin"),
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
    red_away = sum(away_cards == "rot")
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
spielplaene <- rbind(
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
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`AUS-A-League` <- match_data

############################################ 1. Bundesliga ########################################################

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
    get_match_data(spielbericht_urls[x], sleep = 2)
  }
) %>%
  reduce(rbind)

scrapes$`GER-Bundesliga1` <- match_data


############################################ 2. Bundesliga ########################################################







