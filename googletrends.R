library(countrycode)
library(dplyr)
library(gtrendsR)
library(exploratory) # for get google trends
library(rvest)

###################
# GET COUNTRY CODES
###################

# Remove first 2 rows; retain first 2 cols; rename columns
clean_country_codes <- function(df) {
  df[-1, c(1:2)] %>%
  rename(country = X1, 
         country_code = X2)
}

# ------------------------
# Get South American Codes
# ------------------------
url <- "https://www.countrycallingcodes.com/iso-country-codes/south-america-codes.php"
page <- read_html(url)
tables <- html_nodes(page, "table") # it's the 4th table
tables # locate table
south_america <- page %>%
  html_nodes("table") %>%
  .[4] %>%
  html_table() %>%
  .[[1]] %>%
  tbl_df()
# inspect
south_america 
# clean 
south_america <- clean_country_codes(south_america)
# inspect
south_america 

# ------------------------
# Get North American Codes
# ------------------------
url <- "https://www.countrycallingcodes.com/iso-country-codes/north-america-codes.php"
page <- read_html(url)
tables <- html_nodes(page, "table") # it's the 4th table
tables # locate table
north_america <- page %>%
  html_nodes("table") %>%
  .[4] %>%
  html_table() %>%
  .[[1]] %>%
  tbl_df()
# inspect
north_america 
# clean 
north_america <- clean_country_codes(north_america)
# inspect
north_america  

# --------------------------------
# Split North America into regions
# --------------------------------

usa_canada <- north_america %>%
  filter(country %in% c("Canada", "United States"))
# inspect
usa_canada

# Include Spanish-speaking caribbean countries
central_america <- c("Belize", "Costa Rica", "Cuba", "Dominican Republic",
                     "El Salvador", "Guatemala", "Honduras", "Mexico", 
                     "Nicaragua", "Panama", "Puerto Rico")
central_america <- north_america %>%
  filter(country %in% central_america)
# inspect
central_america

# Out of curiousity, include non-sovreign Turks and Caicos 
caribbean <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Cuba", "Dominica", "Dominican Republic",
               "Grenada and Carriacuou", "Haiti", "Jamaica", "St. Kitts", "Puerto Rico",
               "St. Lucia", "St. Vincent", "Turks and Caicos Islands", "Trinidad and Tobago")
length(caribbean)
caribbean <- north_america %>%
  filter(country %in% caribbean)
# inspect
caribbean

# Generalized Latin America Dataset -- note Falkland Islands currently retained out of curiousity
latin_america <- rbind(central_america, south_america) %>%
  filter(!country %in% c("Guyana", "Guiana", "Suriname", "French Guiana")) %>%
  arrange(country)
# inspect
latin_america

############################
# RESEARCH VECTORS
# - # countries must be < 5
############################

# Business trip countries
western_hemisphere <- rbind(south_america, north_america)
business_trip <- c("Bahamas", "Jamaica", "Trinidad and Tobago", "Guyana")
business_trip <- western_hemisphere %>%
  filter(country %in% business_trip) %>%
  arrange(country) %>%
  pull()
# inspect
business_trip

# latin american countries of interest
la <- c("Venezuela", "Colombia", "Argentina", "Chile", "Uruguay")
la <- latin_america %>%
  filter(country %in% la) %>%
  arrange(country) %>%
  pull()
# inspect
la

###################
# GET GOOGLE TRENDS
###################

# GET TRENDS FROM SPANISH-LANGUAGE SEARCHES
# @time: default is since beginning of Google Trends (2004)
# @hl: language code for spanish
spanish_trends <- function(kw, countries, gprop = "web", time = "all") {
  gtrends(kw,
          geo = countries,
          time = time,
          gprop = "web",
          hl = "es")
}

# GET TRENDS FROM ENGLISH-LANGUAGE SEARCHES
# @time: default is since beginning of Google Trends (2004)
english_trends <- function(kw, countries, gprop = "web", time = "all") {
  gtrends(kw,
          geo = countries,
          time = time,
          gprop = "web")
}

# 1. Google or Apple?
keywords <- c("Google", "Apple")

spanish_web.google_apple <- gtrends(c("Google", "Apple"),
                                    geo = c("AR", "CL", "CO", "UY", "VE"),
                                    hl = "es")

gtrends(c("NHL", "NFL"), time = "now 1-H")
  
  spanish_trends(keywords, la)
english_web.google_apple <- english_trends(keywords, business_trip)

spanish_news.google_apple <- spanish_trends(keywords, la, "news")
english_news.google_apple <- english_trends(keywords, business_trip, "news")


un <- "<insert_email>"
p <- "<insert_password>"
exploratory::getGoogleTrends(un, p, c('Google',' Apple',' Facebook',' Amazon',' Microsoft'),'top_regions','5y','') %>%
  exploratory::clean_data_frame() %>%
  group_by(keyword) %>%
  top_n(20, trend) %>%
  mutate(Continent = countrycode(Region,origin="country.name",destination="continent"), Continent = if_else(Region %in% c("United States", "Canada"),
                                                                                                            "North America", Continent), Continent = if_else(Region  == "Taiwan","Asia", Continent))
devtools::install_github("PMassicotte/gtrendsR") 
# 2. Android, iPhone or Huawei?
keywords <- c("Android", "iPhone", "Huawei")
web_results <- gtrends(keywords,
                       geo = "Latin America",
                       time = "all", # since beginning of Google Trends (2004)
                       hl = "es" # language code for spanish
                       )

in_the_news <- gtrends(keywords,
                       geo = "Latin America",
                       time = "today+5-y", # past 5 years (default)
                       gprop = "news", # in the news
                       hl = "es" # language code for spanish
                       )
# 3. Social networks: Facebook, Instagram, Pinterest, Tumblr, Twitter or Youtube?
keywords <- ("Facebook", "Instagram", "Pinterest", "Tumblr", "Twitter", "Youtube")


# 4. Airbnb or Couchsurfing? 
keywords <- ("Airbnb", "Couchsurfing")

data("categories")
categories

# 5. Hilary Clinton vs. Donald Trump
keywords <- ("Hillary Clinton", "Hilary Clinton", "Donald Trump")
in_the_news <- gtrends(keywords,
                       time = "2015-61-01 2017-01-20", # specific timespan
                       gprop = "news", # in the news 
                       hl = "es" # language code for spanish
)

on_youtube <- gtrends(keywords,
                      time = "2015-61-01 2017-01-20", # specific timespan
                      gprop = "youtube", # on youtube
                      hl = "es" # language code for spanish
)

data(countries)
countries
                  