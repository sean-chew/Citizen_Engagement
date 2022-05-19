### Set Environment ------------------------------------------------------------
# Clear the environment
rm(list=ls())
gc()
# So the code will compile warnings as per usual
options(warn = 0)
# So that text data is not read in as factors
options(stringsAsFactors = F)
# Turn off scientific notation
options(scipen = 999)
### Load Packages --------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, magrittr, stringr, reshape2, janitor,
               lubridate, readxl, data.table, ggplot2, scales, readr,
               tidyr, zoo, skimr, openxlsx,ggspatial, rgeos, data.table,RColorBrewer,
               tidyverse, rio, collapse, sf, glue, XML, tm, here, purrr, repurrrsive,
               tmap,tidygraph, nabor,igraph, viridis, hrbrthemes,RSocrata,soql)
# test <- read.socrata("https://data.cityofnewyork.us/resource/erm2-nwe9.json$limit=10")
dataset_id <- "erm2-nwe9"
api_endpoint <- paste0('https://data.cityofnewyork.us/resource/', dataset_id, '.json')

api_token <- Sys.getenv('SOCRATA_TOKEN')

query_params <- soql() %>%
    soql_add_endpoint(api_endpoint) %>%
    soql_limit(20000)
#soql_where(paste0("DATE_API_NAME_GOES_HERE> '", Sys.Date() -100, "T12:00:00'"))
query <- read.socrata(query_params)

nrow(query)


test <- read.socrata("https://data.cityofnewyork.us/resource/erm2-nwe9.json?$limit=10")



