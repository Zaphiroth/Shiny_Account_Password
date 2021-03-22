if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=1000*1024^2)

library(DT)
library(reshape2)
library(plyr)
library(data.table)
library(shiny)
library(stringi)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)
library(openxlsx)
library(shinydashboard)
library(rlang)
library(shinyjs)
library(webshot)
library(leaflet)
library(leafletCN)
library(shinyWidgets)
library(readr)
library(sodium)


credentials = data.frame(
  username_id = c("u", "uu"),
  password   = sapply(c("p", "pp"), password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)
