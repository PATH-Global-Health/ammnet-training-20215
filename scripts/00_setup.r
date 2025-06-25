# R setup script

# Packages installation ==========================================================================
install.packages("janitor")   #"janitor has simple little tools for examining and cleaning dirty data."
install.packages("shiny")     #"shiny: core package to build apps."
install.packages("bslib")     #"theming (Bootstrap customization) and modern dashboard design elements."
install.packages("tidyverse") #"Data wrangling (dplyr, ggplot2, readr…)."
install.packages("DT")        #"DT: interactive tables."
install.packages("plotly")    #"plotly: interactive plots."
install.packages("leaflet")   #"leaflet: mapping spatial data."
install.packages("sf")        #"sf: mapping spatial data."
install.packages("lubridate") #"lubridate for dealting with date-time."
install.packages("bsicons")   #"bsicons: Bootstrap icons for Shiny apps."

# Packages load ===================================================================================
# If any of these packages do not load, you will receive an error. Please follow up with an organizer
# of the help desk.
library(janitor)
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)
library(bsicons)

# Load datd files ================================================================================
# Check to make sure you are able to load the files. If you cannot, you will receive an error. Please
# follow up with an organizer of the help desk.
read_csv("./data-outputs/data_dhis2_laos_ammnet_cleaned.csv")
st_read("./data-outputs/adm1_shapefile.shp")
