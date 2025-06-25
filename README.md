# Development of a Dynamic Dashboard for Visualizing Malaria Epidemiological Surveillance Data with R-Shiny

This repository houses the materials for the AMMnet Session:

**Development of a dynamic dashboard for visualizing malaria epidemiological surveillance data with R-Shiny**

👉 **Full training website and instructions available here:**  
https://path-global-health.github.io/ammnet-training-20215/

---

> **Viewing in French:**  
> To view the website in French, open in Google Chrome, right-click anywhere on the page and select "Translate to français".

**Date:** Friday 27th June  
**Location:** Evasion 3 room  
**Time:** 9am

## Description

Data visualization is a powerful tool for understanding and communicating data. This training session is a practical guide for professionals wishing to master the development of dynamic dashboards for visualizing malaria epidemiological surveillance data with R-Shiny using DHIS2 exports. 

Participants will learn how to:

- Export data from DHIS2 using Pivot Table
- Prepare and clean data for visualization
- Choose appropriate chart types and design effective visualizations
- Build a fully interactive Shiny dashboard
- Deploy a Shiny application

Prior to the training, please ensure you have:

- Basic skills in descriptive data analysis
- Installed R and RStudio IDE
- Installed required packages (see below)

### Installation Guide

- [Download Install guide (EN)](lesson-materials/installation-guide.pdf)
- [Download Install guide (FR)](lesson-materials/installation-guide-FR.pdf)

### Required R Packages
```{r eval = FALSE}
# Packages installation ==========================================================================
install.packages("janitor")   #"janitor has simple little tools for examining and cleaning dirty data."
install.packages("shiny")     #"shiny: core package to build apps."
install.packages("bslib")     #"theming (Bootstrap customization) and modern dashboard design elements."
install.packages("tidyverse") #"Data wrangling (dplyr, ggplot2, readr…)."
install.packages("DT")        #"DT: interactive tables."
install.packages("plotly")    #"plotly: interactive plots."
install.packages("leaflet")   #"leaflet: mapping spatial data."
install.packages("sf")        #"sf: mapping spatial data."
install.paclages("lubridate") #"lubridate for dealting with date-time."
install.packages("bsicons")   #"bsicons: Bootstrap icons for Shiny apps."

# Packages load ===================================================================================
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

```

## Objectives

### Part 1

- Access DHIS2 and explore existing dashboards
- Make data queries (data elements, time periods, geographical units)
- Download data as CSV
- Read data into R
- Clean and prepare the dataset for visualization

**Outcome:** A cleaned malaria dataset ready for dashboard development.

### Part 2

- Understand key elements of Shiny dashboards
- Gain hands-on experience customizing Shiny dashboards
- Build your own interactive Shiny dashboard

**Outcome:** A fully interactive local Shiny App displaying malaria program metrics.

## Materials

| Materials             | Links |
|-----------------------|-------|
| Part 1 Slides (EN)     | [English](https://path-global-health.github.io/ammnet-training-20215/english-slides.html) | 
| Part 1 Slides (FR)     | [French](https://path-global-health.github.io/ammnet-training-20215/french-slides.html) |
| Part 2 - Data Files    | [Download data-outputs.zip](data-outputs.zip) |
| Part 2 - Slides        | [Dashboarding Slides](https://path-global-health.github.io/ammnet-training-20215/dashboarding-slides.html#/title-slide) | 
| Part 2 - Worksheet     | [Dashboard Workshop Materials]([dashboard-workshop-materials.qmd](https://path-global-health.github.io/ammnet-training-20215/dashboard-workshop-materials.html)) | 
