# Data Cleaning

# In this script, we will explore and clean that data that we pulled from DHIS2.
# At the end, we will create a new dataset that is cleaned and ready for analysis, 
# and at be used in our Shiny app.
# 
# We will not have time to cover each of these steps individually, please refer to 
# the slides for more details and links to additional resources.
# 
# Before you start, be sure that you have the following files in your working directory:
# 
# - data-inputs/data_dhis2_laos_ammnet.csv
# - data-inputs/adm1_shapefile.shp
# 
#  And that you have installed the packages in the setup script (00_setup.r).


# Step 1: Read in the data, clean the names and look at the data
# Load the packages
library(tidyverse)  # data wrangling
library(janitor)    # data cleaning/formatting

laos_dat <- read_csv("data-inputs/data_dhis2_laos_ammnet.csv") %>%  
	clean_names()  # this function will clean the column names

# View the data
head(laos_dat)

# Step 2: Clean and reshape the data
# More details on this step can be found in the slides
laos_dat_clean <- laos_dat %>% 
  # Remove national level data
  filter(organisation_unit != "Lao PDR") %>% 
  # Remove unused columns
  dplyr::select(-c(numerator, denominator, factor, multiplier, divisor)) %>% 
  # Clean column names
  mutate(
    data = str_replace_all(data, "MAL - ", ""), # remove starting MAL key
    data = str_to_lower(data),                  # lower case
    data = str_replace_all(data, " ", "_"),     # spaces to underscores
    data = str_remove_all(data, "[()]"),        # remove parenthesis
    data = str_remove_all(data, "[+]")) %>%     # remove plus sign in one variable name 
  # Make month a date variable
  mutate(period = ymd(parse_date_time(period, orders = "B Y"))) 

# Inspect cleaned data
head(laos_dat_clean)

# Step 3: Inspect the data using visualizations
# Inspected unique data values
sort(unique(laos_dat$data))

#' Check for consistency
#' We expect that the total confirmed cases should each the sum of the 
#' microscopy and RDT cases 
#' 
#'   "MAL - Confirmed malaria cases (micr + RDT)" = 
#'   “MAL - Microscopy positive malaria cases” + 
#'   "MAL - RDT positive malaria cases" 
#' 
#' and the total confirmed tests should be the sum
#' of the microscopy and RDT tests.
#' 
#'   “MAL - Malaria cases tested (total)” = 
#'   “MAL - Malaria suspects tested (RDT)” + 
#'   “MAL - Malaria suspects tested with microscopy
#' 
#' We will use this information to check that the data is consistent.
#' Note that we have corrected these names in the cleaned dataset

laos_dat_wide <- laos_dat_clean %>% 	
  # Pivot the data to wide format (for plotting)
	pivot_wider(names_from = data) %>%   
  # Calculate the total confirmed cases and tests
	mutate(
    test_calc_check = rowSums(across(c(
      malaria_suspects_tested_rdt, 
      malaria_suspects_tested_with_microscopy)), na.rm = TRUE), 
    cases_calc_check = rowSums(across(c(
      rdt_positive_malaria_cases,
      microscopy_positive_malaria_cases))))

# Plot the data, if they are consistent then all points should fall on the 1:1 
# line (in red)
ggplot(laos_dat_wide) +  
	geom_point(aes(
    x = malaria_cases_tested_total, 
    y = test_calc_check)) +  	
	geom_abline(intercept = 0, slope = 1, color = "red")

# The tests data looks consistent, now let's check the case data
ggplot(laos_dat_wide) +
  geom_point(aes(x = confirmed_malaria_cases, y = cases_calc_check)) +
  geom_abline(intercept = 0, slope = 1, color = "red")

#' In this data analysts, need to make a decision about which variable to use. 
#'   Option 1: ask a local expert
#'   Option 2: make an educated decision and document that decision so you can 
#'             explain your results transparently in the future.
#' 
#' For this analysis, we will use confirmed malaria cases (as we have spoken to 
#' a Laos surveillance officer who reported that in some setting individuals are
#' tested with both RDT and microscopy which can lead to double counting of cases)

# Check for missing data
laos_dat_wide %>% filter(is.na(confirmed_malaria_cases_micr__rdt))
laos_dat_wide %>% filter(is.na(cases_calc_check))

# Check for unique values in updated dataframe
unique(laos_dat_clean$data)

# Step 4: Prepare final output data
# make a final clean version, selecting just the variable we want,
# making shorter/clearer names, and only selecting the columns we want
laos_dat_clean2 <- laos_dat_clean %>% 
  filter(data %in% c("malaria_cases_tested_total",
                     "confirmed_malaria_cases",
                     "confirmed_malaria_cases_per_1000")) %>% 
  mutate(var_name = case_when(
    data == "malaria_cases_tested_total" ~ "malaria_tests",
    data == "confirmed_malaria_cases" ~ "malaria_cases",
    data == "confirmed_malaria_cases_per_1000" ~ "malaria_cases_per_1000")) %>% 
  dplyr::select(organisation_unit, period, var_name, value) %>% 
  arrange(organisation_unit, period)

# plotting the data to look for anomolies/oddities
ggplot(laos_dat_clean2) +
  geom_line(aes(x = period, y = value, color = var_name, group = var_name),
            linewidth = 0.9)+
  facet_wrap(vars(organisation_unit), scale = "free_y") +
  theme_bw()

# plotting the data to look for anomolies/oddities
ggplot(laos_dat_clean2) +
  geom_line(aes(x = period, y = value, color = var_name),
            linewidth = 0.9)+
  facet_wrap(vars(organisation_unit), scale = "free_y") +
  scale_x_date(breaks = scales::pretty_breaks(), 
               date_minor_breaks="1 month", date_labels = "%b") + 
  theme_bw() +
  theme(legend.position = "bottom")

# remove the province with missing data 
laos_dat_final <- laos_dat_clean2 %>% 
  filter(organisation_unit != "09 Xiangkhouang")

# check the final data 
ggplot(laos_dat_final) +
  geom_line(aes(x = period, y = value, color = var_name),
            linewidth = 0.9)+
  facet_wrap(vars(organisation_unit), scale = "free_y") +
  scale_x_date(breaks = scales::pretty_breaks(), 
             date_minor_breaks="1 month", date_labels = "%b") + 
  theme_bw() +
  theme(legend.position = "bottom")

# save the final clean data 
write_csv(laos_dat_final, "data-outputs/data_dhis2_laos_ammnet_cleaned.csv")


