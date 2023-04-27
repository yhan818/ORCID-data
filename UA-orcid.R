###### Retrieving institutional ORCIDs and related employee data from ORCID public API #######
#### Author: Yan Han 
#### Updated: 2023-04-26
#### Version: 0.20
#### Note: Some portion of the code is based on FSCI 2022 ession by Clarke Lakovakis.

# Install and load packages -----------------------------------------------
install.packages('dplyr')
install.packages('tibble')
install.packages('tidyr')
install.packages('purrr')
install.packages('readr')
install.packages('stringr')
install.packages('jsonlite')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('httr')
install.packages('forcats')
install.packages('rorcid')
install.packages('usethis')
install.packages('anytime')
install.packages('janitor')
install.packages('glue')
install.packages('remotes')
remotes::install_github("ropensci/rcrossref")
install.packages('roadoi')
install.packages('inops')

# load the packages
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(httr)
library(forcats)
library(usethis)
library(anytime)
library(janitor)
library(glue)
library(rorcid)
library(rcrossref)
library(roadoi)
library(inops)

library(testthat)

# Set up orcid 
# https://www.orcid.org in the Your Website URL field, “Getting public API key” in Description field 
# Registering under "Developer tools" is required to getting public API. Read the next 2 tutorials is highly recommend to avoid authentication "401" errors
# Tutorial1: https://ciakovx.github.io/rorcid.html 
# Tutorial2: https://info.orcid.org/ufaqs/how-do-i-register-a-public-api-client/
# If you get error messages, you need to check: 
# 1. "no ORCID token found; attempting OAuth authentication", meaning that you have wrong token
# 2. "Error: Unauthorized (HTTP 401). - 401 Unauthorized: The client application is not authorized for this ORCID record. Full validation error: Access token is for a different record"
# solution to 2. go to your ORCID's "Developer tools" to "Reset client secret". 

# Replacing orcid_client_id and orcid_client_secret with yours. Both can be found at your ORCID's "Developer tools"  

orcid_client_id <- " "      # your client ID from ORCID
orcid_client_secret <- " "  # your secret from ORCID

orcid_request <- POST(url  = "https://orcid.org/oauth/token",
                      config = add_headers(`Accept` = "application/json",
                                           `Content-Type` = "application/x-www-form-urlencoded"),
                      body = list(grant_type = "client_credentials",
                                  scope = "/read-public",
                                  client_id = orcid_client_id,
                                  client_secret = orcid_client_secret),
                      encode = "form")

# parse the API request with content
orcid_response <- content(orcid_request)

print(orcid_response$access_token)

# save to the R env. 
usethis::edit_r_environ()

# This TOKEN will be refreshed every time. 
ORCID_TOKEN="c9e7b108-e092-42af-a9e5-8fee6b4c3e6f"   #"c1a8a2ed-3fd7-4503-bf30-f73f23b00aa0" 

# confirm this by calling orcid_auth(), and it will print the token
# This shall match the ORCID_TOKEN
rorcid::orcid_auth()


#####################################
# Testing data 
testing_family_name1 <- 'carberry'
testing_email1 = 'yhan@email.arizona.edu'

testing_orcid1= "0000-0001-9518-2684"  # not sure why he was not included at later stage.
testing_orcid2="0000-0003-4558-9712"  # not sure why she was not included

### testing: You shall see a tibble 10x3: Josiah Carberry to Benjamin Carberry
testing1 <- rorcid::orcid_search(family_name = testing_family_name1)
testing1

# Testing how quick public records get updated. I made it public and it took <= 1 hr 
# You shall see a tibble 1x3 with yhan ORCID: 0000-0001-9518-2684
testing2 <- rorcid::orcid_search(email = testing_email1)
testing2

# see ORCID fields
View(rorcid:::field_match_list)

###########################################

# rorcid manual: https://cran.r-project.org/web/packages/rorcid/rorcid.pdf (pg. 22 for orcid_employment func)
# rorcid manual: https://cran.r-project.org/web/packages/rorcid/index.html 
# Build the query: at lease use ringgold_id, email_domain and organization_name 
# some people's email setting is not public. Organization_name might have variant. 
ringgold_id <- "8041"      
email_domain <- "@arizona.edu" 
email_domain2 <- "@email.arizona.edu"
organization_name <- "University of Arizona" 
grid_id <- "grid.134563.6"                 
ror_id  <- "https://ror.org/03m2x1q45"       

# Query with institution's RINGGOLD, GRID, ROR IDs, organization_name, email. 
# Per ORCID, "Employment" and "Education" data are coded with organization identifiers as RINGGOLD
# Query string: ringgold-org-id:8041 OR grid-org-id:grid.134563.6 OR ror-org-id:"https://ror.org/03m2x1q45" OR affiliation-org-name:"University of Arizona" OR email:*@arizona.edu OR email:*@email.arizona.edu
# The following results were performed on 2023-04-25. Your numbers may vary (usually larger)
# --results: 7,733
# Query string: ringgold-org-id:8041 OR grid-org-id:grid.134563.6 OR ror-org-id:"https://ror.org/03m2x1q45" OR email:*@arizona.edu
# -- results:  5,746
# Query string: ringgold-org-id:8041 OR grid-org-id:grid.134563.6 OR ror-org-id:"https://ror.org/03m2x1q45"
# -- results: 5,692
# Query string: ringgold-org-id:8041
# -- results: 4617
# Query string: grid-org-id:grid.134563.6
# -- result: 576
# Query string: ror-org-id:"https://ror.org/03m2x1q45"
# -- result: 617
# Query string: affiliation-org-name:"University of Arizona"
# -- result: 7,678
# Query string: email:*@arizona.edu
# -- result: 112 
# Query string: email:*@arizona.edu OR email:*@email.arizona.edu
# -- result: 211 
institution_query <- glue('ringgold-org-id:', ringgold_id, 
                          ' OR grid-org-id:', grid_id, 
                          ' OR ror-org-id:"', ror_id,  '"',
                          ' OR affiliation-org-name:"', organization_name, '"',
                          ' OR email:*', email_domain,
                          ' OR email:*', email_domain2,
                      )
institution_query 

##### Use the most accurate query string: You can construct different query to fit your search goals. 
# For example, remove affiliation-org-name, because someone may graduate from Univ of Arizona or past employment history. 
# For example, query how many use emails to be publicly seen ( I have yhan@email.arizona.edu and yhan@arizona.edu registered, but yhan@arizona.edu is hidden from public view. 
# Query string: ringgold-org-id:8041 OR grid-org-id:grid.134563.6 OR ror-org-id:"https://ror.org/03m2x1q45" OR email:*@arizona.edu OR email:*@email.arizona.edu
# -- results: 5,781
# Give most accurate result??? 

orcids_count <- base::attr(rorcid::orcid(query = institution_query), "found")

# create the page vector
results_pages <- seq(from = 0, to = orcids_count, by = 400)

# get the ORCID iDs
org_orcids <- purrr::map(
  results_pages,
  function(page) {
    print(page)
    org_orcids <- rorcid::orcid(query = institution_query,
                               rows = 400,
                               start = page)
    return(org_orcids)
  })

# Map the ORCID iDs into a single tibble
org_orcids_data <- org_orcids %>%
  map_dfr(., as_tibble) %>%    # map_dfr r for rows, map_dfc() for c for columns
  janitor::clean_names()

# Testing if testing data are in the ORCID data
testing_func1 <- function(orcid, y) {
  orcid %in% y
}

testing_func(org_orcids_data$orcid_identifier_path)

test_that("testing_func1 returns TRUE", {
  output <- testing_func1(testing_orcid1, org_orcids_data$orcid_identifier_path)
  expect_equal(output, TRUE)
}) 
test_that("testing_func1 returns TRUE", {
  output <- testing_func1(testing_orcid2, org_orcids_data$orcid_identifier_path)
  expect_equal(output, TRUE)
}) 


getwd()
setwd("/home/yhan/Documents/UA-data/ORCID-data")
currentDate <-Sys.Date()
orcids_csv_filename <-paste("org_orcids_data_",currentDate ,".csv", sep="")
write_csv(org_orcids_data, orcids_csv_filename) 

# find out the difference of two ORCID CSV files created in a different date
orcids_csv1 <-read.csv("org_orcids_data_2023-04-24.csv")
orcids_csv2 <-read.csv("org_orcids_data_2023-04-26.csv")
new_orcid_diff <-anti_join(orcids_csv2, orcids_csv1, by ="orcid_identifier_path")
new_orcid_diff

#############################################################
# get employment data -----------------------------------------------------

# If testing, using the first 100 records [1:100] 
# measure the time of first 100 records. about 13 seconds to complete
# Here, you can replace rorcid::orcid_employments to other functions to retrieve info. see https://cran.r-project.org/web/packages/rorcid/rorcid.pdf
system.time( org_employment <- rorcid::orcid_employments(org_orcids_data$orcid_identifier_path) )

# Be Patient: 2023-04-18: 7698 obs. It should be 77x of 100 time of records. Real running time: 1,473 seconds
#system.time ( ua_employment <- rorcid::orcid_employments(ua_orcids_data$orcid_identifier_path) )

View(org_employment)

getwd()
org_employment_json_filename <- paste("org_employment_",currentDate, ".json", sep="")
write_json(org_employment, org_employment_json_filename)

# Read it back in
# org_employment <- read_json("org_employment_<date>.json", simplifyVector = TRUE)

# extract the employment data "affiliation-group: summaries" and mutate the dates using anytime package
# see ua_employment data for its structure. 
org_employment_data <- org_employment %>%
  purrr::map(., purrr::pluck, "affiliation-group", "summaries") %>% 
  purrr::flatten_dfr() %>%
  janitor::clean_names() %>%
  dplyr::mutate(employment_summary_end_date = anytime::anydate(employment_summary_end_date/1000),
                employment_summary_created_date_value = anytime::anydate(employment_summary_created_date_value/1000),
                employment_summary_last_modified_date_value = anytime::anydate(employment_summary_last_modified_date_value/1000))

# clean up the column names
# names(org_employment_data) <- names(org_employment_data) %>%
#    stringr::str_replace(., "employment_summary_", "") %>%
#    stringr::str_replace(., "source_source_", "") %>%
#    stringr::str_replace(., "organization_disambiguated_", "")

View(org_employment_data)

#### Testing again 
test_that("testing_orcid returns TRUE", {
  output <- testing_func1(testing_orcid1, org_employment_data$orcid_path)
  expect_equal(output, TRUE)
}) 
test_that("testing_orcid returns TRUE", {
  output <- testing_func1(testing_orcid2, org_employment_data$orcid_path)
  expect_equal(output, TRUE)
}) 


# view the unique institutions in the organization names columns
# keep in mind this will include all institutions a person has in employments section, NOT just U. of Arizona
organizations <- org_employment_data %>%
  group_by(organization_name) %>%
  count() %>%
  arrange(desc(n))

# you can also filter it with a keyword or set of keywords.
# If you are adding more than one keyword, separate them by a pipe (|)
# 2023-04-20: "University of Arizona" generated 129 obs, use regular expression generated 138 obs..
# Double check to see if all of them are UA units! 
str_pattern <- "(?i).*University of Arizona.*"  

### Filter any organization containing 'organization_name'. This shall accurately reflects
orgs_filtered <- organizations %>%
  filter(str_detect(organization_name, str_pattern) ) 

# If you have units within the institution. You can filter again to employees specifically stated that specific unit(s) containing keyword: library, libraries
# Note: This filter removes people who do not state "library or libraries", but entered "University of Arizona". 

str_pattern2 <- "(?i).*Librar.*"
orgs_sub_filtered <- orgs_filtered %>%
  filter(str_detect(organization_name, str_pattern2) )   

# filter the dataset to include only the institutions you wanted based on the column's name "employment_summary_organization_name".
# There may be different variants depending on if the person hand-entered the data. 
# Check the ua_units_filtered, one person can have multiple employment records in different years.
# 2023-04-20: 2,828 obs. 

########################################################3
colnames(org_employment_data)

# ua_employment_data_filtered <- org_employment_data %>% dplyr::filter(organization_name %in% orgs_filtered$organization_name[c(1)])

# 2023-04-26: 3,965 obs.
org_employment_data_filtered <- org_employment_data %>% filter (grepl(organization_name, employment_summary_organization_name))

# filter to include only people who have NA as the end date: current employee
# It filters current employee who has previous employee in different rank end_date_year_value (e.g. 2019). 
# 2023-04-20: 1,546 obs with Clarke's code. 
# 2023-04-26: 2,193 obs with my code
org_employment_data_filtered_current <- org_employment_data_filtered %>% dplyr::filter(is.na(employment_summary_end_date_year_value))

#org_employment_data_filtered_current <- org_employment_data_filtered %>%dplyr::filter(is.na(end_date_year_value))

# sub unit of the organization
org_sub_employment_data_filtered <- org_employment_data_filtered_current %>%
  dplyr::filter(organization_name %in% orgs_sub_filtered$organization_name[c(1)])


#################################################################333
# note that this will give you employment records ONLY. 
# In other words, each row represents a single employment record for an individual.
# the name_value variable refers specifically to the name of the person or system
# that wrote the record, NOT the name of the individual. 

# To get that, you must first get all the unique ORCID iDs from the dataset:

# There is actually no distinct value identifying the orcid ID of the person.
# The orcid_path value corresponds to the path of the person who added the employment record (which is usually, but not always the same)
# Therefore you have to strip out the ORCID iD from the 'path' variable first and put it in it's own value and use it
# 7,146 obs.

names(org_employment_data_filtered_current) <- names(org_employment_data_filtered_current) %>%
    stringr::str_replace(., "employment_summary_", "") %>%
    stringr::str_replace(., "source_source_", "") %>%
    stringr::str_replace(., "organization_disambiguated_", "")

org_current_employment_all <- org_employment_data_filtered_current %>%
  mutate(orcid_identifier = str_sub(path, 2, 20)) %>%
  select(any_of(c("orcid_identifier",
                  "organization_name",
                  "organization_address_city",
                  "organization_address_region",
                  "organization_address_country",
                  "organization_identifier",
                  "organization_disambiguated_organization_identifier",
                  "organization_disambiguation_source",
                  "department_name",
                  "role_title",
                  "url_value",
                  "display_index",
                  "visibility",
                  "created_date_value",
                  "start_date_year_value",
                  "start_date_month_value",
                  "start_date_day_value",
                  "end_date_year_value",
                  "end_date_month_value",
                  "end_date_day_value")))

################### Testing again with Org_current_employment
test_that("testing_func1 returns TRUE", {
  output <- testing_func1(testing_orcid1, org_current_employment_all$orcid_identifier)
  expect_equal(output, TRUE)
}) 
test_that("testing_func2 returns TRUE", {
  output <- testing_func1(testing_orcid2, org_current_employment_all$orcid_identifier)
  expect_equal(output, TRUE)
}) 


org_unique_orcids <-org_current_employment_all$orcid_identifier 

# find the duplicated values
dupes <- org_current_employment_all$orcid_identifier[duplicated(org_current_employment_all$orcid_identifier)]
cat("Duplicate values in orcid_identifier:" , paste(dupes)) 

sum(duplicated(org_current_employment_all$orcid_identifier))

##################################### Accurate So far #################

# create a new vector unique_orcids that includes only unique ORCID iDs from the filtered 
# Bug?: cannot perform unique(), because someone (like yhan) may have multiple employment records with this dataframe. 
# If unique() them, it may select the first one with end date value is not "null". This will be eliminated when filtering out. 
org_unique_orcids <-  unique(org_current_employment_all$orcid_identifier) %>% na.omit(.) %>% as.character()

# find out the duplicated ORCIDs
duplicated_orcids <- duplicated(org_current_employment_all$orcid_identifier) | duplicated(org_current_employment_all$orcid_identifier, fromLast = TRUE)
org_current_employment_all$orcid_identifier[duplicated_orcids]


# Get all the information from those ORCID. syntax orcid_person(orcid, details = FALSE).
# This processed 10 records/sec. So it may take longer time. ~120 seconds for the dataset. 1,475 elements (list) 


system.time ( org_unique_orcid_person <- rorcid::orcid_person(org_unique_orcids) )
testing_orcid
testing_orcid %in% ua_unique_orcid_person # ?? False

yhan_orcid <-rorcid::orcid_person("0000-0001-9518-2684")
yhan_orcid

# then we construct a data frame from the response. 
# See more at https://ciakovx.github.io/rorcid.html#Getting_the_data_into_a_data_frame for this.

#### using ua_unique_orcid_person
org_orcid_person_data2 <- org_unique_orcid_person %>% {
  dplyr::tibble(
    given_name = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    created_date = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_integer_),
    last_modified_date = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_character_),
    family_name = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    credit_name = purrr::map_chr(., purrr::pluck, "name", "credit-name", "value", .default=NA_character_),
    other_names = purrr::map(., purrr::pluck, "other-names", "other-name", "content", .default=NA_character_),
    orcid_identifier_path = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
    biography = purrr::map_chr(., purrr::pluck, "biography", "content", .default=NA_character_),
    researcher_urls = purrr::map(., purrr::pluck, "researcher-urls", "researcher-url", .default=NA_character_),
    emails = purrr::map(., purrr::pluck, "emails", "email", "email", .default=NA_character_),
    keywords = purrr::map(., purrr::pluck, "keywords", "keyword", "content", .default=NA_character_),
    external_ids = purrr::map(., purrr::pluck, "external-identifiers", "external-identifier", .default=NA_character_))
} %>%
  dplyr::mutate(created_date = anytime::anydate(as.double(created_date)/1000),
                last_modified_date = anytime::anydate(as.double(last_modified_date)/1000))

# Join it back with the employment records
orcid_person_employment_join <- org_orcid_person_data2 %>%
  left_join(org_current_employment_all, by = c("orcid_identifier_path" = "orcid_identifier"))


orcid_person_final <- orcid_person_employment_join[!duplicated(orcid_person_employment_join$orcid_identifier_path),]

# orcid_person_final_filtered <- orcid_person_final[subset(orcid_person_final, orcid_person_final$department_name=="")]
# Filter rows where the 'department_name' column contains the string "Marketing"
orcid_person_final_filtered <- orcid_person_final[grepl("Librar*", orcid_person_final$department_name), ignore.case= TRUE]

# Print the filtered dataframe
print(orcid_person_final_filtered)

# now you can write this file to a CSV. only 1525 records left out of 7,634 ORCID
getwd()
write_csv(orcid_person_final, "ua_orcid_employment_file.csv")

