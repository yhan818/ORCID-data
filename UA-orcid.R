###### Retrieving institutional ORCIDs and related employee data from ORCID public API #######
#### Author: Yan Han 
#### Updated: 2023-04-20
#### Version: 0.10
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
orcid_client_id <- ""      # your client ID from ORCID
orcid_client_secret <- ""  # your secret from ORCID

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
ORCID_TOKEN="c1a8a2ed-3fd7-4503-bf30-f73f23b00aa0" 

# confirm this by calling orcid_auth(), and it will print the token
# This shall match the ORCID_TOKEN
rorcid::orcid_auth()


#####################################
# Testing data. Change to 
testing_family_name <- 'carberry'
testing_email = 'yhan@email.arizona.edu'
testing_orcid= "0000-0001-9518-2684" 

### testing
testing1 <- rorcid::orcid_search(family_name = testing_family_name)
testing1

# Testing how quick public records get updated. I made it public and it took <= 1 hr 
# yhan ORCID: 0000-0001-9518-2684
testing2 <- rorcid::orcid_search(email = testing_email)
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
organization_name <- "University of Arizona" 
grid_id <- "grid.134563.6"                 
ror_id  <- "https://ror.org/03m2x1q45"       

# query with institution's GRID and ROR IDs.
institution_query <- glue('ringgold-org-id:', ringgold_id, 
                  ' OR grid-org-id:', grid_id, 
                  ' OR ror-org-id:"', ror_id,  
                  ' OR email:*:"', email_domain, 
                  ' OR affiliation-org-name:"', organization_name, '"')
institution_query
orcid_count <- base::attr(rorcid::orcid(query = institution_query), "found")

# create the page vector
results_pages <- seq(from = 0, to = orcid_count, by = 200)

# get the ORCID iDs
ua_orcids <- purrr::map(
  results_pages,
  function(page) {
    print(page)
    ua_orcids <- rorcid::orcid(query = institution_query,
                               rows = 200,
                               start = page)
    return(ua_orcids)
  })

### 2023-04-18: 7698 obs. 3 variables
### 2023-04-13: 7683 obs. 3 variables (orcid_identifier_uri, orcid_identifier_path, orcid_identifier_host)
### 2023-03-22: 7634 results. 
##### To verify if a user exist. use "curl -iL -H 'Accept: application/xml' https://orcid.org/0000-0000-0000-0000" 
##### Yan Han ORCID https://orcid.org/0000-0001-9518-2684
# Map the ORCID iDs into a single tibble
ua_orcids_data <- ua_orcids %>%
  map_dfr(., as_tibble) %>%    # map_dfr r for rows, map_dfc() for c for columns
  janitor::clean_names()

getwd()
setwd("/home/yhan/Documents/UA-datasets/UA-ORCID")
write_csv(ua_orcids_data, "UA_orcids_data_20230418.csv") 

#############################################################
# get employment data -----------------------------------------------------

# If testing, using the first 100 records [1:100] 
# measure the time of first 100 records. about 13 seconds to complete
system.time( ua_employment <- rorcid::orcid_employments(ua_orcids_data$orcid_identifier_path[1:100]) )

# Be Patient: 2023-04-18: 7698 obs. It should be 77x of 100 time of records. Real running time: 1,473 seconds
system.time ( ua_employment <- rorcid::orcid_employments(ua_orcids_data$orcid_identifier_path) )

ua_employment <-my_employment
View(ua_employment)

getwd()
write_json(ua_employment, "ua_employment.json")

# Read it back in
# ua_employment <- read_json("ua_employment.json", simplifyVector = TRUE)

# extract the employment data "affiliation-group: summaries" and mutate the dates using anytime package
# see ua_employment data for its structure. 
ua_employment_data <- ua_employment %>%
  purrr::map(., purrr::pluck, "affiliation-group", "summaries") %>% 
  purrr::flatten_dfr() %>%
  janitor::clean_names() %>%
  dplyr::mutate(employment_summary_end_date = anytime::anydate(employment_summary_end_date/1000),
                employment_summary_created_date_value = anytime::anydate(employment_summary_created_date_value/1000),
                employment_summary_last_modified_date_value = anytime::anydate(employment_summary_last_modified_date_value/1000))

View(ua_employment_data)

# clean up the column names
names(ua_employment_data) <- names(ua_employment_data) %>%
  stringr::str_replace(., "employment_summary_", "") %>%
  stringr::str_replace(., "source_source_", "") %>%
  stringr::str_replace(., "organization_disambiguated_", "")

# view the unique institutions in the organization names columns
# keep in mind this will include all institutions a person has in employments section, NOT just U. of Arizona
# 5463 objs.
organizations <- ua_employment_data %>%
  group_by(organization_name) %>%
  count() %>%
  arrange(desc(n))

# you can also filter it with a keyword or set of keywords.
# If you are adding more than one keyword, separate them by a pipe (|)
# 2023-04-20: "University of Arizona" generated 129 obs, use regular expression generated 138 obs..
# Double check to see if all of them are UA units! 
str_pattern <- "(?i).*University of Arizona.*"  

ua_units_filtered <- organizations %>%
  filter(str_detect(organization_name, str_pattern) )

# Filter again to employees specifically stated that specific unit(s) containing keyword: library, libraries
# Note: This filter removes people who do not state "library or libraries", but entered "University of Arizona". 

str_pattern2 <- "(?i).*Librar.*"
ua_units_ual_filtered <- ua_units_filtered %>%
  filter(str_detect(organization_name, str_pattern2) )   

# filter the dataset to include only the institutions you want. 
# There may be different variants depending on if the person hand-entered the data. 
# Check the ua_units_filtered, one person can have multiple employment records in different years.
# 2023-04-20: 2,828 obs. 

ua_employment_data_filtered <- ua_employment_data 
  # %>% dplyr::filter(organization_name %in% ua_units_filtered$organization_name[c(1)])

# filter to include only people who have NA as the end date: current employee
# It filters current employee who has previous employee in different rank end_date_year_value (e.g. 2019). 
# 2023-04-20: 1,546 obs.
ua_employment_data_filtered_current <- ua_employment_data_filtered %>%
  dplyr::filter(is.na(end_date_year_value))

# UA-UAL
ua_ual_employment_data_filtered <- ua_employment_data %>%
  dplyr::filter(organization_name %in% ua_units_ual_filtered$organization_name[c(1)])

rm(ua_units_ual_re_filtered)

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
ua_current_employment_all <- ua_employment_data_filtered_current %>%
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

# Testing a specific ORCID existing 
is.data.frame(ua_current_employment_all)
testing_orcid_row <- ua_current_employment_all[ua_current_employment_all$orcid_identifier == testing_orcid, ]
testing_orcid_row


ua_unique_orcids <-ua_current_employment_all$orcid_identifier 

# find the indices of the duplicated values
dupes <- ua_current_employment_all$orcid_identifier[duplicated(ua_current_employment_all$orcid_identifier)]
cat("Duplicate values in orcid_identifier:" , paste(dupes)) 

# 973 dupes
sum(duplicated(ua_current_employment_all$orcid_identifier))


# create a new vector unique_orcids that includes only unique ORCID iDs from the filtered 
# Bug?: cannot perform unique(), because someone (like yhan) may have multiple employment records with this dataframe. 
# If unique() them, it may select the first one with end date value is not "null". This will be eliminated when filtering out. 
# In UA case, applying unique() reduce 7146 to 6,173
unique_orcids <-  unique(ua_current_employment_all$orcid_identifier) %>% na.omit(.) %>% as.character()

# find out the duplicated ORCIDs
duplicated_orcids <- duplicated(ua_current_employment_all$orcid_identifier) | duplicated(ua_current_employment_all$orcid_identifier, fromLast = TRUE)
ua_current_employment_all$orcid_identifier[duplicated_orcids]

# Testing
which (unique_orcids==testing_orcid )
which (ua_unique_orcids == testing_orcid)

# Get all the information from those ORCID. syntax orcid_person(orcid, details = FALSE).
# This processed 10 records/sec. So it may take longer time. ~120 seconds for the dataset. 1,475 elements (list) 
# unique_orcids 6,173 elements. took 7-10 minutes to complete
# ua_orcid_person 6,173 obs.

ua_unique_orcid_person <- rorcid::orcid_person(ua_unique_orcids)
testing_orcid
testing_orcid %in% ua_unique_orcid_person # ?? False

# ua_orcid_person <- rorcid::orcid_person(unique_orcids)

yhan_orcid <-rorcid::orcid_person("0000-0001-9518-2684")
yhan_orcid

# then we construct a data frame from the response. 
# See more at https://ciakovx.github.io/rorcid.html#Getting_the_data_into_a_data_frame for this.

#### using ua_unique_orcid_person
ua_orcid_person_data2 <- ua_unique_orcid_person %>% {
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
orcid_person_employment_join2 <- ua_orcid_person_data2 %>%
  left_join(ua_current_employment_all, by = c("orcid_identifier_path" = "orcid_identifier"))

ua_orcid_person_data <- ua_orcid_person %>% {
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
orcid_person_employment_join <- ua_orcid_person_data %>%
  left_join(current_employment_all, by = c("orcid_identifier_path" = "orcid_identifier"))

orcid_person_final <- orcid_person_employment_join[!duplicated(orcid_person_employment_join$orcid_identifier_path),]

# orcid_person_final_filtered <- orcid_person_final[subset(orcid_person_final, orcid_person_final$department_name=="")]
# Filter rows where the 'department_name' column contains the string "Marketing"
orcid_person_final_filtered <- orcid_person_final[grepl("Librar*", orcid_person_final$department_name), ignore.case= TRUE]

# Print the filtered dataframe
print(orcid_person_final_filtered)

# now you can write this file to a CSV. only 1525 records left out of 7,634 ORCID
getwd()
write_csv(orcid_person_final, "ua_orcid_employment_file.csv")

