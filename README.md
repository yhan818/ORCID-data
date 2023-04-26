# ORCID-data, updated code for getting any institution's ORCIDs and related data such as affiliation (e.g. employment, education, membership) 

ORCID API tutorials are available at https://info.orcid.org/documentation/api-tutorials/ 
Some portion of the code is from FSCI 2022 session by Clarke Lakovakis. Major changes include
- Debug and modify certain algorithm and workflow to ensure correct data pull. 
- Multiple testing cases
- Measure runing times of ORCID API data pulls. Depending on the number of records via ORCID, an API call with 7,700 records could take 30 minutes, which you will not see any system response. 

