####################################################################################
# This script combines internal and external sources of data on CSU education
# organizations to create an 'ed org metadata' file for use in the CSU TPDM implementation
#
# S. Kolbe
####################################################################################

####################################################################################
# Set up
####################################################################################

library(tidyverse)
library(magrittr)

options(stringsAsFactors = FALSE)
options(scipen = 999)

####################################################################################
# Load data
####################################################################################

# Internal data
core <- read.csv("source-data-metadata/Handcoded/CSU ed org handcoded data v1.3.csv")

# External data
ipeds <- read.csv("source-data-metadata/NCES IPEDS/hd2018.csv")
non_ipeds_addresses <- read.csv("source-data-metadata/addresses not in ipeds.csv")

####################################################################################
# Add addresses to each ed org type (network, EPP, and CalStateTEACH. 
# There are different source fields and linking variables for each type of org.
####################################################################################

core_ntwk <- filter(core, orgCategory == "University")
core_epp <- filter(core, orgCategory == "Educator Preparation Provider" & orgName != "CalStateTEACH")
core_cst <- filter(core, orgName == "CalStateTEACH")

addr_ntwk <- filter(non_ipeds_addresses, orgCodeNetwork == 999) %>%
  select(., -orgName)
addr_epp <- ipeds %>%
  select(., UNITID, EIN, OPEID, 
         ADDR, CITY, STABBR, ZIP, COUNTYCD, COUNTYNM, LONGITUD, LATITUDE, LOCALE, GENTELE, WEBADDR,
         INSTSIZE) %>%
  rename_all(tolower) %>%
  rename(., longitude = longitud, orgCodeEpp = unitid) %>%
  mutate(., addr2 = "")
addr_cst <- filter(non_ipeds_addresses, orgName == "CalStateTEACH") %>%
  select(., -orgCodeNetwork)

ntwk <- left_join(core_ntwk, addr_ntwk, by = "orgCodeNetwork")
epp <- left_join(core_epp, addr_epp, by = "orgCodeEpp")
cst <- left_join(core_cst, addr_cst, by = "orgName")

all <- rbind(ntwk, epp, cst) %>%
  select(., orgCategory, parentUniversityIdentifier, 
         orgNameAlternative, orgName, nameOfInstitution,
         orgCodeIPEDS, orgCodeERS, orgCodeTitle2, orgCodeCOSAR, orgCodeEDQ, orgAbbvEDQ, orgCodeEIN, orgCodeOPE,
         addr, addr2, city, stabbr, zip, countyFIPS, countynm, longitude, latitude, locale, gentele, webaddr,
         instsize,
         eppAddr, eppAddr2, eppCity, eppStabbr, eppZip, eppGentele, eppWebaddr)

date_string <- format(Sys.Date(), "%y%m%d")

write.csv(all, paste0("output/education_organization_metadata_", date_string, ".csv"), row.names = FALSE)
