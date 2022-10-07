library(tidyverse)
library(readxl)
library(sf)

# Takes geojson files from geoportal.statistic.gov.uk for lsoa11, msoa11, wd21 and lad21, as well as lookups (including House of Commons Library MSOA names) and creates objects for each with LA names included so they can be filtered by it in the app to avoid drawing geometries that aren't used.

# build lsoa11

lsoa11 <- sf::read_sf("~/Data/Geodata/Boundaries/lsoa11_bgc.geojson")
lsoa11$MSOA11NM <- substr(lsoa11$LSOA11NM, 1, nchar(lsoa11$LSOA11NM) - 1)
MSOA11_WD21_LAD21_EW_LU <- read_excel("~/Data/Geodata/Lookups/MSOA11_WD21_LAD21_EW_LU.xlsx")
lsoa11 <- left_join(lsoa11, MSOA11_WD21_LAD21_EW_LU,
                    by = c("MSOA11NM"))

saveRDS(lsoa11, "Geography-Selector/lsoa11.rds")

# build msoa11

msoa11 <- sf::read_sf("~/Data/Geodata/Boundaries/msoa11_bgc.geojson")
msoa11 <- left_join(msoa11, MSOA11_WD21_LAD21_EW_LU,
                    by = c("MSOA11CD", "MSOA11NM"))
hocl_msoa <- read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest.csv")
msoa11 <- left_join(msoa11, hocl_msoa,
                    by = c("MSOA11CD" = "msoa11cd"))

saveRDS(msoa11, "Geography-Selector/msoa11.rds")

# build wd21

wd21 <- sf::read_sf("~/Data/geodata/Boundaries/wd21_bgc.geojson")
WD21_LAD21_UK_LU <- read_excel("~/Data/Geodata/Lookups/WD21_LAD21_UK_LU.xlsx")
WD21_LAD21_CTY21_RGN21_CTRY21 <- read_csv("~/Data/Geodata/Lookups/WD21_LAD21_CTY21_RGN21_CTRY21.csv")
wd21 <- left_join(wd21, WD21_LAD21_CTY21_RGN21_CTRY21, by = c("WD21CD"))

saveRDS(wd21, "Geography-Selector/wd21.rds")
