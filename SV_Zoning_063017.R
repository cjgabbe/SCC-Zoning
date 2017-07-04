### SV ZONING PAPER ###

setwd("/Users/charlesgabbe/Google Drive/Research_Projects/Zoning_SiliconValley")

# Libraries of packages to use
library("foreign")
library("psych")
library("dplyr")
library("ggplot2")
library("rgdal")
library("rgeos")
library("maptools")
library("mapdata")
library("ggmap")
library("maps")
library("gmodels")
library("reshape")
library("readstata13")
library("sp")
library("scales")
library("pscl")
library("boot")
library("spdep")
library("ape")
library("corrplot")
library("car")
library("MASS")
library("stargazer")
library("RColorBrewer")
library("lm.beta")
library("lme4")
library("xlsx")
library("nlme")
library("plm")
library("lmtest")
library("sandwich")
library("acs")
library("stringr")
library("tigris")
library("spatialEco")
library("RQGIS")

### GENERAL DATA PREP ###

# Create object for California State Plane Zone III CRS
CSP_Zone3_Proj <- CRS("+init=EPSG:2227")


### SCC PARCELS ###

# Read SCC parcels shapefile
SCCparcels <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "09_01_2015_parcels_SCConly")
summary(SCCparcels) # Check projection of SCC parcels
SCCparcels.proj <- spTransform(SCCparcels, CRSobj = CSP_Zone3_Proj) # Project to California State Plane
remove(SCCparcels)

# Create parcel centroids
SCCparcels_centroids <- gCentroid(SCCparcels.proj, byid=TRUE)


### CENSUS/ACS SPATIAL DATA

# Download SCC block group boundaries
SCCbgs <- block_groups(06, county = 085, cb = FALSE, year = 2015)
summary(SCCbgs)
SCCbgs.proj <- spTransform(SCCbgs, CRSobj = CSP_Zone3_Proj)
remove(SCCbgs)

# Spatial join parcel centroids to BGs
Parcels_Join_BGs <- over(SCCparcels_centroids, SCCbgs.proj)
Parcels_Join_BGs$GEOID_BG <- Parcels_Join_BGs$GEOID #seems like it would be better to do this later, but not sure how to rename columns in spatial data frame
SCCparcels.proj <- spCbind(SCCparcels.proj, Parcels_Join_BGs)
# Remove unnecessary columns
drops <- c("CENTROID", "X", "Y", "STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE", "GEOID", "NAMELSAD", "MTFCC", "FUNCSTAT", "ALAND", "AWATER", "INTPTLAT", "INTPTLON") # list of col names
SCCparcels.proj <- SCCparcels.proj[,!(names(SCCparcels.proj) %in% drops)] #remove columns from above
remove(drops)

# Download SCC Census tract boundaries
SCCtracts <- tracts(06, county = 085, cb = FALSE, year = 2015)
summary(SCCtracts)
SCCtracts.proj <- spTransform(SCCtracts, CRSobj = CSP_Zone3_Proj)
remove(SCCtracts)

# Spatial join parcel centroids to tracts
Parcels_Join_Tracts <- over(SCCparcels_centroids, SCCtracts.proj)
Parcels_Join_Tracts$GEOID_TR <- Parcels_Join_Tracts$GEOID
SCCparcels.proj <- spCbind(SCCparcels.proj, Parcels_Join_Tracts)
# Remove unnecessary columns
drops <- c("STATEFP", "COUNTYFP", "TRACTCE", "GEOID", "NAMELSAD", "MTFCC", "FUNCSTAT", "ALAND", "AWATER", "INTPTLAT", "INTPTLON", "NAME") # list of col names
SCCparcels.proj <- SCCparcels.proj[,!(names(SCCparcels.proj) %in% drops)] #remove columns from above
remove(drops)

# Clean up
remove(SCCbgs.proj, SCCtracts.proj, Parcels_Join_BGs, Parcels_Join_Tracts)


### EPA SMART LOCATION DATABASE ###

# Read EPA SLD dbf
EPASLD <- read.dbf("/Users/charlesgabbe/Google Drive/Research_Projects/MobileHomes_Services/Data/EPA_SLD_2014/SmartLocationDb.dbf")
# Keep and rename the key variables that I need
EPASLD = dplyr::select(EPASLD, GEOID_BG = GEOID10, ResDen_10 = D1A, PopDen_10 = D1B, EmpDen_10 = D1C, Auto_Jobs45min = D5ar, Transit_Jobs45min = D5br)
# Join to parcels spatial polygons dataframe
SCCparcels.proj <- merge(SCCparcels.proj, EPASLD, by.x = "GEOID_BG", by.y = "GEOID_BG")
remove(EPASLD)

# Write to shapefile
writeOGR(SCCparcels.proj, dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "09_01_2015_parcels_SCConly_CensusIDs_EPASLD", driver="ESRI Shapefile", morphToESRI=TRUE)


### RAIL STATION DISTANCE

# Read MTC rail station shapefile
RailStations <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "Rail_Stations_MTC_2017")
RailStations.proj <- spTransform(RailStations, CRSobj = CSP_Zone3_Proj) # Project to California State Plane
remove(RailStations)

# Calculate distance from each parcel to the nearest rail station
DistRail <- spDistsN1(SCCparcels_centroids, RailStations.proj, longlat = FALSE)
# This only goes to one point



TEST1 <- as.data.frame(SCCparcels.proj)
TEST2 <- as.data.frame(SCCparcels_centroids)
remove(TEST, TEST1, TEST2)

### SCHOOL QUALITY DATA
# Public elementary school point file
ElemSchools <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "SCC_Schools_PublicElem")
summary(ElemSchools)
ElemSchools.proj <- spTransform(ElemSchools, CRSobj = CSP_Zone3_Proj)
remove(ElemSchools)

# Download CA API data from http://www.cde.ca.gov/ta/ac/ap/apidatafiles.asp
temp <- tempfile()
download.file("http://www3.cde.ca.gov/researchfiles/api/14avgdb.zip",temp, mode="wb")
unzip(temp, "14avgdb.dbf")
APIdata <- read.dbf("14avgdb.dbf")
APIdata_SCC <- subset(APIdata, cname=="Santa Clara")
remove(APIdata, temp)

# Merge the datasets
ElemSchools.dat <- ElemSchools.proj@data
ElemSchools.dat <- sp::merge(x=ElemSchools.dat, y=APIdata_SCC, by.x="UNIQUEID", by.y="cds",
                             all.x=T, sort=F)
ElemSchools.proj@data <- ElemSchools.dat

### CENSUS/ACS TABULAR DATA

# Download Census tract variables
acs.lookup(2015, span = 5, dataset ="acs", table.name="Median Household Income") # Find table number by name

SCCtracts_income <- acs.fetch(2015, span = 5, geography=geo.make(state="CA", county="Santa Clara", tract="*"), 
                              table.number="B19013", dataset = "acs",
                              key="05ea9b3ab3618d0c1d21a2d2b93c3ba0e3b7e380", col.names = "pretty") # Download Census data

#Resolve acs.fetch triming zeros. 
SCCtracts_income@geography$state[nchar(SCCtracts_income@geography$state) == 1]  <- paste0(0, SCCtracts_income@geography$state)
SCCtracts_income@geography$county[nchar(SCCtracts_income@geography$county) == 2]  <- paste0(0, SCCtracts_income@geography$county)
SCCtracts_income.df <- data.frame(paste0(as.character(SCCtracts_income@geography$state), 
                                         as.character(SCCtracts_income@geography$county), 
                                         SCCtracts_income@geography$tract), 
                                  SCCtracts_income@estimate)


colnames(income_df) <- c("GEOID", "hhincome")


# Join spatial and tabular data
SCC_tracts2 <- geo_join(SCCtracts.proj, SCCtracts_income, "GEOID", "geoid")
sf_tracts2 <- sf_tracts2[!is.na(sf_tracts2$pctpov),]
# look at the data
class(sf_tracts2)
str(sf_tracts2)
str(sf_tracts2@data)


