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
library("gstat")
library("raster")
library("gdata")
library("nnet")
library("reshape2")
library("magrittr")





# GENERAL DATA PREP

# Create object for California State Plane Zone III CRS
CSP_Zone3_Proj <- CRS("+init=EPSG:2227")


# 1. SCC PARCELS

# Read SCC parcels shapefile
SCCparcels <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "09_01_2015_parcels_SCConly_proj")
SCCparcels <- spTransform(SCCparcels, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

# Check for duplicates in PARCEL_ID field
n_occur <- data.frame(table(SCCparcels$PARCEL_ID))
SCCparcels[SCCparcels$PARCEL_ID %in% n_occur$Var1[n_occur$Freq > 1],]
remove(n_occur)

# Create parcel centroids
SCCparcels_centroids <- gCentroid(SCCparcels, byid=TRUE)


### CENSUS/ACS SPATIAL DATA

# Download SCC block group boundaries
SCCbgs <- block_groups(06, county = 085, cb = FALSE, year = 2015)
SCCbgs <- spTransform(SCCbgs, CRSobj = CSP_Zone3_Proj)

# Spatial join parcel centroids to BGs
Parcels_Join_BGs <- over(SCCparcels_centroids, SCCbgs)
Parcels_Join_BGs$GEOID_BG <- Parcels_Join_BGs$GEOID #seems like it would be better to do this later, but not sure how to rename columns in spatial data frame
SCCparcels <- spCbind(SCCparcels, Parcels_Join_BGs)
# Remove unnecessary columns
drops <- c("CENTROID", "X", "Y", "STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE", "GEOID", "NAMELSAD", "MTFCC", "FUNCSTAT", "ALAND", "AWATER", "INTPTLAT", "INTPTLON") # list of col names
SCCparcels <- SCCparcels[,!(names(SCCparcels) %in% drops)] #remove columns from above
remove(drops)

# Download SCC Census tract boundaries
SCCtracts <- tracts(06, county = 085, cb = FALSE, year = 2015)
SCCtracts <- spTransform(SCCtracts, CRSobj = CSP_Zone3_Proj)

# Spatial join parcel centroids to tracts
Parcels_Join_Tracts <- over(SCCparcels_centroids, SCCtracts)
Parcels_Join_Tracts$GEOID_TR <- Parcels_Join_Tracts$GEOID
SCCparcels <- spCbind(SCCparcels, Parcels_Join_Tracts)
# Remove unnecessary columns
drops <- c("STATEFP", "COUNTYFP", "TRACTCE", "GEOID", "NAMELSAD", "MTFCC", "FUNCSTAT", "ALAND", "AWATER", "INTPTLAT", "INTPTLON", "NAME") # list of col names
SCCparcels <- SCCparcels[,!(names(SCCparcels) %in% drops)] #remove columns from above
remove(drops)

SCCparcels.df <- as.data.frame(SCCparcels)
SCCparcels.df <- SCCparcels.df[, -(2:13)]
save(SCCparcels.df, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/1_Parcels_DF_CensusIDs.RData")

# Clean up
remove(Parcels_Join_BGs, Parcels_Join_Tracts, SCCparcels.df)

# CENSUS/ACS TABULAR DATA

# Download Census tract variables
# Median household income
acs.lookup(2009, span = 5, dataset ="acs", table.name="Median Household Income") # Find table number by name, I found it easier to look in American Fact Finder
SCCtracts_Income_09 <- acs.fetch(2009, span = 5, geography=geo.make(state="CA", county="Santa Clara", tract="*"), 
                                 table.number="B19013", dataset = "acs",
                                 key="", col.names = "pretty") # Download Census data
#Resolve acs.fetch triming zeros: https://gist.github.com/hansthompson/f087390ef7a1dc284a8b
SCCtracts_Income_09@geography$state[nchar(SCCtracts_Income_09@geography$state) == 1]  <- paste0(0, SCCtracts_Income_09@geography$state)
SCCtracts_Income_09@geography$county[nchar(SCCtracts_Income_09@geography$county) == 2]  <- paste0(0, SCCtracts_Income_09@geography$county)
SCCtracts_Income_09.df <- data.frame(paste0(as.character(SCCtracts_Income_09@geography$state), 
                                            as.character(SCCtracts_Income_09@geography$county), 
                                            SCCtracts_Income_09@geography$tract), 
                                     SCCtracts_Income_09@estimate)
# Look at the long column names in the data table and rename them to something shorter
colnames(SCCtracts_Income_09.df) <- c("GEOID_TR", "HH_Income_09")

# Tenure
acs.lookup(2009, span = 5, dataset ="acs", table.number="B25003") # Find table number by number
SCCtracts_Tenure_09 <- acs.fetch(2009, span = 5, geography=geo.make(state="CA", county="Santa Clara", tract="*"), 
                                 table.number="B25003", dataset = "acs",
                                 key="", col.names = "pretty") # Download Census data

SCCtracts_Tenure_09@geography$state[nchar(SCCtracts_Tenure_09@geography$state) == 1]  <- paste0(0, SCCtracts_Tenure_09@geography$state)
SCCtracts_Tenure_09@geography$county[nchar(SCCtracts_Tenure_09@geography$county) == 2]  <- paste0(0, SCCtracts_Tenure_09@geography$county)
SCCtracts_Tenure_09.df <- data.frame(paste0(as.character(SCCtracts_Tenure_09@geography$state), 
                                            as.character(SCCtracts_Tenure_09@geography$county), 
                                            SCCtracts_Tenure_09@geography$tract), 
                                     SCCtracts_Tenure_09@estimate)
colnames(SCCtracts_Tenure_09.df) <- c("GEOID_TR", "TotalHH_09", "OwnerHH_09", "RenterHH_09")
SCCtracts_Tenure_09.df$Pct_Owner_09 <- (SCCtracts_Tenure_09.df$OwnerHH_09/SCCtracts_Tenure_09.df$TotalHH_09)

# Race
acs.lookup(2009, span = 5, dataset ="acs", table.number="B02001") # Find table number by number
SCCtracts_Race_09 <- acs.fetch(2009, span = 5, geography=geo.make(state="CA", county="Santa Clara", tract="*"), 
                               table.number="B02001", dataset = "acs",
                               key="", col.names = "pretty") # Download Census data

SCCtracts_Race_09@geography$state[nchar(SCCtracts_Race_09@geography$state) == 1]  <- paste0(0, SCCtracts_Race_09@geography$state)
SCCtracts_Race_09@geography$county[nchar(SCCtracts_Race_09@geography$county) == 2]  <- paste0(0, SCCtracts_Race_09@geography$county)
SCCtracts_Race_09.df <- data.frame(paste0(as.character(SCCtracts_Race_09@geography$state), 
                                          as.character(SCCtracts_Race_09@geography$county), 
                                          SCCtracts_Race_09@geography$tract), 
                                   SCCtracts_Race_09@estimate)
colnames(SCCtracts_Race_09.df) <- c("GEOID_TR", "Pop_09", "White_09", "Black_09", "AmerInd_09", "Asian_09", "PacIs_09", "Other_09", "TwoMore_09", "TwoMoreExOth_09", "ThreeMore_09")
SCCtracts_Race_09.df$Pct_White_09 <- (SCCtracts_Race_09.df$White_09/SCCtracts_Race_09.df$Pop_09)
SCCtracts_Race_09.df$Pct_Black_09 <- (SCCtracts_Race_09.df$Black_09/SCCtracts_Race_09.df$Pop_09)
SCCtracts_Race_09.df$Pct_Asian_09 <- (SCCtracts_Race_09.df$Asian_09/SCCtracts_Race_09.df$Pop_09)

# Hispanic
SCCtracts_Hispanic_09 <- acs.fetch(2009, span = 5, geography=geo.make(state="CA", county="Santa Clara", tract="*"), 
                                   table.number="B03003", dataset = "acs",
                                   key="", col.names = "pretty") # Download Census data

SCCtracts_Hispanic_09@geography$state[nchar(SCCtracts_Hispanic_09@geography$state) == 1]  <- paste0(0, SCCtracts_Hispanic_09@geography$state)
SCCtracts_Hispanic_09@geography$county[nchar(SCCtracts_Hispanic_09@geography$county) == 2]  <- paste0(0, SCCtracts_Hispanic_09@geography$county)
SCCtracts_Hispanic_09.df <- data.frame(paste0(as.character(SCCtracts_Hispanic_09@geography$state), 
                                              as.character(SCCtracts_Hispanic_09@geography$county), 
                                              SCCtracts_Hispanic_09@geography$tract), 
                                       SCCtracts_Hispanic_09@estimate)
colnames(SCCtracts_Hispanic_09.df) <- c("GEOID_TR", "Pop_09", "NotHispanic_09", "Hispanic_09")
SCCtracts_Hispanic_09.df$Pct_Hispanic_09 <- (SCCtracts_Hispanic_09.df$Hispanic_09/SCCtracts_Hispanic_09.df$Pop_09)

# Year structure built
SCCtracts_YearStructure_09 <- acs.fetch(2009, span = 5, geography=geo.make(state="CA", county="Santa Clara", tract="*"), 
                                        table.number="B25034", dataset = "acs",
                                        key="", col.names = "pretty") # Download Census data

SCCtracts_YearStructure_09@geography$state[nchar(SCCtracts_YearStructure_09@geography$state) == 1]  <- paste0(0, SCCtracts_YearStructure_09@geography$state)
SCCtracts_YearStructure_09@geography$county[nchar(SCCtracts_YearStructure_09@geography$county) == 2]  <- paste0(0, SCCtracts_YearStructure_09@geography$county)
SCCtracts_YearStructure_09.df <- data.frame(paste0(as.character(SCCtracts_YearStructure_09@geography$state), 
                                                   as.character(SCCtracts_YearStructure_09@geography$county), 
                                                   SCCtracts_YearStructure_09@geography$tract), 
                                            SCCtracts_YearStructure_09@estimate)
colnames(SCCtracts_YearStructure_09.df) <- c("GEOID_TR", "HU_09", "YrBuilt_2005_2009_09", "YrBuilt_2000_2004_09", "YrBuilt_1990_1999_09", "YrBuilt_1980_1989_09", "YrBuilt_1970_1979_09", "YrBuilt_1960_1969_09", "YrBuilt_1950_1959_09", "YrBuilt_1940_1949_09", "YrBuilt_1939_Earlier_09")
SCCtracts_YearStructure_09.df$NewUnits_09 <- (SCCtracts_YearStructure_09.df$YrBuilt_2000_2004_09 + SCCtracts_YearStructure_09.df$YrBuilt_2005_2009_09) # Define new units as though built 2000-09
SCCtracts_YearStructure_09.df$Pct_NewUnits_09 <- (SCCtracts_YearStructure_09.df$NewUnits_09/SCCtracts_YearStructure_09.df$HU_09) # Define new units as though built 2000-09

# Merge all of these variables together
All_ACS_09 <- merge(SCCtracts_Income_09.df, SCCtracts_Tenure_09.df) 
All_ACS_09 <- merge(All_ACS_09, SCCtracts_Race_09.df) 
All_ACS_09 <- merge(All_ACS_09, SCCtracts_Hispanic_09.df) 
All_ACS_09 <- merge(All_ACS_09, SCCtracts_YearStructure_09.df)
# Keep only the variables I need
names(All_ACS_09)
All_ACS_09 <- All_ACS_09 %>%dplyr::select(GEOID_TR, Pop_09, HH_Income_09,TotalHH_09, Pct_Owner_09, Pct_White_09, Pct_Black_09, Pct_Asian_09, Pct_Hispanic_09, HU_09, NewUnits_09, Pct_NewUnits_09)

# Clean up 
remove(SCCtracts_Income_09.df, SCCtracts_Tenure_09.df, SCCtracts_Race_09.df, SCCtracts_Hispanic_09.df, SCCtracts_YearStructure_09.df,
       SCCtracts_Income_09, SCCtracts_Tenure_09, SCCtracts_Race_09, SCCtracts_Hispanic_09, SCCtracts_YearStructure_09)

# Save as Rdata file
save(All_ACS_09, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/2_CensusACS_DF.RData")
remove(All_ACS_09)


### EPA SMART LOCATION DATABASE ###

# Read EPA SLD dbf
EPASLD <- read.dbf("/Users/charlesgabbe/Google Drive/Research_Projects/MobileHomes_Services/Data/EPA_SLD_2014/SmartLocationDb.dbf")
# Keep and rename the key variables that I need
EPASLD = dplyr::select(EPASLD, GEOID_BG = GEOID10, ResDen_10 = D1A, PopDen_10 = D1B, EmpDen_10 = D1C, Auto_Jobs45min = D5ar, Transit_Jobs45min = D5br)
EPASLD <- EPASLD[grep("^06085", EPASLD$GEOID_BG), ] # Narrow dataset to Santa Clara County only

# Save as Rdata file
save(EPASLD, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/3_EPASLD_DF.RData")
remove(EPASLD)


### RAIL STATION DISTANCE

# Read MTC rail station shapefile
RailStations <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "Rail_Stations_MTC_2017.proj")
RailStations <- spTransform(RailStations, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

# Parcel centroids using RQGIS
Parcelcentroids <- run_qgis(alg = "qgis:polygoncentroids", 
                         INPUT_LAYER = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/09_01_2015_parcels_SCConly_proj.shp", 
                         OUTPUT_LAYER = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/SCCparcelcentroids.shp")
Parcelcentroids <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "SCCparcelcentroids")
Parcelcentroids <- spTransform(Parcelcentroids, CRSobj = CSP_Zone3_Proj) # Project to California State Plane
Parcelcentroids <- Parcelcentroids[, -(2:16)] # Remove unnecessary columns

# Distance matrix in QGIS
run_qgis(alg = "qgis:distancematrix", 
         INPUT_LAYER = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/SCCparcelcentroids.shp", INPUT_FIELD = "PARCEL_ID", 
         TARGET_LAYER = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/Rail_Stations_MTC_2017.shp", TARGET_FIELD = "station_na", 
         MATRIX_TYPE = 0, 
         NEAREST_POINTS = 1, 
         DISTANCE_MATRIX = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/RailDistSCC_ft.csv")
Dist_Parcel_Rail <- read.csv(file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/RailDistSCC_ft.csv", header=TRUE)
colnames(Dist_Parcel_Rail) <- c("PARCEL_ID", "NearRailStation", "RailStation_Ft")

# Check for duplicates in PARCEL_ID field and drop
n_occur <- data.frame(table(Dist_Parcel_Rail$PARCEL_ID))
Dist_Parcel_Rail[Dist_Parcel_Rail$PARCEL_ID %in% n_occur$Var1[n_occur$Freq > 1],]
remove(n_occur)
Dist_Parcel_Rail <- subset(Dist_Parcel_Rail, !duplicated(PARCEL_ID))  # Drop duplicates based on Parcel ID

# Save and clean up
save(Dist_Parcel_Rail, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/4_RailDistanceFt_DF.RData")
remove(Parcelcentroids, Dist_Parcel_Rail)


### SCHOOL QUALITY DATA
# Public elementary school point file
ElemSchools <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "SCC_Schools_PublicElem")
ElemSchools <- spTransform(ElemSchools, CRSobj = CSP_Zone3_Proj)

# Download CA API data from http://www.cde.ca.gov/ta/ac/ap/apidatafiles.asp
temp <- tempfile()
download.file("http://www3.cde.ca.gov/researchfiles/api/14avgdb.zip",temp, mode="wb")
unzip(temp, "14avgdb.dbf")
APIdata <- read.dbf("14avgdb.dbf")
APIdata_SCC <- subset(APIdata, cname=="Santa Clara")
remove(APIdata, temp)

# Merge the datasets
ElemSchools.dat <- ElemSchools@data
ElemSchools.dat <- sp::merge(x=ElemSchools.dat, y=APIdata_SCC, by.x="UNIQUEID", by.y="cds",
                             all.x=T, sort=F)
ElemSchools@data <- ElemSchools.dat

writeOGR(ElemSchools, dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "SCC_Schools_PublicElem_API", driver="ESRI Shapefile", morphToESRI=TRUE)
remove(ElemSchools, ElemSchools.dat, APIdata_SCC)
# Creating the API 13 raster in ArcGIS
# Using IDW tool spatial analysis tool
# Variable: API 13
# Inputs: Power = 2, Used school district boundary lines as barriers
# Extract raster values to parcel centroid points

Parcels_Elev_API <- read.dbf("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/SCCparcelcentroids_API_Elev_Slope.dbf")
ElemAPI13 <- Parcels_Elev_API[, -(2:16)] # Remove unnecessary columns
ElemAPI13 <- ElemAPI13[, -(3:4)] # Remove unnecessary columns
colnames(ElemAPI13) <- c("PARCEL_ID", "API13_IDW")
# Save and clean up
save(ElemAPI13, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/5_ElemAPI13_DF.RData")
remove(Parcels_Elev_API, ElemAPI13)


# ELEVATION AND SLOPE
# Elevation and slope done in ArcGIS
# Extract raster values to parcel centroid points
Parcels_Elev_API <- read.dbf("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/SCCparcelcentroids_API_Elev_Slope.dbf")
ElevSlope <- Parcels_Elev_API[, -(2:17)] # Remove unnecessary columns
colnames(ElevSlope) <- c("PARCEL_ID", "Elev_Ft", "Slope_Deg")
# Save and clean up
save(ElevSlope, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/6_ElevSlope_DF.RData")
remove(Parcels_Elev_API, ElevSlope)


# ZONING

# City of Santa Clara - 2012
Zoning_SC_12 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LandParcel2012")
Zoning_SC_12 <- spTransform(Zoning_SC_12, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

Zoning_SC_12$T1_City<-1:nrow(Zoning_SC_12) # Add 'city' attribute
Zoning_SC_12$T1_City<-"Santa Clara" # Assign values to 'city' attribute
Zoning_SC_12$T1_Year<-1:nrow(Zoning_SC_12) # Add 'year' attribute
Zoning_SC_12$T1_Year<-as.numeric(2012) # Assign values to 'year' attribute
Zoning_SC_12$T1_ZoneV1<-Zoning_SC_12$ZONGDSGN # Rename main zoning variable
Zoning_SC_12$T1_ZoneV2<-1:nrow(Zoning_SC_12) # Add second zoning variable
Zoning_SC_12$T1_ZoneV2<-"0" # Assign 0 values to 'ZoneVar2' attribute
Zoning_SC_12$T1_ZoneV2<-as.numeric(Zoning_SC_12$T1_ZoneV2)
Zoning_SC_12$T1_ZoneV3<-Zoning_SC_12$GPLANCRT # Add third zoning variable
Zoning_SC_12 <- Zoning_SC_12[, -(1:10)] # Remove first ten columns

# Create table for unique values - for my densities spreadsheet
Zoning_SC_12.df <- as.data.frame(Zoning_SC_12)
Unique_SC_Zoning_12 <- unique(Zoning_SC_12.df$T1_ZoneV1, incomparables = FALSE)
Unique_SC_Zoning_12 <- as.data.frame(Unique_SC_Zoning_12)
Unique_SC_GP_12 <- unique(Zoning_SC_12$T1_ZoneV3, incomparables = FALSE)
Unique_SC_GP_12 <- as.data.frame(Unique_SC_GP_12)

writeOGR(Zoning_SC_12, dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LUregs_SantaClara_12", driver="ESRI Shapefile", morphToESRI=TRUE)
remove(Zoning_SC_12, Zoning_SC_12.df, Unique_SC_Zoning_12, Unique_SC_GP_12) # Clean up

# City of Santa Clara - 2016
Zoning_SC_16 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LandParcels2016")
Zoning_SC_16 <- spTransform(Zoning_SC_16, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

Zoning_SC_16$T2_City<-1:nrow(Zoning_SC_16) # Add 'city' attribute
Zoning_SC_16$T2_City<-"Santa Clara" # Assign values to 'city' attribute
Zoning_SC_16$T2_Year<-1:nrow(Zoning_SC_16) # Add 'year' attribute
Zoning_SC_16$T2_Year<-as.numeric(2016) # Assign values to 'year' attribute
Zoning_SC_16$T2_ZoneV1<-Zoning_SC_16$ZONGDSGN # Rename main zoning variable
Zoning_SC_16$T2_ZoneV2<-1:nrow(Zoning_SC_16) # Add second zoning variable
Zoning_SC_16$T2_ZoneV2<-"0" # Assign 0 values to 'ZoneVar2' attribute
Zoning_SC_16$T2_ZoneV2<-as.numeric(Zoning_SC_16$T2_ZoneV2)
Zoning_SC_16$T2_ZoneV3<-Zoning_SC_16$GPLANCRT # Add third zoning variable
Zoning_SC_16 <- Zoning_SC_16[, -(1:10)] # Remove first ten columns

# Create table for unique values - for my densities spreadsheet
Zoning_SC_16.df <- as.data.frame(Zoning_SC_16)
Unique_SC_Zoning_16 <- unique(Zoning_SC_16.df$T2_ZoneV1, incomparables = FALSE)
Unique_SC_Zoning_16 <- as.data.frame(Unique_SC_Zoning_16)
Unique_SC_GP_16 <- unique(Zoning_SC_16$T2_ZoneV3, incomparables = FALSE)
Unique_SC_GP_16 <- as.data.frame(Unique_SC_GP_16)

writeOGR(Zoning_SC_16, dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LUregs_SantaClara_16", driver="ESRI Shapefile", morphToESRI=TRUE)
remove(Zoning_SC_16, Zoning_SC_16.df, Unique_SC_Zoning_16, Unique_SC_GP_16) # Clean up

# City of Sunnyvale - 2006
Zoning_Sunnyvale_06 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "Sunnyvale_Zoning_2006")
Zoning_Sunnyvale_06 <- spTransform(Zoning_Sunnyvale_06, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

Zoning_Sunnyvale_06$T1_City<-1:nrow(Zoning_Sunnyvale_06) # Add 'city' attribute
Zoning_Sunnyvale_06$T1_City<-"Sunnyvale" # Assign values to 'city' attribute
Zoning_Sunnyvale_06$T1_Year<-1:nrow(Zoning_Sunnyvale_06) # Add 'year' attribute
Zoning_Sunnyvale_06$T1_Year<-as.numeric(2006) # Assign values to 'year' attribute
Zoning_Sunnyvale_06$T1_ZoneV1<-Zoning_Sunnyvale_06$ZONING # Rename main zoning variable
Zoning_Sunnyvale_06$T1_ZoneV2<-1:nrow(Zoning_Sunnyvale_06) # Add second zoning variable
Zoning_Sunnyvale_06$T1_ZoneV2<-"0" # Assign 0 values to 'ZoneV2' attribute
Zoning_Sunnyvale_06$T1_ZoneV2<-as.numeric(Zoning_Sunnyvale_06$T1_ZoneV2)
Zoning_Sunnyvale_06$T1_ZoneV3<-Zoning_Sunnyvale_06$COMBININGD # Rename third zoning variable
Zoning_Sunnyvale_06 <- Zoning_Sunnyvale_06[, -(1:2)] # Remove first two columns

# Create table for unique values - for my densities spreadsheet
Zoning_Sunnyvale_06.df <- as.data.frame(Zoning_Sunnyvale_06)
Unique_Sunnyvale_Zoning_06 <- unique(Zoning_Sunnyvale_06.df$T1_ZoneV1, incomparables = FALSE)
Unique_Sunnyvale_Zoning_06 <- as.data.frame(Unique_Sunnyvale_Zoning_06)

writeOGR(Zoning_Sunnyvale_06, dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LUregs_Sunnyvale_06", driver="ESRI Shapefile", morphToESRI=TRUE)
remove(Zoning_Sunnyvale_06, Zoning_Sunnyvale_06.df, Unique_Sunnyvale_Zoning_06) # Clean up

# City of Sunnyvale - 2016
Zoning_Sunnyvale_16 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "SunnyvaleParcels2016")
Zoning_Sunnyvale_16 <- spTransform(Zoning_Sunnyvale_16, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

Zoning_Sunnyvale_16$T2_City<-1:nrow(Zoning_Sunnyvale_16) # Add 'city' attribute
Zoning_Sunnyvale_16$T2_City<-"Sunnyvale" # Assign values to 'city' attribute
Zoning_Sunnyvale_16$T2_Year<-1:nrow(Zoning_Sunnyvale_16) # Add 'year' attribute
Zoning_Sunnyvale_16$T2_Year<-as.numeric(2016) # Assign values to 'year' attribute
Zoning_Sunnyvale_16$T2_ZoneV1<-Zoning_Sunnyvale_16$Zoning # Create main zoning variable
Zoning_Sunnyvale_16$T2_ZoneV2<-1:nrow(Zoning_Sunnyvale_16) # Create second zoning variable
Zoning_Sunnyvale_16$T2_ZoneV2<-"0" # Assign 0 values to 'ZoneV2' attribute
Zoning_Sunnyvale_16$T2_ZoneV2<-as.numeric(Zoning_Sunnyvale_16$T2_ZoneV2)
Zoning_Sunnyvale_16$T2_ZoneV3<-Zoning_Sunnyvale_16$CombiningD # Create third zoning variable
Zoning_Sunnyvale_16 <- Zoning_Sunnyvale_16[, -(1:82)] # Remove first 83 columns
names(Zoning_Sunnyvale_16)

# Create table for unique values - for my densities spreadsheet
Zoning_Sunnyvale_16.df <- as.data.frame(Zoning_Sunnyvale_16)
Unique_Sunnyvale_Zoning_16 <- unique(Zoning_Sunnyvale_16.df$T2_ZoneV1, incomparables = FALSE)
Unique_Sunnyvale_Zoning_16 <- as.data.frame(Unique_Sunnyvale_Zoning_16)

writeOGR(Zoning_Sunnyvale_16, dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LUregs_Sunnyvale_16", driver="ESRI Shapefile", morphToESRI=TRUE)
remove(Zoning_Sunnyvale_16, Zoning_Sunnyvale_16.df, Unique_Sunnyvale_Zoning_16) # Clean up

# City of San Jose - 2006
# Taking a different approach from the other cities because I need to merge parcels with zoning and GP shapefiles
SCCparcels <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "09_01_2015_parcels_SCConly")
SCCparcels <- spTransform(SCCparcels, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

CAplaces <- places(06, cb = FALSE, year = 2015)
SJboundary <- subset(CAplaces, NAME=="San Jose") # Select only San Jose
remove(CAplaces)
SJboundary <- spTransform(SJboundary, CRSobj = CSP_Zone3_Proj)
  
# Create parcel centroids
SCCparcels_centroids <- gCentroid(SCCparcels, byid=TRUE) # Create the parcel centroid point file that I'll use 

# San Jose zoning data from 2006
Zoning_SJ_06 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "Zoning_2006_region_1")
Zoning_SJ_06 <- spTransform(Zoning_SJ_06, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

Zoning_SJ_06$T1_City<-1:nrow(Zoning_SJ_06) # Add 'city' attribute
Zoning_SJ_06$T1_City<-"San Jose" # Assign values to 'city' attribute
Zoning_SJ_06$T1_Year<-1:nrow(Zoning_SJ_06) # Add 'year' attribute
Zoning_SJ_06$T1_Year<-as.numeric(2006) # Assign values to 'year' attribute
Zoning_SJ_06$T1_ZoneV1<-Zoning_SJ_06$ZONING # Create main zoning variable
Zoning_SJ_06$T1_ZoneV2<-Zoning_SJ_06$PD_DENSITY # Create second zoning variable
Zoning_SJ_06 <- Zoning_SJ_06[, -(1:9)] # Remove unneeded columns

# Spatial join parcel centroids to 2006 zoning data
SJ_Parcels_Zoning06 <- over(SCCparcels_centroids, Zoning_SJ_06)
SJ_Parcels_Zoning06 <- spCbind(SCCparcels, SJ_Parcels_Zoning06)
SJ_Parcels_Zoning06 <- subset(SJ_Parcels_Zoning06, T1_City=="San Jose") # Select only San Jose parcels

# San Jose General Plan data from 2006
GP_SJ_06 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "General_Plan_2006_region")
GP_SJ_06 <- spTransform(GP_SJ_06, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

GP_SJ_06$T1_ZoneV3<-GP_SJ_06$ABBREVIATI # Create third zoning variable
GP_SJ_06 <- GP_SJ_06[, -(1:9)] # Remove unneeded columns

# Create centroids for SJ_Parcels_Zoning06
SJ_Parcels_Zoning06_centroids <- gCentroid(SJ_Parcels_Zoning06, byid=TRUE) # Create the parcel centroid point file that I'll use 

# Spatial join parcel centroids to tracts
SJ_Parcels_Zoning_GP06 <- over(SJ_Parcels_Zoning06_centroids, GP_SJ_06)
SJ_Parcels_Zoning_GP06 <- spCbind(SJ_Parcels_Zoning06, SJ_Parcels_Zoning_GP06)

# Remove unnecessary columns
SJ_Parcels_Zoning_GP06 <- SJ_Parcels_Zoning_GP06[, -(1:16)]

# Remove stray parcels by clipping to city boundary
find_algorithms(search_term = "clip", name_only = FALSE,
                qgis_env = set_env())
get_usage(alg = "gdalogr:clipvectorsbypolygon", intern = FALSE, qgis_env = set_env())
get_args_man(alg = "gdalogr:clipvectorsbypolygon", options = TRUE)
get_options(alg = "gdalogr:clipvectorsbypolygon", intern = FALSE)

SJ_Parcels_Zoning_GP06_Clip <- run_qgis(alg = "gdalogr:clipvectorsbypolygon", INPUT_LAYER = SJ_Parcels_Zoning_GP06, CLIP_LAYER = SJboundary, OUTPUT_LAYER = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_SanJose_06.shp")
SJ_Parcels_Zoning_GP06_Clip2 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LUregs_SanJose_06")
SJ_Parcels_Zoning_GP06_Clip2 <- spTransform(SJ_Parcels_Zoning_GP06_Clip2, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

# Clean up
remove(Zoning_SJ_06, GP_SJ_06, SJ_Parcels_Zoning06, SJ_Parcels_Zoning06_centroids, SJ_Parcels_Zoning_GP06_Clip)

# Create table for unique values - this will make it possible to prep my densities spreadsheet
SJ_Parcels_Zoning_GP06_Clip2.df <- as.data.frame(SJ_Parcels_Zoning_GP06_Clip2)
Unique_SJ_Zoning_06 <- unique(SJ_Parcels_Zoning_GP06_Clip2.df$T1_ZoneV1, incomparables = FALSE)
Unique_SJ_Zoning_06 <- as.data.frame(Unique_SJ_Zoning_06)
Unique_SJ_GP_06 <- unique(SJ_Parcels_Zoning_GP06_Clip2.df$T1_ZoneV3, incomparables = FALSE)
Unique_SJ_GP_06 <- as.data.frame(Unique_SJ_GP_06)
remove(SJ_Parcels_Zoning_GP06_Clip2.df)

# City of San Jose - 2016
# San Jose zoning data from 2016
Zoning_SJ_16 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "ZONING_2016_1")
Zoning_SJ_16 <- spTransform(Zoning_SJ_16, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

Zoning_SJ_16$T2_City<-1:nrow(Zoning_SJ_16) # Add 'city' attribute
Zoning_SJ_16$T2_City<-"San Jose" # Assign values to 'city' attribute
Zoning_SJ_16$T2_Year<-1:nrow(Zoning_SJ_16) # Add 'year' attribute
Zoning_SJ_16$T2_Year<-as.numeric(2016) # Assign values to 'year' attribute
Zoning_SJ_16$T2_ZoneV1<-Zoning_SJ_16$ZONING # Create main zoning variable
Zoning_SJ_16$T2_ZoneV2<-Zoning_SJ_16$PD_DENSITY # Create second zoning variable
Zoning_SJ_16 <- Zoning_SJ_16[, -(1:7)] # Remove unneeded columns ## CHECK ##

# Spatial join parcel centroids to 2016 zoning data
SJ_Parcels_Zoning16 <- over(SCCparcels_centroids, Zoning_SJ_16)
SJ_Parcels_Zoning16 <- spCbind(SCCparcels, SJ_Parcels_Zoning16)
SJ_Parcels_Zoning16 <- subset(SJ_Parcels_Zoning16, T2_City=="San Jose") # Select only San Jose parcels

# San Jose General Plan data from 2016
GP_SJ_16 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "GENERAL_PLAN2040")
GP_SJ_16 <- spTransform(GP_SJ_16, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

GP_SJ_16$T2_ZoneV3 <- GP_SJ_16$GP_ABBR # Create third zoning variable
GP_SJ_16 <- GP_SJ_16[, -(1:7)] # Remove unneeded columns

# Create centroids for SJ_Parcels_Zoning16
SJ_Parcels_Zoning16_centroids <- gCentroid(SJ_Parcels_Zoning16, byid=TRUE) # Create the parcel centroid point file that I'll use 

# Spatial join parcel centroids to tracts
SJ_Parcels_Zoning_GP16 <- over(SJ_Parcels_Zoning16_centroids, GP_SJ_16)
SJ_Parcels_Zoning_GP16 <- spCbind(SJ_Parcels_Zoning16, SJ_Parcels_Zoning_GP16)

# Remove unnecessary columns
SJ_Parcels_Zoning_GP16 <- SJ_Parcels_Zoning_GP16[, -(1:16)]

# Remove stray parcels
SJ_Parcels_Zoning_GP16_Clip <- run_qgis(alg = "gdalogr:clipvectorsbypolygon", INPUT_LAYER = SJ_Parcels_Zoning_GP16, CLIP_LAYER = SJboundary, OUTPUT_LAYER = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_SanJose_16.shp")
SJ_Parcels_Zoning_GP16_Clip2 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LUregs_SanJose_16")
SJ_Parcels_Zoning_GP16_Clip2 <- spTransform(SJ_Parcels_Zoning_GP16_Clip2, CRSobj = CSP_Zone3_Proj) # Project to California State Plane

# Clean up
remove(Zoning_SJ_16, GP_SJ_16, SJ_Parcels_Zoning16, SJ_Parcels_Zoning16_centroids, SJ_Parcels_Zoning_GP16_Clip)

# Create table for unique values - this will make it possible to prep my densities spreadsheet
SJ_Parcels_Zoning_GP16_Clip2.df <- as.data.frame(SJ_Parcels_Zoning_GP16_Clip2)
Unique_SJ_Zoning_16 <- unique(SJ_Parcels_Zoning_GP16_Clip2.df$T2_ZoneV1, incomparables = FALSE)
Unique_SJ_Zoning_16 <- as.data.frame(Unique_SJ_Zoning_16)
Unique_SJ_GP_16 <- unique(SJ_Parcels_Zoning_GP16_Clip2.df$T2_ZoneV3, incomparables = FALSE)
Unique_SJ_GP_16 <- as.data.frame(Unique_SJ_GP_16)
remove(SJ_Parcels_Zoning_GP16_Clip2.df)


### Merge the 3 cities' shapefiles and join to parcel centroids
SCCparcels <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "09_01_2015_parcels_SCConly_proj")
SCCparcels <- spTransform(SCCparcels, CRSobj = CSP_Zone3_Proj) # Project to California State Plane
SCCparcels_centroids <- gCentroid(SCCparcels, byid=TRUE)

SCC_3Cities_Zoning_T1 <- run_qgis(alg = "qgis:mergevectorlayers", LAYERS = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_SanJose_06.shp;/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_SantaClara_12.shp;/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_Sunnyvale_06.shp", 
                                  OUTPUT = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_ThreeCities_T1.shp")
SCC_3Cities_Zoning_T1 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LUregs_ThreeCities_T1")
SCC_3Cities_Zoning_T1 <- spTransform(SCC_3Cities_Zoning_T1, CRSobj = CSP_Zone3_Proj)

Parcels_Join_T1Zoning <- over(SCCparcels_centroids, SCC_3Cities_Zoning_T1)
SCCparcels <- spCbind(SCCparcels, Parcels_Join_T1Zoning)
SCCparcels <- SCCparcels[, -(2:16)] # Remove unneeded columns
  
SCC_3Cities_Zoning_T2 <- run_qgis(alg = "qgis:mergevectorlayers", LAYERS = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_SanJose_16.shp;/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_SantaClara_16.shp;/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_Sunnyvale_16.shp", 
                                  OUTPUT = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/LUregs_ThreeCities_T2.shp")
SCC_3Cities_Zoning_T2 <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "LUregs_ThreeCities_T2")
SCC_3Cities_Zoning_T2 <- spTransform(SCC_3Cities_Zoning_T2, CRSobj = CSP_Zone3_Proj)

Parcels_Join_T2Zoning <- over(SCCparcels_centroids, SCC_3Cities_Zoning_T2)
SCCparcels <- spCbind(SCCparcels, Parcels_Join_T2Zoning)
SCCparcels <- SCCparcels[, -(2:16)] # Remove unneeded columns

Parcels_Join_T1_T2_Zoning <- as.data.frame(SCCparcels)
save(Parcels_Join_T1_T2_Zoning, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/7_ZoningByParcel_DF.RData")

remove(SCCparcels, SCCparcels_centroids, SCC_3Cities_Zoning_T1, SCC_3Cities_Zoning_T2, Parcels_Join_T1Zoning, Parcels_Join_T2Zoning, Parcels_Join_T1_T2_Zoning)

# Bring in lot size
SCCparcels <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "09_01_2015_parcels_SCConly_proj")
ParcelLotSize <- as.data.frame(SCCparcels)
ParcelLotSize <- ParcelLotSize[, (1:4)] # Remove unneeded columns
ParcelLotSize <- ParcelLotSize[, -(2:3)] # Remove unneeded columns
save(ParcelLotSize, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/8_LotSizeByParcel_DF.RData")
remove(SCCparcels, ParcelLotSize)

# Merging
# Load data frames to combine
# Import allowable data tabs from csv tables
T1_Zoning <- read.csv("/Users/charlesgabbe/Google Drive/Research_Projects/Zoning_SiliconValley/Data/AllowableDensity_ByCity/ThreeCities_Zoning_T1.csv", header = T)
T1_GP <- read.csv("/Users/charlesgabbe/Google Drive/Research_Projects/Zoning_SiliconValley/Data/AllowableDensity_ByCity/ThreeCities_GP_T1.csv", header = T)
T2_Zoning <- read.csv("/Users/charlesgabbe/Google Drive/Research_Projects/Zoning_SiliconValley/Data/AllowableDensity_ByCity/ThreeCities_Zoning_T2.csv", header = T)
T2_GP <- read.csv("/Users/charlesgabbe/Google Drive/Research_Projects/Zoning_SiliconValley/Data/AllowableDensity_ByCity/ThreeCities_GP_T2.csv", header = T)
# Open other components
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/1_Parcels_DF_CensusIDs.RData")  # load SCCparcels.df
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/2_CensusACS_DF.RData")  # load All_ACS_09
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/3_EPASLD_DF.RData")  # load EPA SLD
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/4_RailDistanceFt_DF.RData")  # load Dist_Parcel_Rail
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/5_ElemAPI13_DF.RData")  # load ElemAPI13
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/6_ElevSlope_DF.RData")  # load ElevSlope
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/7_ZoningByParcel_DF.RData")  # load Parcels_Join_T1_T2_Zoning
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/8_LotSizeByParcel_DF.RData")  # load ParcelLotSize

# Merge data
ParcelVars_SV <- left_join(SCCparcels.df, All_ACS_09)
ParcelVars_SV <- left_join(ParcelVars_SV, EPASLD)
ParcelVars_SV <- left_join(ParcelVars_SV, Dist_Parcel_Rail)
ParcelVars_SV <- left_join(ParcelVars_SV, ElemAPI13)
ParcelVars_SV <- left_join(ParcelVars_SV, ElevSlope)
ParcelVars_SV <- left_join(ParcelVars_SV, Parcels_Join_T1_T2_Zoning)
ParcelVars_SV <- left_join(ParcelVars_SV, SCCparcels.df)
ParcelVars_SV <- left_join(ParcelVars_SV, T1_Zoning)
ParcelVars_SV <- left_join(ParcelVars_SV, T1_GP)
ParcelVars_SV <- left_join(ParcelVars_SV, T2_Zoning)
ParcelVars_SV <- left_join(ParcelVars_SV, T2_GP)
ParcelVars_SV <- left_join(ParcelVars_SV, ParcelLotSize)

# Clean up
remove(SCCparcels.df, All_ACS_09, EPASLD, Dist_Parcel_Rail, ElemAPI13, ElevSlope, Parcels_Join_T1_T2_Zoning, ParcelLotSize)
remove(T1_Zoning, T1_GP, T2_Zoning, T2_GP)

# Save
save(ParcelVars_SV, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/Parcels_AllVars_DF.RData")

# If statements for maximum allowable density
load("/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/Parcels_AllVars_DF.RData")  # load Parcels_Join_T1_T2_Zoning

# San Jose: If PD > 0, use PD, if PD = 0, then lower of GP and Zoning densities.
# Santa Clara: If Zoning = PD or PD-MC, then GP density. If not, use Zoning Density.
# Sunnyvale: = zoning density
ParcelVars_SV <- ParcelVars_SV %>% mutate(MaxDensity_T1 = ifelse(T1_City=="San Jose" & T1_ZoneV2 > 0, as.numeric(T1_ZoneV2),
                                                          ifelse(T1_City=="San Jose" & T1_ZoneV2 == 0 & MaxDensity_T1_GP>MaxDensity_T1_ZONING, as.numeric(MaxDensity_T1_ZONING),
                                                          ifelse(T1_City=="San Jose" & T1_ZoneV2 == 0 & MaxDensity_T1_GP<=MaxDensity_T1_ZONING, as.numeric(MaxDensity_T1_GP),
                                                          ifelse(T1_City=="Santa Clara" & T1_ZoneV1=="PD" | T1_ZoneV1=="PD-MC", as.numeric(MaxDensity_T1_GP),
                                                          ifelse(T1_City=="Santa Clara" & T1_ZoneV1!="PD" & T1_ZoneV1!="PD-MC", as.numeric(MaxDensity_T1_ZONING),
                                                          ifelse(T1_City=="Sunnyvale", as.numeric(MaxDensity_T1_ZONING),
                                                          0)))))))

ParcelVars_SV <- ParcelVars_SV %>% mutate(MaxDensity_T2 = ifelse(T2_City=="San Jose" & T2_ZoneV2 > 0, as.numeric(T2_ZoneV2),
                                                          ifelse(T2_City=="San Jose" & T2_ZoneV2 == 0 & MaxDensity_T2_GP>MaxDensity_T2_ZONING, as.numeric(MaxDensity_T2_ZONING),
                                                          ifelse(T2_City=="San Jose" & T2_ZoneV2 == 0 & MaxDensity_T2_GP<=MaxDensity_T2_ZONING, as.numeric(MaxDensity_T2_GP),
                                                          ifelse(T2_City=="Santa Clara" & T2_ZoneV1=="PD" | T2_ZoneV1=="PD-MC", as.numeric(MaxDensity_T2_GP),
                                                          ifelse(T2_City=="Santa Clara" & T2_ZoneV1!="PD" & T2_ZoneV1!="PD-MC", as.numeric(MaxDensity_T2_ZONING),
                                                          ifelse(T2_City=="Sunnyvale", as.numeric(MaxDensity_T2_ZONING),
                                                          0)))))))

ParcelVars_SV$ChgDensity_T1_T2 <- (ParcelVars_SV$MaxDensity_T2 - ParcelVars_SV$MaxDensity_T1) 
ParcelVars_SV <- base::subset(ParcelVars_SV, !(is.na(ChgDensity_T1_T2))) # Dropped observations from other cities and ones not overlapping with one or both zoning shapefiles

# Create dummy variables
ParcelVars_SV$NoRezone_Ind <- as.numeric(ParcelVars_SV$ChgDensity_T1_T2==0)
ParcelVars_SV$Downzone_Ind <- as.numeric(ParcelVars_SV$ChgDensity_T1_T2<0)  
ParcelVars_SV$Upzone_Ind <- as.numeric(ParcelVars_SV$ChgDensity_T1_T2>0)
# Create categorical variables, 0=no change, 1=downzoned, 2=upzoned
ParcelVars_SV <- ParcelVars_SV %>% mutate(RezoneCat = ifelse(NoRezone_Ind==1, as.numeric(0),
                                                      ifelse(Downzone_Ind == 1, as.numeric(1),
                                                      ifelse(Upzone_Ind == 1, as.numeric(2),  
                                                      0))))
ParcelVars_SV$RezoneCat <- factor(ParcelVars_SV$RezoneCat) # Convert RezoneCat to factor

save(ParcelVars_SV, file="/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles/Parcels_AllVars_DF.RData")


# DESCRIPTIVE STATISTICS
# Counts and land area
cities <- group_by(ParcelVars_SV, T1_City, RezoneCat)
parcelcounts <- dplyr::count(cities) # Count of rezoned parcels
ungroup(parcelcounts)
group_by(parcelcounts, T1_City)
parcelcounts <- parcelcounts %>% dplyr::mutate(sum(n)) # Number of parcels by city
colnames(parcelcounts) <- c("T1_City", "RezoneCat", "RezonedParcels", "CityTotalParcels")

acres <- dplyr::summarise(cities, RezonedAcres=sum(ACRES)) # Land area of rezoned parcels
ungroup(acres)
group_by(acres, T1_City)
acres <- acres %>% dplyr::mutate(CityTotalAcres=sum(RezonedAcres)) # Total land area by city

SummaryStats <- merge(parcelcounts, acres) # Combine
SummaryStats <- SummaryStats %>% dplyr::mutate(PctRezonedCityParcels = RezonedParcels/CityTotalParcels) # Calc pct parcels rezoned
SummaryStats <- SummaryStats %>% dplyr::mutate(PctRezonedCityArea = RezonedAcres/CityTotalAcres) # Calc pct land area rezoned
remove(cities, parcelcounts, acres) # Clean up


# MODELS


##### DO THE STEPS BELOW AGAIN ONCE I'VE GOTTEN THE TABLE FINALIZED

# Join parcel shapefile with table
SCCparcels <- readOGR(dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "09_01_2015_parcels_SCConly_proj")
SCCparcels <- spTransform(SCCparcels, CRSobj = CSP_Zone3_Proj) # Project to California State Plane
SCCparcels <- SCCparcels[, -(2:16)] # Remove unnecessary columns
SCCParcels_SpatialVars <- merge(SCCparcels, ParcelVars_SV, by.x ="PARCEL_ID", by.y = "PARCEL_ID")
names(SCCParcels_SpatialVars) # Check to make sure the merge worked.
writeOGR(SCCParcels_SpatialVars, dsn = "/Users/charlesgabbe/Dropbox/SV_Zoning_WorkingFiles", layer = "SCC_Parcels_SpatialVars", driver="ESRI Shapefile", morphToESRI=TRUE)
remove(SCCparcels)

# Mapping data

# Map entire extent - NOT WORKING YET
spplot(SCCParcels_SpatialVars, "RezoneCat", main = "Rezonings in Silicon Valley", 
       col = "transparent")
# Zoom to each city 


# MISC SAVED CODE

# Join Census tract boundaries (spatial) with variables (tabular data)
SCC_tracts_ACS09 <- geo_join(SCCtracts, All_ACS_09, "GEOID", "GEOID_TR")
