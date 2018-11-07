# Originally written by Jake Roush on 5/16/17
# Last modified: 8/24/17 by Jake Roush

# This script preps the manure data for further GIS and Python analysis for the NOAA Tipping Points project. 
# It looks at the 2012 USDA Ag Census, DEQ CAFOs, and NRCS nutrient rates for different types of 
# animal manure.

# Many of the methods and processes used in this script come from an Access Database created by 
# Emily Luscz. This original database is located at: S:\Users\lusczemi\Hydroload\HAB_GLRI\Animals

# Disabling scientific notation (comment out if not wanted)
options(scipen=999)

### Load packages

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(xlsx)

### Setting working directory - should change according to whoever is working on this
setwd("S:/Users/roushjac/Tipping Points/Manure/Script")

### Load external Excel files
# All CAFOs, most recently updated as of 1-31-13
"All CAFOs" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/QFH GLB CAFO updated final.xlsx", sheet = 1)

# All animal inventories (number of animals by type/state/county)
"All Inventories" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/QFH FINAL ANIMAL INVENTORY AND OPERATION TABLES.xlsx", sheet = 1)

# All operations (number of farms by type/state/county)
"All Operations" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/QFH FINAL ANIMAL INVENTORY AND OPERATION TABLES.xlsx", sheet = 2)

# Spreadable acres per county - Ag land treated with manure
"Manure Acres" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/GLB_total_manure_acres_cty_2012.xlsx", sheet = 1)

# Operations with manure treated acres
"Manure Operations" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/GLB_operations_with_manure_2012.xlsx", sheet = 1)

# Excel sheet made my exporting the clipped GLB counties from GIS -
# only useful here for pulling out unique county IDs
"GLBCountyIDs" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/GLB_Counties_WithIDs.xls", sheet = 1)

# Animal types for CAFOs, Ag Census, and NRCS data. For use in matching up animal types,
# which are different between data sources
"Animal Type Match" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/QFH animal type matches.xlsx", sheet = 2)

# Bin sizes matched with inventory data types
"Bin Sizes" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/QFH Bin size.xlsx", sheet = 2)
`Bin Sizes`<-`Bin Sizes`[!(is.na(`Bin Sizes`$ItemBin)),]

# Limits on what animal numbers are considered confined and unconfined for each species type
"Limits on Confinement" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/ECL Limits on Confinement.xlsx", sheet = 1)

# Nutrient excretion rates for each animal type. Taken from the NRCS. Also loading in a sheet Emily already made with average nutrient excretion rates.
"NRCS Nutrients" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/ECL NRCS Nutrients.xlsx", sheet = 1)
# For nutrient rate averages, rates are in animal/day
"Nutrient Rate Averages" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/ECL-NRCS Nutrients Rate Averages-8a.xlsx", sheet = 1)

# Empirical N:P ratios in lagoons, accounting for denitification
"Manure NP Ratios" <- read_excel("S:/Users/roushjac/Tipping Points/Manure/Excel Sheets/QFH_ManureExcretionNPRatios.xlsx", sheet = 1)


######################################################

### Listing all magic numbers here. Still need to find what these are and where they come from.
# Update - figured these out!
AcresToMetersSquaredConversion <- 4046.856
LbToKgConversion <- 0.45359237

# The given Nutrient Rate Averages dataframe has rates in (lb of nutrient)/animal/day - converting to kg
`Nutrient Rate Averages`$`AvgOfN (as excreted)` <- `Nutrient Rate Averages`$`AvgOfN (as excreted)` * LbToKgConversion
`Nutrient Rate Averages`$`AvgOfP (as excreted)` <- `Nutrient Rate Averages`$`AvgOfP (as excreted)` * LbToKgConversion

### Combine inventories and operations into one dataframe. This is a full outer join (includes
### all records for both data frames, even if one has NA data)

"All Census Data" <-merge(`All Inventories`, `All Operations`, by = colnames(`All Inventories`[c("CTYID", "Data Item", "Bin", "ItemBin")]), all = TRUE)
colnames(`All Census Data`)[5]<-"Animal Number"
colnames(`All Census Data`)[7]<-"Operations"
`All Census Data`$Type.x <- NULL
`All Census Data`$Type.y <- NULL
# This merge creates duplicate records (and I don't know why!) - getting rid of them
`All Census Data` <- `All Census Data`[!duplicated(`All Census Data`),]


# Cleaning up manure acres dataframe
`Manure Acres` <- `Manure Acres`[c("State", "State ANSI", "County", "County ANSI", "Value")]
colnames(`Manure Acres`)[5] <- "Acres"

# Cleaning up manure operations dataframe
`Manure Operations` <- `Manure Operations`[c("State", "State ANSI", "County", "County ANSI", "Value")]
colnames(`Manure Operations`)[5] <- "Operations"

# Combine manure acres and operations into one dataframe with a full outer join
"Manure Acres and Operations" <- merge(`Manure Acres`, `Manure Operations`, all = TRUE)

# Renaming some counties to be consistent with other dataframes for merging
`Manure Acres and Operations`$County <- gsub("DE KALB", "DEKALB", `Manure Acres and Operations`$County)
`Manure Acres and Operations`$County <- gsub("ST CLAIR", "ST. CLAIR", `Manure Acres and Operations`$County)
`Manure Acres and Operations`$County <- gsub("ST JOSEPH", "ST. JOSEPH", `Manure Acres and Operations`$County)
`Manure Acres and Operations`$County <- gsub("ST LAWRENCE", "ST. LAWRENCE", `Manure Acres and Operations`$County)

# Changing (D) to NA for easier indexing
`Manure Acres and Operations`$Acres <- gsub("(D)", NA, `Manure Acres and Operations`$Acres)
`Manure Acres and Operations`$Acres <- as.numeric(`Manure Acres and Operations`$Acres)

### Find CAFOs per county, their bin size, and count animal number

# Pulling out unique county IDs for each county. County IDs are taken from the attribute table of 
# the clipped GLB county shapefile on the S drive.
CountyIds <- GLBCountyIDs[c("County", "GEOID", "State")]
CountyIds <- unique(CountyIds)
colnames(CountyIds) <- c("County", "CTYID", "State")
CountyIds$County <- toupper(CountyIds$County)

# Changing all county and state names of CAFO data table to uppercase for matching with the County ID data table
`All CAFOs`$County <- toupper(`All CAFOs`$County)
`All CAFOs`$State <- toupper(`All CAFOs`$State)

# Removing extraneous spaces from the end of counties in the CAFO dataframe. This was causing issues when merging;
# specifically for Wisconsin, all counties had extra spaces at the end
# Using a function from the stringr package. Need to write a loop because the function only works on 1 character vector at a time
for (a in 1:nrow(`All CAFOs`)){
  `All CAFOs`[a,"County"] <- str_trim(`All CAFOs`[a,"County"])
}

# Some counties from the GLBCountyIDs with 2 part names have been concatenated into 1 name,
# reuslting in incorrect county names - figuring out which counties this happened to by comparing
# to the CAFO dataframe and replacing them with correct, 2-part names
CorrectCountyNames <- as.data.frame(unique(CountyIds$County))
colnames(CorrectCountyNames) <- "County"
CorrectCountyNames$InCAFOTable <- NULL
for (a in 1:nrow(CorrectCountyNames)){
  CorrectCountyNames[a,"InCAFOTable"] <- any(grepl(`CorrectCountyNames`[a,"County"], `All CAFOs`$County))
}

CountyIds$County <- gsub("LAPORTE", "LA PORTE", CountyIds$County)


# # Renaming states to match up with the correct CAFO table
# CountyIds[(CountyIds$State == "WI"), "State"] <- "WISCONSIN"
# CountyIds[(CountyIds$State == "PA"), "State"] <- "PENNSYLVANIA"
# CountyIds[(CountyIds$State == "OH"), "State"] <- "OHIO"
# CountyIds[(CountyIds$State == "NY"), "State"] <- "NEW YORK"
# CountyIds[(CountyIds$State == "MN"), "State"] <- "MINNESOTA"
# CountyIds[(CountyIds$State == "MI"), "State"] <- "MICHIGAN"
# CountyIds[(CountyIds$State == "IN"), "State"] <- "INDIANA"
# CountyIds[(CountyIds$State == "IL"), "State"] <- "ILLINOIS"
# 
# # Fixing county IDs for some records that don't have complete information
# # Manually searched for missing IDs using https://www.census.gov/geo/reference/codes/cou.html
# 
# CountyIds[(CountyIds$State == "ILLINOIS" & CountyIds$County == "DEWITT"), "CTYID"] <- "17039"
# CountyIds[(CountyIds$State == "INDIANA" & CountyIds$County == "DE KALB"), "County"] <- "DEKALB"
# CountyIds[(CountyIds$State == "INDIANA" & CountyIds$County == "DEKALB"), "CTYID"] <- "18033"
# CountyIds[(CountyIds$State == "INDIANA" & CountyIds$County == "LA PORTE"), "CTYID"] <- "18091"
# CountyIds[(CountyIds$State == "INDIANA" & CountyIds$County == "ST JOSEPH"), "CTYID"] <- "18141"
# 
# 
# # Removing county names that are typos
# CountyIds<-CountyIds[!(CountyIds$State == "ILLINOIS" & CountyIds$County == "JODAVIESS"), ]
# CountyIds<-CountyIds[!(CountyIds$State == "ILLINOIS" & CountyIds$County == "MARHSALL"), ]
# CountyIds<-CountyIds[!(CountyIds$State == "ILLINOIS" & CountyIds$County == "MONTOGMERY"), ]
# CountyIds<-CountyIds[!(CountyIds$State == "ILLINOIS" & CountyIds$County == "MORGON"), ]
# CountyIds<-CountyIds[!(CountyIds$State == "ILLINOIS" & CountyIds$County == "VERMILLION"), ]
# CountyIds<-CountyIds[!(CountyIds$State == "MICHIGAN" & CountyIds$County == "LENEWEE"), ]
# CountyIds<-CountyIds[!(CountyIds$State == "NEW YORK" & CountyIds$County == "ONIEDA"), ]
# CountyIds<-CountyIds[!(CountyIds$State == "NEW YORK" & CountyIds$County == "WYOMIING"), ]


# Matching County IDs to CAFOs. Do not want to merge all rows - not all counties in the GLB have CAFOs
`All CAFOs` <- merge(`All CAFOs`, CountyIds, by = c("State", "County"))

# Changing column names of CAFO and Census species columns to reflect where they came from -
# need to keep these straight for matching up data
colnames(`All CAFOs`)[9] <- "CAFO Species"
colnames(`All Census Data`)[2] <- "Census Species"
colnames(`Animal Type Match`)[2] <- "Census Species"
colnames(`Animal Type Match`)[3] <- "CAFO Species"

# Removing rows in CAFO table that have primary species of "other" - 
# these CAFOs are reported as having 0 animal number anyway
`All CAFOs` <- `All CAFOs`[!(`All CAFOs`$`CAFO Species` == "OTHER"), ]

# Changing all CAFO chickens into generic chickens

`All CAFOs`$`CAFO Species` <- gsub("CHICKEN-LAYER", "CHICKEN", `All CAFOs`$`CAFO Species`)
`All CAFOs`$`CAFO Species` <- gsub("CHICKEN-BROILER", "CHICKEN", `All CAFOs`$`CAFO Species`)

# Match CAFOs to a bin size
`All CAFOs`$`Bin` <- NA
  # Need to find Census species corresponding with CAFO species to find the appropriate bin -
  # which is separated according to both bin size and Census species
`All CAFOs` <- merge(`All CAFOs`, `Animal Type Match`[-1], by = "CAFO Species", all.x = TRUE)

`All CAFOs` <- `All CAFOs`[!duplicated(`All CAFOs`),]

for (a in 1:nrow(`All CAFOs`)){
  species<-`All CAFOs`[a, "Census Species"]
  animalnumber<-round(`All CAFOs`[a, "Animal Number"])
 if(!(is.na(species) | is.na(animalnumber) | animalnumber == 0 | !(species %in% `Bin Sizes`$Animal))){ # If any one of these conditions are true, do not assign a bin
   # Skipping rows with NA animal number/species, or if species does not have a bin size
    `All CAFOs`[a, "Bin"] <- `Bin Sizes`[(`Bin Sizes`$Animal == species) & 
                                               (`Bin Sizes`$`Bin Max` >= animalnumber) & 
                                               (`Bin Sizes`$`Bin Min` <= animalnumber), "Bin Size"]
 }
}

# Removing CAFOs with 0 animal number since they are probably no longer operational
`All CAFOs` <- `All CAFOs`[!(`All CAFOs`$`Animal Number` == 0),]


# Finding total CAFOs per county, sorted by bin size and species type
"Summed CAFOs" <- as.data.frame(unique(`All CAFOs`$CTYID))
"Summed CAFOs" <- as.data.frame(`Summed CAFOs`[!(is.na(`Summed CAFOs`[,1])),])
colnames(`Summed CAFOs`) <- c("CTYID")
"Summed CAFOs" <- merge(`Summed CAFOs`, unique(`All CAFOs`[,c("CTYID", "Bin", "CAFO Species", "State")]), by = "CTYID")


# Counting number of CAFOs and their animal number for each county, species, and bin size
# This loop only looks at species that have designated bin sizes
# Had to write this in a strange way since my indices kept ending up with a bunch of extra NA rows...?
for (a in 1:nrow(`Summed CAFOs`)){
onecaforow <- `All CAFOs`[`All CAFOs`$`CTYID` == (`Summed CAFOs`[a,"CTYID"]) & 
                              `All CAFOs`$`Bin` == (`Summed CAFOs`[a,"Bin"]) &
                              `All CAFOs`$`CAFO Species` == (`Summed CAFOs`[a,"CAFO Species"]),]
onecaforow <- onecaforow[!is.na(onecaforow$State),] # Removing CAFOs from Ontario
`Summed CAFOs`[a,"Total Animal Number"] <- sum(onecaforow$`Animal Number`)
`Summed CAFOs`[a,"Number of CAFOs"] <- nrow(onecaforow)
}

# Now summing animal numbers for species with no given bin sizes
for (a in 1:nrow(`Summed CAFOs`)){
  if (is.na(`Summed CAFOs`[a,"Bin"])){
  onecaforow <- `All CAFOs`[`All CAFOs`$`CTYID` == (`Summed CAFOs`[a,"CTYID"]) & 
                              `All CAFOs`$`CAFO Species` == (`Summed CAFOs`[a,"CAFO Species"]),]
  onecaforow <- onecaforow[!is.na(onecaforow$State),] # Removing CAFOs from Ontario
  `Summed CAFOs`[a,"Total Animal Number"] <- sum(onecaforow$`Animal Number`)
  `Summed CAFOs`[a,"Number of CAFOs"] <- nrow(onecaforow)
  }
}


# Somehow the county IDs in `Summed CAFOs` were converted into data class "levels" - converting to numeric
`Summed CAFOs`$CTYID <- as.numeric(as.character(`Summed CAFOs`$CTYID))

# Also adding in corresponding Census Species to the CAFO dataframe for future matching of data
`Summed CAFOs` <- merge(`Summed CAFOs`, `Animal Type Match`[c(2,3)], by = "CAFO Species", all.x = TRUE)

# Removing duplicate records from the summed CAFOs, probably created as a result of there being two records 
# in "Animal Type Match" that correspond to "BEEF/VEAL"
`Summed CAFOs` <- `Summed CAFOs`[!duplicated(`Summed CAFOs`),]


### Estimating acres for those records with NA

# Finding county IDs for the manure acres & operations data table

`Manure Acres and Operations` <- merge(`Manure Acres and Operations`, CountyIds, by = c("State", "County"), all.y = TRUE)
# Giving counties with no State ANSI value a number of farms based on the census records
`Manure Acres and Operations`[is.na(`Manure Acres and Operations`$`State ANSI`), "Operations"] <- 
  sum(`All Census Data`[`All Census Data`$CTYID == `Manure Acres and Operations`[is.na(`Manure Acres and Operations`$`State ANSI`),]$CTYID, "Operations"], na.rm=TRUE)

# Some counties have NA acres - using a statewide average of acres/farm to estimate NA values
estimateacresdf <- `Manure Acres and Operations`[is.na(`Manure Acres and Operations`$Acres),]
for (a in 1:nrow(estimateacresdf)){
  statetotalacres <- sum(`Manure Acres and Operations`[(`Manure Acres and Operations`$State == estimateacresdf[a,"State"]) & !(`Manure Acres and Operations`$CTYID == estimateacresdf[a,"CTYID"]), "Acres"], na.rm = TRUE)
  statetotalfarms <- sum(`Manure Acres and Operations`[(`Manure Acres and Operations`$State == estimateacresdf[a,"State"]) & !(`Manure Acres and Operations`$CTYID == estimateacresdf[a,"CTYID"]), "Operations"], na.rm = TRUE)
  `Manure Acres and Operations`[(`Manure Acres and Operations`$"CTYID" == estimateacresdf[a,"CTYID"]), "Acres"] <-
    ((statetotalacres/statetotalfarms)*`Manure Acres and Operations`[(`Manure Acres and Operations`$"CTYID" == estimateacresdf[a,"CTYID"]), "Operations"])
}

# Finding something called "Man_m2" from query 3b in the Access database. Still do not know what this is
# or where the magic number comes from, but I know that this value is useful somewhere down the line.
# Update -- I now know that m2 is meters squared
`Manure Acres and Operations`$Man_m2 <- `Manure Acres and Operations`$Acres * AcresToMetersSquaredConversion

### Summing up all non-CAFO inventories. We are assuming that the Ag Census data includes both CAFOs and 
### non-CAFOs. To estimate unknowns for only non-CAFOs, we need to have data on exclusively non-CAFOs. We 
### are finding this by subtracting CAFO data out of the Ag Census data.

`All non-CAFOs` <- `All Census Data`

# Selecting only those counties that are in the GLB, as defined by the CountyIds dataframe
`All non-CAFOs` <- `All non-CAFOs`[grepl(paste(CountyIds$CTYID, collapse = "|"), `All non-CAFOs`$CTYID),]

## Creating one generic species for all chickens in the census data. CAFOs are originally reported with 1 generic chicken species,
## we need them to match up to ensure that CAFO chickens are properly subtracted out from census numbers

# First, totaling up all chickens of each type throughout the whole basin. This will be used to create a "weight" that will scale the generic chicken
# nutrient excretion rate and limit on confinement proportional to the distribution
chickenspecies <- c("CHICKENS, BROILERS","CHICKENS, LAYERS","CHICKENS, PULLETS, REPLACEMENT", "CHICKENS, ROOSTERS")
totalBasinChickens <- data.frame("Census Species" = chickenspecies, "BasinTotal" = NA, check.names = FALSE)
for (a in 1:nrow(totalBasinChickens)){
  onespecies <- totalBasinChickens[a,"Census Species"]
  speciesBasinTotal <- sum(`All non-CAFOs`[`All non-CAFOs`$`Census Species` == onespecies, "Animal Number"], na.rm = TRUE)
  totalBasinChickens[a,"BasinTotal"] <- speciesBasinTotal
}

allBasinChickenNumber <- sum(totalBasinChickens$BasinTotal)
totalBasinChickens$Weight <- totalBasinChickens$BasinTotal / allBasinChickenNumber

# Creating new generic nutrient excretion rate for chickens using a weighted average
totalBasinChickens <- merge(totalBasinChickens, `Nutrient Rate Averages`, by = "Census Species")
totalBasinChickens$NContrib <- totalBasinChickens$`AvgOfN (as excreted)` * totalBasinChickens$Weight
totalBasinChickens$PContrib <- totalBasinChickens$`AvgOfP (as excreted)` * totalBasinChickens$Weight

`Nutrient Rate Averages`[`Nutrient Rate Averages`$`Census Species` == "CHICKENS", "AvgOfN (as excreted)"] <- sum(totalBasinChickens$NContrib)
`Nutrient Rate Averages`[`Nutrient Rate Averages`$`Census Species` == "CHICKENS", "AvgOfP (as excreted)"] <- sum(totalBasinChickens$PContrib)

# Creating new generic confinement limit for chickens using a weighted average
totalBasinChickens <- merge(totalBasinChickens, `Limits on Confinement`[,c(2,6)], by = "Census Species", all.x = TRUE)
totalBasinChickens$ConfContrib <- totalBasinChickens$`Lower Confinement Limit` * totalBasinChickens$Weight

`Limits on Confinement`[`Limits on Confinement`$`Census Species` == 'CHICKENS', 'Lower Confinement Limit'] <- sum(totalBasinChickens$ConfContrib, na.rm = TRUE)

`All non-CAFOs` <- merge(`All non-CAFOs`, `Bin Sizes`[,c("Bin Min", "Bin Max", "ItemBin", "Bin Middle")], by = "ItemBin", all.x = TRUE)

## For "CHICKENS, LAYERS", the data was entered strangely - one record has a sum total of all other records with no bin size, while other records
## have a bin size and number of operations but no animal number (see OneNote document w/ screenshots). We could estimate the unknown animal numbers
## using the bin middle as we do for other records, but since we have the total, it makes more sense to distribute it proportionally across other
## records. This is currently done by distributing the total from the smallest bins upward, using the bin middles as an estimate. If the remainder
## of chickens, after distributing them out to all but the largest bin, fits within the bin size, the remainder is used. If the remainder does not
## fit in the bin size, the other bins are redistributed according to the ratio between the observed total and the total estimated by using only
## the bin middle values.

for (a in 1:length(unique(`All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS",]$CTYID))){
  oneCountyID <- unique(`All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS",]$CTYID)[a]
  oneCLCounty <- `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS"),]
  ObservedCount <- oneCLCounty[is.na(oneCLCounty$Operations), "Animal Number"]
  oneCLCountyBinsOnly <- oneCLCounty[!is.na(oneCLCounty$Operations),]
  oneCLCountyBinsOnlyNoMax <- oneCLCountyBinsOnly[!oneCLCountyBinsOnly$`Bin Middle` == max(oneCLCountyBinsOnly$`Bin Middle`),]
  maxBinMiddle <- max(oneCLCountyBinsOnly$`Bin Middle`)
  maxRemainder <- ObservedCount - sum(oneCLCountyBinsOnlyNoMax$`Operations`*oneCLCountyBinsOnlyNoMax$`Bin Middle`)
  maxLowerThreshold <- oneCLCountyBinsOnly[oneCLCountyBinsOnly$`Bin Middle` == max(oneCLCountyBinsOnly$`Bin Middle`),"Bin Min"]*oneCLCountyBinsOnly[oneCLCountyBinsOnly$`Bin Middle` == max(oneCLCountyBinsOnly$`Bin Middle`),"Operations"]
  if (maxRemainder > maxLowerThreshold){
    `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & (`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Animal Number"] <- maxRemainder
    `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Animal Number"] <-
      `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Operations"] *
      `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Bin Middle"]
  } else {
    DistRatio <- ObservedCount / (sum(oneCLCountyBinsOnly$`Operations` * oneCLCountyBinsOnly$`Bin Middle`))
    `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Animal Number"] <-
      `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Operations"] *
      `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Bin Middle"] * DistRatio
    `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & (`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Animal Number"] <- ObservedCount -
      sum(`All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Animal Number"])

  }
}

## Estimating new bin middles for "chicken, layers" bins based on observed values.
## This will be done later in the script for other species, need to do it now in order to sum up chickens

chickenLayerBins <- unique(`Bin Sizes`[`Bin Sizes`$Animal == "CHICKENS, LAYERS", ]$`Bin Size`)
for (a in 1:length(chickenLayerBins)){
avgFarmsOneBin <- `All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & `All non-CAFOs`$Bin == chickenLayerBins[a] & `All non-CAFOs`$`Animal Number` != 0 & `All non-CAFOs`$Operations != 0, "Animal Number"] /
  `All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & `All non-CAFOs`$Bin == chickenLayerBins[a] & `All non-CAFOs`$`Animal Number` != 0 & `All non-CAFOs`$Operations != 0, "Operations"]  
avgFarmSizeOneBin <- mean(avgFarmsOneBin)
  `All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & `All non-CAFOs`$Bin == chickenLayerBins[a], "Bin Middle"] <- avgFarmSizeOneBin
}

# Some counties do not have a total "CHICKENS, LAYERS" number, but still have farms separated by bin size. For these, using the bin middle to estimate animal numbers
# for each bin size.

countieswithnochickenlayers <- na.omit(unique(`All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & `All non-CAFOs`$`Animal Number` == 0,]$CTYID))
`All non-CAFOs`[`All non-CAFOs`$CTYID %in% countieswithnochickenlayers & `All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & `All non-CAFOs`$`Animal Number` == 0, "Animal Number"] <-
  `All non-CAFOs`[`All non-CAFOs`$CTYID %in% countieswithnochickenlayers & `All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & `All non-CAFOs`$`Animal Number` == 0, "Operations"] *
  `All non-CAFOs`[`All non-CAFOs`$CTYID %in% countieswithnochickenlayers & `All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & `All non-CAFOs`$`Animal Number` == 0, "Bin Middle"]

# Changing bins with no size to NA to make for easier indexing
`All non-CAFOs`[,"Bin"] <- gsub("NOT SPECIFIED", NA, `All non-CAFOs`[,"Bin"])

# Removing records for "CHICKENS, LAYERS" with the county totals. We do not need these anymore; they were just distributed through the other records
`All non-CAFOs` <- `All non-CAFOs`[!(`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & is.na(`All non-CAFOs`$Bin)),]


# Need to estimate number of chickens for (D) records of those species that do not have bins. Cannot do this after subtracting out
# CAFOs because we need to put chickens into a generic bin before subtracting them out.
# Going to use the lowest reported herd size for each species as an estimate for (D) farms
chickenspecies <- c("CHICKENS, BROILERS","CHICKENS, LAYERS","CHICKENS, PULLETS, REPLACEMENT", "CHICKENS, ROOSTERS")

chickensNoBins <- unique(`All non-CAFOs`[`All non-CAFOs`$`Census Species` %in% chickenspecies & is.na(`All non-CAFOs`$`Bin`),"Census Species"])
for (a in 1:length(chickensNoBins)){
  onespecies <- chickensNoBins[a]
  lowestfarmsize <- min(`All non-CAFOs`[`All non-CAFOs`$`Census Species` == onespecies & `All non-CAFOs`$`Animal Number` != 0 & `All non-CAFOs`$`Operations` != 0, "Animal Number"] /
    `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onespecies & `All non-CAFOs`$`Animal Number` != 0 & `All non-CAFOs`$`Operations` != 0, "Operations"])
  `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onespecies & `All non-CAFOs`$`Animal Number` == 0 & `All non-CAFOs`$`Operations` != 0, "Animal Number"] <- lowestfarmsize * `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onespecies & `All non-CAFOs`$`Animal Number` == 0 & `All non-CAFOs`$`Operations` != 0, "Operations"]
}

# Giving bins, based on farm size, to those chicken species with no reported bins
chickenbins <- as.data.frame(`Bin Sizes`[`Bin Sizes`$Animal == "CHICKENS, LAYERS",])
for (a in 1:nrow(chickenbins)){
  onechickenbin <- chickenbins[a,]
  binmin <- onechickenbin[['Bin Min']]
  binmax <- onechickenbin[['Bin Max']]
  `All non-CAFOs`[`All non-CAFOs`$`Census Species` %in% chickensNoBins & round(`All non-CAFOs`$`Animal Number` / `All non-CAFOs`$Operations) >= binmin & round(`All non-CAFOs`$`Animal Number` / `All non-CAFOs`$Operations) <= binmax, "Bin"] <- onechickenbin[['Bin Size']]
}

# Now totaling up all species and creating new rows with the totals, separated by bin
countieswithchickens <- unique(`All non-CAFOs`[`All non-CAFOs`$`Census Species` %in% c("CHICKENS, BROILERS","CHICKENS, LAYERS","CHICKENS, PULLETS, REPLACEMENT", "CHICKENS, ROOSTERS"), "CTYID"])
chickenbinlist <- `Bin Sizes`[`Bin Sizes`$Animal == "CHICKENS", ]$`Bin Size`
for (a in 1:length(countieswithchickens)){
  chickencounty <- countieswithchickens[a]
  for (a in 1:length(chickenbinlist)){
  totalChickNumber <- sum(`All non-CAFOs`[`All non-CAFOs`$CTYID == chickencounty & `All non-CAFOs`$`Census Species` %in% chickenspecies & `All non-CAFOs`$Bin == chickenbinlist[a], "Animal Number"], na.rm = TRUE)
  totalChickFarms <- sum(`All non-CAFOs`[`All non-CAFOs`$CTYID == chickencounty & `All non-CAFOs`$`Census Species` %in% chickenspecies & `All non-CAFOs`$Bin == chickenbinlist[a], "Operations"], na.rm = TRUE)
  genericChick <- data.frame(CTYID = chickencounty, "Census Species" = "CHICKENS", Bin = chickenbinlist[a], ItemBin = "CHICKENSNOT SPECIFIED", "Animal Number" = totalChickNumber, Operations = totalChickFarms, "Bin Min" = NA, "Bin Max" = NA, "Bin Middle" = NA, check.names = FALSE)
  `All non-CAFOs` <- rbind(`All non-CAFOs`, genericChick)
  }
}

# Removing extra generic chicken bins that have no records
`All non-CAFOs` <- `All non-CAFOs`[!(`All non-CAFOs`$`Census Species` == "CHICKENS" & `All non-CAFOs`$Operations == 0),]

# Now getting rid of all specific chicken bins from the census
`All non-CAFOs` <- `All non-CAFOs`[!(`All non-CAFOs`$`Census Species` %in% chickenspecies),]


## Matching Ag Census species with CAFO species

`All non-CAFOs` <- merge(`All non-CAFOs`, `Animal Type Match`[-1], by = "Census Species", all.x = TRUE)
`All non-CAFOs` <- `All non-CAFOs`[!is.na(`All non-CAFOs`$CTYID),]
# This merge creates duplicate records because the animal type match dataframe has two records for beef cows -
# could change this in the animal type match dataframe itself, but I'll leave it alone for now and
# remove the duplicates here
`All non-CAFOs` <- `All non-CAFOs`[!duplicated(`All non-CAFOs`), ]
# Also removing NA operations census records because they have no use
`All non-CAFOs` <- `All non-CAFOs`[!is.na(`All non-CAFOs`$Operations),]
# Also giving newly created chicken bins a minimum bin value to allow for comparison in the 
# CAFO occupancy fraction calculation below
# Also properly labelling the ItemBin field for the generic chickens

genericChickenBins <- `Bin Sizes`[`Bin Sizes`$Animal == 'CHICKENS',]
for (a in 1:nrow(genericChickenBins)){
  firstGenericBin <- genericChickenBins[a,]
  `All non-CAFOs`[`All non-CAFOs`$`Census Species` == firstGenericBin$Animal & `All non-CAFOs`$Bin == firstGenericBin$`Bin Size`, 'ItemBin'] <- firstGenericBin$ItemBin
  `All non-CAFOs`[`All non-CAFOs`$ItemBin == firstGenericBin$ItemBin, "Bin Min"] <- firstGenericBin$`Bin Min`
}


# Marking summed CAFO records that have equivalent census bins
`Summed CAFOs`$HasCensusBin <- NA
for (a in 1:nrow(`Summed CAFOs`)){
  onesummedCAFO <- `Summed CAFOs`[a,]
  eqCensusRow <- `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onesummedCAFO$`Census Species` & `All non-CAFOs`$`CTYID` == onesummedCAFO$CTYID & `All non-CAFOs`$Bin == onesummedCAFO$Bin,]
  if (nrow(eqCensusRow) > 0){
    `Summed CAFOs`[a, "HasCensusBin"] <- "Yes"
  } else {
    `Summed CAFOs`[a, "HasCensusBin"] <- "No"
  }
}


### There is a discrepancy between CAFO reported numbers and Census reported numbers.
# CAFOs reported maximum licensed animals, while Census is self-reported by farmers.
# We are going to assume that some CAFOs actually have less (or more) animals than they are
# licensed for, and give them an "occupancy fraction" based on what the census has reported.
# The fraction can be > or < 1.

`Summed CAFOs`$OcFraction <- 1
for (a in 1:nrow(`Summed CAFOs`)){
  onesummedCAFO <- `Summed CAFOs`[a,]
  if (!is.na(onesummedCAFO$Bin)){ # Only want to find occupancy fraction if CAFO record has a bin
  eqCensusRow <- `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onesummedCAFO$`Census Species` & `All non-CAFOs`$`CTYID` == onesummedCAFO$CTYID & `All non-CAFOs`$Bin == onesummedCAFO$Bin,]
  if (nrow(eqCensusRow) > 0 && eqCensusRow$`Animal Number` == 0){ # Giving census records an animal number estimate if they have none based on default bin middle and reassigning eqCensusRow
    `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onesummedCAFO$`Census Species` & `All non-CAFOs`$`CTYID` == onesummedCAFO$CTYID & `All non-CAFOs`$Bin == onesummedCAFO$Bin, "Animal Number"] <-
      `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onesummedCAFO$`Census Species` & `All non-CAFOs`$`CTYID` == onesummedCAFO$CTYID & `All non-CAFOs`$Bin == onesummedCAFO$Bin, "Operations"] * 
      `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onesummedCAFO$`Census Species` & `All non-CAFOs`$`CTYID` == onesummedCAFO$CTYID & `All non-CAFOs`$Bin == onesummedCAFO$Bin, "Bin Middle"]
    eqCensusRow <- `All non-CAFOs`[`All non-CAFOs`$`Census Species` == onesummedCAFO$`Census Species` & `All non-CAFOs`$`CTYID` == onesummedCAFO$CTYID & `All non-CAFOs`$Bin == onesummedCAFO$Bin,]
  }
  if (nrow(eqCensusRow) > 0 && eqCensusRow$Operations > onesummedCAFO$`Number of CAFOs` && # If non-CAFO size is less than smallest CAFO and bigger than bin minimum
      ((eqCensusRow$`Animal Number` - onesummedCAFO$`Total Animal Number`) / (eqCensusRow$`Operations` - onesummedCAFO$`Number of CAFOs`)) > `Bin Sizes`[`Bin Sizes`$ItemBin == eqCensusRow$ItemBin,]$`Bin Min` &&
      ((eqCensusRow$`Animal Number` - onesummedCAFO$`Total Animal Number`) / (eqCensusRow$`Operations` - onesummedCAFO$`Number of CAFOs`)) < min(`All CAFOs`[`All CAFOs`$`CAFO Species` == onesummedCAFO$`CAFO Species` & `All CAFOs`$State == onesummedCAFO$State & `All CAFOs`$Bin == onesummedCAFO$Bin, ]$`Animal Number`)){
    `Summed CAFOs`[`Summed CAFOs`$`CAFO Species` == onesummedCAFO$`CAFO Species` & `Summed CAFOs`$CTYID == onesummedCAFO$CTYID & `Summed CAFOs`$Bin == onesummedCAFO$Bin, "OcFraction"] <- 1
  }
    if (nrow(eqCensusRow) > 0 && eqCensusRow$Operations > onesummedCAFO$`Number of CAFOs` && # If non-CAFO size is less than smallest CAFO and less than bin minimum
        ((eqCensusRow$`Animal Number` - onesummedCAFO$`Total Animal Number`) / (eqCensusRow$`Operations` - onesummedCAFO$`Number of CAFOs`)) < `Bin Sizes`[`Bin Sizes`$ItemBin == eqCensusRow$ItemBin, ]$`Bin Min` &&
        ((eqCensusRow$`Animal Number` - onesummedCAFO$`Total Animal Number`) / (eqCensusRow$`Operations` - onesummedCAFO$`Number of CAFOs`)) < min(`All CAFOs`[`All CAFOs`$`CAFO Species` == onesummedCAFO$`CAFO Species` & `All CAFOs`$State == onesummedCAFO$State & `All CAFOs`$Bin == onesummedCAFO$Bin, ]$`Animal Number`)){
      `Summed CAFOs`[`Summed CAFOs`$`CAFO Species` == onesummedCAFO$`CAFO Species` & `Summed CAFOs`$CTYID == onesummedCAFO$CTYID & `Summed CAFOs`$Bin == onesummedCAFO$Bin, "OcFraction"] <- 
        ((eqCensusRow$`Animal Number` - eqCensusRow$Operations*`Bin Sizes`[`Bin Sizes`$ItemBin == eqCensusRow$ItemBin, ]$`Bin Min`) / onesummedCAFO$`Total Animal Number`)
    }
  }
}

# Merging OcFraction with all individual CAFOs because we need corrected animal numbers
# for each CAFO individually for calculating nutrient totals, and for the python
# analysis
`All CAFOs` <- merge(`All CAFOs`, `Summed CAFOs`[,c(1,2,3,9)], by = c("CAFO Species", "CTYID", "Bin"), all.x = TRUE)
# Also calculating new CAFO animal numbers
`All CAFOs`$`Animal Number` <- `All CAFOs`$`Animal Number` * `All CAFOs`$OcFraction


# Using the OcFraction parameter to calculate new animal numbers for CAFOs

`Summed CAFOs`$`Total Animal Number` <- `Summed CAFOs`$`Total Animal Number`*`Summed CAFOs`$OcFraction


# Subtracting out CAFOs from the Ag Census data. Final, non-CAFO counts are labeled as "corrected".
# Looping through the `Summed CAFOs` datatable because it is shorter - requires less time

`All non-CAFOs`$`Corrected Animal Number` <- `All non-CAFOs`$`Animal Number`
`All non-CAFOs`$`Corrected Operations` <- `All non-CAFOs`$`Operations`


for (a in 1:nrow(`Summed CAFOs`)){
  if (!is.na(`Summed CAFOs`[a,"Bin"])){
  onesummedCAFO <- `Summed CAFOs`[a,]
  onenonCAFO <- `All non-CAFOs`[(`All non-CAFOs`$CTYID == onesummedCAFO[,"CTYID"]) & 
                    (`All non-CAFOs`$Bin == onesummedCAFO[,"Bin"]) &
                    (`All non-CAFOs`$`CAFO Species` == onesummedCAFO[,"CAFO Species"]) &
                      !is.na(`All non-CAFOs`$`Animal Number`) & !is.na(`All non-CAFOs`$`Operations`) & 
                      !is.na(`All non-CAFOs`$`CAFO Species`),]
  onenonCAFO <- onenonCAFO[!is.na(onenonCAFO$`Census Species`),][1,] # removing NA rows that show up and duplicate rows
  `All non-CAFOs`[(`All non-CAFOs`$CTYID == onesummedCAFO[,"CTYID"]) & 
                    (`All non-CAFOs`$Bin == onesummedCAFO[,"Bin"]) &
                    (`All non-CAFOs`$`CAFO Species` == onesummedCAFO[,"CAFO Species"]) &
                    !is.na(`All non-CAFOs`$`Animal Number`) & !is.na(`All non-CAFOs`$`Operations`) & 
                    !is.na(`All non-CAFOs`$`CAFO Species`), "Corrected Animal Number"] <- ((onenonCAFO[,"Animal Number"]) - (onesummedCAFO[,"Total Animal Number"]))
  `All non-CAFOs`[(`All non-CAFOs`$CTYID == onesummedCAFO[,"CTYID"]) & 
                    (`All non-CAFOs`$Bin == onesummedCAFO[,"Bin"]) &
                    (`All non-CAFOs`$`CAFO Species` == onesummedCAFO[,"CAFO Species"]) &
                    !is.na(`All non-CAFOs`$`Animal Number`) & !is.na(`All non-CAFOs`$`Operations`) &
                    !is.na(`All non-CAFOs`$`CAFO Species`), "Corrected Operations"] <- ((onenonCAFO[,"Operations"]) - (onesummedCAFO[,"Number of CAFOs"]))

  }
  else if (is.na(`Summed CAFOs`[a,"Bin"])) {
    onesummedCAFO <- `Summed CAFOs`[a,]
    onenonCAFO <- `All non-CAFOs`[(`All non-CAFOs`$CTYID == onesummedCAFO[,"CTYID"]) & 
                                    (`All non-CAFOs`$`CAFO Species` == onesummedCAFO[,"CAFO Species"]) &
                                    !is.na(`All non-CAFOs`$`Animal Number`) & !is.na(`All non-CAFOs`$`Operations`) & 
                                    !is.na(`All non-CAFOs`$`CAFO Species`),]
    onenonCAFO <- onenonCAFO[!is.na(onenonCAFO$`Census Species`),][1,] # removing NA rows that show up and duplicate rows
    `All non-CAFOs`[(`All non-CAFOs`$CTYID == onesummedCAFO[,"CTYID"]) & 
                      (`All non-CAFOs`$`CAFO Species` == onesummedCAFO[,"CAFO Species"]) &
                      !is.na(`All non-CAFOs`$`Animal Number`) & !is.na(`All non-CAFOs`$`Operations`) & 
                      !is.na(`All non-CAFOs`$`CAFO Species`), "Corrected Animal Number"] <- ((onenonCAFO[,"Animal Number"]) - (onesummedCAFO[,"Total Animal Number"]))
    `All non-CAFOs`[(`All non-CAFOs`$CTYID == onesummedCAFO[,"CTYID"]) & 
                      (`All non-CAFOs`$`CAFO Species` == onesummedCAFO[,"CAFO Species"]) &
                      !is.na(`All non-CAFOs`$`Animal Number`) & !is.na(`All non-CAFOs`$`Operations`) &
                      !is.na(`All non-CAFOs`$`CAFO Species`), "Corrected Operations"] <- ((onenonCAFO[,"Operations"]) - (onesummedCAFO[,"Number of CAFOs"]))
    
  }
}
  # else if (!is.na(`Summed CAFOs`[a,"Bin"]) & `Summed CAFOs`[a,"CAFO Species"] == "CHICKEN-LAYER"){ # Need to subtract the number of farms from the individual bins AND
  #   # sum the total number from the CAFO dataframe to subtract from the total in the ag census. Only subtracting the number of farms here -
  #   # going to subtract animal totals in a different loop
  #   onesummedCAFO <- `Summed CAFOs`[a,]
  #   onenonCAFO <- `All non-CAFOs`[(`All non-CAFOs`$CTYID == onesummedCAFO[,"CTYID"]) & 
  #                                   (`All non-CAFOs`$`CAFO Species` == onesummedCAFO[,"CAFO Species"]) &
  #                                   (`All non-CAFOs`$Bin == onesummedCAFO[,"Bin"]) &
  #                                   !is.na(`All non-CAFOs`$`Operations`) & 
  #                                   !is.na(`All non-CAFOs`$`CAFO Species`),]
  #   onenonCAFO <- onenonCAFO[!is.na(onenonCAFO$`Census Species`),][1,] # removing NA rows that show up and duplicate rows
  #   `All non-CAFOs`[(`All non-CAFOs`$CTYID == onesummedCAFO[,"CTYID"]) & 
  #                     (`All non-CAFOs`$`CAFO Species` == onesummedCAFO[,"CAFO Species"]) &
  #                     (`All non-CAFOs`$Bin == onesummedCAFO[,"Bin"]) &
  #                     !is.na(`All non-CAFOs`$`Operations`) &
  #                     !is.na(`All non-CAFOs`$`CAFO Species`), "Corrected Operations"] <- ((onenonCAFO[,"Operations"]) - (onesummedCAFO[,"Number of CAFOs"]))
  #   
  # }


# # Subtracting out chicken-layer totals from CAFOs.
# # Also deals with conditions where there is an itembin from CAFOs that is not in the census - the census is 
# # assumed to report only the total from the census, excluding CAFO counts that do not have a corresponding bin
# 
# for (a in 1:nrow(`All non-CAFOs`[`All non-CAFOs`$ItemBin == "CHICKENS, LAYERSNOT SPECIFIED",])){
#   oneCLtotalrow <- `All non-CAFOs`[`All non-CAFOs`$ItemBin == "CHICKENS, LAYERSNOT SPECIFIED",][a,]
#   CLcountyID <- oneCLtotalrow[,"CTYID"]
#   CLspecies <- oneCLtotalrow[,"CAFO Species"]
#   BinList <- `All non-CAFOs`[`All non-CAFOs`$CTYID == CLcountyID & `All non-CAFOs`$`CAFO Species` == CLspecies, "Bin"]
#   BinList <- BinList[!is.na(BinList)]
#   BinList <- paste(BinList, collapse = "|")
#   CLfromCAFO <- sum(`Summed CAFOs`[`Summed CAFOs`$`CAFO Species` == CLspecies & `Summed CAFOs`$`CTYID` == CLcountyID & grepl(BinList, `Summed CAFOs`$Bin),"Total Animal Number"])
#   `All non-CAFOs`[`All non-CAFOs`$ItemBin == "CHICKENS, LAYERSNOT SPECIFIED",][a,"Corrected Animal Number"] <- (`All non-CAFOs`[`All non-CAFOs`$ItemBin == "CHICKENS, LAYERSNOT SPECIFIED",][a,"Animal Number"] - CLfromCAFO)
# }



# Getting rid of records that have negative corrected operations.
# These are assumed to be an undercount by the census and are taken care of
# by CAFOs

`All non-CAFOs` <- `All non-CAFOs`[!(`All non-CAFOs`$`Corrected Operations` <= 0),]
# For those species that are left with negative corrected animals because they don't have bins,
# converting negative corrected animals to 0 so that the remaining farms can be estimated by the
# basin-wide farm average
`All non-CAFOs`[`All non-CAFOs`$`Corrected Animal Number` < 0, "Corrected Animal Number"] <- 0

# Adding species that were not initially in the 'bin sizes' dataframe in, so that we can give them a "bin middle" 
# for estimating those records with no animal number but a non-zero number of farms

allitembinlist <- unique(`All non-CAFOs`$ItemBin)
allitembinlist <- allitembinlist[!(allitembinlist %in% `Bin Sizes`$ItemBin)]
allitembindf <- data.frame(matrix(0, ncol = ncol(`Bin Sizes`), nrow = length(allitembinlist)))
colnames(allitembindf) <- colnames(`Bin Sizes`)
allitembindf[,'ItemBin'] <- allitembinlist
`Bin Sizes` <- rbind(`Bin Sizes`, allitembindf)

# We do not think that using the given median of each bin size is the best method, as we think actual distributions are skewed to the right.
# For each species in the Bin Size dataframe, finding a new "bin middle" from our observations

for(a in 1:nrow(`Bin Sizes`[!(`Bin Sizes`$Animal == "CHICKENS, LAYERS") & !(`Bin Sizes`$ItemBin == "CATTLE, COWS, BEEF500 or more"),])){ # Ignoring "CHICKENS, LAYERS" - they have a unique situation (see below)
  # Also ignoring 500 or more beef cows because they have no records to pull a new estimate from
oneitembin<-`Bin Sizes`[!(`Bin Sizes`$Animal == "CHICKENS, LAYERS") & !(`Bin Sizes`$ItemBin == "CATTLE, COWS, BEEF500 or more"),][[a,"ItemBin"]]
actualbinmed<-median(`All non-CAFOs`[`All non-CAFOs`$ItemBin == oneitembin & `All non-CAFOs`$`Animal Number` != 0 & !is.na(`All non-CAFOs`$`Corrected Operations`) & `All non-CAFOs`$`Corrected Operations` != 0, "Corrected Animal Number"] / 
                       `All non-CAFOs`[`All non-CAFOs`$ItemBin == oneitembin & `All non-CAFOs`$`Animal Number` != 0 & !is.na(`All non-CAFOs`$`Corrected Operations`) & `All non-CAFOs`$`Corrected Operations` != 0, "Corrected Operations"])
`Bin Sizes`[`Bin Sizes`$ItemBin == oneitembin, "Bin Middle"] <- actualbinmed
}

# Removing individual bin middle values so that the averages may be joined back from the bin size table
`All non-CAFOs`$`Bin Middle` <- NULL


## For the largest bins only (*** or more), we are estimating a bin middle on a state-by-state basis. This bin middle
## will be the middle of the range between the minimum size of the bin and the smallest CAFO of that bin in each state.
## Bin middles are found in this way only for the largest bins because CAFO criteria differ by state, and we assume that
## all census records remaining after having CAFOs subtracted out are smaller than the smallest CAFO.
`All non-CAFOs` <- merge(`All non-CAFOs`, CountyIds[,c("CTYID","State")], by = "CTYID", all.x = TRUE)
allstatelist <- unique(`All non-CAFOs`$State)
largebinlist <- c("CATTLE, COWS, BEEF500 or more", "CATTLE, COWS, MILK500 or more", "HOGS1,000 or more", "SHEEP, INCL LAMBS1,000 or more", "CHICKENS100,000 or more")
largebinlist <- rep(largebinlist, times = length(allstatelist))
largebinlist <- largebinlist[order(largebinlist)]
allstatelist <- rep(allstatelist, times = length(unique(largebinlist)))
`Large Bin Sizes` <- data.frame("State" = allstatelist, "ItemBin" = largebinlist)
`Large Bin Sizes` <- merge(`Large Bin Sizes`, `Bin Sizes`[,c("ItemBin","Animal","Bin Min","Bin Middle","Bin Max","Bin Size")], by = "ItemBin", all.x = TRUE)

for (a in 1:nrow(`Large Bin Sizes`)){
  binanimal <- `Large Bin Sizes`[a,"Animal"]
  binsize <- `Large Bin Sizes`[a,"Bin Size"]
  binstate <- `Large Bin Sizes`[a,"State"]
  binmin <- `Large Bin Sizes`[a,"Bin Min"]
  onecaforange <- `All CAFOs`[`All CAFOs`$`Census Species` == binanimal & `All CAFOs`$Bin == binsize & `All CAFOs`$State == binstate, ]
  smallestcafonumber <- min(onecaforange$`Animal Number`)
  bindiff <- (smallestcafonumber - binmin)
  newbinmid <- (binmin + (bindiff/2))
  `Large Bin Sizes`[a, "Bin Middle"] <- newbinmid
}
`Large Bin Sizes`[`Large Bin Sizes`$`Bin Middle` == Inf | is.na(`Large Bin Sizes`$`Bin Middle`), "Bin Middle"] <- NA

# For those bins that couldn't find a new bin middle from the CAFO data (because there are no CAFOs to pull numbers from),
# estimating bin middles as the average of other values it was able to find
for (a in 1:length(unique(`Large Bin Sizes`$ItemBin))){
  oneitembin <- unique(`Large Bin Sizes`$ItemBin)[a]
  `Large Bin Sizes`[`Large Bin Sizes`$ItemBin == oneitembin & is.na(`Large Bin Sizes`$`Bin Middle`), "Bin Middle"] <- mean(`Large Bin Sizes`[`Large Bin Sizes`$ItemBin == oneitembin & !is.na(`Large Bin Sizes`$`Bin Middle`), "Bin Middle"])
}


# For those records with no given bin size, estimating bin size for non-CAFOs by finding the average 
# animal/farm and sorting into a bin size. Merging for all bins but the largest, then the largest bins.
# This is done in two merges because it was simpler to create a separate dataframe for the largest bins.

# Removing largest bins from regular "bin sizes" dataframe, since we don't want to join them twice
`Bin Sizes` <- `Bin Sizes`[!(`Bin Sizes`$ItemBin %in% largebinlist),]

`All non-CAFOs` <- merge(`All non-CAFOs`, `Bin Sizes`[,c("ItemBin", "Bin Middle")], by = "ItemBin", all.x = TRUE)
`All non-CAFOs` <- merge(`All non-CAFOs`, `Large Bin Sizes`[,c("ItemBin","State","Bin Middle")], by = c("ItemBin","State"), all.x = TRUE)
`All non-CAFOs`[is.na(`All non-CAFOs`$`Bin Middle.x`), "Bin Middle.x"] <- `All non-CAFOs`[is.na(`All non-CAFOs`$`Bin Middle.x`), "Bin Middle.y"]
`All non-CAFOs`$`Bin Middle.y` <- NULL
colnames(`All non-CAFOs`)[13] <- "Bin Middle"

# Sorting records with no given bins (that should have bins) into bins
for (a in 1:nrow(`All non-CAFOs`)){
  onebinrow <- `All non-CAFOs`[a,]
  onespecies <- onebinrow[,"Census Species"]
  onebinmiddle <- onebinrow[,"Bin Middle"]
  if (!is.na(onebinmiddle) & any(grepl(onespecies, unique(`Bin Sizes`$Animal))) & any(grepl(onespecies, unique(`All non-CAFOs`[is.na(`All non-CAFOs`$Bin),"Census Species"])))){
    onebinsize <- `Bin Sizes`[`Bin Sizes`$`Bin Min` < onebinmiddle & `Bin Sizes`$`Bin Max` > onebinmiddle & `Bin Sizes`$`Animal` == onespecies, "Bin Size"]
    `All non-CAFOs`[a, "Bin"] <- onebinsize
  }
}

# For records with no animal number but at least 1 registered, non-CAFO operation, using the bin middle/estimate to guess how many animals there are

`All non-CAFOs`[(`All non-CAFOs`$`Corrected Animal Number` == 0) & (`All non-CAFOs`$`Corrected Operations` != 0) & (`All non-CAFOs`$`Bin Middle` != 0) & !is.na(`All non-CAFOs`$`Bin Middle`) & !is.na(`All non-CAFOs`$`Corrected Operations`) & !is.na(`All non-CAFOs`$`Corrected Animal Number`), "Corrected Animal Number"] <- 
  `All non-CAFOs`[(`All non-CAFOs`$`Corrected Animal Number` == 0) & (`All non-CAFOs`$`Corrected Operations` != 0) & (`All non-CAFOs`$`Bin Middle` != 0) & !is.na(`All non-CAFOs`$`Bin Middle`) & !is.na(`All non-CAFOs`$`Corrected Operations`) & !is.na(`All non-CAFOs`$`Corrected Animal Number`), "Bin Middle"] * 
  `All non-CAFOs`[(`All non-CAFOs`$`Corrected Animal Number` == 0) & (`All non-CAFOs`$`Corrected Operations` != 0) & (`All non-CAFOs`$`Bin Middle` != 0) & !is.na(`All non-CAFOs`$`Bin Middle`) & !is.na(`All non-CAFOs`$`Corrected Operations`) & !is.na(`All non-CAFOs`$`Corrected Animal Number`), "Corrected Operations"]

# ## For "CHICKENS, LAYERS", the data was entered strangely - one record has a sum total of all other records with no bin size, while other records
# ## have a bin size and number of operations but no animal number (see OneNote document w/ screenshots). We could estimate the unknown animal numbers
# ## using the bin middle as we do for other records, but since we have the total, it makes more sense to distribute it proportionally across other 
# ## records. This is currently done by distributing the total from the smallest bins upward, using the bin middles as an estimate. If the remainder
# ## of chickens, after distributing them out to all but the largest bin, fits within the bin size, the remainder is used. If the remainder does not
# ## fit in the bin size, the other bins are redistributed according to the ratio between the observed total and the total estimated by using only
# ## the bin middle values.
# 
# for (a in 1:length(unique(`All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS",]$CTYID))){
#   oneCountyID <- unique(`All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS",]$CTYID)[a]
#   oneCLCounty <- `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS"),]
#   ObservedCount <- oneCLCounty[is.na(oneCLCounty$Operations), "Corrected Animal Number"]
#   oneCLCountyBinsOnly <- oneCLCounty[!is.na(oneCLCounty$Operations),]
#   oneCLCountyBinsOnlyNoMax <- oneCLCountyBinsOnly[!oneCLCountyBinsOnly$`Bin Middle` == max(oneCLCountyBinsOnly$`Bin Middle`),]
#   maxBinMiddle <- max(oneCLCountyBinsOnly$`Bin Middle`)
#   maxRemainder <- ObservedCount - sum(oneCLCountyBinsOnlyNoMax$`Corrected Operations`*oneCLCountyBinsOnlyNoMax$`Bin Middle`)
#   maxLowerThreshold <- oneCLCountyBinsOnly[oneCLCountyBinsOnly$`Bin Middle` == max(oneCLCountyBinsOnly$`Bin Middle`),"Bin Min"]*oneCLCountyBinsOnly[oneCLCountyBinsOnly$`Bin Middle` == max(oneCLCountyBinsOnly$`Bin Middle`),"Corrected Operations"]
#   if (maxRemainder > maxLowerThreshold){
#     `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & (`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Corrected Animal Number"] <- maxRemainder
#     `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Corrected Animal Number"] <- 
#       `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Corrected Operations"] * 
#       `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Bin Middle"]
#   } else {
#     DistRatio <- ObservedCount / (sum(oneCLCountyBinsOnly$`Corrected Operations` * oneCLCountyBinsOnly$`Bin Middle`))
#     `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Corrected Animal Number"] <- 
#       `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Corrected Operations"] * 
#       `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Bin Middle"] * DistRatio
#     `All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & (`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Corrected Animal Number"] <- ObservedCount -
#       sum(`All non-CAFOs`[(`All non-CAFOs`$CTYID == oneCountyID) & (`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS") & !(`All non-CAFOs`$`Bin Middle` == maxBinMiddle) & !is.na(`All non-CAFOs`$`Bin Middle`), "Corrected Animal Number"])
#       
#   }
# }
# 
# 
# # Some counties do not have a total "CHICKENS, LAYERS" number, but still have farms separated by bin size. For these, using the bin middle to estimate animal numbers
# # for each bin size.
# 
# countieswithnochickenlayers <- na.omit(unique(`All non-CAFOs`[`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & `All non-CAFOs`$`Animal Number` == 0,]$CTYID))
# `All non-CAFOs`[`All non-CAFOs`$CTYID %in% countieswithnochickenlayers & `All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & is.na(`All non-CAFOs`$`Animal Number`), "Corrected Animal Number"] <-
#   `All non-CAFOs`[`All non-CAFOs`$CTYID %in% countieswithnochickenlayers & `All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & is.na(`All non-CAFOs`$`Animal Number`), "Corrected Operations"] * 
#   `All non-CAFOs`[`All non-CAFOs`$CTYID %in% countieswithnochickenlayers & `All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & is.na(`All non-CAFOs`$`Animal Number`), "Bin Middle"]
# 
# # Removing records for "CHICKENS, LAYERS" with the county totals. We do not need these anymore; they were just distributed through the other records
# `All non-CAFOs` <- `All non-CAFOs`[!(`All non-CAFOs`$`Census Species` == "CHICKENS, LAYERS" & is.na(`All non-CAFOs`$Bin)),]

# Finding herd size, or the average animals/farm, for all records. Herd size is used to label records as confined or unconfined

`All non-CAFOs`[!is.na(`All non-CAFOs`$`Corrected Animal Number`) & !is.na(`All non-CAFOs`$`Corrected Operations`), "Herd Size"] <- (`All non-CAFOs`[!is.na(`All non-CAFOs`$`Corrected Animal Number`) & !is.na(`All non-CAFOs`$`Corrected Operations`), "Corrected Animal Number"] / 
  `All non-CAFOs`[!is.na(`All non-CAFOs`$`Corrected Animal Number`) & !is.na(`All non-CAFOs`$`Corrected Operations`), "Corrected Operations"])

### Using calculated herd size and the given limits on what is considered confined or unconfined for Ag Census data to give 
### each row in the non-CAFO dataframe a confinement status. Limits on confinement for each animal type are taken from
### Kellogg et al, 2000. Some farms, although they are not considered CAFOs, are estimated to fully confine their animals.

# Labelling species over the lower threshold of confinement as confined. This may be a place for future improvement -
# records could be labelled as "unconfined", "partially confined", or "confined" according to their place in
# the range of given limits on confinement.

`All non-CAFOs` <- merge(`All non-CAFOs`, `Limits on Confinement`[,c("Census Species","Lower Confinement Limit", "Recoverability Factor")], by = "Census Species", all.x = TRUE)

`All non-CAFOs`[(`All non-CAFOs`$`Herd Size` > `All non-CAFOs`$`Lower Confinement Limit`) & !is.na(`All non-CAFOs`$`Lower Confinement Limit`) & !is.na(`All non-CAFOs`$`Herd Size`), "Confined"] <- "YES"
`All non-CAFOs`[(`All non-CAFOs`$`Herd Size` < `All non-CAFOs`$`Lower Confinement Limit`) & !is.na(`All non-CAFOs`$`Lower Confinement Limit`) & !is.na(`All non-CAFOs`$`Herd Size`), "Confined"] <- "NO"
`All non-CAFOs`[is.na(`All non-CAFOs`$`Lower Confinement Limit`),"Confined"] <- "NO"

# Getting rid of records that have no data - those with 0 corrected operations (taken care of by CAFO data)
# or NA operations

`All non-CAFOs` <- `All non-CAFOs`[!is.na(`All non-CAFOs`$`Corrected Operations`),]
`All non-CAFOs` <- `All non-CAFOs`[!(`All non-CAFOs`$`Corrected Operations` == 0),]


### Calculating total nutrient loads per county

# Merging nutrient rates to CAFOs and calculating the load of each farm
`All CAFOs` <- merge(`All CAFOs`, `Nutrient Rate Averages`, by = "Census Species", all.x = TRUE)
`All CAFOs` <- merge(`All CAFOs`, `Limits on Confinement`[,c("Census Species","Recoverability Factor")], by = "Census Species", all.x = TRUE)

`All CAFOs`[is.na(`All CAFOs`$`Recoverability Factor`),"Recoverability Factor"] <- 1

# Labelling these as rates "per farm" so that joining them with the GME farms is easier down the line
`All CAFOs`$`kgN_farm_y` <- `All CAFOs`$`Animal Number` * `All CAFOs`$`AvgOfN (as excreted)` * `All CAFOs`$`Recoverability Factor` * 365
`All CAFOs`$`kgP_farm_y` <- `All CAFOs`$`Animal Number` * `All CAFOs`$`AvgOfP (as excreted)` * `All CAFOs`$`Recoverability Factor` * 365
`All CAFOs`$`kgN/y/urec` <- `All CAFOs`$`Animal Number` * `All CAFOs`$`AvgOfN (as excreted)` * (1-`All CAFOs`$`Recoverability Factor`) * 365
`All CAFOs`$`kgP/y/urec` <- `All CAFOs`$`Animal Number` * `All CAFOs`$`AvgOfP (as excreted)` * (1-`All CAFOs`$`Recoverability Factor`) * 365

# Removing duplicates that show up for some reason?
`All CAFOs` <- `All CAFOs`[!duplicated(`All CAFOs`),]

# Renaming some species to match those that we have nutrient rate averages for. Somewhere along the line,
# species names became inconsistent, causing the nutrient rate merge to work incorrectly for those species.
`All non-CAFOs`[,"Census Species"]<-gsub("EQUINE, MULES & BURROS & DONKEYS", "EQUINE, HORSES & PONIES", `All non-CAFOs`[,"Census Species"])


# Matching up nutrient rates to species for Census data
`All non-CAFOs` <- merge(`All non-CAFOs`, `Nutrient Rate Averages`, by = "Census Species", all.x = TRUE)


## Creating new dataframe for all confined animals. This is confined animals from the census ONLY -- CAFOs are in their own dataframe

#There is 1 county that does not have any confined animals - 36041. It is removed in this merge, and does not have a record in the final 
#table of "total confined county loads".
`All Confined Animals` <- `All non-CAFOs`[`All non-CAFOs`$Confined == "YES",]
# Removing NA rows that, once again, show up for an undetermined reason
`All Confined Animals` <- `All Confined Animals`[!is.na(`All Confined Animals`$CTYID),]

# Calculating total N and P per year for each bin totally, and finding rate/farm
`All Confined Animals`$`kgP/y/rec` <- `All Confined Animals`$`Corrected Animal Number` * `All Confined Animals`$`AvgOfP (as excreted)` * `All Confined Animals`$`Recoverability Factor` * 365
`All Confined Animals`$`kgN/y/rec` <- `All Confined Animals`$`Corrected Animal Number` * `All Confined Animals`$`AvgOfN (as excreted)` * `All Confined Animals`$`Recoverability Factor` * 365

# We want the unrecovered loads to be per farm - not per county total!
`All Confined Animals`$`kgP/y/urec` <- (`All Confined Animals`$`Corrected Animal Number` * `All Confined Animals`$`AvgOfP (as excreted)` * (1-`All Confined Animals`$`Recoverability Factor`) * 365)/`All Confined Animals`$`Corrected Operations`
`All Confined Animals`$`kgN/y/urec` <- (`All Confined Animals`$`Corrected Animal Number` * `All Confined Animals`$`AvgOfN (as excreted)` * (1-`All Confined Animals`$`Recoverability Factor`) * 365)/`All Confined Animals`$`Corrected Operations`

`All Confined Animals`$`kgP_farm_y` <- `All Confined Animals`$`Herd Size` * `All Confined Animals`$`AvgOfP (as excreted)` * `All Confined Animals`$`Recoverability Factor` * 365
`All Confined Animals`$`kgN_farm_y` <- `All Confined Animals`$`Herd Size` * `All Confined Animals`$`AvgOfN (as excreted)` * `All Confined Animals`$`Recoverability Factor` * 365


## Using empirically derived N:P ratios to account for denitrification during the period between when manure is excreted
## and when it is spread. This only affects nitrogen content of spread manure - not excreted manure.
`Manure NP Ratios`$NLossRatio <- NA
for (a in 1:nrow(`Manure NP Ratios`)){
  if(!is.na(`Manure NP Ratios`[a,"N:P Ratio"])){
    oneNPspecies <- `Manure NP Ratios`[[a,"Census Species"]]
    oneNPspeciesN <- sum(`All Confined Animals`[`All Confined Animals`$`Census Species` == oneNPspecies,"kgN/y/rec"], `All CAFOs`[`All CAFOs`$`Census Species` == oneNPspecies, "kgN/y/rec"])
    oneNPspeciesP <- sum(`All Confined Animals`[`All Confined Animals`$`Census Species` == oneNPspecies,"kgP/y/rec"], `All CAFOs`[`All CAFOs`$`Census Species` == oneNPspecies, "kgP/y/rec"])
    oneNPspeciesNLoss <- oneNPspeciesN - (`Manure NP Ratios`[a,"N:P Ratio"] * oneNPspeciesP)
    NLossRatio <- (oneNPspeciesN - oneNPspeciesNLoss) / oneNPspeciesN
    `Manure NP Ratios`[a,"NLossRatio"] <- NLossRatio
  }
}


# Merging NLoss ratios with confined animals and recalculating recovered nitrogen loads
`All Confined Animals` <- merge(`All Confined Animals`, `Manure NP Ratios`[,c("Census Species","NLossRatio")], by = 'Census Species', all.x = TRUE)
# Recalculating recovered N loads. This operation is done on the same column as the initial N loads - be careful
# about not doing this operations twice if running this script in discrete sections!
`All Confined Animals`[!is.na(`All Confined Animals`$NLossRatio),"kgN/y/rec"] <- `All Confined Animals`[!is.na(`All Confined Animals`$NLossRatio),"kgN/y/rec"] * `All Confined Animals`[!is.na(`All Confined Animals`$NLossRatio),"NLossRatio"]

# Now doing the NLoss merge for CAFOs and recalculating N loads
`All CAFOs` <- merge(`All CAFOs`, `Manure NP Ratios`[,c("Census Species","NLossRatio")], by = 'Census Species', all.x = TRUE)
`All CAFOs`[!is.na(`All CAFOs`$NLossRatio),"kgN/y/rec"] <- `All CAFOs`[!is.na(`All CAFOs`$NLossRatio),"kgN/y/rec"] * `All CAFOs`[!is.na(`All CAFOs`$NLossRatio),"NLossRatio"]


## Creating dataframe for all pasture (non-confined) animals
`All Pasture Animals` <- `All non-CAFOs`[`All non-CAFOs`$Confined == "NO",]


# Removing NA rows
`All Pasture Animals` <- `All Pasture Animals`[!is.na(`All Pasture Animals`$CTYID),]

# Merging renamed species with new nutrient rates
# Easier at this point to drop the nutrient rate/animal column and remerge it
`All Pasture Animals`$`AvgOfP (as excreted)` <- NULL
`All Pasture Animals`$`AvgOfN (as excreted)` <- NULL
`All Pasture Animals` <- merge(`All Pasture Animals`, `Nutrient Rate Averages`, by = "Census Species")


# Calculating N and P excretion rates per farm for each county and bin - same thing as above, except pasture animals do not have a recoverability factor. 
# All manure from pasture animals is considered to be "unrecovered".

`All Pasture Animals`$`kgP_farm_y` <- `All Pasture Animals`$`Herd Size` * `All Pasture Animals`$`AvgOfP (as excreted)` * 365
`All Pasture Animals`$`kgN_farm_y` <- `All Pasture Animals`$`Herd Size` * `All Pasture Animals`$`AvgOfN (as excreted)` * 365

## Finding total confined county loads
`Total Confined County Loads` <- as.data.frame(unique(`All Confined Animals`$CTYID))
colnames(`Total Confined County Loads`) <- "CTYID"

for (a in 1:nrow(`Total Confined County Loads`)){
  countyID<-`Total Confined County Loads`[a,"CTYID"]
  `Total Confined County Loads`[a,"kgP/year"] <- sum(`All Confined Animals`[`All Confined Animals`$CTYID == countyID,"kgP/y/rec"], `All CAFOs`[`All CAFOs`$CTYID == countyID,"kgP_farm_y"])
  `Total Confined County Loads`[a,"kgN/year"] <- sum(`All Confined Animals`[`All Confined Animals`$CTYID == countyID,"kgN/y/rec"], `All CAFOs`[`All CAFOs`$CTYID == countyID,"kgN_farm_y"])
}


# Creating dataframes for GME labels - one unique label for every confined itembin, and one for every pasture itembin
`GME Bin IDs Confined` <- as.data.frame(unique(`All Confined Animals`$ItemBin))
`GME Bin IDs Confined`$GMEID <- 1:nrow(`GME Bin IDs Confined`)
colnames(`GME Bin IDs Confined`) <- c("ItemBin", "GMEID")

`GME Bin IDs Pasture` <- as.data.frame(unique(`All Pasture Animals`$ItemBin))
`GME Bin IDs Pasture`$GMEID <- 1:nrow(`GME Bin IDs Pasture`)
colnames(`GME Bin IDs Pasture`) <- c("ItemBin", "GMEID")

# Merging GME ID dataframes with final dataframes to label every record with a GME bin ID number
`All Confined Animals` <- merge(`All Confined Animals`, `GME Bin IDs Confined`, by = "ItemBin")
`All Pasture Animals` <- merge(`All Pasture Animals`, `GME Bin IDs Pasture`, by = "ItemBin")

# Concatenating GMEID and CTYID fields in pasture and confined dataframes to create one field that 
# uniquely labels every row
`All Confined Animals`$`BIN_CTY` <- paste(`All Confined Animals`$GMEID, `All Confined Animals`$CTYID, sep = "")
`All Pasture Animals`$`BIN_CTY` <- paste(`All Pasture Animals`$GMEID, `All Pasture Animals`$CTYID, sep = "")





# Finding the number of confined farms per county and comparing to the number of farms reported by the census
# as spreading manure. We have assumed that all confined farms are spreading their manure -
# I don't think that's a good assumption, as the number of spreading farms reported by the census
# is much lower than the number of "confined" farms from the other census sources

for (a in 1:nrow(`Manure Acres and Operations`)){
  onecounty <- `Manure Acres and Operations`[a,"CTYID"]
  OneConfCounty <- `All Confined Animals`[`All Confined Animals`$CTYID == onecounty,]
  ConfOps <- sum(OneConfCounty$Operations)
  `Manure Acres and Operations`[a,"Confined Operations"] <- ConfOps
}
`Manure Acres and Operations`$`farm_m2_ratio` <- `Manure Acres and Operations`$Operations / `Manure Acres and Operations`$Man_m2
`Manure Acres and Operations`$`Conf_reported_ratio` <- `Manure Acres and Operations`$`Confined Operations` / `Manure Acres and Operations`$Operations



############### Exporting final dataframes as .xlsx files for use in the Python script
write.xlsx(`All Confined Animals`, file = "Confined_Animals.xlsx")
write.xlsx(`All Pasture Animals`, file = "Pasture_Animals.xlsx")
write.xlsx(`Total Confined County Loads`, file = "Total_Confined_County_Loads.xlsx")
write.xlsx(`Manure Acres and Operations`, file = "Manure_Acres_and_Operations.xlsx")
write.xlsx(`All CAFOs`, file = "All_CAFOs_Final.xlsx")




# I have manually changed the column names of these exported CSV files to be less than 10 characters
# because of Arc's field names - keep this in mind when exporting these again!

############# Plotting some stuff

mandensity <- ggplot(`Manure Acres and Operations`, aes(farm_m2_ratio)) + geom_density()
mandensity

conf_histo <- ggplot(`Manure Acres and Operations`, aes(Conf_reported_ratio)) + geom_density()
conf_histo

Conf_and_ratio <- ggplot(`Manure Acres and Operations`, aes(x = Conf_reported_ratio, y = farm_m2_ratio)) + geom_point()
Conf_and_ratio

ratiorelations <- lm(Conf_reported_ratio ~ farm_m2_ratio, data = `Manure Acres and Operations`)
