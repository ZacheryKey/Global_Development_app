
##### ------------------------ LIBRARIES FOR APP ------------------------------#####

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org"); library(shiny)

##### LIBRARIES FOR SERVER: SCATTERPLOT ####

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org"); library(dplyr)
if(!require(ggvis)) install.packages("ggvis", repos = "http://cran.us.r-project.org"); library(ggvis)

##### LIBRARIES FOR SERVER: WORLDMAP ####

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org"); library(tidyverse)
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org"); library(rvest)
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org"); library(magrittr)
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org"); library(ggmap)
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org"); library(stringr)
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org"); library(countrycode)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org"); library(ggplot2)
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org"); library(RColorBrewer) # interactive labels for plot
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org"); library(plotly) # interactive labels for plot
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org"); library(maps)
if(!require(sp)) install.packages("sp", repos = "http://cran.us.r-project.org"); library(sp)
if(!require(rworldmap)) install.packages("rworldmap", repos = "http://cran.us.r-project.org"); library(rworldmap)


##### LIBRARIES FOR DATA PROCESSING  #####

if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org"); library(tidyr)
if(!require(WDI)) install.packages("WDI", repos = "http://cran.us.r-project.org"); library(WDI)
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org"); library(dplyr)
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org"); library(rvest)
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org"); library(reshape2)


##### ----------------------- GENERAL DATA PROCESSING ------------------------- #####

# World Bank data from API Query with WDI Package: GDP DATA
gdp <- WDI(country = "all", indicator = "NY.GDP.PCAP.KD", start = 1980, end = 2018)
write.csv(gdp, 'gdp.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: MALE LITERACY DATA
literacyMale <- WDI(country = "all", indicator = "SE.ADT.LITR.MA.ZS", start = 1980, end = 2018)
write.csv(literacyMale, 'literacyMale.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: FEMALE LITERACY DATA
literacyFemale <- WDI(country = "all", indicator = "SE.ADT.LITR.FE.ZS", start = 1980, end = 2018)
write.csv(literacyFemale, 'literacyFemale.csv', row.names = FALSE) # save raw data to csv

# Pull WHO data from API Query: MORTALITY UNDER AGE 5 DATA
mortalityUnder5 <- read.csv(url("https://apps.who.int/gho/athena/api/GHO/MDG_0000000007?format=csv"))
write.csv(mortalityUnder5, 'mortalityUnder5.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: FEMALE LIFE EXPECTANCY DATA 
expectancyFemale <- WDI(country = "all", indicator = "SP.DYN.LE00.FE.IN", start = 1980, end = 2018)
write.csv(expectancyFemale, 'expectancyFemale.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: MALE LIFE EXPECTANCY DATA 
expectancyMale <- WDI(country = "all", indicator = "SP.DYN.LE00.MA.IN", start = 1980, end = 2018)
write.csv(expectancyMale, 'expectancyMale.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: TOTAL LIFE EXPECTANCY DATA 
expectancyTotal <- WDI(country = "all", indicator = "SP.DYN.LE00.IN", start = 1980, end = 2018)
write.csv(expectancyTotal, 'expectancyTotal.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: SERVICES AS % OF GDP DATA 
serviceGDP <- WDI(country = "all", indicator = "NV.SRV.TOTL.ZS", start = 1980, end = 2018)
write.csv(serviceGDP, 'serviceGDP.csv', row.names = FALSE) # save raw data to csv

# World Bank data from API Query with WDI Package: TRADE AS % OF GDP DATA 
tradeGDP <- WDI(country = "all", indicator = "NE.TRD.GNFS.ZS", start = 1980, end = 2018)
write.csv(tradeGDP, 'tradeGDP.csv', row.names = FALSE) # save raw data to csv



#####---------------------------- CLEANING OF WORLD BANK DATA -------------------------#####

# Change Column Names
names(gdp)[3] <- "RealGDP"
names(literacyFemale)[3] <- "LiteracyFemale"
names(literacyMale)[3] <- "LiteracyMale"
names(mortalityUnder5)[5] <- "iso3"
names(mortalityUnder5)[8] <- "mortalityValue"
names(mortalityUnder5)[3] <- "year"
names(expectancyFemale)[3] <- "ExpectancyFemale"
names(expectancyMale)[3] <- "ExpectancyMale"
names(expectancyTotal)[3] <- "ExpectancyTotal"
names(serviceGDP)[3] <- "ServiceGDP"
names(tradeGDP)[3] <- "TradeGDP"

## remove rows with NA values
# set na values to -1
# remove records that have -1
gdpClean <- gdp %>% mutate(RealGDP = replace(RealGDP, is.na(RealGDP), -1)) %>% filter(RealGDP != -1)
literacyFemaleClean <- literacyFemale %>% mutate(LiteracyFemale = replace(LiteracyFemale, is.na(LiteracyFemale), -1))%>% filter(LiteracyFemale != -1)
literacyMaleClean <- literacyMale %>% mutate(LiteracyMale = replace(LiteracyMale, is.na(LiteracyMale), -1)) %>% filter(LiteracyMale != -1)
mortalityUnder5Clean <- mortalityUnder5 %>% mutate(mortalityValue = replace(mortalityValue, is.na(mortalityValue), -1)) %>% filter(mortalityValue != -1)
expectancyFemaleClean <- expectancyFemale %>% mutate(ExpectancyFemale = replace(ExpectancyFemale, is.na(ExpectancyFemale), -1)) %>% filter(ExpectancyFemale != -1)
expectancyMaleClean <- expectancyMale %>% mutate(ExpectancyMale = replace(ExpectancyMale, is.na(ExpectancyMale), -1)) %>% filter(ExpectancyMale != -1)
expectancyTotalClean <- expectancyTotal %>% mutate(ExpectancyTotal = replace(ExpectancyTotal, is.na(ExpectancyTotal), -1)) %>% filter(ExpectancyTotal != -1)
serviceGDPClean <- serviceGDP %>% mutate(ServiceGDP = replace(ServiceGDP, is.na(ServiceGDP), -1)) %>% filter(ServiceGDP != -1)
tradeGDPClean <- tradeGDP %>% mutate(TradeGDP = replace(TradeGDP, is.na(TradeGDP), -1)) %>% filter(TradeGDP != -1)

## drop columns
mortalityColDel <- c("GHO", "PUBLISHSTATE", "REGION", "Display.Value", "Low", "High", "Comments")
mortalityUnder5Clean <- select(mortalityUnder5Clean, -mortalityColDel)

# create list of country codes for table joining

url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")

# Filter Tables to Countries Only
# Inner Join with List of Country Codes (if a country and in dataset, keep record)

gdpJoin <- inner_join(iso_codes, gdpClean, by = c("ISO2" = "iso2c"))
literacyFemaleJoin <- inner_join(iso_codes, literacyFemaleClean, by = c("ISO2" = "iso2c"))
literacyMaleJoin <- inner_join(iso_codes, literacyMaleClean, by = c("ISO2" = "iso2c"))
mortalityUnder5Join <- inner_join(iso_codes, mortalityUnder5Clean, by = c("ISO3" = "iso3"))
expectancyFemaleJoin <- inner_join(iso_codes, expectancyFemaleClean, by = c("ISO2" = "iso2c"))
expectancyMaleJoin <- inner_join(iso_codes, expectancyMaleClean, by = c("ISO2" = "iso2c"))
expectancyTotalJoin <- inner_join(iso_codes, expectancyTotalClean, by = c("ISO2" = "iso2c"))
tradeGDPJoin <- inner_join(iso_codes, tradeGDPClean, by = c("ISO2" = "iso2c"))
serviceGDPJoin <- inner_join(iso_codes, serviceGDPClean, by = c("ISO2" = "iso2c"))

## Mortality Data by Gender

mortalityFemaleJoin <- filter(mortalityUnder5Join, SEX == "FMLE") # female
mortalityMaleJoin<- filter(mortalityUnder5Join, SEX == "MLE") # male
mortalityBTSXJoin <- filter(mortalityUnder5Join, SEX == "BTSX") # both sexes

## Get a cleaned up table that has the years as attributes

literacyMaleFinal <- spread(literacyMaleJoin, year, LiteracyMale)
literacyFemaleFinal <- spread(literacyFemaleJoin, year, LiteracyFemale)
gdpFinal <- spread(gdpJoin, year , RealGDP)
mortalityFemaleFinal <- spread(mortalityFemaleJoin, year, mortalityValue)
mortalityMaleFinal <- spread(mortalityMaleJoin, year, mortalityValue)
mortalityBTSXFinal <- spread(mortalityBTSXJoin, year, mortalityValue)
expectancyFemaleFinal <- spread(expectancyFemaleJoin, year, ExpectancyFemale)
expectancyMaleFinal <- spread(expectancyMaleJoin, year, ExpectancyMale)
expectancyTotalFinal <- spread(expectancyTotalJoin, year, ExpectancyTotal)
tradeGDPFinal <- spread(tradeGDPJoin, year, TradeGDP)
serviceGDPFinal <- spread(serviceGDPJoin, year, ServiceGDP)

# Add a continents column to all of the final dataframes. This will speed up the scatterplot color coding in the future
literacyMaleFinal$continents <- countrycode(sourcevar = literacyMaleFinal[,which(names(literacyMaleFinal)=="Country")], origin = "country.name",destination = "continent")
gdpFinal$continents <- countrycode(sourcevar = gdpFinal[,which(names(gdpFinal)=="Country")], origin = "country.name",destination = "continent")
literacyFemaleFinal$continents <- countrycode(sourcevar = literacyFemaleFinal[,which(names(literacyFemaleFinal)=="Country")], origin = "country.name",destination = "continent")
mortalityFemaleFinal$continents <- countrycode(sourcevar = mortalityFemaleFinal[,which(names(mortalityFemaleFinal)=="Country")], origin = "country.name",destination = "continent")
mortalityMaleFinal$continents <- countrycode(sourcevar = mortalityMaleFinal[,which(names(mortalityMaleFinal)=="Country")], origin = "country.name",destination = "continent")
mortalityBTSXFinal$continents <- countrycode(sourcevar = mortalityBTSXFinal[,which(names(mortalityBTSXFinal)=="Country")], origin = "country.name",destination = "continent")
expectancyFemaleFinal$continents <- countrycode(sourcevar = expectancyFemaleFinal[,which(names(expectancyFemaleFinal)=="Country")], origin = "country.name",destination = "continent")
expectancyMaleFinal$continents <- countrycode(sourcevar = expectancyMaleFinal[,which(names(expectancyMaleFinal)=="Country")], origin = "country.name",destination = "continent")
expectancyTotalFinal$continents <- countrycode(sourcevar = expectancyTotalFinal[,which(names(expectancyTotalFinal)=="Country")], origin = "country.name",destination = "continent")
tradeGDPFinal$continents <- countrycode(sourcevar = tradeGDPFinal[,which(names(tradeGDPFinal)=="Country")], origin = "country.name",destination = "continent")
serviceGDPFinal$continents <- countrycode(sourcevar = serviceGDPFinal[,which(names(serviceGDPFinal)=="Country")], origin = "country.name",destination = "continent")


##### Processing for World Map Creation #####

# Append ISO3 codes to the map.word data to be able to merge long/lat polygon data with the other data frames 
map.world<-map_data("world")
map.world$ISO = 0
map.world$ISO<-countrycode(map.world$region,"country.name","iso3c")
head(map.world)

# Create dataframes from left joining GDP,Literacy and Mortality to map.world
map.world_joined<-left_join(map.world,gdpFinal,by=c('ISO'='ISO3'))                   # GDP 
map.world_joined2<-left_join(map.world,literacyMaleFinal,by=c('ISO'='ISO3'))         # Male Literacy 
map.world_joined3<-left_join(map.world,literacyFemaleFinal,by=c('ISO'='ISO3'))       # Female Literacy 
map.world_joined4<-left_join(map.world,mortalityMaleFinal,by=c('ISO'='ISO3'))        # Male Mortality
map.world_joined5<-left_join(map.world,mortalityFemaleFinal,by=c('ISO'='ISO3'))      # Female Mortality
map.world_joined6<-left_join(map.world,mortalityBTSXFinal,by=c('ISO'='ISO3'))        # Both Sex Mortality
map.world_joined7<-left_join(map.world,expectancyFemaleFinal,by=c('ISO'='ISO3'))     # Female Life Expectancy
map.world_joined8<-left_join(map.world,expectancyMaleFinal,by=c('ISO'='ISO3'))       # Male Life Expectancy
map.world_joined9<-left_join(map.world,expectancyTotalFinal,by=c('ISO'='ISO3'))      # Total Life Expectancy
map.world_joined11<-left_join(map.world,tradeGDPFinal,by=c('ISO'='ISO3'))            # Trade GDP
map.world_joined12<-left_join(map.world,serviceGDPFinal,by=c('ISO'='ISO3'))          # Service GDP


#### Processing for plots Under the world map ####

# GDP dataframe for line plot
gdpData <- rbind(round(gdpFinal[which(sapply(gdpFinal, class)=="numeric")],0) , apply(na.omit(round(gdpFinal[which(sapply(gdpFinal, class)=="numeric")],0)),2,mean))
val_col = gdpFinal$Country
val_col[nrow(gdpFinal)+1] = "world_avg"
gdpData$country_name = val_col
iso_3_col = gdpFinal$ISO3
iso_3_col[nrow(gdpFinal)+1] = "world_avg"
gdpData$ISO3 = iso_3_col
gdpData <- gdpData %>% melt(id.vars = c("country_name","ISO3"))
colnames(gdpData) = c("country","ISO3","year","value")

# Male mortality data for line plot
mortMaleData <- rbind(round(mortalityMaleFinal[which(sapply(mortalityMaleFinal, class)=="numeric")],0) , apply(na.omit(round(mortalityMaleFinal[which(sapply(mortalityMaleFinal, class)=="numeric")],0)),2,mean))
mortMaleData <- mortMaleData[,which(as.integer(names(mortMaleData)) > 1979)]
val_col = mortalityMaleFinal$Country
val_col[nrow(mortalityMaleFinal)+1] = "world_avg"
mortMaleData$country_name = val_col
iso_3_col = mortalityMaleFinal$ISO3
iso_3_col[nrow(mortalityMaleFinal)+1] = "world_avg"
mortMaleData$ISO3 = iso_3_col
mortMaleData <- mortMaleData %>% melt(id.vars = c("country_name","ISO3"))
colnames(mortMaleData) = c("country","ISO3","year","value")

# Female mortality dataframe 
mortFemData <- rbind(round(mortalityFemaleFinal[which(sapply(mortalityFemaleFinal, class)=="numeric")],0) , apply(na.omit(round(mortalityFemaleFinal[which(sapply(mortalityFemaleFinal, class)=="numeric")],0)),2,mean))
mortFemData <- mortFemData[,which(as.integer(names(mortFemData)) > 1979)]
val_col = mortalityFemaleFinal$Country
val_col[nrow(mortalityFemaleFinal)+1] = "world_avg"
mortFemData$country_name = val_col
iso_3_col = mortalityFemaleFinal$ISO3
iso_3_col[nrow(mortalityFemaleFinal)+1] = "world_avg"
mortFemData$ISO3 = iso_3_col
mortFemData <- mortFemData %>% melt(id.vars = c("country_name","ISO3"))
colnames(mortFemData) = c("country","ISO3","year","value")

# Both sex mortality 
mortAllData <- rbind(round(mortalityBTSXFinal[which(sapply(mortalityBTSXFinal, class)=="numeric")],0) , apply(na.omit(round(mortalityBTSXFinal[which(sapply(mortalityBTSXFinal, class)=="numeric")],0)),2,mean))
mortAllData <- mortAllData[,which(as.integer(names(mortAllData)) > 1979)]
val_col = mortalityBTSXFinal$Country
val_col[nrow(mortalityBTSXFinal)+1] = "world_avg"
mortAllData$country_name = val_col
iso_3_col = mortalityBTSXFinal$ISO3
iso_3_col[nrow(mortalityBTSXFinal)+1] = "world_avg"
mortAllData$ISO3 = iso_3_col
mortAllData <- mortAllData %>% melt(id.vars = c("country_name","ISO3"))
colnames(mortAllData) = c("country","ISO3","year","value")

# Male literacy rate
litMaleData <- rbind(round(literacyMaleFinal[which(sapply(literacyMaleFinal, class)=="numeric")],0) , apply(na.omit(round(literacyMaleFinal[which(sapply(literacyMaleFinal, class)=="numeric")],0)),2,mean))
litMaleData <- litMaleData[,which(as.integer(names(litMaleData)) > 1979)]
val_col = literacyMaleFinal$Country
val_col[nrow(literacyMaleFinal)+1] = "world_avg"
litMaleData$country_name = val_col
iso_3_col = literacyMaleFinal$ISO3
iso_3_col[nrow(literacyMaleFinal)+1] = "world_avg"
litMaleData$ISO3 = iso_3_col
litMaleData <- litMaleData %>% melt(id.vars = c("country_name","ISO3"))
colnames(litMaleData) = c("country","ISO3","year","value")

# Female literacy data
litFemData <- rbind(round(literacyFemaleFinal[which(sapply(literacyFemaleFinal, class)=="numeric")],0) , apply(na.omit(round(literacyFemaleFinal[which(sapply(literacyFemaleFinal, class)=="numeric")],0)),2,mean))
litFemData <- litFemData[,which(as.integer(names(litFemData)) > 1979)]
val_col = literacyFemaleFinal$Country
val_col[nrow(literacyFemaleFinal)+1] = "world_avg"
litFemData$country_name = val_col
iso_3_col = literacyFemaleFinal$ISO3
iso_3_col[nrow(literacyFemaleFinal)+1] = "world_avg"
litFemData$ISO3 = iso_3_col
litFemData <- litFemData %>% melt(id.vars = c("country_name","ISO3"))
colnames(litFemData) = c("country","ISO3","year","value")

# Male life expectancy data 
expctMaleData <- rbind(round(expectancyMaleFinal[which(sapply(expectancyMaleFinal, class)=="numeric")],0) , apply(na.omit(round(expectancyMaleFinal[which(sapply(expectancyMaleFinal, class)=="numeric")],0)),2,mean))
expctMaleData <- expctMaleData[,which(as.integer(names(expctMaleData)) > 1979)]
val_col = expectancyMaleFinal$Country
val_col[nrow(expectancyMaleFinal)+1] = "world_avg"
expctMaleData$country_name = val_col
iso_3_col = expectancyMaleFinal$ISO3
iso_3_col[nrow(expectancyMaleFinal)+1] = "world_avg"
expctMaleData$ISO3 = iso_3_col
expctMaleData <- expctMaleData %>% melt(id.vars = c("country_name","ISO3"))
colnames(expctMaleData) = c("country","ISO3","year","value")

#Female life expectancy Data
expctFemData <- rbind(round(expectancyFemaleFinal[which(sapply(expectancyFemaleFinal, class)=="numeric")],0) , apply(na.omit(round(expectancyFemaleFinal[which(sapply(expectancyFemaleFinal, class)=="numeric")],0)),2,mean))
expctFemData <- expctFemData[,which(as.integer(names(expctFemData)) > 1979)]
val_col = expectancyFemaleFinal$Country
val_col[nrow(expectancyFemaleFinal)+1] = "world_avg"
expctFemData$country_name = val_col
iso_3_col = expectancyFemaleFinal$ISO3
iso_3_col[nrow(expectancyFemaleFinal)+1] = "world_avg"
expctFemData$ISO3 = iso_3_col
expctFemData <- expctFemData %>% melt(id.vars = c("country_name","ISO3"))
colnames(expctFemData) = c("country","ISO3","year","value")

# Total life expectancy 
expctAllData <- rbind(round(expectancyTotalFinal[which(sapply(expectancyTotalFinal, class)=="numeric")],0) , apply(na.omit(round(expectancyTotalFinal[which(sapply(expectancyTotalFinal, class)=="numeric")],0)),2,mean))
expctAllData <- expctAllData[,which(as.integer(names(expctAllData)) > 1979)]
val_col = expectancyTotalFinal$Country
val_col[nrow(expectancyTotalFinal)+1] = "world_avg"
expctAllData$country_name = val_col
iso_3_col = expectancyTotalFinal$ISO3
iso_3_col[nrow(expectancyTotalFinal)+1] = "world_avg"
expctAllData$ISO3 = iso_3_col
expctAllData <- expctAllData %>% melt(id.vars = c("country_name","ISO3"))
colnames(expctAllData) = c("country","ISO3","year","value")

# Trade GDP info data
tradeGDPdata <- rbind(round(tradeGDPFinal[which(sapply(tradeGDPFinal, class)=="numeric")],0) , apply(na.omit(round(tradeGDPFinal[which(sapply(tradeGDPFinal, class)=="numeric")],0)),2,mean))
tradeGDPdata <- tradeGDPdata[,which(as.integer(names(tradeGDPdata)) > 1979)]
val_col = tradeGDPFinal$Country
val_col[nrow(tradeGDPFinal)+1] = "world_avg"
tradeGDPdata$country_name = val_col
iso_3_col = tradeGDPFinal$ISO3
iso_3_col[nrow(tradeGDPFinal)+1] = "world_avg"
tradeGDPdata$ISO3 = iso_3_col
tradeGDPdata <- tradeGDPdata %>% melt(id.vars = c("country_name","ISO3"))
colnames(tradeGDPdata) = c("country","ISO3","year","value")

# Service GDP info data
serviceGDPdata <- rbind(round(serviceGDPFinal[which(sapply(serviceGDPFinal, class)=="numeric")],0) , apply(na.omit(round(serviceGDPFinal[which(sapply(serviceGDPFinal, class)=="numeric")],0)),2,mean))
serviceGDPdata <- serviceGDPdata[,which(as.integer(names(serviceGDPdata)) > 1979)]
val_col = serviceGDPFinal$Country
val_col[nrow(serviceGDPFinal)+1] = "world_avg"
serviceGDPdata$country_name = val_col
iso_3_col = serviceGDPFinal$ISO3
iso_3_col[nrow(serviceGDPFinal)+1] = "world_avg"
serviceGDPdata$ISO3 = iso_3_col
serviceGDPdata <- serviceGDPdata %>% melt(id.vars = c("country_name","ISO3"))
colnames(serviceGDPdata) = c("country","ISO3","year","value")

#### ------------------------ Functions to Use in worldmap and scatterplot ---------------------####

setDataSource <- function(name){
  # returns correct data source based on given name
  if(name=="Female Mortality"){
    return(map.world_joined5)
  }else{
    if(name=="Male Mortality"){
      return(map.world_joined4)
    }else{
      if(name=="GDP"){
        return(map.world_joined)
      }else{
        if(name=="Female Literacy"){
          return(map.world_joined3)
        }else{
          if(name=="Male Literacy"){
            return(map.world_joined2)
          }else{
            if(name=="Overall Mortality"){
              return(map.world_joined6)
            }else{
              if(name=="Female Life Expectancy"){
                return(map.world_joined7)
              }else{
                if(name=="Male Life Expectancy"){
                  return(map.world_joined8)
                }else{
                  if(name=="Overall Life Expectancy"){
                    return(map.world_joined9)
                  }else{
                    if(name=="Trade GDP"){
                      return(map.world_joined11)
                    }else{
                      if(name=="Service GDP"){
                        return(map.world_joined12)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

setContributor <- function(name){
  # returns correct contributor info based on given name
  if(name=="Female Mortality"){
    return("World Health Organization") 
  }else{
    if(name=="Male Mortality"){
      return("World Health Organization")
    }else{
      if(name=="GDP"){
        return("World Bank")
      }else{
        if(name=="Female Literacy"){
          return("World Bank")
        }else{
          if(name=="Male Literacy"){
            return("World Bank")
          }else{
            if(name=="Overall Mortality"){
              return("World Health Organization")
            }else{
              if(name=="Female Life Expectancy"){
                return("World Bank")
              }else{
                if(name=="Male Life Expectancy"){
                  return("World Bank")
                }else{
                  if(name=="General Life Expectancy"){
                    return("World Bank")
                  }else{
                    if(name=="Trade GDP"){
                      return("World Bank")
                    }else{
                      if(name=="Service GDP"){
                        return("World Bank")
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

setData_Units <- function(name){
  # returns correct data units info based on name
  if(name=="Female Mortality"){
    return("number of deaths female children under 5, per 1000")
  }else{
    if(name=="Male Mortality"){
      return("number of deaths male children under 5, per 1000")
    }else{
      if(name=="GDP"){
        return("GDP in USD per capita")
      }else{
        if(name=="Female Literacy"){
          return("percent of females who can read and write")
        }else{
          if(name=="Male Literacy"){
            return("percent of males who can read and write")
          }else{
            if(name=="Overall Mortality"){
              return("number of deaths of children under 5, per 1000")
            }else{
              if(name=="Female Life Expectancy"){
                return("projected length of life at birth")
              }else{
                if(name=="Male Life Expectancy"){
                  return("projected length of life at birth")
                }else{
                  if(name=="General Life Expectancy"){
                    return("projected length of life at birth")
                  }else{
                    if(name=="Trade GDP"){
                      return("percent to which trading makes up a country's GDP")
                    }else{
                      if(name=="Service GDP"){
                        return("percent to which services make up a country's GDP")
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

# Functions to get country name and ISO3 code from a data.frame containing a set of lat and long coordinates
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  # return the ADMIN names of each country
  indices$ADMIN
}
coords2ISO3 = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  indices$ISO3 # returns the ISO3 code 
}


# Create the world Map itself
createWorldMap<-function(name,year){
# Get DataSource, Contributor and map subtitle from the Name of Plot
  DataSource = setDataSource(name)
  Contributor = setContributor(name)
  data_units = setData_Units(name)

# Get column corresponding to user specified date and create a boolean column for missing values
a = as.numeric(which(names(DataSource)==year))                              # get the column corresponding to user specified date 
DataSource$missingD2 = with(DataSource, is.na(DataSource[,a]))              # see which rows are missing data for the specified year


# Apply a logarithmic scale to normalize the color distribution of the world map for GDP
Selected = DataSource[,a]                                                             # Data for choosen attribute for user selected year
DataSource$factors = Selected/(max(na.omit(Selected)))                                # Get a ratio for GDP values from max GDP for that year
DataSource$lnfactors = with(DataSource, pmin(log(1.5+DataSource$factors),.6))         # Apply ln transform to normalize colors on graph

# CHOOSE WHAT TYPE OF COLOR SCHEMA YOU WANT (regular or log-scale) based on 
# type of data set and "spread" of the data altogether

if(name=="GDP"){
  filler = DataSource$lnfactors
}else{
    filler = DataSource$factors}

  # See what Type of plot you are using -- (Could be replaced by just a name check but who knows if you'll need it later)
  ln = na.omit(ifelse(filler==DataSource$factors,"linear","logarithmic"))
  plotType = ln[1]

  # Calculate the scale to customize the legend used in the Map
  max_n = max(na.omit(filler))
  min_n = min(na.omit(filler))
  range_n = max_n-min_n
  size_ticks = range_n/6
  
  
  # Get the units of the tick marks on the legend set
  tick0 = min_n                                                
  tick1 = tick0 + size_ticks
  tick2 = tick1 + size_ticks
  tick3 = tick2 + size_ticks
  tick4 = tick3 + size_ticks
  tick5 = tick4 + size_ticks
  tick6 = max_n
  
  # Get quartiles for the legend in linear and log format
  if(plotType=="linear"){
  max_value = as.integer(max(na.omit(DataSource[,a])))           
  min_value = as.integer(min(na.omit(DataSource[,a])))
  
  q1_value = as.integer((1*(max_value-min_value)/6)+min_value)
  q2_value = as.integer((2*(max_value-min_value)/6)+min_value)
  q3_value = as.integer((3*(max_value-min_value)/6)+min_value)
  q4_value = as.integer((4*(max_value-min_value)/6)+min_value)
  q5_value = as.integer((5*(max_value-min_value)/6)+min_value)
  }
  
  if(plotType=="logarithmic"){
    max_value = as.integer(max(na.omit(DataSource[,a]))) 
    max_ln = log(max_value)
    min_value = as.integer(min(na.omit(DataSource[,a])))
    min_ln = log(min_value)
    diff = max_ln - min_ln
    
    q1_value_ln = ((1*(diff)/6)+min_ln)
    q2_value_ln = ((2*(diff)/6)+min_ln)
    q3_value_ln = ((3*(diff)/6)+min_ln)
    q4_value_ln = ((4*(diff)/6)+min_ln)
    q5_value_ln = ((5*(diff)/6)+min_ln)
    
    q1_value = round(exp(q1_value_ln),-2)
    q2_value = round(exp(q2_value_ln),-2)
    q3_value = round(exp(q3_value_ln),-3)
    q4_value = round(exp(q4_value_ln),-3)
    q5_value = round(exp(q5_value_ln),-3)
    
    max_value = round(max_value,-3)
    min_value = round(min_value,-2)
  }
  
  #Set labels for the legend ticks
  lab0 = min_value 
  lab1 = q1_value
  lab2 = q2_value
  lab3 = q3_value
  lab4 = q4_value
  lab5 = q5_value
  lab6 = max_value
  
  # Hexadecimal custom colors for theme of shiny app
  mycol =  "#BDBDBD" #dark gray
  mycol2 = "#111111" #black
  mycol3 = "#E3F2FD" #light blue
  mycol5 = "#BBDEFB" #darker blue
  mycol6 = "#B3E5Fc" #med blue
  mycol4 = "#FFFFFF" #white
  mycol7 = "#F5F5F5" #light gray

# Generate a chloropleth World Map of the User-selected datasets based on attribute and year
myMap<-ggplot() +
  geom_polygon(data = DataSource, aes(x = long, y = lat, group = group, fill = filler)) +
  labs(title = paste(year,name,sep = " "),subtitle = paste(data_units,"with colors displayed on a ",plotType,"scale.")) +
  scale_fill_gradientn(name="Legend",colours = brewer.pal(5, "RdYlBu"), na.value = "#BDBDBD",
                       breaks=c(tick0,tick1,tick2,tick3,tick4,tick5,tick6),
                       labels=c(lab0,lab1,lab2,lab3,lab4,lab5,lab6)) +
  theme(text = element_text(color = mycol2)
        ,panel.background = element_rect(fill = mycol7)
        ,panel.grid = element_blank()
        ,plot.background = element_rect(fill = mycol7)
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 15)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "right"
        ,legend.background = element_rect(fill = mycol7)
        ,legend.text = element_text(size = 12)
        ,legend.key.size = unit(10, "mm")
        ,legend.title = element_text(size = 15)
          
  )
return(myMap)
#ggsave("worldMap.png", plot = last_plot(), dpi = 800)
}


#####-------- Put the map into an R-shiny APP and add click capabilites as well as customized tooltip ---------#####

# Create the User Interface to include the map, data on the countries that have been selected in the plot
# and a hovertooltip

ui <- fluidPage(
  titlePanel("Global Health and Economic Factors Visualization"),
  fluidRow(
    column(3,
      wellPanel(style = "background:#BDBDBD",
           wellPanel(
             h4("Welcome!"),
             p("Hover over countries in the world map or points on the scatterplot for more information."),
             p("Click on countries in the world map to display additional information in the 'Selected Country Data' column and in the graphs below the map."),
             p("Brush and double click world map to zoom in. Double click to restore original image.")
           ),
           wellPanel(  
             h4("Select World Map Options"),
             selectInput("map_factor", "Choose a factor for world map visualization",
                         c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
                           "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
                           "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5",
                           "Female Life Expectancy at Birth, in years", "Male Life Expectancy at Birth, in years",
                           "Overall Life Expectancy at Birth, in years",
                           "Trade as % of GDP", "Services as % of GDP")
             ),
             selectizeInput("year_map", "Choose a year between 1980 and 2018", seq(1980, 2018, 1), selected = 2000)
           ),
           wellPanel(
             h4("Selected Country Data"),
             verbatimTextOutput("coordinates"),
             verbatimTextOutput("country"),
             verbatimTextOutput("ISO3"),
             verbatimTextOutput("value")
           )
      )
    ),
    column(6, align = "center",
          wellPanel(style = "background:#BDBDBD",
            div(
                style = "position:relative",
                plotOutput("myplot",width = 850,height = 500,clickOpts(id="on_click"),hover = hoverOpts("plot_hover"),brush = brushOpts(id="zoom_brush",resetOnNew = TRUE),dblclickOpts("plot1_dblclick")),
                uiOutput("hover_info")
            ),
            br(),
            fluidRow(
              column(6, align = "center",
                  plotOutput("line_plot", width = 400, height = 300),
                  actionButton("reset","reset")
                ),
              column(6,
                verbatimTextOutput("countryStats"),
                br(),
                br(),
              )
            )
          )
    ),
    column(3,
      wellPanel(style = "background:#BDBDBD",
        wellPanel(
          h4("World Development Indices"),
          selectInput("y_factor", "Choose a factor for the y-axis of the scatter plot",
                    c("Real GDP per Capita, 2010 US Dollars", "Male Literacy Rate, over 15 years old",
                      "Female Literacy Rate, over 15 years old", "Infant Mortality Rate per 1000, under 5",
                      "Female Infant Mortality Rate per 1000, under 5", "Male Infant Mortality Rate per 1000, under 5",
                      "Female Life Expectancy at Birth, in years", "Male Life Expectancy at Birth, in years",
                      "Overall Life Expectancy at Birth, in years",
                      "Trade as % of GDP", "Services as % of GDP"),
                    selected = "Male Literacy Rate, over 15 years old"
          ),
          p("Note: The x-axis and year factors of the scatterplot correspond to the world map selection."),
          br(),
          div(
            ggvisOutput("scatter1")
          ),
          br(),
          p("The purpose of this plot is to discern relationships between different global development factors."),
        ),
        wellPanel(
          h4("Notes"),
          p("Map may take a few moments to load."),
          textOutput("data_source"),
          textOutput("data_source2")
        )
      )
    )
  )
)


# Create a Server to implement the User Interface.  

server <- function(input, output) {
  
  #### World Map and Scatterplot functions #####
  
  # reactive year for world map and scatterplot
  thisyear = reactive({
    format(input$year_map)
  })
  
  # output the data sources used in both the world map and the scatterplot map 
  output$data_source <- renderText({paste("Map/Scatterplot x-axis data source: ",setContributor(name()))})
  output$data_source2 <- renderText({paste("Scatterplot y-axis data source: ",scatterContributor())})
  
  
  ##### World Map #####
  
  # setup reactive variable for map factor in UI
  name <- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$map_factor) return("GDP")
    if ("Male Literacy Rate, over 15 years old" %in% input$map_factor) return("Male Literacy")
    if ("Female Literacy Rate, over 15 years old" %in% input$map_factor) return("Female Literacy")
    if ("Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return("Overall Mortality") # NEEDS IMPLEMENTED
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return("Female Mortality")
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return("Male Mortality")
    if ("Female Life Expectancy at Birth, in years" %in% input$map_factor) return("Female Life Expectancy")
    if ("Male Life Expectancy at Birth, in years" %in% input$map_factor) return("Male Life Expectancy")
    if ("Overall Life Expectancy at Birth, in years" %in% input$map_factor) return("Overall Life Expectancy")
    if ("Trade as % of GDP" %in% input$map_factor) return("Trade GDP")
    if ("Services as % of GDP" %in% input$map_factor) return("Service GDP")
  })
  
  # Create the plot of the world map
  output$myplot<-renderPlot({
    createWorldMap(name(),thisyear())+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) # allow for brushOpts to work
    })
  
  # Get the Latitude, Longitude, countryname, data and ISO3 countrycode upon clicking on the map
  LatCoord <- reactive({
    if(is.null(input$on_click$y)){-100
    }else{format(input$on_click$x)}
    })
  LongCoord <- reactive({
    if(is.null(input$on_click$y)){40
    }else{format(input$on_click$y)}
    })
  countryName <- reactive({
    tryCatch(format(coords2country(data.frame(as.numeric(LatCoord()),as.numeric(LongCoord())))),error = function(e) NA)
    })
  ISO3 <- reactive({
    tryCatch(format(coords2ISO3(data.frame(as.numeric(LatCoord()),as.numeric(LongCoord())))),error = function(e) NA)
    })
  getDataPoint <- reactive({
    DataSource = setDataSource(name())
    idx = as.numeric(which(DataSource[,7]==ISO3())[1])
    a = as.numeric(which(names(DataSource)==thisyear()))                        
    round(DataSource[idx,a],2)
    })
  
  # Get the country, coordinates, plotted attribute and ISO3 value to output as text to the UI 
  output$coordinates <- renderText({
    paste0("Latitude: ",LatCoord(),"\n","Longitude: ",LongCoord())
    })
  output$country <- renderText({
    paste0("Country: ",countryName())
    })
  output$ISO3 <- renderText({
    paste0("ISO3: ",ISO3())
    })
  output$value <- renderText({
    paste0(thisyear()," ",name(),": ",getDataPoint())
    })
  
  # Create a custom tooltip for World Map with render UI. Fill tooltip with the country and value in use 
  output$hover_info <- renderUI({
    DataSource = setDataSource(name())
    # get the information for the x and y coordinates from the hover over values in the plot
    hover <- input$plot_hover
    lat<-hover$x
    long<-hover$y
    coords = data.frame(lat,long)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(210, 210, 210, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    a = as.numeric(which(names(DataSource)==thisyear())) 
    # actual tooltip created as wellPanel: if the functions in the tooltip throw errors, return a Null tooltip
    tryCatch(
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Country: </b>", coords2country(coords), "<br/>",
                      "<b>",thisyear()," ",name(),": ","</b>", round(DataSource[which(DataSource[,7]==coords2ISO3(coords))[1],a],0), "<br/>"
                    )))
                ), error = function(e) NULL
            )
    
    })
  
  ###### Scatterplot #####
  
  scatter_x_reactive <- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$map_factor) return(gdpFinal)
    if ("Male Literacy Rate, over 15 years old" %in% input$map_factor) return(literacyMaleFinal)
    if ("Female Literacy Rate, over 15 years old" %in% input$map_factor) return(literacyFemaleFinal)
    if ("Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortalityBTSXFinal)
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortalityFemaleFinal)
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortalityMaleFinal)
    if ("Female Life Expectancy at Birth, in years" %in% input$map_factor) return(expectancyFemaleFinal)
    if ("Male Life Expectancy at Birth, in years" %in% input$map_factor) return(expectancyMaleFinal)
    if ("Overall Life Expectancy at Birth, in years" %in% input$map_factor) return(expectancyTotalFinal)
    if ("Trade as % of GDP" %in% input$map_factor) return(tradeGDPFinal)
    if ("Services as % of GDP" %in% input$map_factor) return(serviceGDPFinal)
  })
  
  # setup reactive variable for y factor in UI
  scatter_y_reactive<- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$y_factor) return(gdpFinal)
    if ("Male Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyMaleFinal)
    if ("Female Literacy Rate, over 15 years old" %in% input$y_factor) return(literacyFemaleFinal)
    if ("Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityBTSXFinal)
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityFemaleFinal)
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return(mortalityMaleFinal)
    if ("Female Life Expectancy at Birth, in years" %in% input$y_factor) return(expectancyFemaleFinal)
    if ("Male Life Expectancy at Birth, in years" %in% input$y_factor) return(expectancyMaleFinal)
    if ("Overall Life Expectancy at Birth, in years" %in% input$y_factor) return(expectancyTotalFinal)
    if ("Trade as % of GDP" %in% input$y_factor) return(tradeGDPFinal)
    if ("Services as % of GDP" %in% input$y_factor) return(serviceGDPFinal)
  })
  
  scatterContributor<- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$y_factor) return("World Bank")
    if ("Male Literacy Rate, over 15 years old" %in% input$y_factor) return("World Bank")
    if ("Female Literacy Rate, over 15 years old" %in% input$y_factor) return("World Bank")
    if ("Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return("World Health Organization")
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return("World Health Organization")
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$y_factor) return("World Health Organization")
    if ("Female Life Expectancy at Birth, in years" %in% input$y_factor) return("World Bank")
    if ("Male Life Expectancy at Birth, in years" %in% input$y_factor) return("World Bank")
    if ("Overall Life Expectancy at Birth, in years" %in% input$y_factor) return("World Bank")
    if ("Trade as % of GDP" %in% input$y_factor) return("World Bank")
    if ("Services as % of GDP" %in% input$y_factor) return("World Bank")
  })
  
  
  # setup reactive variable for x axis label in scatterplot
  # matches factor displayed from world map
  scatter_x_reactive_label <- reactive({input$map_factor})
  
  # setup reactive variable for y axis label in scatterplot
  scatter_y_reactive_label <- reactive({input$y_factor})
  
  # reactive data join of scatter
  scatterDataReactive = reactive({
    # What we really want is a data frame with country, y_variable for year Y and x_variable for year Y in the same plot
    year = thisyear()
    ax = as.numeric(which(names(scatter_x_reactive())==year))                         # desired column for x-axis data
    bx = as.numeric(which(names(scatter_x_reactive())=="Country"))                    # country names column
    combined_x <- data.frame(round(scatter_x_reactive()[ax],3),scatter_x_reactive()[bx])
    
    ay = as.numeric(which(names(scatter_y_reactive())==year))                         # desired column for y-axis data
    by = as.numeric(which(names(scatter_y_reactive())=="Country"))                    # country names column
    cy = as.numeric(which(names(scatter_y_reactive())=="continents"))                 # add continents into df to color coat points in scatterplot
    combined_y <- data.frame(round(scatter_y_reactive()[ay],3),scatter_y_reactive()[by],scatter_y_reactive()[cy])
    
    scatterframe<-merge(combined_x,combined_y,by=c("Country"="Country"))              # merge x and y data by country 
    colnames(scatterframe) <- c("Country", "x", "y","Continent")                      # update column names in dataframe
    
    return(na.omit(scatterframe))
  })
  
  # Function for hover data for ggvis visualisation
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
        # Get the x axis value hovered over in plot
        primary1 = round(as.numeric(x[1]),3)
        # Get the scatterplot Dataframe constructed from the given variables 
        scatterData2 = scatterDataReactive()
        # round the values in the x col to 3 decimal places
        scatterData2$x = round(scatterData2$x,3)
        # get the index of the row in the x-axis column which is equal to x-axis value hovered over in plot 
        idx = which(scatterData2$x==primary1)
        # Use index from previous step to get associated country name in the scatterplot
        cName = scatterData2$Country[idx]
        # paste countryname and x and y values into the tooltip for the scatterplot
        paste0(format(cName),"<br/>",names(x[1]), ": ", format(x[1]),"<br/>",names(x[2]),": ",format(x[2]),collapse = "<br />")
  }
  
  # Create ggvis scatter plot output with points, set dimensions, tooltip and specialized axis labels
  vis <- reactive({
    scatterframe2 <- scatterDataReactive()
    scatterframe2 %>%
    ggvis(x = ~x, y = ~y, fill = ~factor(Continent)) %>%
    set_options(height = 350,width = 379) %>% 
    layer_points() %>%
    add_legend("fill", title = "Continents") %>%
    add_axis("x",title = scatter_x_reactive_label(),title_offset = 50,properties = axis_props(
      labels = list(angle = 45, align = "left", fontSize = 10)
    )) %>%
    add_axis("y",title = scatter_y_reactive_label(), title_offset = 50) %>%
    add_tooltip(all_values, "hover") 
  })
  
  # Bind the ggvis object to the shiny object as output named scatter1
  vis %>% bind_shiny("scatter1")
  
  # Set up zoom capabilities for the world map on brushing and clicking on the scatterplot
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot1_dblclick, {
    brush <- input$zoom_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  ##### Line plot underneath the world Map #####
  
  line_plot_data <- reactive({
    if ("Real GDP per Capita, 2010 US Dollars" %in% input$map_factor) return(gdpData)
    if ("Male Literacy Rate, over 15 years old" %in% input$map_factor) return(litMaleData)
    if ("Female Literacy Rate, over 15 years old" %in% input$map_factor) return(litFemData)
    if ("Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortAllData)
    if ("Female Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortFemData)
    if ("Male Infant Mortality Rate per 1000, under 5" %in% input$map_factor) return(mortMaleData)
    if ("Female Life Expectancy at Birth, in years" %in% input$map_factor) return(expctFemData)
    if ("Male Life Expectancy at Birth, in years" %in% input$map_factor) return(expctMaleData)
    if ("Overall Life Expectancy at Birth, in years" %in% input$map_factor) return(expctAllData)
    if ("Trade as % of GDP" %in% input$map_factor) return(tradeGDPdata)
    if ("Services as % of GDP" %in% input$map_factor) return(serviceGDPdata)
  })
  
  # List of countries that have been selected on the world map 
  myVals <- reactiveValues(mylist=list())
  
  # If the country is NOT already in the list AND it has been clicked (taken from the ISO3 call), add to the list
  values <- reactive({
    if (ISO3() %in% myVals$mylist){
    }else{
      myVals$mylist = append(myVals$mylist,ISO3())}
  })
  
  # Empty the list of clicked countries when the reset button is pressed
  observeEvent(input$reset,{
    myVals$mylist = list()
  })
  
  # Reactively update data frame to be used in the gdpplot function below
  getDF <- reactive({
    values()                       # Call the function to add ISO3() values to a list on clicking their corresponding countries
    dataSourcee = line_plot_data() # Choose what dataset you want to use 
    # Get a list of all of the world avg vals in the datasource and use to subset the data frame to only include world avgs
    list_idx <- which(dataSourcee$country=="world_avg")
    my_df = dataSourcee[list_idx,]
    # Select the data from dataSourcee corresponding to the countries that have been clicked
    clicked_countries = myVals$mylist
    # Combine existing dataset with all of the data from the countries that have been clicked on 
    my_df = rbind(my_df,subset(dataSourcee, dataSourcee$ISO3 %in% myVals$mylist))
    return(my_df)
  })
  
  # Function to dynamically plot the GDP world average and all selected countries GDP over time 
  output$line_plot <- renderPlot({
    dynamic_frame = na.omit(getDF())
    ggplot(dynamic_frame, aes(x = year, y = value, group = country, colour = ISO3))+
      labs(x = "Year", y = scatter_x_reactive_label(), title = name())+
      scale_x_discrete(name = "Year", breaks = c(1980,1985,1990,1995,2000,2005,2010,2015,2020), labels = c("1980","1985","1990","1995","2000","2005","2010","2015","2020"))+
      geom_line(size = 1.1)+
      theme(text = element_text(color = "#111111")
        ,panel.background = element_rect(fill = "#E0E0E0")
        ,panel.grid = element_line(color = "#FFFFFF")
        ,panel.border = element_blank()
        ,plot.background = element_rect(fill = "#F5F5F5")
        ,plot.title = element_text(size = 15)
        ,plot.subtitle = element_blank()
        ,legend.background = element_rect(fill = "#F5F5F5")
      )
  })
  
  ##### Text output w/ country stats underneath the World Map #####
  
  # Whenever the year is changed, updated the rankDF with all of the ranks of the countries in it
  updateDF <- reactive({
    valList = list(gdpFinal,tradeGDPFinal,serviceGDPFinal,literacyFemaleFinal,literacyMaleFinal,expectancyFemaleFinal,expectancyMaleFinal,expectancyTotalFinal,mortalityBTSXFinal,mortalityMaleFinal,mortalityFemaleFinal)
    nameValList = list("ISO3","GDP","tradeGDP","serviceGDP","femaleLiteracy","maleLiteracy","femaleLifeExpectancy","maleLifeExpectancy","BTSXlifeExpectancy","BTSXmortality","maleMortality","femaleMortality")
    
    rankDF = data.frame(gdpFinal$ISO3)
    colnames(rankDF)<-c("ISO3")
    
    for(i in 1:length(valList)){
      year = thisyear()
      name = as.character(nameValList[i])
      DataSource7 = data.frame(valList[i])
      Selected = data.frame(DataSource7[,as.numeric(which(names(DataSource7)==paste0("X",year)))], DataSource7[,which(names(DataSource7)=="ISO3")])
      colnames(Selected) <- c("Value","ISO3")
      Selected$Rank = rank(Selected$Value,na.last = "keep")
      Selected$Value = NULL
      rankDF = left_join(rankDF,Selected,by="ISO3")
    }
    colnames(rankDF) <- nameValList
    
    return(rankDF)
  })
  
  # Output the stats for the selected country
  output$countryStats<-renderText({
    rankDF = updateDF()
    currentClick = ISO3()
    paste0(countryName()," (",thisyear(),")","\n","\n",
           " GDP per capita rank: ",rankDF[which(rankDF$ISO3==currentClick),2]," out of ",max(na.omit(rankDF[,2]))," countries.","\n",
           " Trade as % of GDP rank: ",rankDF[which(rankDF$ISO3==currentClick),3]," out of ",max(na.omit(rankDF[,3]))," countries.","\n",
           " Services as % of GDP rank: ",rankDF[which(rankDF$ISO3==currentClick),4]," out of ",max(na.omit(rankDF[,4]))," countries.","\n",
           
           " Female Life Expectancy rank: ",round(rankDF[which(rankDF$ISO3==currentClick),7],0)," out of ",max(na.omit(rankDF[,7]))," countries.","\n",
           " Male Life Expectancy rank: ",round(rankDF[which(rankDF$ISO3==currentClick),8],0)," out of ",max(na.omit(rankDF[,8]))," countries.","\n",
           
           " Female Infant Mortality rank: ",rankDF[which(rankDF$ISO3==currentClick),12]," out of ",max(na.omit(rankDF[,12]))," countries.","\n",
           " Male Infant Mortality rank: ",rankDF[which(rankDF$ISO3==currentClick),11]," out of ",max(na.omit(rankDF[,11]))," countries.","\n",
           
           " Female Literacy rank: ",rankDF[which(rankDF$ISO3==currentClick),5],"\n",
           " Male Literacy rank: ",rankDF[which(rankDF$ISO3==currentClick),6],"\n",
           
           "\n",
           
           "NOTE: Rank of 1 is the lowest value.","\n", 
           "Number of countries in ranking is dependent on the","\n",
           "availability of data for a given year."
           )
  })
  
}

# Run the App itself
shinyApp(ui, server)






#### ------------- Prior Code Snippets --------- #####

# # Function using online API to get the ISO3 value
# ISO3 <- reactive({
#   if(!is.na(getISO2(LatCoord(),LongCoord()))){
#     GNcountryInfo(getISO2(LatCoord(),LongCoord()))$isoAlpha3
#     }else{
#       NA
#     }
#   })

# # Function using online API to get the country name from the long and lat coordinates  
# countryName <- reactive({
#   format(getCountry(LatCoord(),LongCoord()))
# })

# # Generate the country name from the latitude and longitude coordinates that have been selected from the map
# getCountry <- function(Lat,Long){
#   options(geonamesUsername = "zachkey")
#    # if the API throws an error, then deal with it by returning NA
#   return(tryCatch(GNcountryCode(Long,Lat)$countryName, error=function(e) NA))
# }
# # Generate ISO2 codes from the countries that have been selected
# getISO2 <- function(Lat,Long){
#   options(geonamesUsername = "zachkey")
#   myfunc = tryCatch(GNcountryCode(Long,Lat)[3], error = function(e){NA})
#   return(myfunc)
# }

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
# coords2country = function(points)
# {  
#   countriesSP <- getMap(resolution='low')
#   #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
#   
#   # convert our list of points to a SpatialPoints object
#   
#   # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
#   
#   #setting CRS directly to that from rworldmap
#   pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
#   
#   # use 'over' to get indices of the Polygons object containing each point 
#   indices = over(pointsSP, countriesSP)
#   
#   # return the ADMIN names of each country
#   indices$ADMIN
#   #indices$ISO3 # returns the ISO3 code 
#   #indices$continent   # returns the continent (6 continent model)
#   #indices$REGION   # returns the continent (7 continent model)
# }

# ##### -------------------------- Interactive version of the ggplot Map ------------------------------- #####
# 
# require(ggplot2)
# require(ggiraph) # Use interactive library to get interactive labels and zoom capabilities for plot
# require(RColorBrewer)
# 
# # Get the value to be shown when you hover over the map with the mouse
# DataSource$Country = DataSource$region
# DataSource$Country = paste(DataSource$Country,":",as.integer(DataSource[,a]))
# 
# myMap2<-ggplot() +
#   geom_polygon_interactive(data = DataSource, aes(x = long, y = lat, group = group, fill = filler, tooltip = Country, data_id = region)) +
#   labs(title = paste(year,name,sep = " "), subtitle = paste(data_units,"with colors displayed on a ",plotType,"scale."), caption = paste("source: ",Contributor)) +
#   scale_fill_gradientn(name=name,
#                        colours = brewer.pal(5, "RdYlBu"), 
#                        na.value = 'white',
#                        breaks=c(tick0,tick1,tick2,tick3,tick4,tick5,tick6),
#                        labels=c(lab0,lab1,lab2,lab3,lab4,lab5,lab6)) +
#   theme(text = element_text(color = "#FFFFFF")
#         ,panel.background = element_rect(fill = "#444444")
#         ,plot.background = element_rect(fill = "#444444")
#         ,panel.grid = element_blank()
#         ,plot.title = element_text(size = 30)
#         ,plot.subtitle = element_text(size = 10)
#         ,axis.text = element_blank()
#         ,axis.title = element_blank()
#         ,axis.ticks = element_blank()
#         ,legend.position = "right"
#         ,legend.background = element_rect(fill = "#444444")
#   )
# 
# # Create an interactive plot with zoom and hover controls with an aspect ratio of 10:6
# ggiraph(code = print(myMap2),tooltip_offx = 20, tooltip_offy = -10,width_svg = 10,height_svg = 6,zoom_max = 8)
# 
# year = 2010
# ax = as.numeric(which(names(gdpFinal)==year))                         # desired column
# bx = as.numeric(which(names(gdpFinal)=="Country"))                    # country names column
# combined_x <- data.frame(gdpFinal[ax],gdpFinal[bx])
# 
# ay = as.numeric(which(names(expectancyFemaleFinal)==year))            # desired column
# by = as.numeric(which(names(expectancyFemaleFinal)=="Country"))       # country names column
# cy = as.numeric(which(names(expectancyFemaleFinal)=="continents"))    # continents colummn 
# combined_y <- data.frame(expectancyFemaleFinal[ay],expectancyFemaleFinal[by],expectancyFemaleFinal[cy])
# 
# scatterframe<-merge(combined_x,combined_y,by=c("Country"="Country"))
# colnames(scatterframe) <- c("Country", "x", "y","Continent")

# 
# typeof(names(scatterframe)[1])
# names(scatterframe)[1]      
# typeof(names(scatterframe)[2])
# names(scatterframe)[2]
# scatterframe$x = round(scatterframe$x,3)
# which(scatterframe$x==234.236)
#
# gdpFinal
# a = as.numeric(which(names(gdpFinal)==2010))
# valFrame = round(unique(na.omit(gdpFinal[,a])),3)
# primary = 543.303
# val = gdpFinal[which(valFrame==primary),5]
# value = round(gdpFinal[which(valFrame==primary),a],3)


# ---Sources sited/Snippets of code used---
# coords to country: https://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-name-in-r
# ggvis axis: https://ggvis.rstudio.com/axes-legends.html
# customized tooltip for worldmap: https://gitlab.com/snippets/16220


#### Create a lineplot that maps a particular value over the course of time for a given country ####

# # Get world plot of all of the countries in the world
# DataSourcer = expectancyFemaleJoin
# colnames(DataSourcer)[ncol(DataSourcer)-1] <- "value"
# head(DataSourcer)
# 
# worldPlot<-DataSourcer %>%
#   ggplot(aes(x=year, y=value, group = Country)) + 
#   geom_line()
# worldPlot  

# # Get world average for all time for selected DataSourcer
# a = as.numeric(which(names(DataSourcer)=="value"))
# world_average_all_time = mean(na.omit(DataSourcer[,a]))

# 
# # Functions for scatterplot
# scatterDataMerge <- function(x, y, year) {
#   # joins final data tables on country for year given
#   x_data <- x %>% select(c("Country", toString(year))) # selects correct year and all countries
#   head(x_data)
#   y_data <- y %>% select(c("Country", toString(year))) # selects correct year and all countries
#   head(y_data)
#   joinedTable <- inner_join(x_data, y_data, by = c("Country" = "Country"))
#   colnames(joinedTable)[1] = "i"
#   colnames(joinedTable)[2] = "x"
#   colnames(joinedTable)[3] = "y"
#   joinedTable <- joinedTable %>% mutate(x = replace(x, is.na(x), -1)) %>% filter(x != -1) %>% mutate(y=replace(y, is.na(y), -1)) %>% filter(y != -1)
#   return(joinedTable)
# }
# 
# createScatterplot <- function(data, xName, yName){
#   # creates a scatterplot from table with x and y columns
#   plot <- data %>%
#     ggvis(~x, ~y) %>%
#     # scale_numeric("x", trans = "log", expand = 0, nice = TRUE) %>%
#     layer_points(fillOpacity := 0.45, size := 50) %>% 
#     add_axis("x", title = toString(xName), properties = axis_props(title = list(dy = 25))) %>% 
#     add_axis("y", title = toString(yName), properties = axis_props(title = list(dy = -30))) %>%
#     add_axis("x", orient = "top", ticks = 0, title = toString(paste(yName, "vs.")), properties = axis_props(title = list(fontSize = 14))) %>%
#     add_axis("x", orient = "top", ticks = 0, title = toString(xName), properties = axis_props(title = list(fontSize = 14, dy=18))) %>%
#     set_options(width = 500, height = 500) %>% layer_smooths() %>%
#     set_options(keep_aspect = TRUE) 
# }
# 
# 
# 
# # data2[nrow(data2),] = world_avg_yearly # append averages to GDP for each country
# 
# # Get single country's info 
# head(gdpFinal)
# DataSourcer = gdpFinal
# country = "Andorra"
# 
# idxer = as.numeric(which(names(DataSourcer)=="Country")) # Get index of the country column in the Datasourcer df
# value = as.numeric(which(DataSourcer[,idxer]==country))              # Get particular index of row in country column of Datasourcer df 
# sliceDataSourcer = DataSourcer[value,]                   # Get a slice DataSourcer corresponding to the country you have selected 
# sliceDataSourcer                                         # print out the slice of info for the country 
# 
# # Get all of the values in SliceDataSourcer that are years 
# year_indices<-which(sapply(sliceDataSourcer, class)=="numeric")
# 
# # Get a data frame with the years and values for each year for a specific country 
# data<-round(sliceDataSourcer[year_indices],0)
# data[2,]<-world_avg_yearly
# data[3,]<-as.integer(names(data))
# 
# # Transpose data so year and value for year form a column
# library(data.table)
# data<-transpose(data)
# colnames(data) <- c("country_value","global_avg","year")
# 
# 
# 
# # plot a single country's data with ggplot
# df <- data %>%
#   select(global_avg, country_value, year) %>%
#   gather(key = "variable", value = "value", -year)
# head(df)
# 
# myPlot<-ggplot(df, aes(x=year, y=value)) + # group equals 1 bc all points should be connected
#   geom_line(aes(color=variable))
# 
# myPlot
# 
# dynamic_frame = na.omit(data2)
# ggplot(dynamic_frame, aes(x = year, y = value, group = country))+
#   geom_line()+
#   labs(x="Years", y= "GDP", title = "GDP per year")
# 
# length(unique(my_df$country))

# Get world average for each year for GDP
#year_indices2 <- which(sapply(gdpFinal, class)=="numeric")    # Get all of the year cols from gdp final 
#years = names(gdpFinal[,year_indices2])
#data2 <- round(gdpFinal[year_indices],0)                      # round to be integers basically 
#world_avg_yearly <- apply(na.omit(data2),2,mean)              # List of yearly averages for GDP
#data2[nrow(data2)+1,] = round(world_avg_yearly,0)             # add a new row to data2 for global averages over the years

# Add the country names to the dataframe of GDP 
#val_col = gdpFinal$Country                                    # get the country column of the dataframe you are using
#val_col[nrow(gdpFinal)+1] = "world_avg"                       # add a row 'world_avg' to country col 
#data2$country_name = val_col                                  # append to data2 dataframe

# Add the ISO3 values to the dataframe of GDP 
# iso_3_col = gdpFinal$ISO3
# iso_3_col[nrow(gdpFinal)+1] = "world_avg"
# data2$ISO3 = iso_3_col
# 
# library(reshape2)
# data2 = melt(data2,id.vars =c("country_name","ISO3"))         # get data in form cols = country, iso3 year, value
# colnames(data2) = c("country","ISO3","year","value")
# mylist = c("USA","CAN","AUS")
# 
# # Set datasource and year
# DataSource = literacyFemaleFinal
# year = 2010
# 
# # Get the cols of country and selected year into a dataframe
# Selected = na.omit(data.frame(DataSource[,as.numeric(which(names(DataSource)==year))], DataSource[,which(names(DataSource)=="Country")]))
# 
# # Rename the columns and add a new column that ranks the existing value column 
# colnames(Selected) <- c("Value","Country")
# Selected$Rank = rank(newSelected$Value)

# create a matrix of order for all of the countries for every attribute in a given year 
# valList = list(gdpFinal,tradeGDPFinal,serviceGDPFinal,literacyFemaleFinal,literacyMaleFinal,expectancyFemaleFinal,expectancyMaleFinal,expectancyTotalFinal,mortalityBTSXFinal,mortalityMaleFinal,mortalityFemaleFinal)
# nameValList = list("ISO3","GDP","tradeGDP","serviceGDP","femaleLiteracy","maleLiteracy","femaleLifeExpectancy","maleLifeExpectancy","BTSXlifeExpectancy","BTSXmortality","maleMortality","femaleMortality")
# 
# rankDF = data.frame(gdpFinal$ISO3)
# colnames(rankDF)<-c("ISO3")
# 
# for(i in 1:length(valList)){
#   year = 2010
#   name = as.character(nameValList[i])
#   DataSource7 = data.frame(valList[i])
#   Selected = data.frame(DataSource7[,as.numeric(which(names(DataSource7)==paste0("X",year)))], DataSource7[,which(names(DataSource7)=="ISO3")])
#   colnames(Selected) <- c("Value","ISO3")
#   Selected$Rank = rank(Selected$Value,na.last = "keep")
#   Selected$Value = NULL
#   rankDF = left_join(rankDF,Selected,by="ISO3")
# }
# 
# colnames(rankDF) <- nameValList
