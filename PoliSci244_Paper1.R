## Tejas Athni
## PoliSci 244A, Response Paper 1

library(dplyr)
library(readxl)
library(tidyverse)
library(haven)
library(tidyr)
library(magrittr)
library(stargazer)
library(equatiomatic)
setwd("E:/SynologyDrive/Tejas_Server/! Stanford University/! Year 3 - Quarter 2/PoliSci 244A/Response Papers/L12 - Repression/")


# Country code dictionary for transformation, raw found at https://unstats.un.org/unsd/tradekb/knowledgebase/country-code
countryCodes <- read.delim("clipboard") %>%
  separate("ABW.Aruba", c("Country_Code", "Country_Name"), " ")
saveRDS(countryCodes, "countryCodes.Rds")


# Load in datasets
load("PoliticalTerrorScale.RData")
PTS <- PTS_2020 %>%
  mutate(Repression = ifelse(PTS_S %in% c(3:5), 1, 0)) %>%
  select("Country","Year", "WordBank_Code_A", "Repression") %>%
  filter(Year %in% c(1990,2000))

HIEF <- read.csv("HIEF_data.csv") %>%
  mutate(countryCode = countryCodes$Country_Code[match(HIEF$Country, countryCodes$Country_Name)])
HIEF$countryCode[HIEF$Country == "United Kingdom"] <- "GBR"
HIEF$countryCode[HIEF$Country == "German Federal Republic"] <- "DEU"
HIEF$countryCode[HIEF$Country == "German Democratic Republic"] <- "DEU"
HIEF$countryCode[HIEF$Country == "Czechoslovakia"] <- "CZE"
HIEF$countryCode[HIEF$Country == "Czech Republic"] <- "CZE"
HIEF$countryCode[HIEF$Country == "Republic of Korea"] <- "KOR"
HIEF$countryCode[HIEF$Country == "Sri Lanka"] <- "LKA"
HIEF$countryCode[HIEF$Country == "Macedonia"] <- "MKD"
HIEF$countryCode[HIEF$Country == "Bosnia-Herzegovina"] <- "BIH"
HIEF$countryCode[HIEF$Country == "USSR"] <- "RUS"
HIEF$countryCode[HIEF$Country == "Russia"] <- "RUS"
HIEF$countryCode[HIEF$Country == "Moldova"] <- "MDA"
HIEF$countryCode[HIEF$Country == "Cote d'Ivoire"] <- "CIV"
HIEF$countryCode[HIEF$Country == "Burkina Faso"] <- "MDA"
HIEF$countryCode[HIEF$Country == "Yugoslavia"] <- "YGS"
HIEF$countryCode[HIEF$Country == "Saudi Arabia"] <- "SAU"
HIEF$countryCode[HIEF$Country == "Central African Republic"] <- "CAF"
HIEF$countryCode[HIEF$Country == "Syria"] <- "SYR"
HIEF$countryCode[HIEF$Country == "Liberia"] <- "LBR"
HIEF$countryCode[HIEF$Country == "Libya"] <- "LBY"
HIEF$countryCode[HIEF$Country == "Cape Verde"] <- "CPV"
HIEF$countryCode[HIEF$Country == "Sierra Leone"] <- "SLE"
HIEF$countryCode[HIEF$Country == "Democratic Republic of Congo"] <- "COD"
HIEF$countryCode[HIEF$Country == "Tanzania"] <- "TZA"
HIEF$countryCode[HIEF$Country == "South Africa"] <- "ZAF"
HIEF$countryCode[HIEF$Country == "United States of America"] <- "USA"
HIEF$countryCode[HIEF$Country == "Dominican Republic"] <- "DOM"
HIEF$countryCode[HIEF$Country == "Trinidad and Tobago"] <- "TTO"
HIEF$countryCode[HIEF$Country == "El Salvador"] <- "SLV"
HIEF$countryCode[HIEF$Country == "Costa Rica"] <- "CRI"
HIEF$countryCode[HIEF$Country == "Venezuela"] <- "VEN"
HIEF$countryCode[HIEF$Country == "Iran"] <- "IRN"
HIEF$countryCode[HIEF$Country == "Yemen Arab Republic"] <- "YEM"
HIEF$countryCode[HIEF$Country == "Yemen PDR"] <- "YEM"
HIEF$countryCode[HIEF$Country == "United Arab Emirates"] <- "ARE"
HIEF$countryCode[HIEF$Country == "Taiwan"] <- "TWN"
HIEF$countryCode[HIEF$Country == "Democratic People's Republic of Korea"] <- "PRK"
HIEF$countryCode[HIEF$Country == "Laos"] <- "LAO"
HIEF$countryCode[HIEF$Country == "Democratic Republic of Vietnam"] <- "VNM"
HIEF$countryCode[HIEF$Country == "Republic of Vietnam"] <- "VNM"
HIEF$countryCode[HIEF$Country == "East Timor"] <- "TLS"
HIEF$countryCode[HIEF$Country == "New Zealand"] <- "NZL"
HIEF$countryCode[HIEF$Country == "Solomon Islands"] <- "SLB"
HIEF$countryCode[HIEF$Country == "Bolivia"] <- "BOL"
HIEF$countryCode[HIEF$Country == "Kyrgyz Republic"] <- "KGZ"


GDP_percent_change <- read.csv("GDP_Percent_Change.csv") %>%
  select("Country.Name", "Country.Code", "X1990..YR1990.", "X2000..YR2000.")

FDI_inflows <- read.csv("Development_Indicators_1960_2020.csv") %>%
  filter(Series.Code == "BX.KLT.DINV.WD.GD.ZS") %>%
  select("Country.Name", "Country.Code", "X1990..YR1990.", "X2000..YR2000.")

state_security <- read_dta("Erica De Bruin/SSFD_secforce_oct2019.dta") %>%
  mutate(rowCount = 1) %>%
  group_by(cabb, year) %>%
  summarize(effective_number = sum(rowCount)) %>%
  ungroup()

excluded_pop <- read_dta("Erica De Bruin/SSFD_BRF_replication.dta") %>%
  select("year", "cname", "rexclpop") %>%
  filter(year %in% c(1990,2000))

regimes <- read_excel("Data_Set.xls") %>%
  select("scode", "year", "regime_r") %>%
  filter(regime_r %in% c("Military","Multiparty","Single Party","Monarchy"))


# Prepare data for logistic regression model
regimes_1990 <- regimes %>% filter(year == 1990) %>% pull(scode)
num_regimes1990 <- length(regimes_1990)

regimes_2000 <- regimes %>% filter(year == 2000) %>% pull(scode)
num_regimes2000 <- length(regimes_2000)



# Create consolidated data for 1990
pts_1990 <- PTS %>%
  filter(Year == 1990) %>%
  select(WordBank_Code_A, Repression) %>%
  filter(!is.na(Repression))
hief_1990 <- HIEF %>%
  filter(Year == 1990) %>%
  select(countryCode, EFindex) %>%
  filter(!is.na(EFindex))
fdi_1990 <- FDI_inflows %>%
  select(Country.Code, "X1990..YR1990.") %>%
  filter(!is.na("X1990..YR1990.")) %>%
  filter(!`X1990..YR1990.`== "..")
security_1990 <- state_security %>%
  filter(year == 1990) %>%
  select(cabb, effective_number) %>%
  filter(!is.na(effective_number))
exclusion_1990 <- excluded_pop %>%
  filter(year == 1990) %>%
  select(cname, rexclpop) %>%
  filter(!is.na(rexclpop))

list_1990 <- Reduce(intersect, list(pts_1990$WordBank_Code_A,
                                    hief_1990$countryCode,
                                    fdi_1990$Country.Code,
                                    security_1990$cabb,
                                    exclusion_1990$cname))
num_final1990 <- length(list_1990)

pts_1990 %<>% filter(WordBank_Code_A %in% list_1990)
hief_1990 %<>% filter(countryCode %in% list_1990)
hief_1990 <- hief_1990[-9,] %>% select(EFindex)
fdi_1990 %<>% filter(Country.Code %in% list_1990) %>% select("X1990..YR1990.")
security_1990 %<>% filter(cabb %in% list_1990) %>% select(effective_number)
exclusion_1990 %<>% filter(cname %in% list_1990) %>% select(rexclpop)

data_1990 <- cbind(pts_1990, hief_1990, fdi_1990, security_1990, exclusion_1990)
colnames(data_1990) <- c("CountryCode", "Repression", "HIEF", "FDI_Inflows_Perc_GDP","Security_Agencies","Excluded_Population")
data_1990 %<>% mutate(Excluded_Population_Perc = as.numeric(Excluded_Population)) %>% select(-"Excluded_Population")
data_1990$FDI_Inflows_Perc_GDP <- as.numeric(data_1990$FDI_Inflows_Perc_GDP)



# Create consolidated data for 2000
pts_2000 <- PTS %>%
  filter(Year == 2000) %>%
  select(WordBank_Code_A, Repression) %>%
  filter(!is.na(Repression))
hief_2000 <- HIEF %>%
  filter(Year == 2000) %>%
  select(countryCode, EFindex) %>%
  filter(!is.na(EFindex))
fdi_2000 <- FDI_inflows %>%
  select(Country.Code, "X2000..YR2000.") %>%
  filter(!is.na("X2000..YR2000.")) %>%
  filter(!`X2000..YR2000.`== "..")
security_2000 <- state_security %>%
  filter(year == 2000) %>%
  select(cabb, effective_number) %>%
  filter(!is.na(effective_number))
exclusion_2000 <- excluded_pop %>%
  filter(year == 2000) %>%
  select(cname, rexclpop) %>%
  filter(!is.na(rexclpop))

list_2000 <- Reduce(intersect, list(pts_2000$WordBank_Code_A,
                                    hief_2000$countryCode,
                                    fdi_2000$Country.Code,
                                    security_2000$cabb,
                                    exclusion_2000$cname))
num_final2000 <- length(list_2000)

pts_2000 %<>% filter(WordBank_Code_A %in% list_2000)
hief_2000 %<>% filter(countryCode %in% list_2000)
hief_2000 <- hief_2000[-c(28,29,35,36),] %>% select(EFindex)
fdi_2000 %<>% filter(Country.Code %in% list_2000) %>% select("X2000..YR2000.")
security_2000 %<>% filter(cabb %in% list_2000) %>% select(effective_number)
exclusion_2000 %<>% filter(cname %in% list_2000) %>% select(rexclpop)

data_2000 <- cbind(pts_2000, hief_2000, fdi_2000, security_2000, exclusion_2000)
colnames(data_2000) <- c("CountryCode", "Repression", "HIEF", "FDI_Inflows_Perc_GDP","Security_Agencies","Excluded_Population")
data_2000 %<>% mutate(Excluded_Population_Perc = as.numeric(Excluded_Population)) %>% select(-"Excluded_Population")
data_2000$FDI_Inflows_Perc_GDP <- as.numeric(data_2000$FDI_Inflows_Perc_GDP)



# Check for linearity in the logit for continuous predictors, 1990
logit.plot(data_1990, "HIEF", "Repression")
logit.plot(data_1990, "FDI_Inflows_Perc_GDP", "Repression")
logit.plot(data_1990, "Security_Agencies", "Repression")
logit.plot(data_1990, "Excluded_Population_Perc", "Repression")



# Linear regression models for 1990
HIEF.1990 <- glm(Repression ~ HIEF, data=data_1990, family=binomial)
FDI.1990 <- glm(Repression ~ FDI_Inflows_Perc_GDP, data=data_1990, family=binomial)
Security.1990 <- glm(Repression ~ Security_Agencies, data=data_1990, family=binomial)
Excluded.1990 <- glm(Repression ~ Excluded_Population_Perc, data=data_1990, family=binomial)
glm.1990 <- glm(Repression ~ HIEF + FDI_Inflows_Perc_GDP + Security_Agencies + Excluded_Population_Perc,
                data=data_1990, family=binomial)

summary(HIEF.1990)
summary(FDI.1990)
summary(Security.1990)
summary(Excluded.1990)
summary(glm.1990)

extract_eq(glm.1990, wrap=T) # Convert to usable text format via https://quicklatex.com/
           
stargazer_1990_raw <- stargazer(HIEF.1990, FDI.1990, Security.1990, Excluded.1990, glm.1990,
                            title="1990: Logistic Regression Results for 36 Autocracies", align=T,
                            covariate.labels = c("Ethnic Fractionalization Index", "FDI Inflows (% GDP)",
                                                 "Regime Security Agencies","Excluded Population (%)"),
                            dep.var.labels = "Logit of Political Terror Occurrence",
                            type="html", out="Stargazer_1990_Logit.htm")

stargazer_1990 <- stargazer(HIEF.1990, FDI.1990, Security.1990, Excluded.1990, glm.1990,
                                title="1990: Logistic Regression Results for 36 Autocracies", align=T,
                                covariate.labels = c("Ethnic Fractionalization Index", "FDI Inflows (% GDP)",
                                                     "Regime Security Agencies","Excluded Population (%)"),
                                dep.var.labels = "Odds of Political Terror Occurrence",
                                type="html", out="Stargazer_1990_Odds.htm", apply.coef=exp)



# Check for linearity in the logit for continuous predictors, 2000
logit.plot(data_2000, "HIEF", "Repression")
logit.plot(data_2000, "FDI_Inflows_Perc_GDP", "Repression")
logit.plot(data_2000, "Security_Agencies", "Repression")
logit.plot(data_2000, "Excluded_Population_Perc", "Repression")


# Linear regression models for 2000
HIEF.2000 <- glm(Repression ~ HIEF, data=data_2000, family=binomial)
FDI.2000 <- glm(Repression ~ FDI_Inflows_Perc_GDP, data=data_2000, family=binomial)
Security.2000 <- glm(Repression ~ Security_Agencies, data=data_2000, family=binomial)
Excluded.2000 <- glm(Repression ~ Excluded_Population_Perc, data=data_2000, family=binomial)
glm.2000 <- glm(Repression ~ HIEF + FDI_Inflows_Perc_GDP + Security_Agencies + Excluded_Population_Perc,
                data=data_2000, family=binomial)

summary(HIEF.2000)
summary(FDI.2000)
summary(Security.2000)
summary(Excluded.2000)
summary(glm.2000)

extract_eq(glm.2000, wrap=T)

stargazer_2000_raw <- stargazer(HIEF.2000, FDI.2000, Security.2000, Excluded.2000, glm.2000,
                            title="2000: Logistic Regression Results for 38 Autocracies", align=T,
                            covariate.labels = c("Ethnic Fractionalization Index", "FDI Inflows (% GDP)",
                                                 "Regime Security Agencies","Excluded Population (%)"),
                            dep.var.labels = "Logit of Political Terror Occurrence",
                            type="html", out="Stargazer_2000_Logit.htm")

stargazer_2000 <- stargazer(HIEF.2000, FDI.2000, Security.2000, Excluded.2000, glm.2000,
                                title="2000: Logistic Regression Results for 38 Autocracies", align=T,
                                covariate.labels = c("Ethnic Fractionalization Index", "FDI Inflows (% GDP)",
                                                     "Regime Security Agencies","Excluded Population (%)"),
                                dep.var.labels = "Odds of Political Terror Occurrence",
                                type="html", out="Stargazer_2000_Odds.htm", apply.coef=exp)



