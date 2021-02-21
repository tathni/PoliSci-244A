## Tejas Athni
## PoliSci 244A, Response Paper 2

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(tidyr)
library(epitools)
library(magrittr)
library(stargazer)
library(equatiomatic)
library(gridExtra)
library(grid)
setwd("E:/SynologyDrive/Tejas_Server/! Stanford University/! Year 3 - Quarter 2/PoliSci 244A/Response Papers/L13 - Political Culture/")


# Load in datasets
world_regions <- read.csv("allCountries.csv") %>%
  select(name, alpha.3, region)

press_freedom <- read.csv("PressFreedom.csv") %>%
  select(ISO, EN_country, Score.2019)
colnames(press_freedom) <- c("ISO","Country","PressFreedom")

gov_effectiveness <- read.csv("GovEffectiveness.csv") %>%
  select(X, X.1, X2019) %>% .[-1,]
colnames(gov_effectiveness) <- c("Country","Code","GovEffectiveness")
gov_effectiveness %<>% .[-c(7,43,121,143,174),]



# Filter to those datasets that contain data across both sets, and create consolidated df
list_countries <- Reduce(intersect, list(world_regions$alpha.3,
                                         press_freedom$ISO,
                                         gov_effectiveness$Code))


world_regions %<>% filter(alpha.3 %in% list_countries) %>%
  arrange(alpha.3, desc(alpha.3))

press_freedom %<>% filter(ISO %in% list_countries) %>%
  arrange(ISO, desc(ISO))

gov_effectiveness %<>% filter(Code %in% list_countries) %>%
  arrange(Code, desc(Code))

df <- cbind(world_regions, press_freedom$PressFreedom, gov_effectiveness$GovEffectiveness)
colnames(df) <- c("Country","CountryCode","Region","PressFreedom","GovEffectiveness")



# Linear regression with univariate predictor
lm.fit <- lm(GovEffectiveness ~ PressFreedom, data=df)
summary(lm.fit)

stargazer <- stargazer(lm.fit,
                        title="2019: Linear Regression Results for 172 Countries", align=T,
                        covariate.labels = c("Press Freedom Index"),
                        dep.var.labels = "Government Effectiveness Perception Score",
                        type="html", out="Stargazer_Press.htm")


extract_eq(lm.fit, wrap=T) # Convert to usable text format via https://quicklatex.com/



# Prep data for odds ratios contingency tables
df_binary <- df
df_binary$GovEffectiveness <- ifelse(df_binary$GovEffectiveness > 0, "GE+", "GE-")
df_binary$PressFreedom <- ifelse(df_binary$PressFreedom > 35, "PFD+", "PFD-")



# Odds ratios stratified by continent
allCountries <- table(df_binary$PressFreedom, df_binary$GovEffectiveness)
allCountries <- allCountries[2:1, 2:1]
allCountries
OR_allCountries <- oddsratio.fisher(allCountries)


df_Europe <- df_binary %>% filter(Region == "Europe")
Europe <- table(df_Europe$PressFreedom, df_Europe$GovEffectiveness)
Europe <- Europe[2:1, 2:1]
Europe
OR_Europe <- oddsratio.fisher(Europe)


df_Asia <- df_binary %>% filter(Region == "Asia")
Asia <- table(df_Asia$PressFreedom, df_Asia$GovEffectiveness)
Asia <- Asia[2:1, 2:1]
Asia
OR_Asia <- oddsratio.fisher(Asia)


df_Africa <- df_binary %>% filter(Region == "Africa")
Africa <- table(df_Africa$PressFreedom, df_Africa$GovEffectiveness)
Africa <- Africa[2:1, 2:1]
Africa
OR_Africa <- oddsratio.fisher(Africa)


df_Americas <- df_binary %>% filter(Region == "Americas")
Americas <- table(df_Americas$PressFreedom, df_Americas$GovEffectiveness)
Americas <- Americas[2:1, 2:1]
Americas
OR_Americas <- oddsratio.fisher(Americas)




# Odds ratios stratified by regime type
regimes <- read_excel("Data_Set.xls") %>%   
  select(scode, year, regime_r) %>%
  filter(year == 2012) # Assume that regime status in 2012, the most recent year in dataset, is same as 2019


list_regimes <- Reduce(intersect, list(world_regions$alpha.3,
                                       press_freedom$ISO,
                                       gov_effectiveness$Code,
                                       regimes$scode))


world_regions %<>% filter(alpha.3 %in% list_regimes) %>%
  arrange(alpha.3, desc(alpha.3))

press_freedom %<>% filter(ISO %in% list_regimes) %>%
  arrange(ISO, desc(ISO))

gov_effectiveness %<>% filter(Code %in% list_regimes) %>%
  arrange(Code, desc(Code))

regimes %<>% filter(scode %in% list_regimes) %>%
  arrange(scode, desc(scode))

df <- cbind(world_regions, regimes$regime_r, press_freedom$PressFreedom, gov_effectiveness$GovEffectiveness)
colnames(df) <- c("Country","CountryCode","Region","RegimeType","PressFreedom","GovEffectiveness")

df_binary <- df
df_binary$GovEffectiveness <- ifelse(df_binary$GovEffectiveness > 0, "GE+", "GE-")
df_binary$PressFreedom <- ifelse(df_binary$PressFreedom > 35, "PFD+", "PFD-")




allRegimes <- table(df_binary$PressFreedom, df_binary$GovEffectiveness)
allRegimes <- allRegimes[2:1, 2:1]
allRegimes
OR_regime_allRegimes <- oddsratio.fisher(allRegimes)


df_Democracies <- df_binary %>% filter(RegimeType == "Democracy")
Democracies <- table(df_Democracies$PressFreedom, df_Democracies$GovEffectiveness)
Democracies <- Democracies[2:1, 2:1]
Democracies
OR_regime_Democracies <- oddsratio.fisher(Democracies)


df_Autocracies <- df_binary %>% filter(RegimeType %in% c("Single Party","Multiparty","Military","Monarch","NA"))
Autocracies <- table(df_Autocracies$PressFreedom, df_Autocracies$GovEffectiveness)
Autocracies <- Autocracies[2:1, 2:1]
Autocracies
OR_regime_Autocracies <- oddsratio.fisher(Autocracies)




# Forest plot of ORs by continent

boxLabels = c("All Countries", "Africa", "Americas", "Asia", "Europe")
yAxis = length(boxLabels):1

forest <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(1.05, 1.031, 0.3411, 1.366, 2.138),
  boxCILow = c(0.545, 0.296, 0.0264, 0.378, 0.463),
  boxCIHigh = c(2.023, 3.608, 2.7252, 5.021, 10.840)
)

p <- ggplot(forest, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,11,1) ) +
  ylab("") +
  xlab("Odds Ratio") +
  ggtitle("Low Press Freedom and Gov't Effectiveness Perception\n Stratified by Continent")



# Forest plot of ORs by regime type

boxLabels = c("All Regimes", "Democracies", "Autocracies")
yAxis = length(boxLabels):1

forest <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(0.338, 0.574, 0.24454),
  boxCILow = c(0.110, 0.077, 0.00809),
  boxCIHigh = c(0.974, 3.490, 19.60481)
)

p <- ggplot(forest, aes(x = boxOdds, y = yAxis))
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxis, labels = boxLabels) +
  scale_x_continuous(breaks = seq(0,20,1) ) +
  ylab("") +
  xlab("Odds Ratio") +
  ggtitle("Low Press Freedom and Gov't Effectiveness Perception\n Stratified by Regime Type")



# Contingency tables
grid_allCountries <- tableGrob(allCountries)
grid_Europe <- tableGrob(Europe)
grid_Asia <- tableGrob(Asia)
grid_Africa <- tableGrob(Africa)
grid_Americas <- tableGrob(Americas)

grid_allRegimes <- tableGrob(allRegimes)
grid_Democracies <- tableGrob(Democracies)
grid_Autocracies <- tableGrob(Autocracies)

grid.arrange(grid_allCountries, grid_Europe, grid_Asia, grid_Africa, grid_Americas, grid_allRegimes, grid_Democracies, grid_Autocracies)



