# Script to model the relationship between threat impact and population growth rates 

# libraries
library(readr)
library(sf)
library(ggplot2)
library(lme4)
library(nlme)
library(viridis)
sf_use_s2(F)

## load the data

growthrates <- read_csv("outputs/lpd_fw_growthrates.csv")

cumulative_impact_maps = readRDS("data/CumulativeThreatMaps.rds")
impact = st_as_sf(cumulative_impact_maps)

## Intersect the growth rates with the watersheds

growthrates_sf <- st_as_sf(growthrates, coords = c("Longitude", "Latitude"), crs = 4326)
growthrates_impact <- st_join(growthrates_sf, impact, join = st_within)

# divide mean by variability for variability test
growthrates_impact$mean_div_se = growthrates_impact$mean/(growthrates_impact$sd/sqrt(growthrates_impact$n_datapoints))
growthrates_impact$mean_div_sd = growthrates_impact$mean/(growthrates_impact$sd)


# build models -----------------------------------------------------------------

# prepare data for modelling
dat <- as.data.frame(growthrates_impact)
#names(dat)
colnames(dat)[19] <- "Cumulative_Impact"

# keep finite values on;y
dat <- dat[
  is.finite(dat$mean_div_sd) &
    is.finite(dat$Cumulative_Impact),]

# remove NA rows
dat<-dat[complete.cases(dat$mean_div_sd),]

# model the mean with std error
m2 <- lme(scale(mean_div_sd, center = TRUE) ~  Cumulative_Impact
          , random = ~ 1 | Class,
          weights = varFixed(~Cumulative_Impact),
          method = "ML",
          data = dat)

summary(m2)
