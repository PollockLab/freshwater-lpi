# Script to calculate growth rates of the populations filtered in filter-lpd.R

# first time running? set up ---------------------------------------------------

# load libraries
library(tidyverse)
library(rlpi)

# load filtered LPD data
lpd = read.csv(file = "data/LivingPlanetIndex_2024_PublicData/LPD_2024_public.csv")

# load ids of populations of species that are in the study
popids = readRDS("data/lpd-popids.rds")

# get index of populations to include
index = which(lpd$ID %in% popids)

# Step 1. create an infile
# (basically a text file telling the LPI package what to run)
rlpi::create_infile(pop_data_source = lpd,
                    index_vector = index,
                    name = "freshwater-instudy",
                    start_col_name = "X1950",
                    end_col_name = "X2020")

# Step 2. Format dataset to run with rlpi
lpd_r = rlpi::convert_to_rows(lpd,
                      start_col_name = "X1950",
                      end_col_name = "X2020")
write.table(lpd_r, "freshwater-instudy.txt")

# Step 3. Run rlpi

lpi <- rlpi::LPIMain("freshwater-instudy_infile.txt")

# Remove NAs (trailing years with no data)
lpi <- lpi[complete.cases(lpi), ]

# This produces a simple plot, but we can use ggplot_lpi to produce a nicer version
rlpi::ggplot_lpi(lpi, ylims=c(0, 2))
ggsave("figures/lpi-spinstudy-global.png")

# Step 4. Inspect the growth rates

# annual growth rates
gr = read.csv("freshwater-instudy_pops_PopLambda.txt")
gr2 = read.csv("freshwater-instudy_pops_Minmax.txt")

# calculate time series length
# note: not all time series are annually updated, 
# so this doesn't mean there are this many data points
gr2$duration = gr2$max_year - gr2$min_year

# pivot to longer
gr_l = pivot_longer(gr,cols = -c(population_id),
                    names_to = "year", 
                    values_to = "lambda") 

# summarise growth rates per population
gr_pop = gr_l |>
  # remove NA years
  na.omit() |>
  # group by population
  group_by(population_id) |>
  # remove year 1 (defaults to 1 as a baseline for the LPI calculation
  # because it's not a real growth rate)
  slice(2:n()) |>
  # summarise growth rates per pop
  summarise(
    "n_datapoints" = n(),
    "mean" = mean(lambda, na.rm = T),
    "median" = median(lambda, na.rm = T),
    "min" = min(lambda, na.rm = T),
    "max" = max(lambda, na.rm = T),
    "sd" = sd(lambda, na.rm = T)
  )

## append the species names to this table

# subset LPD to the rows and columns we need
lpd_subset = lpd[index,] |>
  select(c(ID, Binomial, Class, Latitude, Longitude, FW_realm, FW_biome, Units))

# join!
df = left_join(lpd_subset, gr_pop, by = c("ID" = "population_id"))
df = left_join(df, gr2, by = "ID")
saveRDS(df, "outputs/lpd_fw_growthrates.rds")
write.csv(df, "outputs/lpd_fw_growthrates.csv", row.names = FALSE)
