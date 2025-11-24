# Script to filter the LPD to the species in Christophe's model
# and map them

## Set-up ----------------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

# set ggplot theme
theme_set(theme_void())

## Load data -------------------------------------------------------------------

# load species list
sp = read.csv("data/sp_list_model.csv", row.names = 1)

# load Living Planet Database
lpd = read.csv("data/LivingPlanetIndex_2024_PublicData/LPD_2024_public.csv")


## Filtering -------------------------------------------------------------------

# filter to geo-located freshwater populations
lpd_filter = lpd |>
  # filter to specific locations 
  filter(Specific_location == 1) |>
  # filter to freshwater realm
  filter(System == "Freshwater")


# Map everything ---------------------------------------------------------------

# get base map
world.map = geodata::world() |> st_as_sf()

# make pts layer
lpd_filter.pts = sf::st_as_sf(lpd_filter, 
                              coords = c("Longitude", "Latitude"),
                              crs = "epsg:4326")
# map the points on the global map
ggplot() +
  geom_sf(data = world.map, col = "grey90", fill = "grey90") +
  geom_sf(data = lpd_filter.pts, aes(fill = Class), pch = 21, linewidth = .01, size = 2) +
  colorspace::scale_fill_discrete_qualitative(palette = "Dynamic") +
  labs(caption = paste0("Full freshwater LPI Database:\n", 
                        nrow(lpd_filter.pts)," populations of ", 
                        length(unique(lpd_filter.pts$Binomial)), " species")) +
  theme(legend.position = "right")
ggsave("figures/worldmap_freshwater_all.png", width = 13, height = 8)


# Filter by species list -------------------------------------------------------

# change underscores to spaces
lpd_filter.pts$sci_name = gsub("_", " ", lpd_filter.pts$Binomial)

# how many species are in the LPD?  
# 600/17505 - note: LPD won't have Odonata since only verts are included)
which(sp$sci_name %in% lpd_filter.pts$sci_name)

# filter to species in the list
lpd_sp = lpd_filter.pts |>
  filter(sci_name %in% sp$sci_name)

# map the points on the global map
ggplot() +
  geom_sf(data = world.map, col = "grey90", fill = "grey90") +
  geom_sf(data = lpd_sp, aes(fill = Class), pch = 21, linewidth = .01, size = 2) +
  colorspace::scale_fill_discrete_qualitative(palette = "Dynamic") +
  labs(caption = paste0("Filtered LPI Database:\n", nrow(lpd_sp),
                        " populations of ", length(unique(lpd_sp$sci_name)),
                        " species")) +
  theme(legend.position = "right")
ggsave("figures/worldmap_freshwater_matchedspecies.png", width = 13, height = 8)

