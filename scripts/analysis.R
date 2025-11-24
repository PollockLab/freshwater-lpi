
library(readr)
library(sf)
library(ggplot2)
library(lme4)
library(viridis)
sf_use_s2(F)


## load the data

growthrates <- read_csv("outputs/lpd_fw_growthrates.csv")

load("D:/Users/cbraba7/OneDrive - McGill University/Christophe - IversenLab_Group/Freshwater Threat Maps/R/For publication/cumulative_impact_maps.RData")

impact = st_as_sf(cumulative_impact_maps)


## Intersect the growthrates with the watersheds


growthrates_sf <- st_as_sf(growthrates, coords = c("Longitude", "Latitude"), crs = 4326)

growthrates_impact <- st_join(growthrates_sf, impact, join = st_within)
names(growthrates_impact)

## plot relation to LPI

a = ggplot()+
  geom_sf(data=impact, aes(fill=`Cumulative Impact`, color=`Cumulative Impact`))+
  scale_fill_viridis(na.value = "grey") +
  scale_colour_viridis(na.value = "grey") +
  geom_sf(data=growthrates_sf, colour="red", fill="red")+
  theme_minimal()
ggsave(plot=a, filename="figures/map_test.png", height=14, width=14, units="in")



b=ggplot(data=growthrates_impact, aes(x=`Cumulative Impact`, y=mean))+
  geom_point(alpha=0.5)+
  stat_smooth(method="lm", colour="red",se=T)+
  theme_classic()
b
ggsave(plot=b, filename="figures/lpi_vs_impact_all.png")


reg = lm(mean ~ `Cumulative Impact`, data= growthrates_impact)
summary(reg)

summary(as.factor(growthrates_impact$Class))

p = ggplot()+
  stat_smooth(data=growthrates_impact[growthrates_impact$Class == "Reptilia", ],
              aes(x=`Cumulative Impact Reptile`, y=mean), method="lm", color="orange", se=T) +
  stat_smooth(data=growthrates_impact[growthrates_impact$Class == "Amphibia", ],
              aes(x=`Cumulative Impact Amphibian`, y=mean), method="lm", color="red", se=T)+
  stat_smooth(data=growthrates_impact[growthrates_impact$Class == "Actinopteri", ],
              aes(x=`Cumulative Impact Fish`, y=mean), method="lm", color="blue", se=T) +
  stat_smooth(data=growthrates_impact[growthrates_impact$Class == "Mammalia", ],
              aes(x=`Cumulative Impact Mammal`, y=mean), method="lm", color="green", se=T) +
  labs(
    x = "Cumulative Impact",
    y = "Mean Population Growth Rate") +
  theme_classic()
p

ggsave(plot=p, filename="figures/lpi_vs_impact_separate.png")

