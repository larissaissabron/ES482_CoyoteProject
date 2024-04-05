# #0. Helpful websites ----------------------------------------------------

# Data Transformation in R (R for data science 2e): https://r4ds.hadley.nz/data-transform

# Marissa's tutorial website: https://marissadyck.github.io/ES482-R-labs.github.io/index.html

# 1. Getting everything set up --------------------------------------------

# 1a. Install/load packages
library(tidyverse)
library(ggpubr)
library(PerformanceAnalytics)
library(car)
library(gridExtra)
library(MASS)
library(MuMIn)

# 1b. Confirm working directory correct
print(getwd())

# 1c. Import .csv files

# Human  (HFI, Human Factors Indices) and natural (landcover types) variables at varying buffer distances
covariates2 <- read_csv("data/raw/OSM_2022_covariates2.csv")

# Proportional detections: number of months present and absent out of total number of months (15)? camera operated. Use a negative binomial model ***don't use without talking with Marissa and Jake
proportional_detections2 <- read_csv("data/raw/OSM_2022_proportional_detections2.csv")

## What is up with this proportional detection file?
proportional_detections2 <- proportional_detections2 %>% 
  mutate(
    moose_total = moose + absent_moose,
    coyote_total = coyote + absent_coyote, 
    deer_total = `white-tailed_deer` + `absent_white-tailed_deer`,
    hare_total = snowshoe_hare + absent_snowshoe_hare,
    wolf_total = grey_wolf + absent_grey_wolf
  ) %>% 
  relocate(moose_total, moose, absent_moose, coyote_total, coyote, absent_coyote, deer_total, `white-tailed_deer`, `absent_white-tailed_deer`, hare_total, snowshoe_hare, absent_snowshoe_hare, wolf_total, grey_wolf, absent_grey_wolf)

### Removed 7 rows

# Total detections: total independant detections (events > 30 min apart). Model with GLM. 
total_detections <- read_csv("/Users/larissabron/Documents/BSc/23_24/Spring/BorealMammals/Project/RProject/data/raw/OSM_2022_total_detections.csv")  %>% 
  setNames(
    names(.) %>% 
      tolower() %>% 
      gsub(" ", "_", .)) # also get some underscores in there, dang

# 1d. In preparation for visualization, put datasets of interest together (covariates, total detections, proportional) and filter out what's obviously not going to serve you (covariates don't need every human feature, natural feature, or buffer size)

# Extract project-relevant data from total detections
project_tot_det <- total_detections %>%
  dplyr::select(site, coyote, moose, snowshoe_hare, `white-tailed_deer`, grey_wolf, black_bear, red_squirrel, lynx, red_fox, caribou, owl, cougar) %>% 
  dplyr::rename(coy_tot_det = coyote,
         moose_tot_det = moose,
         hare_tot_det = snowshoe_hare,
         deer_tot_det = `white-tailed_deer`,
         wolf_tot_det = grey_wolf,
         bear_tot_det = black_bear,
         squirrel_tot_det = red_squirrel,
         lynx_tot_det = lynx,
         fox_tot_det = red_fox,
         caribou_tot_det = caribou,
         owl_tot_det = owl,
         cougar_tot_det = cougar)

# Extract project-relevant data from proportion detections. 
project_prop_det <- proportional_detections2 %>% 
  dplyr::select(site, coyote, absent_coyote, grey_wolf, absent_grey_wolf, moose, absent_moose, `white-tailed_deer`, `absent_white-tailed_deer`, snowshoe_hare, absent_snowshoe_hare, black_bear, absent_black_bear, lynx, absent_lynx, red_fox, absent_red_fox, caribou, absent_caribou, moose_total) %>% # adding moose total just to filter anything with less than 12 camera months (from above)
  dplyr::rename(coy_prop_pres = coyote,
         coy_prop_abs = absent_coyote,
         wolf_prop_pres = grey_wolf,
         wolf_prop_abs = absent_grey_wolf,
         moose_prop_pres = moose,
         moose_prop_abs = absent_moose,
         deer_prop_pres = 'white-tailed_deer',
         deer_prop_abs = 'absent_white-tailed_deer',
         hare_prop_pres = snowshoe_hare,
         hare_prop_abs = absent_snowshoe_hare,
         bear_prop_pres = black_bear,
         bear_prop_abs = absent_black_bear,
         lynx_prop_pres = lynx,
         lynx_prop_abs = absent_lynx,
         fox_prop_pres = red_fox,
         fox_prop_abs = absent_red_fox,
         caribou_prop_pres = caribou,
         caribou_prop_abs = absent_caribou) 
  
# Leave presence alone because its very coarse scaled

# Create tibble of all covariates and relevant coyote data
project_data <- covariates2 %>% 
  filter(buff_dist == 1000) %>% 
  dplyr::select(site, array, camera, pipeline, transmission_line, road_gravel_1l, road_gravel_2l, road_paved_undiv_2l, road_paved_undiv_1l, rlwy_sgl_track, road_paved_1l, road_winter, trail, conventional_seismic, low_impact_seismic, vegetated_edge_roads, vegetated_edge_railways, truck_trail, road_unclassified, road_unimproved, lc_class20, lc_class50, lc_class110, lc_class210, lc_class220, lc_class230) %>% #all potential linear features and natural features
  right_join(project_prop_det,
             by = 'site') %>% 
  right_join(project_tot_det,
             by = 'site') %>% 
  arrange(site) %>% 
  relocate(site, array, camera) %>% 
  rename(water = lc_class20,
         shrub = lc_class50,
         grass = lc_class110,
         broadleaf = lc_class220,
         mixed_forest = lc_class230,
         conifer = lc_class210) %>% 
  na.omit() %>% # Further down the line you are going to appreciate removing rows with NA values
  filter(moose_total > 11) %>% 
  dplyr::select(-moose_total) %>% # okay now we can get rid of moose_total because it's confusing because its not the total detections its the total absence and presence from the proportional .csv
  mutate(array = as.factor(array)) # curious how array as a factor plays out for later

str(project_data)

# Save as a new .csv
write.csv(project_data, "/Users/larissabron/Documents/BSc/23_24/Spring/BorealMammals/Project/RProject/data/processed/project_data.csv", row.names = FALSE)

# 2. Visualizing Data -----------------------------------------------------


# Visualizing how coyote data varies with different covariate sets (wide feats, narrow feats, natural feats, predator and prey) separated by land use area. 
## Now inset the distribution of each feature by array - second plot per covariate.
### Additional inset of distribution of each feature looking at the landscape - third plot per covariate. 

# 2a. Plotting coyote total against each wide feature
# Pipeline
plot_1 <- project_data %>% 
  ggplot(mapping = aes(x = pipeline, y = coy_tot_det, color = array)) +
  geom_point() 

plot_44 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = pipeline, color = array)) +
  geom_point()

plot_79 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = pipeline, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

#check it out
print(plot_79)

# Transmission line
plot_2 <- project_data %>% 
  ggplot(mapping = aes(x = transmission_line, y = coy_tot_det, color = array)) +
  geom_point() 

plot_45 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = transmission_line, color = array)) +
  geom_point()

plot_80 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = transmission_line, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

print(plot_80)

# Road gravel 1l
plot_3 <- project_data %>% 
  ggplot(mapping = aes(x = road_gravel_1l, y = coy_tot_det, color = array)) +
  geom_point() 

plot_46 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = road_gravel_1l, color = array)) +
  geom_point()

plot_81 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = road_gravel_1l, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

print(plot_81)

# Road gravel 2l
plot_4 <- project_data %>% 
  ggplot(mapping = aes(x = road_gravel_2l, y = coy_tot_det, color = array)) +
  geom_point() 

plot_47 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = road_gravel_2l, color = array)) +
  geom_point()

plot_82 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = road_gravel_2l, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# Road paved undiv 2l
plot_5 <- project_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_2l, y = coy_tot_det, color = array)) +
  geom_point() 

plot_48 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = road_paved_undiv_2l, color = array)) +
  geom_point()

plot_83 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = road_paved_undiv_2l, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

print(plot_48)

# rlwy sgl track 
plot_6 <- project_data %>% 
  ggplot(mapping = aes(x = rlwy_sgl_track, y = coy_tot_det, color = array)) +
  geom_point() 

plot_49 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = rlwy_sgl_track, color = array)) +
  geom_point()

plot_84 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = rlwy_sgl_track, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

print(plot_49)

# road paved 1l
plot_7 <- project_data %>% 
  ggplot(mapping = aes(x = road_paved_1l, y = coy_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

plot_50 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = road_paved_1l, color = array)) +
  geom_point()

plot_85 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = road_paved_1l, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

print(plot_50)

# road paved undiv 1l
plot_8 <- project_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_1l, y = coy_tot_det, color = array)) +
  geom_point() 

plot_51 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = road_paved_undiv_1l, color = array)) +
  geom_point()

plot_86 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = road_paved_undiv_1l, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

print(plot_51)

# road winter
plot_9 <- project_data %>% 
  ggplot(mapping = aes(x = road_winter, y = coy_tot_det, color = array)) +
  geom_point() 

plot_52 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = road_winter, color = array)) +
  geom_point()

plot_87 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = road_winter, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

print(plot_87)

figure_1 <- ggarrange(plot_1,
                      plot_2,
                      plot_3,
                      plot_4,
                      plot_5,
                      plot_6,
                      plot_7,
                      plot_8,
                      plot_9)

figure_6 <- ggarrange(plot_44,
                      plot_45,
                      plot_46,
                      plot_47,
                      plot_48,
                      plot_49,
                      plot_50,
                      plot_51,
                      plot_52)

figure_10 <- ggarrange(plot_79,
                      plot_80,
                      plot_81,
                      plot_82,
                      plot_83,
                      plot_84,
                      plot_85,
                      plot_86,
                      plot_87)

print(figure_10)

# 2b. Coyote total vs. natural feats 
## Now inset the distribution of each feature by array.

# water
plot_19 <- project_data %>% 
  ggplot(mapping = aes(x = water, y = coy_tot_det, color = array)) +
  geom_point() 

plot_53 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = water, color = array)) +
  geom_point()

plot_88 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = water, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

plot_88

# Shrub
plot_20 <- project_data %>% 
  ggplot(mapping = aes(x = shrub, y = coy_tot_det, color = array)) +
  geom_point()

plot_54 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = shrub, color = array)) +
  geom_point()

plot_89 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = shrub, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# grass 
plot_21 <- project_data %>% 
  ggplot(mapping = aes(x = grass, y = coy_tot_det, color = array)) +
  geom_point()

plot_55 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = grass, color = array)) +
  geom_point()

plot_90 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = grass, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# conifer
plot_22 <- project_data %>% 
  ggplot(mapping = aes(x = conifer, y = coy_tot_det, color = array)) +
  geom_point() 

plot_56 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = grass, color = array)) +
  geom_point()

plot_91 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = conifer, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# broadleaf
plot_23 <- project_data %>% 
  ggplot(mapping = aes(x = broadleaf, y = coy_tot_det, color = array)) +
  geom_point() 

plot_57 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = broadleaf, color = array)) +
  geom_point()

plot_92 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = broadleaf, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# mixed forest
plot_24 <- project_data %>% 
  ggplot(mapping = aes(x = mixed_forest, y = coy_tot_det, color = array)) +
  geom_point() 

plot_58 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = mixed_forest, color = array)) +
  geom_point()

plot_93 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = mixed_forest, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

figure_3 <- ggarrange(plot_19,
                      plot_20,
                      plot_21,
                      plot_22,
                      plot_23,
                      plot_24)

figure_7 <- ggarrange(plot_53,
                      plot_54,
                      plot_55,
                      plot_56,
                      plot_57,
                      plot_58)

figure_11 <- ggarrange(plot_88,
                      plot_89,
                      plot_90,
                      plot_91,
                      plot_92,
                      plot_93)

# 2d. Coyote total vs. narrow features
## Now inset the distribution of each feature by array.

#trail
plot_25 <- project_data %>% 
  ggplot(mapping = aes(x = trail, y = coy_tot_det, color = array)) +
  geom_point() 

plot_59 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = trail, color = array)) +
  geom_point()

plot_94 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = trail, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# conventional seismic
plot_26 <- project_data %>% 
  ggplot(mapping = aes(x = conventional_seismic, y = coy_tot_det, color = array)) +
  geom_point()

plot_60 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = conventional_seismic, color = array)) +
  geom_point()

plot_95 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = conventional_seismic, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# low impact seismic
plot_27 <- project_data %>% 
  ggplot(mapping = aes(x = low_impact_seismic, y = coy_tot_det, color = array)) +
  geom_point()

plot_61 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = low_impact_seismic, color = array)) +
  geom_point()

plot_96 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = low_impact_seismic, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# vegetated edge roads
plot_28 <- project_data %>% 
  ggplot(mapping = aes(x = vegetated_edge_roads, y = coy_tot_det, color = array)) +
  geom_point() 

plot_62 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = vegetated_edge_roads, color = array)) +
  geom_point()

plot_97 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = vegetated_edge_roads, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# Vegetated edge railways
plot_29 <- project_data %>% 
  ggplot(mapping = aes(x = vegetated_edge_railways, y = coy_tot_det, color = array)) +
  geom_point() 

plot_63 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = vegetated_edge_railways, color = array)) +
  geom_point()

plot_98 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = vegetated_edge_railways, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# truck trail
plot_30 <- project_data %>% 
  ggplot(mapping = aes(x = truck_trail, y = coy_tot_det, color = array)) +
  geom_point() 

plot_64 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = truck_trail, color = array)) +
  geom_point()

plot_99 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = truck_trail, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

#road unclassified
plot_31 <- project_data %>% 
  ggplot(mapping = aes(x = road_unclassified, y = coy_tot_det, color = array)) +
  geom_point() 

plot_65 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = road_unclassified, color = array)) +
  geom_point()

plot_100 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = road_unclassified, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# road unimproved
plot_32 <- project_data %>% 
  ggplot(mapping = aes(x = road_unimproved, y = coy_tot_det, color = array)) +
  geom_point() 

plot_66 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = road_unimproved, color = array)) +
  geom_point()

plot_101 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = road_unimproved, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

figure_4 <- ggarrange(plot_25,
                      plot_26,
                      plot_27,
                      plot_28,
                      plot_29,
                      plot_30,
                      plot_31,
                      plot_32)

figure_8 <- ggarrange(plot_59,
                      plot_60,
                      plot_61,
                      plot_62,
                      plot_63,
                      plot_64,
                      plot_65,
                      plot_66)

figure_12 <- ggarrange(plot_94,
                      plot_95,
                      plot_96,
                      plot_97,
                      plot_98,
                      plot_99,
                      plot_100,
                      plot_101)

figure_12

# 2e. Coyote total vs each mammal
## Now inset the distribution of each feature by array.

# Moose
plot_33 <- project_data %>% 
  ggplot(mapping = aes(x = moose_tot_det, y = coy_tot_det, color = array)) +
  geom_point() 

plot_67 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = moose_tot_det, color = array)) +
  geom_point()

plot_102 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = moose_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# Hare
plot_34 <- project_data %>% 
  ggplot(mapping = aes(x = hare_tot_det, y = coy_tot_det, color = array)) +
  geom_point()

plot_68 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = hare_tot_det, color = array)) +
  geom_point()

plot_103 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = hare_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# deer
plot_35 <- project_data %>% 
  ggplot(mapping = aes(x = deer_tot_det, y = coy_tot_det, color = array)) +
  geom_point()

plot_69 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = deer_tot_det, color = array)) +
  geom_point()

plot_104 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = deer_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# wolf
plot_36 <- project_data %>% 
  ggplot(mapping = aes(x = wolf_tot_det, y = coy_tot_det, color = array)) +
  geom_point() 

plot_70 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = wolf_tot_det, color = array)) +
  geom_point()

plot_105 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = wolf_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# bear 
plot_37 <- project_data %>% 
  ggplot(mapping = aes(x = bear_tot_det, y = coy_tot_det, color = array)) +
  geom_point() 

plot_71 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = bear_tot_det, color = array)) +
  geom_point()

plot_106 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = bear_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# squirrel
plot_38 <- project_data %>% 
  ggplot(mapping = aes(x = squirrel_tot_det, y = coy_tot_det, color = array)) +
  geom_point() 

plot_72 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = squirrel_tot_det, color = array)) +
  geom_point()

plot_107 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = squirrel_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))


# lynx
plot_39 <- project_data %>% 
  ggplot(mapping = aes(x = lynx_tot_det, y = coy_tot_det, color = array)) +
  geom_point() 

plot_73 <- project_data %>% 
  ggplot(mapping = aes(x = array, y =lynx_tot_det, color = array)) +
  geom_point()

plot_108 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = lynx_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# fox
plot_40 <- project_data %>% 
  ggplot(mapping = aes(x = fox_tot_det, y = coy_tot_det, color = array)) +
  geom_point() 

plot_74 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = fox_tot_det, color = array)) +
  geom_point()

plot_109 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = fox_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# caribou
plot_41 <- project_data %>% 
  ggplot(mapping = aes(x = caribou_tot_det, y = coy_tot_det, color = array)) +
  geom_point() 

plot_75 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = caribou_tot_det, color = array)) +
  geom_point()

plot_110 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = caribou_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# owl
plot_42 <- project_data %>% 
  ggplot(mapping = aes(x = owl_tot_det, y = caribou_tot_det, color = array)) +
  geom_point() 

plot_76 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = owl_tot_det, color = array)) +
  geom_point()

plot_111 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = owl_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# cougar
plot_43 <- project_data %>% 
  ggplot(mapping = aes(x = cougar_tot_det, y = coy_tot_det, color = array)) +
  geom_point() 

plot_77 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = cougar_tot_det, color = array)) +
  geom_point()

plot_112 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = cougar_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# coyote
plot_78 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = coy_tot_det, color = array)) +
  geom_point()

plot_113 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = coy_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

figure_5 <- ggarrange(plot_33,
                      plot_34,
                      plot_35,
                      plot_36,
                      plot_37,
                      plot_38,
                      plot_39,
                      plot_40,
                      plot_41,
                      plot_42,
                      plot_43)

figure_9 <- ggarrange(plot_67,
                      plot_68,
                      plot_69,
                      plot_70,
                      plot_71,
                      plot_72,
                      plot_73,
                      plot_74,
                      plot_75,
                      plot_76,
                      plot_77,
                      plot_78)

figure_13 <- ggarrange(plot_102,
                      plot_103,
                      plot_104,
                      plot_105,
                      plot_106,
                      plot_107,
                      plot_108,
                      plot_109,
                      plot_110,
                      plot_111,
                      plot_112,
                      plot_113)

figure_13

# Before April 3rd ---- old ---- From the figures above, it looks like the variables that could be of interest are: Wide - pipeline, road_gravel_1l; narrow - trail, conventional_seismic, vegetated_edge_roads, ; prey - moose, white-tailed_deer, hare; competitor - wolf. 

# After April 3rd ---- new ---- Features of interest based on seeing number of coyote  captures vs. each feature would be... Wide: pipeline, road_gravel_1l; Narrow: trail, conventional seismic, vegetated edge roads, road_unimproved; Natural: shrub, grass, conifer, broadleaf, mixed_forest, water (even though low, want to keep all natural for “world without humans” candidate model); Animals - Prey: moose, hare, deer; Competitor: wolf; - Bonus consideration due to associations (not added below yet): black bear, lynx, red squirrel

# April 5: I added in plotting each potential covariate against the array, showing the amount of each present as each camera separated by array. This may be a more full picture of whats going on instead of just "cherry picking" what works with coyote. 

#### Wide features: landscape unit, or array, 21 doesn't have much of anything for wide features, could be could to compare as the "baseline." Transmission line may be good to add, as well as road_gravel_2l - though maybe add it to road_gravel_1l?

### Natural features: 

### Overall: After visualizing, it could be good to get the median values for each covariate of interest at each landscape unit/array. 

# 3. Generalized linear models, Poisson for count data --------------------------------------------

# Anticipated models 
# H0: Null, coyotes ~ 

# H1: Coyotes like wide features, coyotes ~ pipeline + road_gravel_1l

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ pipeline + road_gravel_1l + pipeline:wolf_tot_det + road_gravel_1l:wolf_tot_det

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det

# H4: Coyotes like narrow features, coyote ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved

# H5: Coyote use of narrow features depends on presence of a competitor, coyote ~ trail + conventional_seismic + vegetated_edge_roads + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det + road_unimproved:wolf_tot_det

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + deer + hare + moose

# H7: Coyotes like both wide and narrow features, coyote ~ pipeline + road_gravel_1l + trail + conventional seismic + vegetated_edge_roads + road_unimproved

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + conifer + broadleaf + mixed_forest

# Okay, so are we violating any assumptions? Only test one before running models, rest of assumptions tested at the end.

# Assumption 1 - model variables are independent
chart.Correlation(project_data[c("pipeline", "road_gravel_1l", "trail", "conventional_seismic", "vegetated_edge_roads", "road_unimproved", "wolf_tot_det", "deer_tot_det", "moose_tot_det", "coy_tot_det", "hare_tot_det", "water", "shrub", "grass", "conifer", "broadleaf", "mixed_forest")], 
                  histogram = TRUE, 
                  method = "spearman",
                  text.scale = 2)
### Result: none of the covariates are highly correlated, aka r^2 < 0.7 for all


# GLM TIME https://marissadyck.github.io/ES482-R-labs.github.io/mo --------


# H0: Null
glm_H0 <- glm(coy_tot_det ~ 1,
              data = project_data,
              family = poisson)

summary(glm_H0)

# H1: Coyotes like wide features, coyotes ~ pipeline + road_gravel_1l
glm_H1 <- glm(coy_tot_det ~ pipeline + road_gravel_1l,
              data = project_data,
              family = poisson)

summary(glm_H1)

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ pipeline + road_gravel_1l + pipeline:wolf + road_gravel_1l:wolf
glm_H2 <- glm(coy_tot_det ~ pipeline + road_gravel_1l + pipeline:wolf_tot_det + road_gravel_1l:wolf_tot_det,
              data = project_data,
              family = poisson)

summary(glm_H2)

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det
glm_H3 <- glm(coy_tot_det ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det,
              data = project_data,
              family = poisson)

summary(glm_H3)

# H4: Coyotes like narrow features, coyote ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved 
glm_H4 <- glm(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved,
              data = project_data,
              family = poisson)

summary(glm_H4)

# H5: Coyote use of narrow features depends on presence of competitors, coyote ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det
glm_H5 <- glm(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det,
              data = project_data,
              family = poisson)

summary(glm_H5)

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + deer + hare + moose
glm_H6 <- glm(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + deer_tot_det + hare_tot_det + moose_tot_det,
              data = project_data,
              family = poisson)

summary(glm_H6)

# H7: Coyotes like both wide and narrow features, coyote ~ pipeline + road_gravel_1l + trail + conventional seismic + vegetated_edge_roads + road_unimproved 
glm_H7 <- glm(coy_tot_det ~ pipeline + road_gravel_1l + trail + conventional_seismic + vegetated_edge_roads + road_unimproved,
              data = project_data,
              family = poisson)

summary(glm_H7)

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + conifer + broadleaf + mixed_forest
glm_H8 <- glm(coy_tot_det ~ water + shrub + grass + conifer + broadleaf + mixed_forest,
              data = project_data,
              family = poisson)

summary(glm_H8)


# GLM assumptions continued  ----------------------------------------------


# Assumption 2 - homogeneity of variance using Levene's test
# Specify the variables you want to test
vars_to_test <- c("array", "pipeline", "road_gravel_1l", "trail", "conventional_seismic", "vegetated_edge_roads", "wolf_tot_det", "deer_tot_det", "moose_tot_det", "coy_tot_det", "hare_tot_det", "water", "shrub", "grass", "conifer", "broadleaf", "mixed_forest")

# Perform Levene's test for homogeneity of variances
levene_results <- leveneTest(as.formula(paste0("~", paste(vars_to_test, collapse = "+"))), data = project_data)

## I get an error that says "Levene's test is not appropriate for quantitative explanatory variables." I don't think that we have categorical variables or groups, so moving on

# Assumption 3 - Normality of residuals 

# Calculate chi square for residual deviance for each glm
# H0
summary(glm_H0)
# = residual deviance / degrees of freedom
1558.8/146
# = 10.67671, so overdispersed

# H1 
summary(glm_H1)
# = residual deviance / degrees of freedom
1102.4/144
# = 7.6556, so overdispersed 

# H2
summary(glm_H2)
# = residual deviance / degrees of freedom
1100.5/142
# = 7.75

# H3
summary(glm_H3)
# = residual deviance / degrees of freedom
913.23/141
# = 6.4768

# H4
summary(glm_H4)
# = residual deviance / degrees of freedom
1040.4/142
# = 7.3277

# H5
summary(glm_H5)
# = residual deviance / degrees of freedom
1021.9/139
# = 7.3518

# H6
summary(glm_H6)
# = residual deviance / degrees of freedom
798.85/139
# = 5.7471

# H7
summary(glm_H7)
# = residual deviance / degrees of freedom
833.3/140
# = 5.9521

# H8
summary(glm_H8)
# = residual deviance / degrees of freedom
910.66/140
# = 6.5047

# Recap: So yeah, everything is still overdispersed so redo the glm's as negative binomial model family (https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/count_data_and_overdispersion.html)


# Redoing all the glms but this time as negative binomial  ----------------

# H0: Null
glm_H0_2 <- glm.nb(coy_tot_det ~ 1,
              data = project_data,
              link = "log")

summary(glm_H0_2)
# = residual deviance / degrees of freedom
161.05/146
# = 1.1031

# H1: Coyotes like wide features, coyotes ~ pipeline + road_gravel_1l
glm_H1_2 <- glm.nb(coy_tot_det ~ pipeline + road_gravel_1l,
              data = project_data,
              link = "log")

summary(glm_H1_2)
# = residual deviance / degrees of freedom
162.52/144
# = 1.1286

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ pipeline + road_gravel_1l + pipeline:wolf + road_gravel_1l:wolf
glm_H2_2 <- glm.nb(coy_tot_det ~ pipeline + road_gravel_1l + pipeline:wolf_tot_det + road_gravel_1l:wolf_tot_det,
              data = project_data,
              link = "log")

summary(glm_H2_2)
# = residual deviance / degrees of freedom
162.52/142
# = 1.1445

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det
glm_H3_2 <- glm.nb(coy_tot_det ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det,
              data = project_data,
              link = "log")

summary(glm_H3_2)
# = residual deviance / degrees of freedom
164.58/141
# = 1.1672

# H4: Coyotes like narrow features, coyote ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved 
glm_H4_2 <- glm.nb(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved,
              data = project_data,
              link = "log")

summary(glm_H4_2)
# = residual deviance / degrees of freedom
162.82/142
# = 1.1466

# H5: Coyote use of narrow features depends on presence of competitors, coyote ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det
glm_H5_2 <- glm.nb(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det,
              data = project_data,
              link = "log")

summary(glm_H5_2)
# = residual deviance / degrees of freedom
162.99/139
# = 1.1726

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + deer + hare + moose
glm_H6_2 <- glm.nb(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + road_unimproved + deer_tot_det + hare_tot_det + moose_tot_det,
              data = project_data,
              link = "log")

summary(glm_H6_2)
# = residual deviance / degrees of freedom
166.02/139
# = 1.1944

# H7: Coyotes like both wide and narrow features, coyote ~ pipeline + road_gravel_1l + trail + conventional seismic + vegetated_edge_roads + road_unimproved 
glm_H7_2 <- glm.nb(coy_tot_det ~ pipeline + road_gravel_1l + trail + conventional_seismic + vegetated_edge_roads + road_unimproved,
              data = project_data,
              link = "log")

summary(glm_H7_2)
# = residual deviance / degrees of freedom
165.23/140
# = 1.1802

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + conifer + broadleaf + mixed_forest
glm_H8_2 <- glm.nb(coy_tot_det ~ water + shrub + grass + conifer + broadleaf + mixed_forest,
              data = project_data,
              link = "log")

summary(glm_H8_2)
# = residual deviance / degrees of freedom
164.45/140
# = 1.1746

# Assumption 4 - Influential observations. **Swing back to this after getting things rolling.

# Interpreting GLM output -------------------------------------------------

# Plot function (you need to press enter in the console to go through the different plots)

# A. Residuals vs fitted: [ChatGPT] Residuals vs. Fitted Values Plot: This plot shows the residuals (deviance residuals) on the y-axis against the fitted values (predicted values) on the x-axis. It helps assess the linearity assumption. Ideally, the residuals should be randomly scattered around the horizontal line at zero, indicating that the relationship between the predictors and the response variable is adequately captured by the model

# B. Quantile-Quantile Plot: [ChatGPT] This plot compares the quantiles of the deviance residuals to the quantiles of a theoretical normal distribution. It helps assess the assumption of normality for the residuals. If the points lie approximately along the diagonal line, it suggests that the residuals are normally distributed.

# C. Scale-Location Plot (Squared Residuals vs. Fitted Values): [ChatGPT] This plot shows the square root of the absolute deviance residuals against the fitted values. It helps assess the assumption of homoscedasticity (constant variance). Ideally, the points should form a horizontal band around the horizontal line, indicating constant variance across the range of fitted values

# D. Residuals vs. Leverage: [ChatGPT] This plot shows the leverage values (measures of the influence of observations on the model fit) against the deviance residuals.It helps identify influential observations that have high leverage and high residuals. These points may have a significant impact on the model estimates. This also includes Cook's distance (hashed line) -     Cook's distance measures the influence of each observation on the model estimates. Points with high Cook's distance are potential outliers or influential observations that may have a disproportionate impact on the model's parameters.

plot(glm_H0_2)
plot(glm_H1_2)
plot(glm_H2_2)
plot(glm_H3_2)
plot(glm_H4_2)
plot(glm_H5_2)
plot(glm_H6_2)
plot(glm_H7_2)
plot(glm_H8_2)

# Plotting predicted vs. response

# H0
# Obtain predicted values
glm_H0_2_pred <- predict(glm_H0_2, type = "response")

# Create the plot

glm_H0_2_pred_plot <- plot(glm_H0_2_pred, project_data$coy_tot_det,
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Predicted vs. Observed Plot for H0",
     pch = 16,  # Use solid circles as points
     col = "blue"  # Color of points
) +

# Add a reference line (line of equality)
abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line

# H1
# Obtain predicted values
glm_H1_2_pred <- predict(glm_H1_2, type = "response")

# Create the plot
glm_H1_2_pred_plot <- plot(glm_H1_2_pred, project_data$coy_tot_det,
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main = "Predicted vs. Observed Plot for H1",
     pch = 16,  # Use solid circles as points
     col = "blue"  # Color of points
) +

# Add a reference line (line of equality)
abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line

# H2
# Obtain predicted values
glm_H2_2_pred <- predict(glm_H2_2, type = "response")

# Create the plot
glm_H2_2_pred_plot <- plot(glm_H2_2_pred, project_data$coy_tot_det,
                           xlab = "Predicted Values",
                           ylab = "Observed Values",
                           main = "Predicted vs. Observed Plot for H2",
                           pch = 16,  # Use solid circles as points
                           col = "blue"  # Color of points
) +
  
  # Add a reference line (line of equality)
  abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line

# H3
# Obtain predicted values
glm_H3_2_pred <- predict(glm_H3_2, type = "response")

# Create the plot
glm_H3_2_pred_plot <- plot(glm_H3_2_pred, project_data$coy_tot_det,
                           xlab = "Predicted Values",
                           ylab = "Observed Values",
                           main = "Predicted vs. Observed Plot for H3",
                           pch = 16,  # Use solid circles as points
                           col = "blue"  # Color of points
) +
  
  # Add a reference line (line of equality)
  abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line

# H4
# Obtain predicted values
glm_H4_2_pred <- predict(glm_H4_2, type = "response")

# Create the plot
glm_H4_2_pred_plot <- plot(glm_H4_2_pred, project_data$coy_tot_det,
                           xlab = "Predicted Values",
                           ylab = "Observed Values",
                           main = "Predicted vs. Observed Plot for H4",
                           pch = 16,  # Use solid circles as points
                           col = "blue"  # Color of points
) +
  
  # Add a reference line (line of equality)
  abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line

# H5
# Obtain predicted values
glm_H5_2_pred <- predict(glm_H5_2, type = "response")

# Create the plot
glm_H5_2_pred_plot <- plot(glm_H5_2_pred, project_data$coy_tot_det,
                           xlab = "Predicted Values",
                           ylab = "Observed Values",
                           main = "Predicted vs. Observed Plot for H5",
                           pch = 16,  # Use solid circles as points
                           col = "blue"  # Color of points
) +
  
  # Add a reference line (line of equality)
  abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line

# H6
# Obtain predicted values
glm_H6_2_pred <- predict(glm_H6_2, type = "response")

# Create the plot
glm_H6_2_pred_plot <- plot(glm_H6_2_pred, project_data$coy_tot_det,
                           xlab = "Predicted Values",
                           ylab = "Observed Values",
                           main = "Predicted vs. Observed Plot for H6",
                           pch = 16,  # Use solid circles as points
                           col = "blue"  # Color of points
) +
  
  # Add a reference line (line of equality)
  abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line

# H7
# Obtain predicted values
glm_H7_2_pred <- predict(glm_H7_2, type = "response")

# Create the plot
glm_H7_2_pred_plot <- plot(glm_H7_2_pred, project_data$coy_tot_det,
                           xlab = "Predicted Values",
                           ylab = "Observed Values",
                           main = "Predicted vs. Observed Plot for H7",
                           pch = 16,  # Use solid circles as points
                           col = "blue"  # Color of points
) +
  
  # Add a reference line (line of equality)
  abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line

# H8
# Obtain predicted values
glm_H8_2_pred <- predict(glm_H8_2, type = "response")

# Create the plot
glm_H8_2_pred_plot <- plot(glm_H8_2_pred, project_data$coy_tot_det,
                           xlab = "Predicted Values",
                           ylab = "Observed Values",
                           main = "Predicted vs. Observed Plot for H8",
                           pch = 16,  # Use solid circles as points
                           col = "blue"  # Color of points
) +
  
  # Add a reference line (line of equality)
  abline(a = 0, b = 1, col = "red", lty = 2)  # Dashed red line


# Model Selection ---------------------------------------------------------

model_selection <- model.sel(glm_H0_2,
          glm_H1_2,
          glm_H2_2,
          glm_H3_2,
          glm_H4_2,
          glm_H5_2,
          glm_H6_2,
          glm_H7_2,
          glm_H8_2) 

# When comparing models we want to focus on the last 4 columns (LogLik, AICc, delta, and weight)

# Log likelihood

# The likelihood function tells us the relative probability (range - infinity to + infinity) that the given variables in our model generated the data we provided. The higher the log likelihood the better the model fits the data set. There is no standard log likelihood value that indicates a good model fit rather these values are used to compare across models from the same data set, and are used to compute AIC scores.

#AICc

# Akaike Information Criterion corrected for small sample size or AICc is a mathematical method for evaluating how well a model fits the data it was generated from. AICc is used to compare models and determine which is the best fit to the data with the lowest score representing the best fit. AICc scores are based of the maximum likelhood estimate and the number of variables in the model. Since the more things you add to a model the more likely you are to explain additional variation in the data, AICc penalizes a model for each additional variable, so if a variable isn’t explaining enough variation in the data the AICc score for that model shouldn’t decrease much when that variable is added to the model.

# Delta AICc

# Delta AICc represents the difference between the top model and each following model in the model selection table. Generally a rule-of-thumb is if a model has a delta AICc less than 2 from the top model those models are considered to perform the same.

# Weight

# The AICc weight represents the relatively likelihood of a model compared to all other models in the in the table where a value of 1.0 = the most likely.


