# 0. Useful stuff ---------------------------------------------------------

# Marissa lab page: https://marissadyck.github.io/ES482-R-labs.github.io/mod_2_glm.html#Materials

# R for Data Science 2e: https://r4ds.hadley.nz/ 

# 1. Getting everything set up --------------------------------------------

# 1a. Load packages -----------------------------------------------------
library(tidyverse)
library(ggpubr)
library(PerformanceAnalytics)
library(car)
library(gridExtra)
library(MASS)
library(dplyr)
library(MuMIn)

# 1b. Confirm working directory where you want it -------------------------
print(getwd())

# 1c. Import and review data from ACME lab OSR project -------------------------------
## Human (HFI, Human Factors Indices) and natural (landcover types) variables at varying buffer distances. Updated early April. 
covariates <- read_csv("data/raw/OSM_2022_covariates2.csv")

## Proportional detections: number of months present and absent out of total number of months camera operated (15).
proportional_detections <- read_csv("data/raw/OSM_2022_proportional_detections2.csv")

## Total detections: total number of detections more than 30 minutes apart for each animal
total_detections <- read_csv("/Users/larissabron/Documents/BSc/23_24/Spring/BorealMammals/Project/RProject/data/raw/OSM_2022_total_detections.csv")  %>% 
  setNames(
    names(.) %>% 
      tolower() %>% 
      gsub(" ", "_", .)) # also get some underscores in there, dang

# 2. Visualize potential variables ---------------------------------------

# Visualizing how coyote data varies with different covariate sets (wide feats, narrow feats, natural feats, predator and prey) separated by land use area. 
## Now inset the distribution of each feature by array - second plot per covariate.
### Additional inset of distribution of each feature looking at the landscape - third plot per covariate. 


# 2a. first, get together everything potentially of interest into one file  --------
project_data <- covariates %>% 
  # filter for buffer distance of 1000m only
  filter(buff_dist == 1000) %>% 
  # select all potential features of interest
  dplyr::select(site, array, camera, pipeline, transmission_line, road_gravel_1l, road_gravel_2l, road_paved_undiv_2l, road_paved_undiv_1l, rlwy_sgl_track, road_paved_1l, road_winter, trail, conventional_seismic, low_impact_seismic, vegetated_edge_roads, vegetated_edge_railways, truck_trail, road_unclassified, road_unimproved, lc_class20, lc_class50, lc_class110, lc_class210, lc_class220, lc_class230) %>% 
  # join proportional detection data to covariates
  right_join(proportional_detections,
             by = 'site') %>% 
  # renaming columns to get some order in place
  rename(water = lc_class20,
         shrub = lc_class50,
         grass = lc_class110,
         broadleaf = lc_class220,
         mixed_forest = lc_class230,
         conifer = lc_class210,
         coy_prop_pres = coyote,
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
         caribou_prop_abs = absent_caribou) %>% 
  # Total months used in data review in the future
mutate( 
    coy_tot_month = coy_prop_pres + coy_prop_abs,
    moose_tot_month = moose_prop_pres + moose_prop_abs,
    hare_tot_month = hare_prop_pres + hare_prop_abs,
    deer_tot_month = deer_prop_pres + deer_prop_abs,
    wolf_tot_month = wolf_prop_pres + wolf_prop_abs) %>% 
  # Need to add total detections for modelling
  right_join(total_detections,
             by = 'site') %>% 
  # renaming total detection columns so they don't get lost
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
                owl_tot_det = owl) %>% 
  # rearranging data frame for easier review
  relocate(site, array, camera, coy_prop_pres, coy_prop_abs, coy_tot_month, coy_tot_det, moose_prop_pres, moose_prop_abs, moose_tot_month, moose_tot_det, hare_prop_pres, hare_prop_abs, hare_tot_month, hare_tot_det, deer_prop_pres, deer_prop_abs, deer_tot_month, deer_tot_det, wolf_prop_pres, wolf_prop_abs, wolf_tot_month, wolf_tot_det) %>% 
  # Further down the line you are going to appreciate removing rows with NA value
  na.omit() 


# 2b. Threshold for months cameras operational? ---------------------------
### How many months operational are all cameras?
project_data %>% 
  dplyr::select(coy_tot_month, moose_tot_month, hare_tot_month, deer_tot_month, wolf_tot_month) %>% 
  summary()
### Max is 15 months for all, seems like some were quite a bit less. Filtering for minimum 12 months operational. 


# 2c. Filter out cameras operational less than 12 months ------------------
project_data <- project_data %>% 
  filter(coy_tot_month > 11) 

# 2d. Visualizing wide features  -------------------------
# First graph: feature vs. coyote detections across the whole site
# Second graph: percent cover of the variable at each camera, separated by array
# Third graph: percent cover of the variable at each camera, considered over the whole study area 
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

# Combine into larger figures
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


# 2e. Visualizing natural features  -----------------------------------------
# First graph: feature vs. coyote detections across the whole site
# Second graph: percent cover of the variable at each camera, separated by array
# Third graph: percent cover of the variable at each camera, considered over the whole study area

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

# Combine into larger figures
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

# 2f. Visualizing narrow features ----------------------------------------
# First graph: feature vs. coyote detections across the whole site
# Second graph: percent cover of the variable at each camera, separated by array
# Third graph: percent cover of the variable at each camera, considered over the whole study area

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

# Combine into larger features
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


# 2g. Visualizing animals --------------------------------------------
# First graph: feature vs. coyote detections across the whole site
# Second graph: percent cover of the variable at each camera, separated by array
# Third graph: percent cover of the variable at each camera, considered over the whole study area 

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

# cougar # plots 43, 77, 112 removed because cougar data wasnt avail.

# coyote
plot_78 <- project_data %>% 
  ggplot(mapping = aes(x = array, y = coy_tot_det, color = array)) +
  geom_point()

plot_113 <- project_data %>% 
  ggplot(mapping = aes(x = site, y = coy_tot_det, color = array)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2))

# Combine into larger figures
figure_5 <- ggarrange(plot_33,
                      plot_34,
                      plot_35,
                      plot_36,
                      plot_37,
                      plot_38,
                      plot_39,
                      plot_40,
                      plot_41,
                      plot_42)

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
                       plot_113)

# 3. Confirm which variables are relevant for modelling ------------------

# 3a. Initial thoughts ------------------------------------------------
# Keeping all natural features: shrub, water, mixed_forest, conifer, broadleaf, grass
# Animals: competitor is grey_wolf; prey is moose, white-tailed_deer, and snowshoe_hare
# Linear features: Checking with histograms below whether the potential linear features have enough presence to be relevant. There shouldn't just be a large tail of only one value (usually 0's).

# 3b. View histograms (notes on the side) ---------------------------------
## Covariates should already be filtered to only buffer of 1000m at this point, if not your histograms will have a crazy amount of extra zeros!

hist_1 <- hist(covariates$pipeline) # in
hist_2 <- hist(covariates$truck_trail) # out
hist_3 <- hist(covariates$vegetated_edge_roads) # in
hist_4 <- hist(covariates$road_paved_1l) # out
hist_5 <- hist(covariates$conventional_seismic) # in
hist_6 <- hist(covariates$road_gravel_1l) # in
hist_7 <- hist(covariates$road_paved_undiv_1l) # out
hist_8 <- hist(covariates$road_gravel_2l) # out (actually in and merging with 1l)
hist_9 <- hist(covariates$trail) # in
hist_10 <- hist(covariates$road_unclassified) # out
hist_11 <- hist(covariates$low_impact_seismic) # in (lots of zeros)
hist_12 <- hist(covariates$road_paved_undiv_2l) # out
hist_13 <- hist(covariates$road_unimproved) # out (could be used, lots of 0s)
hist_14 <- hist(covariates$vegetated_edge_railways) # out
hist_15 <- hist(covariates$transmission_line) # in
hist_16 <- hist(covariates$road_unpaved_2l) # out
hist_17 <- hist(covariates$road_paved_3l) # out
hist_18 <- hist(covariates$road_paved_div) # out
hist_19 <- hist(covariates$road_unpaved_1l) # out
hist_20 <- hist(covariates$road_paved_5l) # out
hist_21 <- hist(covariates$road_paved_4l) # out

# Variables moving on to the next step: pipeline, conventional_seismic, road_gravel_1l, trail, low_impact_seismic, transmission_line, road unimproved. Gegetated_edge_roads kicked out because we don't know which roads this is on the side of and we are only using some roads.

# 3c. Pre-modelling assumption: Check if there is correlation between variables -------------------------
chart.Correlation(project_data[c("pipeline", "transmission_line", "road_gravel_1l", "road_gravel_2l", "trail", "conventional_seismic", "low_impact_seismic", "road_unimproved", "wolf_tot_det", "deer_tot_det", "moose_tot_det", "coy_tot_det", "hare_tot_det", "coy_prop_abs", "coy_prop_pres", "water", "shrub", "grass", "conifer", "broadleaf", "mixed_forest")],
                  histogram = TRUE, 
                  method = "spearman",
                  text.scale = 8)
### r^2 cut off of 0.7 for variables to be used together. 
# Pipeline and transmission line 0.63, merge later
# Broadleaf and conifer -0.66, merge later
# Broadleaf and deer 0.63, not independant variables in a model

# 3d. Dealing with correlated variables and merging for modelling  --------

# Wide: pipeline and transmission line merged into infrastructure_line, merging road_gravel_1l and road_gravel_2l into gravel_road
# Narrow: trail; conventional seismic and low impact seismic merged into seismic; road_unimproved
# Natural: by canopy level - forest is merged conifer, broadleaf, and mixed forest; shrub, grass, and water.

# 3e. Update potential covariates in project_data dataframe -----------------------------------------
project_data <- project_data %>% 
  ### Filter data for covariates of interest
  dplyr::select(site, array, camera, 
                # animals
                coy_prop_abs, coy_prop_pres, coy_tot_month, coy_tot_det, moose_prop_abs, moose_prop_pres, moose_tot_det, moose_tot_month, hare_prop_pres, hare_prop_abs, hare_tot_month, hare_tot_det, deer_prop_pres, deer_tot_month, deer_prop_abs, deer_tot_det, wolf_prop_pres, wolf_prop_abs, wolf_tot_month, wolf_tot_det,
                # linear features
                pipeline, transmission_line, road_gravel_1l, road_gravel_2l, trail, conventional_seismic, low_impact_seismic, road_unimproved,
                # natural features
                water, shrub, grass, conifer, broadleaf, mixed_forest) %>% 
  # combine some features
  mutate(
    gravel_road = road_gravel_2l + road_gravel_1l,
    seismic_line = conventional_seismic + low_impact_seismic,
    infrastructure_line = transmission_line + pipeline,
    forest = conifer + broadleaf + mixed_forest,
  ) %>% 
  # remove old unmerged features
  dplyr::select(-road_gravel_2l, -road_gravel_1l, -conventional_seismic, -low_impact_seismic, -transmission_line, -pipeline, -conifer, -broadleaf, -mixed_forest) %>% 
  # remove total month from proportional used to figure out number of camera months operational
  dplyr::select(-coy_tot_month, -moose_tot_month, -hare_tot_month, -deer_tot_month, -wolf_tot_month)

# 3f. Pre-modelling assumption: Check if correlation between merged variables -----------------------
chart.Correlation(project_data[c("infrastructure_line", "gravel_road", "trail", "seismic_line", "road_unimproved", "wolf_tot_det", "deer_tot_det", "moose_tot_det", "coy_tot_det", "hare_tot_det", "coy_prop_abs", "coy_prop_pres", "water", "grass", "forest", "shrub")],
                  histogram = TRUE, 
                  method = "spearman",
                  text.scale = 6)
### Result: r^2 cut off of 0.7 for variables to be used together. Considering potential variable combinations from the models above
# infrastructure_line and gravel_road are at 0.59 *** pretty high!
# shrub and forest -0.55 *** pretty high! when I combine shrub and forest then they are highly correlated with grass. This might just be unavoidable with natural features?

## Decided to keep these even though they are high, they are below the threshold so we can keep the covariates and use this in interpreting our results!

# 4. Proportional binomial model time -------------------------------------

# 4a. Anticipated models ------------------------------------------------------
# H0: Null, coyotes ~ 

# H1: Coyotes like wide features, coyotes ~ infrastructure_line + gravel_road

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ infrastructure_line + gravel_road + infrastructure_line:wolf_tot_det + gravel_road:wolf_tot_det

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ infrastructure_line + gravel_road + deer_tot_det + hare_tot_det + moose_tot_det

# H4: Coyotes like narrow features, coyote ~ trail + seismic_line + road_unimproved

# H5: Coyote use of narrow features depends on presence of a competitor, coyote ~ trail + seismic_line  + road_unimproved + trail:wolf_tot_det + seismic_line:wolf_tot_det + road_unimproved:wolf_tot_det

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + seismic_line + road_unimproved + deer_tot_det + hare_tot_det + moose_tot_det

# H7: Coyotes like both wide and narrow features, coyote ~ infrastructure_line + gravel_road + trail + seismic_line + road_unimproved

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + forest

# 4c. Models --------------------------------------------------------------

# H0: Null, coyotes ~ 
H0 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 1,
  data = project_data,
  family = binomial)

summary(H0)
#Residual deviance: 654.45  on 146  degrees of freedom
#AIC: 926.33

# H1: Coyotes like wide features, coyotes ~ infrastructure_line + gravel_road
H1 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 
    scale(infrastructure_line) + 
    scale(gravel_road),
  data = project_data,
  family = binomial)

summary(H1)
#Residual deviance: 541.08  on 144  degrees of freedom
#AIC: 816.96

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ infrastructure_line + gravel_road + infrastructure_line:wolf_tot_det + gravel_road:wolf_tot_det
H2 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(infrastructure_line):scale(wolf_tot_det) +
    scale(gravel_road):scale(wolf_tot_det),
  data = project_data,
  family = binomial)

summary(H2)
#Residual deviance: 540.26  on 142  degrees of freedom
#AIC: 820.13

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ infrastructure_line + gravel_road + deer_tot_det + hare_tot_det + moose_tot_det
H3 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(deer_tot_det) +
    scale(hare_tot_det) +
    scale(moose_tot_det),
  data = project_data,
  family = binomial)

summary(H3)
#Residual deviance: 401.59  on 141  degrees of freedom
#AIC: 683.46

# H4: Coyotes like narrow features, coyote ~ trail + seismic_line + road_unimproved
H4 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = project_data,
  family = binomial)

summary(H4)
#Residual deviance: 604.42 on 143  degrees of freedom
#AIC: 882.29

# H5: Coyote use of narrow features depends on presence of a competitor, coyote ~ trail + seismic_line  + road_unimproved + trail:wolf_tot_det + seismic_line:wolf_tot_det + road_unimproved:wolf_tot_det
H5 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(trail):scale(wolf_tot_det) +
    scale(seismic_line):scale(wolf_tot_det) +
    scale(road_unimproved):scale(wolf_tot_det),
  data = project_data,
  family = binomial)

summary(H5)
#Residual deviance: 599.09  on 140  degrees of freedom
#AIC: 882.96

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + seismic_line + road_unimproved + deer_tot_det + hare_tot_det + moose_tot_det
H6 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(deer_tot_det) +
    scale(hare_tot_det) +
    scale(moose_tot_det),
  data = project_data,
  family = binomial)

summary(H6)
#Residual deviance: 424.96  on 140  degrees of freedom
#AIC: 708.83

# H7: Coyotes like both wide and narrow features, coyote ~ infrastructure_line + gravel_road + trail + seismic_line + road_unimproved
H7 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = project_data,
  family = binomial)

summary(H7)
#Residual deviance: 476.83  on 141  degrees of freedom
#AIC: 758.7

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + forest
H8 <- glm(
  cbind(coy_prop_abs, coy_prop_pres) ~ 
    scale(water) +
    scale(shrub) +
    scale(grass) +
    scale(forest),
  data = project_data,
  family = binomial)

summary(H8)  
#Residual deviance: 417.55 on 142 degrees of freedom
#AIC: 697.42

# 5 Post-model assumption testing ----------------------------------------

# 5a. Homogeneity of Variance -------------------------------------------------
#We don't have any factor variables (groups) in our model so we don't need to test this assumption. 

# 5b. Dispersion --------------------------------------------------------------
# Proportional binomial model doesn't care about this

# 5c. Pseudo r^2 for the normality of residuals --------------------------------------------------------------
# *** to be done ***

# 6. Interpreting model output -------------------------------------------
### **** ##### ****** Plotting odds ratios (would be good to see if any variables with large standard error that should be tossed from modelling), graphing predictions ##### **** #### ***** 
# *** Graphing crew! 

# 7. Model Selection ------------------------------------------------------
model_selection <- model.sel(H0, H1, H2, H3, H4, H5, H6, H7, H8) 

model_selection

# 8. Plot models -----------------------------------------------------------
### **** ##### ****** Plot function (only care about one, is it residuals vs fitted?), cant do AUC, Marissa was looking into pseudo R^2 - ask? ##### **** #### ***** 
# *** Graphing crew! 

