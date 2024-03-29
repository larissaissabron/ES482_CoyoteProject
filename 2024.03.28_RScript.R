# #0. Helpful websites ----------------------------------------------------

# Data Transformation in R (R for data science 2e): https://r4ds.hadley.nz/data-transform

# Marissa's tutorial website: https://marissadyck.github.io/ES482-R-labs.github.io/index.html

# 1. Getting everything set up --------------------------------------------

# 1a. Install packages
library(tidyverse)
library(ggpubr)

# 1b. Confirm working directory correct
print(getwd())

# 1c. Import .csv files

# Human  (HFI, Human Factors Indices) and natural (landcover types) variables at varying buffer distances
covariates <- read_csv("data/raw/OSM_2022_covariates.csv")

# Proportional detections: number of months present and absent out of total number of months camera operated. Use a negative binomial model ***don't use without talking with Marissa and Jake
proportional_detections <- read_csv("data/raw/OSM_2022_proportional_detections.csv")

# Species presence: presence (1) and absence (0) of each species. Model with GLM. 
species_presence <- read_csv("data/raw/OSM_2022_species_presence.csv") %>% 
  setNames(
    names(.) %>% 
      tolower()
  )

# Total detections: total independant detections (events > 30 min apart). Model with GLM. 
total_detections <- read_csv("/Users/larissabron/Documents/BSc/23_24/Spring/BorealMammals/Project/RProject/data/raw/OSM_2022_total_detections.csv")  %>% 
  setNames(
    names(.) %>% 
      tolower() %>% 
      gsub(" ", "_", .)) # also get some underscores in there, dang

# 1d. Bring together data you are interested in for coyote project

# Extract coyote-relevant data from species presence
coy_tot_det <- total_detections %>%
  select(site, coyote, moose, snowshoe_hare, `white-tailed_deer`, `grey_wolf`) %>% 
  rename(coy_tot_det = coyote,
         moose_tot_det = moose,
         hare_tot_det = snowshoe_hare,
         deer_tot_det = `white-tailed_deer`,
         wolf_tot_det = grey_wolf)

# Extract coyote-relevant data from proportion detections. *There is no data for hares
coy_prop_det <- proportional_detections %>% 
  select(site, coyote, absent_coyote, grey_wolf, absent_grey_wolf, moose, absent_moose, 'white-tailed_deer', 'absent_white-tailed_deer') %>% 
  rename(coy_prop_pres = coyote,
         coy_prop_abs = absent_coyote,
         wolf_prop_pres = grey_wolf,
         wolf_prop_abs = absent_grey_wolf,
         moose_prop_pres = moose,
         moose_prop_abs = absent_moose,
         deer_prop_pres = 'white-tailed_deer',
         deer_prop_abs = 'absent_white-tailed_deer') %>% 
  mutate( # This is the proportion of total possible ---- not sure if a good idea -----
    coy_prop = replace(coy_prop_pres / (coy_prop_pres + coy_prop_abs), is.infinite(coy_prop_pres / (coy_prop_pres + coy_prop_abs)), 0),
    wolf_prop = replace(wolf_prop_pres / (wolf_prop_pres + wolf_prop_abs), is.infinite(wolf_prop_pres / (wolf_prop_pres + wolf_prop_abs)), 0),
    moose_prop = replace(moose_prop_pres / (moose_prop_pres + moose_prop_abs), is.infinite(moose_prop_pres / (moose_prop_pres + moose_prop_abs)), 0),
    deer_prop = replace(deer_prop_pres / (deer_prop_pres + deer_prop_abs), is.infinite(deer_prop_pres / (deer_prop_pres + deer_prop_abs)), 0)
  )

# Leave presence alone because its very coarse scaled

# Create tibble of all covariates and relevant coyote data
coyote_data <- covariates %>% 
  filter(buff_dist == 1000) %>% 
  select(site, pipeline, transmission_line, road_gravel_1l, road_gravel_2l, road_paved_undiv_2l, road_paved_undiv_1l, rlwy_sgl_track, road_paved_1l, road_winter, trail, conventional_seismic, low_impact_seismic, vegetated_edge_roads, vegetated_edge_railways, truck_trail, road_unclassified, road_unimproved, lc_class20, lc_class50, lc_class110, lc_class210, lc_class220, lc_class230) %>% 
  right_join(coy_prop_det,
             by = 'site') %>% 
  right_join(coy_tot_det,
             by = 'site') %>% 
  arrange(site) %>% 
  relocate(site) %>% 
  separate(site, into = c("site", "camera"), sep = "_", remove = FALSE) %>% 
  rename(water = lc_class20,
         shrub = lc_class50,
         grass = lc_class110,
         broadleaf = lc_class220,
         mixed_forest = lc_class230,
         conifer = lc_class210)

# Save as a new .csv
write.csv(coyote_data, "/Users/larissabron/Documents/BSc/23_24/Spring/BorealMammals/Project/RProject/data/processed/coyote_data.csv", row.names = FALSE)

# 2. Visualizing Data -----------------------------------------------------

# Visualizing how coyote data varies with different covariate sets (wide feats, narrow feats, natural feats, predator and prey) separated by land use area. 

# 2a. Plotting coyote total against each wide feature
plot_1 <- coyote_data %>% 
  ggplot(mapping = aes(x = conventional_seismic, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_2 <- coyote_data %>% 
  ggplot(mapping = aes(x = low_impact_seismic, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_3 <- coyote_data %>% 
  ggplot(mapping = aes(x = transmission_line, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_4 <- coyote_data %>% 
  ggplot(mapping = aes(x = pipeline, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_5 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_gravel_1l, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_6 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_gravel_2l, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_7 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_2l, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_8 <- coyote_data %>% 
  ggplot(mapping = aes(x = rlwy_sgl_track, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_9 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_1l, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_10 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_1l, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_11 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_winter, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

figure_1 <- ggarrange(plot_1,
                      plot_2,
                      plot_3,
                      plot_4,
                      plot_5,
                      plot_6,
                      plot_7,
                      plot_8,
                      plot_9,
                      plot_10,
                      plot_11)

# 2b. Plotting coyote proportional against each wide feature
plot_12 <- coyote_data %>% 
  ggplot(mapping = aes(x = conventional_seismic, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_13 <- coyote_data %>% 
  ggplot(mapping = aes(x = low_impact_seismic, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_14 <- coyote_data %>% 
  ggplot(mapping = aes(x = transmission_line, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_15 <- coyote_data %>% 
  ggplot(mapping = aes(x = pipeline, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_16 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_gravel_1l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_17 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_gravel_2l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_18 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_2l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_19 <- coyote_data %>% 
  ggplot(mapping = aes(x = rlwy_sgl_track, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_20 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_1l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_21 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_1l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_22 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_winter, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

figure_2 <- ggarrange(plot_12,
             plot_13,
             plot_14,
             plot_15,
             plot_16,
             plot_17,
             plot_18,
             plot_19,
             plot_20,
             plot_21,
             plot_22)
figure_2 # Looks similar to 1, maybe not relevant to plot the proportion if it looks similar (or at least what I did for proportion)

# 2c. Coyote total vs. natural feats 

plot_22 <- coyote_data %>% 
  ggplot(mapping = aes(x = water, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_23 <- coyote_data %>% 
  ggplot(mapping = aes(x = shrub, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_24 <- coyote_data %>% 
  ggplot(mapping = aes(x = grass, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_25 <- coyote_data %>% 
  ggplot(mapping = aes(x = conifer, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_26 <- coyote_data %>% 
  ggplot(mapping = aes(x = broadleaf, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_27 <- coyote_data %>% 
  ggplot(mapping = aes(x = mixed_forest, y = coy_tot_det, color = site)) +
  geom_point() +
  facet_wrap(~site)

figure_3 <- ggarrange(plot_22,
                      plot_23,
                      plot_24,
                      plot_25,
                      plot_26,
                      plot_27)

figure_3

# 2d. Coyote total vs. narrow features
# 2e. Coyote total vs each mammal
