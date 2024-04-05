# ES582 - Coyote GLM
# Jamie Clarke

### NEED ABSENCES AS WELL AS PRESENCES!!
### NEED PROPORTIONAL DETS OF SNOWSHOE HARES

# last updated Apr 5

library(cowplot)
library(tidyverse)
library(leaflet)
library(ggpubr)
library(RColorBrewer)
library(stringr)
library(tidyverse)
library(PerformanceAnalytics)
library(lme4)
library(rphylopic)
library(MuMIn)
library(AER)

# STEP 1 - COMBINE DATA INTO SINGLE FILE -------------------------------------------------

# first: selecting LF covariates
# lf_covs = LF-related covariates
lf_covs <- read.csv('data/OSM_2022_covariates.csv') %>% 
  
  select(site, buff_dist, trail, conventional_seismic, pipeline, transmission_line, road_unimproved, road_gravel_1l, road_gravel_2l, truck_trail, road_paved_undiv_2l, low_impact_seismic, road_paved_1l, lc_class20, lc_class50, lc_class110, lc_class210, lc_class220, lc_class230) %>% 
  
  rename(water = lc_class20,
         shrub = lc_class50,
         grass = lc_class110,
         broadleaf = lc_class220,
         mixed_forest = lc_class230,
         conifer = lc_class210) %>% 
  
  na.omit()

# next: selecting wildlife (hare, deer, moose, wolf) detection covariates - total number of detections of each of these spp
# wildlife_covs = prey and competitor detection covariates
wildlife_covs <- read.csv('data/OSM_2022_total_detections.csv') %>% 
  
  set_names(
    names(.) %>%  
      tolower() %>% 
      str_replace_all(pattern = '\\.',
                      replacement = '_')) %>% 
  
  select(site, snowshoe_hare, unknown_deer, white_tailed_deer, grey_wolf, moose)

# next: select for response variable = proportional detections of coyotes
# prop_det means proportional detections of coyotes
coyote_det <- read.csv('data/OSM_2022_proportional_detections.csv') %>% 
  
  select(site, coyote, absent_coyote)

# next: combine data frames!
# looking for: one row per site, one column per species' response metric, columns for each covariate

# coyote_data = full data frame
coyote_data <- coyote_det %>% 
  
  left_join(lf_covs,
            by = 'site') %>% 
  
  left_join(wildlife_covs,
            by = 'site') %>% 
  
  filter(buff_dist == 1000) %>% 
  
  na.omit() %>% 
  
  mutate_if(is.numeric, scale)


# STEP 2 = GLM ----------------------------------------------------------------------

# check for autocorrelation

chart.Correlation(coyote_data[c('trail', 'conventional_seismic', 'pipeline', 'transmission_line', 'road_unimproved', 'road_gravel_1l', 'road_gravel_2l', 'truck_trail', 'road_paved_undiv_2l', 'low_impact_seismic', 'road_paved_1l', 'snowshoe_hare', 'unknown_deer', 'white_tailed_deer', 'grey_wolf', 'moose')], 
                  histogram = TRUE, 
                  method = "pearson")


chart.Correlation(coyote_data[c('trail', 'conventional_seismic', 'pipeline', 'transmission_line', 'road_unimproved', 'road_gravel_1l', 'road_gravel_2l', 'truck_trail', 'road_paved_undiv_2l', 'low_impact_seismic', 'road_paved_1l', 'snowshoe_hare', 'unknown_deer', 'white_tailed_deer', 'grey_wolf', 'moose')], 
                  histogram = TRUE, 
                  method = "spearman")

# LEARNED FROM CORRELATION PLOTS: 1) unknown_deer and white_tailed_deer are autocorrelated (0.7 or 0.73), so get rid of unknown_deer (checked w/ Marissa)
#                                 2) pipeline and transmission_line are autocorrelated (0.77 or 0.63), so get rid of transmission line (?)
#                                 3) there are no detections on road_paved_1l, so don't need!
#                                 4) Pearson's and Spearman give similar results!

# testing candidate models

# null hypothesis
H0 <- glm(
          cbind(coyote, absent_coyote) ~ 1,
          data = coyote_data,
          family = binomial)

# coyotes like wide features
H1 <- glm(
          cbind(coyote, absent_coyote) ~
            pipeline +
            road_unimproved +
            road_gravel_1l +
            road_gravel_2l +
            truck_trail +
            road_paved_undiv_2l,
          data = coyote_data,
          family = binomial)

# coyote use of wide features depends on presence of competitors
H2 <- glm(
  cbind(coyote, absent_coyote) ~
    pipeline +
    road_unimproved +
    road_gravel_1l +
    road_gravel_2l +
    truck_trail +
    road_paved_undiv_2l +
    pipeline:grey_wolf +
    transmission_line:grey_wolf +
    road_unimproved:grey_wolf +
    road_gravel_1l:grey_wolf +
    road_gravel_2l:grey_wolf +
    truck_trail:grey_wolf +
    road_paved_undiv_2l:grey_wolf,
  data = coyote_data,
  family = binomial)

# coyotes use of wide features also varies with presence of prey
H3 <- glm(
  cbind(coyote, absent_coyote) ~
    pipeline +
    road_unimproved +
    road_gravel_1l +
    road_gravel_2l +
    truck_trail +
    road_paved_undiv_2l +
    snowshoe_hare +
    white_tailed_deer +
    moose,
  data = coyote_data,
  family = binomial)

# coyotes like narrow features
H4 <- glm(
  cbind(coyote, absent_coyote) ~
    trail +
    conventional_seismic +
    low_impact_seismic,
  data = coyote_data,
  family = binomial)

# coyote use of narrow features depends on presence of competitors
H5 <- H4 <- glm(
  cbind(coyote, absent_coyote) ~
    trail +
    conventional_seismic +
    low_impact_seismic +
    trail:grey_wolf +
    conventional_seismic:grey_wolf +
    low_impact_seismic:grey_wolf,
  data = coyote_data,
  family = binomial)

# coyote use of narrow features also varies with presence of prey
H6 <- glm(
  cbind(coyote, absent_coyote) ~
    trail +
    conventional_seismic +
    low_impact_seismic +
    snowshoe_hare +
    white_tailed_deer +
    moose,
  data = coyote_data,
  family = binomial)

# coyotes like wide and narrow features
H7 <- glm(
  cbind(coyote, absent_coyote) ~
    pipeline +
    road_unimproved +
    road_gravel_1l +
    road_gravel_2l +
    truck_trail +
    road_paved_undiv_2l +
    trail +
    conventional_seismic +
    low_impact_seismic,
  data = coyote_data,
  family = binomial)

# coyotes like natural features
H8 <- glm(
  cbind(coyote, absent_coyote) ~
    water +
    shrub +
    grass +
    conifer +
    broadleaf +
    mixed_forest,
  data = coyote_data,
  family = binomial)


# STEP 3 - TEST ASSUMPTIONS -----------------------------------------------

# Levene's test is for groups - we don't have factor data, so skip

# test for residual deviance - proportional binomial does NOT assume even dispersion so ALL GOOD DAWG


# STEP 4 - MODEL SELECTION -------------------------------------------

model.sel(H0, H1, H2, H3, H4, H5, H6, H7, H8)

# model 3 = best-performing, by more than 2 AICc!
# so: coyotes use of wide features also varies with presence of prey!

# model 7 = second-best performing
# null model = third-best performing

# STEP 5 - INTERPRETING MODEL OUTPUTS ------------------------------------------------

# odds ratios
# interpretation: below 1 = negative association, above 1 = positive association
#                 value of the exponentiated coefficient = change in odds of the response outcome given 1 unit increase in the explanatory variable

exp(coefficients(H3))

summary(H3)

hist(coyote_data$pipeline)
hist(coyote_data$truck_trail)
