#DATA EXPLORATION


# data set-up -------------------------------------------------------------

# libraries 

library(tidyverse)
library(dplyr)
library(leaflet)
library(car)
library(ggpubr)
library(RColorBrewer)
library(stringr)
library(PerformanceAnalytics)
library(lme4)
library(rphylopic)
library(MuMIn)
library(AER)
# might not all be relevant

# read in data sets
covariates <- read.csv("data/raw/OSM_2022_covariates.csv")
prop_detects <- read.csv("data/raw/OSM_2022_proportional_detections.csv")
tot_detects <- read.csv("data/raw/OSM_2022_total_detections.csv")


# set buffer distance of 1000m 
covariates1000 <- subset(covariates, buff_dist == '1000')

# select explanatory variables 
cor_cov <- covariates1000 %>% # covariate dataset
  select(site,
         pipeline,
         road_gravel_1l,
         trail, 
         conventional_seismic, 
         vegetated_edge_roads)

cor_spp <- tot_detects %>% # species detection dataset
  select(site,
         Coyote,
         Moose,
         Snowshoe.hare,
         White.tailed.deer,
         Grey.wolf)

# join explanatory variable datasets
teamcoyote <- cor_cov %>%
  full_join(cor_spp, 
            by = 'site')

# make coyote first variable after site
teamcoyote <- teamcoyote %>%
  relocate(Coyote, .before = pipeline)

# simplify column names 
teamcoyote <- teamcoyote %>%
  rename(gravel_road = road_gravel_1l,
         wolf = Grey.wolf,
         seismic_lines = conventional_seismic,
         veg_road = vegetated_edge_roads,
         hare = Snowshoe.hare,
         deer = White.tailed.deer) %>%
  
  set_names(
    names(.) %>%  
      tolower())

# GLMs! -------------------------------------------------------------------

# IGNORE THIS SECTION RN, DATA EXPLORATION IN THE PROPORTIONAL SECTION

# Assumptions - variance
leveneTest(site ~ Coyote, data = tot_detects)

# check autocorrelation
chart.Correlation(teamcoyote, 
                  histogram = TRUE, 
                  method = "spearman") # or pearson?

# H0: Null Model
H0 <- glm(coyote ~ NULL,    
    data = teamcoyote,   
    family = poisson)

    # extract AIC
    extractAIC(H0)

# H1: Coyotes like wide features
H1 <- glm(coyote ~ pipeline + gravel_road,
          data = teamcoyote,
          family = poisson)

    # extract AIC
    extractAIC(H1)
    
# H2: Coyote use of wide features depends on presence of competitors 
H2 <- glm(coyote ~ gravel_road + pipeline + wolf + gravel_road:wolf + pipeline:wolf,
          data = teamcoyote,
          family = poisson)

    #extractAIC
    extractAIC(H2)

    
# Proportional Exploration ------------------------------------------------

# select explanatory variables
cor_spp_prop <- prop_detects %>% # species detection dataset
  select(site,
         coyote,
         moose,
         white.tailed_deer,
         grey_wolf,
         snowshoe_hare,
         absent_coyote,
         absent_moose,
         absent_snowshoe_hare,
         absent_white.tailed_deer,
         absent_grey_wolf) 

# simplify column names 
cor_spp_prop <- cor_spp_prop %>%
  rename(wolf = grey_wolf,
         deer = white.tailed_deer,
         hare = snowshoe_hare,
         hare_a = absent_snowshoe_hare,
         deer_a = absent_white.tailed_deer,
         coyote_absences = absent_coyote,
         wolf_a = absent_grey_wolf,
         moose_a = absent_moose)

# join explanatory variable datasets
teamcoyote_prop <- cor_cov %>%
  full_join(cor_spp_prop, 
            by = 'site')

# make coyote first variable after site
teamcoyote_prop <- teamcoyote_prop %>%
  relocate(coyote, .before = pipeline) %>%
  relocate(coyote_absences, .before = pipeline) %>%
  na.omit() # havent run this na code yet

# simplify column names ... again lol
teamcoyote_prop <- teamcoyote_prop %>%
  rename(gravel_road = road_gravel_1l,
         seismic_lines = conventional_seismic,
         veg_road = vegetated_edge_roads)

# GLMs - proportional

# H0: Null Model
H0_prop <- glm(cbind(coyote, coyote_absences) ~ 1,    
          data = teamcoyote_prop,   
          family = binomial)

# extract AIC
extractAIC(H0_prop)

# H1: Coyotes like wide features
H1_prop <- glm(cbind(coyote, coyote_absences) ~ pipeline + gravel_road,
          data = teamcoyote_prop,
          family = binomial)

# extract AIC
extractAIC(H1_prop)

# H2: Coyote use of wide features depends on presence of competitors 
H2_prop <- glm(cbind(coyote, coyote_absences) ~ gravel_road + pipeline + cbind(wolf, wolf_a) + gravel_road:cbind(wolf, wolf_a) + pipeline:cbind(wolf, wolf_a), 
          data = teamcoyote_prop,
          family = binomial)

#extractAIC
extractAIC(H2_prop)

# H3: Coyote use of wide features also varies with presence of prey
H3_prop <- glm(cbind(coyote, coyote_absences) ~ pipeline + gravel_road + cbind(deer, deer_a) + cbind(moose, moose_a),
               data = teamcoyote_prop,
               family = binomial)

# H4: Coyotes like narrow features
H4_prop <- glm(cbind(coyote, coyote_absences) ~ trail + seismic_lines + veg_road,
               data = teamcoyote_prop,
               family = binomial)

# H5: Coyote use of narrow features depends on presence of competitors
H5_prop <- glm(cbind(coyote, coyote_absences) ~ trail + seismic_lines + veg_road + trail:cbind(wolf, wolf_a) + seismic_lines:cbind(wolf, wolf_a)+ veg_road:cbind(wolf, wolf_a), 
               data = teamcoyote_prop,
               family = binomial)
length(teamcoyote_prop)
summary(teamcoyote_prop)
str(teamcoyote_prop)

# H6: Coyote use of narrow features also varies with presence of prey 
H6_prop <- glm(cbind(coyote, coyote_absences) ~ trail + seismic_lines + veg_road + cbind(deer, deer_a) + cbind(moose, moose_a),
               data = teamcoyote_prop,
               family = binomial)

# H7: Coyotes like both wide and narrow features
H7_prop <- glm(cbind(coyote, coyote_absences) ~ pipeline + gravel_road + trail + seismic_lines + veg_road,
               data = teamcoyote_prop,
               family = binomial)

# H8: “baseline” aka all natural features

    # create subset dataset? include in other dataset?
H8_prop <- glm(cbind(coyote, coyote_absences) ~ natural featuressss, # havent coded this in yet
               data = teamcoyote_natfeat,
               family = binomial)

# model selection
model.sel(H0_prop,
          H1_prop,
          H2_prop,
          H3_prop,
          H4_prop,
          H5_prop,
          H6_prop,
          H7_prop)


# Interpretation ----------------------------------------------------------

summary(H1_prop) 
  # pipeline -1.2, 
  # gravel road 140

summary(H2_prop)
  # gravel road -145
  # pipeline 114
  # wolf 0ish
  # gravel road:wolf 22/20 
  # pipeline:wolf -10/-8.8

summary(H7_prop) # 2nd highest AIC
  # pipeline -7
  # gravel_road 104
  # trail 190
  # seismic 56
  # veg road 36

summary(H6_prop) # highest AIC
  # trail 42
  # seismic 86
  # veg road 41
  # deer -0/-0.1
  # moose 0/NA

# etc...

# Odds Ratios

exp(coefficients(H0_prop))
exp(coefficients(H1_prop))
exp(coefficients(H2_prop))
exp(coefficients(H3_prop))
exp(coefficients(H4_prop))
exp(coefficients(H5_prop))
exp(coefficients(H6_prop))
exp(coefficients(H7_prop))

# Scaling
# im assuming these havent been scaled? unsure about this part...

# Predicted Probabilities

plogis(coefficients(H0_prop))
plogis(coefficients(H1_prop))
plogis(coefficients(H2_prop))
plogis(coefficients(H3_prop))
plogis(coefficients(H4_prop))
plogis(coefficients(H5_prop))
plogis(coefficients(H6_prop))
plogis(coefficients(H7_prop))

  # new predicted probability data set
  new_coyote_data <- expand.grid(coyote_abund = seq(min(teamcoyote_prop$cbind(coyote,coyote_absences)),
                                                    max(teamcoyote_prop$cbind(coyote,coyote_absences)), # error in here idk probably cause its proportional?
                                                    by = 0.1),
                                 pipeline = mean(teamcoyote_prop$pipeline),
                                 gravel_road = mean(teamcoyote_prop$gravel_road),
                                 trail = mean(teamcoyote_prop$trail),
                                 seismic_lines = mean(teamcoyote_prop$seismic_lines),
                                 veg_road = mean(teamcoyote_prop$veg_road)) # not sure how to add in other data as its proportional... 

# Graphing ----------------------------------------------------------------


