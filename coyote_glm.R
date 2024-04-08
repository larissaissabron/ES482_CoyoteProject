# ES582 - Coyote GLM
# Jamie Clarke

# last updated Apr 8

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
library(broom)


# STEP 0 - DATA VIS -------------------------------------------------------

# plot frequency for LFs
# lf_vis = visualizing LFs!
lf_vis <- read.csv('data/OSM_2022_covariates.csv') %>% 
  filter(buff_dist == 1000)

hist(lf_vis$pipeline) # in
hist(lf_vis$truck_trail) # out
hist(lf_vis$vegetated_edge_roads) # could use this...
hist(lf_vis$road_paved_1l) # out
hist(lf_vis$conventional_seismic) # in
hist(lf_vis$road_gravel_1l) # in
hist(lf_vis$road_paved_undiv_1l) # out
hist(lf_vis$road_gravel_2l) # out
hist(lf_vis$trail) # in
hist(lf_vis$road_unclassified) # out
hist(lf_vis$low_impact_seismic) # in
hist(lf_vis$road_paved_undiv_2l) # out
hist(lf_vis$road_unimproved) # out
hist(lf_vis$vegetated_edge_railways) # out
hist(lf_vis$transmission_line) # in
hist(lf_vis$road_unpaved_2l) # out
hist(lf_vis$road_paved_3l) # out
hist(lf_vis$road_paved_div) # out
hist(lf_vis$road_unpaved_1l) # out
hist(lf_vis$road_paved_5l) # out
hist(lf_vis$road_paved_4l) # out

# SUMMARY: wide features = gravel roads + pipelines, narrow features = trails, seismic lines + unimproved roads

# NEW THOUGHTS - confirming we want to use unimproved roads - justification?
#                in this version, I also combine seismic line types... makes sense to me, but any thoughts/feelings/concerns?


# STEP 1 - COMBINE DATA INTO SINGLE FILE -------------------------------------------------

# first: selecting LF covariates
# lf_covs = LF-related covariates
lf_covs <- read.csv('data/OSM_2022_covariates.csv') %>% 
  
  select(site, buff_dist, trail, conventional_seismic, low_impact_seismic, pipeline, transmission_line, road_gravel_1l, road_gravel_2l, road_unimproved, lc_class20, lc_class50, lc_class110, lc_class210, lc_class220, lc_class230) %>% 
  
  rename(water = lc_class20,
         shrub = lc_class50,
         grass = lc_class110,
         broadleaf = lc_class220,
         mixed_forest = lc_class230,
         conifer = lc_class210) %>% 
  
  mutate(gravel_road = road_gravel_1l + road_gravel_2l) %>% 
  
  mutate(seismic_line = conventional_seismic + low_impact_seismic) %>% 
  
  mutate(infrastructure_line = transmission_line + pipeline) %>% 
  
  mutate(forest = conifer + broadleaf + mixed_forest) %>% 
  
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
  
  na.omit()

# STEP 2 = GLM ----------------------------------------------------------------------

# first: check for autocorrelation

coyote_cor <- coyote_data %>%
  
  select(trail, seismic_line, infrastructure_line, road_unimproved, gravel_road, snowshoe_hare, unknown_deer, white_tailed_deer, grey_wolf, moose, water, shrub, grass, forest)

summary(coyote_cor)

chart.Correlation(coyote_cor,
                  histogram = TRUE,
                  method = 'pearson')

chart.Correlation(coyote_cor,
                  histogram = TRUE,
                  method = 'spearman')

# LEARNED FROM CORRELATION PLOTS: 1) unknown_deer and white_tailed_deer are autocorrelated (0.7 or 0.73), so get rid of unknown_deer
#                                 2) pipeline and transmission_line are autocorrelated (0.77 or 0.63)
#                                 3) broadleaf and conifer are autocorrelated (0.78 and 0.66) AND broadleaf and WTD are autocorrelated (0.54 and 0.63), this seems legit so not worried about it??
#                                 4) pipeline and gravel_roads are pretty autocorrelated, esp according to Spearman
#                                 5) Pearson's and Spearman give similar results!

# SUMMARY: wide features = transmission lines + gravel roads, narrow features = trails, seismic lines + unimproved roads

# NEW THOUGHTS: should we group transmission lines + piplelines = above-ground LF disturbances and see if not autocorrelated w/ gravel roads? I know there wasn't huge love for this idea last week but worth revisiting?

# next: test candidate models using proportional binomial GLMs

# null hypothesis
H0 <- glm(
          cbind(coyote, absent_coyote) ~ 1,
          data = coyote_data,
          family = binomial)

# coyotes like wide features
H1 <- glm(
          cbind(coyote, absent_coyote) ~
            scale(infrastructure_line) +
            scale(gravel_road),
          data = coyote_data,
          family = binomial)

# coyote use of wide features depends on presence of competitors
H2 <- glm(
  cbind(coyote, absent_coyote) ~
    scale(infrastructure_line) +
    scale(gravel_road) +
    scale(infrastructure_line):scale(grey_wolf) +
    scale(gravel_road):scale(grey_wolf),
  data = coyote_data,
  family = binomial)

# coyotes use of wide features also varies with presence of prey
H3 <- glm(
  cbind(coyote, absent_coyote) ~
    scale(infrastructure_line) +
    scale(gravel_road) +
    scale(snowshoe_hare) +
    scale(white_tailed_deer) +
    scale(moose),
  data = coyote_data,
  family = binomial)

# coyotes like narrow features
H4 <- glm(
  cbind(coyote, absent_coyote) ~
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = coyote_data,
  family = binomial)

# coyote use of narrow features depends on presence of competitors
H5 <- glm(
  cbind(coyote, absent_coyote) ~
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(trail):scale(grey_wolf) +
    scale(seismic_line):scale(grey_wolf) +
    scale(road_unimproved):scale(grey_wolf),
  data = coyote_data,
  family = binomial)

# coyote use of narrow features also varies with presence of prey
H6 <- glm(
  cbind(coyote, absent_coyote) ~
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(snowshoe_hare) +
    scale(white_tailed_deer) +
    scale(moose),
  data = coyote_data,
  family = binomial)

# coyotes like wide and narrow features
H7 <- glm(
  cbind(coyote, absent_coyote) ~
    scale(transmission_line) +
    scale(gravel_road) +
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = coyote_data,
  family = binomial)

# coyotes like natural features
H8 <- glm(
  cbind(coyote, absent_coyote) ~
    scale(water) +
    scale(shrub) +
    scale(grass) +
    scale(forest),
  data = coyote_data,
  family = binomial)


# STEP 3 - TEST ASSUMPTIONS -----------------------------------------------

# Levene's test is for groups - we don't have factor data, so skip

# test for residual deviance - proportional binomial does NOT assume even dispersion so ALL GOOD DAWG

# STEP 4 - MODEL SELECTION -------------------------------------------

model.sel(H0, H1, H2, H3, H4, H5, H6, H7, H8)

# model 3 (wide features + prey) = best-performing, BUT not by 2 AICc
# model 8 (natural features) = second best

# STEP 5 - INTERPRETING MODEL OUTPUTS ------------------------------------------------

# odds ratios for H3
H3_odds <- tidy(H3,
       exponentiate = TRUE,
       confint.int = TRUE) %>% 
  
  # bind estimates + confidence intervals from model
  cbind(exp(confint(H3))) %>% 
  
  # change format to a tibble so works nicely with ggplot
  as_tibble() %>% 
  
  rename(lower = '2.5 %',
         upper = '97.5 %') %>% 
  
  filter(term != '(Intercept)')

# plot odds ratios for H3
plot_1 <- ggplot(data = H3_odds,aes(x = term, y = estimate)) +
    
    # add points for odds
    geom_point() +
  
  # add error bars = confidence intervals
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                linewidth = 0.5,
                width = 0.4) +
  
  geom_hline(yintercept = 1,
             alpha = 0.5) +
  
  # rename the x axis labels
  scale_x_discrete(labels = c('transmission line',
                              'gravel roads',
                              'snowshoe hare',
                              'white-tailed deer',
                              'moose')) +
  
  # axis titles
  ylab('odds ratio') +
  
  # flip x and y axis 
  coord_flip() +
  
  # specify theme
  theme_bw() +
  
  # specify theme elements
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

plot_1 # shows that gravel roads have negative effect, moose + snowshoe hare detections and transmission lines have positive effect on coyote occurrence; white-tailed deer detections have a smaller impact (cross over 1 line)