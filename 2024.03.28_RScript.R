# #0. Helpful websites ----------------------------------------------------

# Data Transformation in R (R for data science 2e): https://r4ds.hadley.nz/data-transform

# Marissa's tutorial website: https://marissadyck.github.io/ES482-R-labs.github.io/index.html

# 1. Getting everything set up --------------------------------------------

# 1a. Install packages
library(tidyverse)
library(ggpubr)
library(PerformanceAnalytics)
library(car)
library(gridExtra)
library(MASS)

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

# 1d. Bring together data you are interested in for coyote project (based on initial group interest)

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
  ggplot(mapping = aes(x = pipeline, y = coy_tot_det, color = site)) +
  geom_point() 

plot_2 <- coyote_data %>% 
  ggplot(mapping = aes(x = transmission_line, y = coy_tot_det, color = site)) +
  geom_point() 

plot_3 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_gravel_1l, y = coy_tot_det, color = site)) +
  geom_point() 

plot_4 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_gravel_2l, y = coy_tot_det, color = site)) +
  geom_point() 

plot_5 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_2l, y = coy_tot_det, color = site)) +
  geom_point() 

plot_6 <- coyote_data %>% 
  ggplot(mapping = aes(x = rlwy_sgl_track, y = coy_tot_det, color = site)) +
  geom_point() 

plot_7 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_1l, y = coy_tot_det, color = site)) +
  geom_point() 

plot_8 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_1l, y = coy_tot_det, color = site)) +
  geom_point() 

plot_9 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_winter, y = coy_tot_det, color = site)) +
  geom_point() 

figure_1 <- ggarrange(plot_1,
                      plot_2,
                      plot_3,
                      plot_4,
                      plot_5,
                      plot_6,
                      plot_7,
                      plot_8,
                      plot_9)
figure_1

# 2b. Plotting coyote proportional against each wide feature (i don't think this is a good way to visualize coyote proportional)
plot_10 <- coyote_data %>% 
  ggplot(mapping = aes(x = pipeline, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_11 <- coyote_data %>% 
  ggplot(mapping = aes(x = transmission_line, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_12 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_gravel_1l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_13 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_gravel_2l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_14 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_2l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_15 <- coyote_data %>% 
  ggplot(mapping = aes(x = rlwy_sgl_track, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_16 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_1l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_17 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_paved_undiv_1l, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

plot_18 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_winter, y = coy_prop, color = site)) +
  geom_point() +
  facet_wrap(~site)

figure_2 <- ggarrange(plot_10,
             plot_11,
             plot_12,
             plot_13,
             plot_14,
             plot_15,
             plot_16,
             plot_17,
             plot_18)
figure_2 # Looks similar to 1, maybe not relevant to plot the proportion if it looks similar (or at least what I did for proportion)

# 2c. Coyote total vs. natural feats 

plot_19 <- coyote_data %>% 
  ggplot(mapping = aes(x = water, y = coy_tot_det, color = site)) +
  geom_point() 

plot_20 <- coyote_data %>% 
  ggplot(mapping = aes(x = shrub, y = coy_tot_det, color = site)) +
  geom_point()

plot_21 <- coyote_data %>% 
  ggplot(mapping = aes(x = grass, y = coy_tot_det, color = site)) +
  geom_point()

plot_22 <- coyote_data %>% 
  ggplot(mapping = aes(x = conifer, y = coy_tot_det, color = site)) +
  geom_point() 

plot_23 <- coyote_data %>% 
  ggplot(mapping = aes(x = broadleaf, y = coy_tot_det, color = site)) +
  geom_point() 

plot_24 <- coyote_data %>% 
  ggplot(mapping = aes(x = mixed_forest, y = coy_tot_det, color = site)) +
  geom_point() 

figure_3 <- ggarrange(plot_19,
                      plot_20,
                      plot_21,
                      plot_22,
                      plot_23,
                      plot_24)

figure_3

# 2d. Coyote total vs. narrow features
plot_25 <- coyote_data %>% 
  ggplot(mapping = aes(x = trail, y = coy_tot_det, color = site)) +
  geom_point() 

plot_26 <- coyote_data %>% 
  ggplot(mapping = aes(x = conventional_seismic, y = coy_tot_det, color = site)) +
  geom_point()

plot_27 <- coyote_data %>% 
  ggplot(mapping = aes(x = low_impact_seismic, y = coy_tot_det, color = site)) +
  geom_point()

plot_28 <- coyote_data %>% 
  ggplot(mapping = aes(x = vegetated_edge_roads, y = coy_tot_det, color = site)) +
  geom_point() 

plot_29 <- coyote_data %>% 
  ggplot(mapping = aes(x = vegetated_edge_railways, y = coy_tot_det, color = site)) +
  geom_point() 

plot_30 <- coyote_data %>% 
  ggplot(mapping = aes(x = truck_trail, y = coy_tot_det, color = site)) +
  geom_point() 

plot_31 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_unclassified, y = coy_tot_det, color = site)) +
  geom_point() 

plot_32 <- coyote_data %>% 
  ggplot(mapping = aes(x = road_unimproved, y = coy_tot_det, color = site)) +
  geom_point() 

figure_4 <- ggarrange(plot_25,
                      plot_26,
                      plot_27,
                      plot_28,
                      plot_29,
                      plot_30,
                      plot_31,
                      plot_32)

figure_4

# 2e. Coyote total vs each mammal
plot_33 <- coyote_data %>% 
  ggplot(mapping = aes(x = moose_tot_det, y = coy_tot_det, color = site)) +
  geom_point() 

plot_34 <- coyote_data %>% 
  ggplot(mapping = aes(x = hare_tot_det, y = coy_tot_det, color = site)) +
  geom_point()

plot_35 <- coyote_data %>% 
  ggplot(mapping = aes(x = deer_tot_det, y = coy_tot_det, color = site)) +
  geom_point()

plot_36 <- coyote_data %>% 
  ggplot(mapping = aes(x = wolf_tot_det, y = coy_tot_det, color = site)) +
  geom_point() 

figure_5 <- ggarrange(plot_33,
                      plot_34,
                      plot_35,
                      plot_36)

figure_5

# From the figures above, it looks like the variables that could be of interest are: Wide - pipeline, road_gravel_1l; narrow - trail, conventional_seismic, vegetated_edge_roads; prey - moose, white-tailed_deer, hare; competitor - wolf. 

# 2f.  How correlated are these variables?
chart.Correlation(coyote_data[c("pipeline", "road_gravel_1l", "trail", "conventional_seismic", "vegetated_edge_roads", "wolf_tot_det", "deer_tot_det", "moose_tot_det", "coy_tot_det", "hare_tot_det", "coy_prop_abs", "coy_prop_pres", "wolf_prop_pres", "wolf_prop_abs", "deer_prop_abs", "deer_prop_pres", "moose_prop_pres", "moose_prop_abs")], 
                  histogram = TRUE, 
                  method = "pearson") # pearson is wrong because it asseses linear relationships and we don't have that, lol

# --- Notes of things you would like to try still: 
#2g. Rethink how you are doing coyote prop, could you compare presence vs. absence?
#2h. Try all mammals for presence
#2i. Try all features for presence
#2j. Scratch everything you consider to be for the coyote project as currently imagined and visualize the covariates and mammal data

# Motoring forward in the name of avoiding a rabbit hole

# 3. Generalized linear models, Poisson for count data --------------------------------------------

# Anticipated models 
# H0: Null, coyotes ~ 

# H1: Coyotes like wide features, coyotes ~ pipeline + road_gravel_1l

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ pipeline + road_gravel_1l + pipeline:wolf_tot_det + road_gravel_1l:wolf_tot_det

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det

# H4: Coyotes like narrow features, coyote ~ trail + conventional_seismic + vegetated_edge_roads

# H5: Coyote use of narrow features depends on presence of competitors, coyote ~ trail + conventional_seismic + vegetated_edge_roads + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + conventional_seismic + vegetated_edge_roads + deer + hare + moose

# H7: Coyotes like both wide and narrow features, coyote ~ pipeline + road_gravel_1l + trail + conventional seismic + vegetated_edge_roads

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + conifer + broadleaf + mixed_forest

# Okay, so are we violating any assumptions? Only test one before, rest at the end.

# Assumption 1 - model variables are independent
chart.Correlation(coyote_data[c("pipeline", "road_gravel_1l", "trail", "conventional_seismic", "vegetated_edge_roads", "wolf_tot_det", "deer_tot_det", "moose_tot_det", "coy_tot_det", "hare_tot_det", "water", "shrub", "grass", "conifer", "broadleaf", "mixed_forest")], 
                  histogram = TRUE, 
                  method = "spearman")
### Result: none of the covariates are highly correlated, aka r^2 > 0.7


# GLM TIME https://marissadyck.github.io/ES482-R-labs.github.io/mo --------


# H0: Null
glm_H0 <- glm(coy_tot_det ~ 1,
              data = coyote_data,
              family = poisson)

summary(glm_H0)

# H1: Coyotes like wide features, coyotes ~ pipeline + road_gravel_1l
glm_H1 <- glm(coy_tot_det ~ pipeline + road_gravel_1l,
              data = coyote_data,
              family = poisson)

summary(glm_H1)

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ pipeline + road_gravel_1l + pipeline:wolf + road_gravel_1l:wolf
glm_H2 <- glm(coy_tot_det ~ pipeline + road_gravel_1l + pipeline:wolf_tot_det + road_gravel_1l:wolf_tot_det,
              data = coyote_data,
              family = poisson)

summary(glm_H2)

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det
glm_H3 <- glm(coy_tot_det ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det,
              data = coyote_data,
              family = poisson)

summary(glm_H3)

# H4: Coyotes like narrow features, coyote ~ trail + conventional_seismic + vegetated_edge_roads
glm_H4 <- glm(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads,
              data = coyote_data,
              family = poisson)

summary(glm_H4)

# H5: Coyote use of narrow features depends on presence of competitors, coyote ~ trail + conventional_seismic + vegetated_edge_roads + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det
glm_H5 <- glm(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det,
              data = coyote_data,
              family = poisson)

summary(glm_H5)

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + conventional_seismic + vegetated_edge_roads + deer + hare + moose
glm_H6 <- glm(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + deer_tot_det + hare_tot_det + moose_tot_det,
              data = coyote_data,
              family = poisson)

summary(glm_H6)

# H7: Coyotes like both wide and narrow features, coyote ~ pipeline + road_gravel_1l + trail + conventional seismic + vegetated_edge_roads
glm_H7 <- glm(coy_tot_det ~ pipeline + road_gravel_1l + trail + conventional_seismic + vegetated_edge_roads,
              data = coyote_data,
              family = poisson)

summary(glm_H7)

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + conifer + broadleaf + mixed_forest
glm_H8 <- glm(coy_tot_det ~ water + shrub + grass + conifer + broadleaf + mixed_forest,
              data = coyote_data,
              family = poisson)

summary(glm_H8)


# GLM assumptions continued  ----------------------------------------------


# Assumption 2 - homogeneity of variance using Levene's test
# Specify the variables you want to test
vars_to_test <- c("pipeline", "road_gravel_1l", "trail", "conventional_seismic", "vegetated_edge_roads", "wolf_tot_det", "deer_tot_det", "moose_tot_det", "coy_tot_det", "hare_tot_det", "water", "shrub", "grass", "conifer", "broadleaf", "mixed_forest")

# Perform Levene's test for homogeneity of variances
levene_results <- leveneTest(as.formula(paste0("~", paste(vars_to_test, collapse = "+"))), data = coyote_data)

## I get an error that says "Levene's test is not appropriate for quantitative explanatory variables." I don't think that we have categorical variables or groups, so moving on

# Assumption 3 - Normality of residuals 

# Calculate chi square for residual deviance for each glm
# H0
summary(glm_H0)
# = residual deviance / degrees of freedom
1593.5/154
# = 10, so overdispersed

# H1 
summary(glm_H1)
# = residual deviance / degrees of freedom
1386.9/149
# = 9.3081, so overdispersed 

# H2
summary(glm_H2)
# = residual deviance / degrees of freedom
1356.3/147
# = 9.2265 

# H3
summary(glm_H3)
# = residual deviance / degrees of freedom
1011.9/146
# = 6.9308

# H4
summary(glm_H4)
# = residual deviance / degrees of freedom
1395.3/148
# = 9.4277

# H5
summary(glm_H5)
# = residual deviance / degrees of freedom
1385.2/145
# = 9.5531

# H6
summary(glm_H6)
# = residual deviance / degrees of freedom
945.55/145
# = 6.5210

# H7
summary(glm_H7)
# = residual deviance / degrees of freedom
1318.2/146
# = 9.0288

# H8
summary(glm_H8)
# = residual deviance / degrees of freedom
1365.0/145
# = 9.4138

# Recap: So yeah, everything is overdispersed so redo the glm's as negative binomial model family (https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/count_data_and_overdispersion.html)


# Redoing all the glms but this time as negative binomial  ----------------

# H0: Null
glm_H0_2 <- glm.nb(coy_tot_det ~ 1,
                   data = coyote_data,
                   link = "log")

summary(glm_H0_2)

# Chi square residual deviance
# = residual deviance / degrees of freedom
170.78/154
# = 1.1090

# H1: Coyotes like wide features, coyotes ~ pipeline + road_gravel_1l
glm_H1_2 <- glm.nb(coy_tot_det ~ pipeline + road_gravel_1l,
              data = coyote_data,
              link = "log")

summary(glm_H1_2)

# Chi square residual deviance
# = residual deviance / degrees of freedom
169.02/149
# = 1.1344

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ pipeline + road_gravel_1l + pipeline:wolf + road_gravel_1l:wolf
glm_H2_2 <- glm.nb(coy_tot_det ~ pipeline + road_gravel_1l + pipeline:wolf_tot_det + road_gravel_1l:wolf_tot_det,
              data = coyote_data,
              link = "log")

summary(glm_H2_2)
# Chi square residual deviance
# = residual deviance / degrees of freedom
169.11/147
# = 1.1505

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det
glm_H3_2 <- glm.nb(coy_tot_det ~ pipeline + road_gravel_1l + deer_tot_det + hare_tot_det + moose_tot_det,
              data = coyote_data,
              link = "log")

summary(glm_H3_2)
###Result: Warning messages:
#1: glm.fit: algorithm did not converge 
#2: In glm.nb(coy_tot_det ~ pipeline + road_gravel_1l + deer_tot_det +  :
#               alternation limit reached

# Chi square residual deviance
# = residual deviance / degrees of freedom
171.70/146
# = 1.176027

# H4: Coyotes like narrow features, coyote ~ trail + conventional_seismic + vegetated_edge_roads
glm_H4_2 <- glm.nb(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads,
              data = coyote_data,
              link = "log")

summary(glm_H4_2)
# Chi square residual deviance
# = residual deviance / degrees of freedom
168.76/148
# = 1.1403

# H5: Coyote use of narrow features depends on presence of competitors, coyote ~ trail + conventional_seismic + vegetated_edge_roads + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det
glm_H5_2 <- glm.nb(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + trail:wolf_tot_det + conventional_seismic:wolf_tot_det + vegetated_edge_roads:wolf_tot_det,
              data = coyote_data,
              link = "log")

summary(glm_H5_2)
# Chi square residual deviance
# = residual deviance / degrees of freedom
168.73/145
# = 1.1637

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + conventional_seismic + vegetated_edge_roads + deer + hare + moose
glm_H6_2 <- glm.nb(coy_tot_det ~ trail + conventional_seismic + vegetated_edge_roads + deer_tot_det + hare_tot_det + moose_tot_det,
              data = coyote_data,
              link = "log")

summary(glm_H6_2)
# Chi square residual deviance
# = residual deviance / degrees of freedom
172.19/145
# = 1.1875

# H7: Coyotes like both wide and narrow features, coyote ~ pipeline + road_gravel_1l + trail + conventional seismic + vegetated_edge_roads
glm_H7_2 <- glm.nb(coy_tot_det ~ pipeline + road_gravel_1l + trail + conventional_seismic + vegetated_edge_roads,
              data = coyote_data,
              link = "log")

summary(glm_H7_2)
# Chi square residual deviance
# = residual deviance / degrees of freedom
168.99/146
# = 1.1575

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + conifer + broadleaf + mixed_forest
glm_H8_2 <- glm.nb(coy_tot_det ~ water + shrub + grass + conifer + broadleaf + mixed_forest,
              data = coyote_data,
              link = "log")

summary(glm_H8_2)
# Chi square residual deviance
# = residual deviance / degrees of freedom
168.86/145
# = 1.1646

# Assumption 4 - Influential observations. **Swing back to this after getting things rolling.

# Interpreting GLM output -------------------------------------------------

# 
