# Graphing!

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
library(pROC)

# redoing exploration with total detections and poisson glm

project_data2 <- covariates %>% 
  # filter for buffer distance of 1000m only
  filter(buff_dist == 1000) %>% 
  # select all potential features of interest
  dplyr::select(site, array, camera, pipeline, transmission_line, road_gravel_1l, road_gravel_2l, road_paved_undiv_2l, road_paved_undiv_1l, rlwy_sgl_track, road_paved_1l, road_winter, trail, conventional_seismic, low_impact_seismic, vegetated_edge_roads, vegetated_edge_railways, truck_trail, road_unclassified, road_unimproved, lc_class20, lc_class50, lc_class110, lc_class210, lc_class220, lc_class230) %>% 
  # join proportional detection data to covariates
  right_join(total_detections,
             by = 'site') %>% 
  # renaming columns to get some order in place
  rename(water = lc_class20,
         shrub = lc_class50,
         grass = lc_class110,
         broadleaf = lc_class220,
         mixed_forest = lc_class230,
         conifer = lc_class210) %>%
  na.omit()

project_data2 <- project_data2 %>%
  dplyr::select(site, array, camera, 
                # animals
                coyote, snowshoe_hare, moose, `white-tailed_deer`, grey_wolf,
                # linear features
                pipeline, transmission_line, road_gravel_1l, road_gravel_2l, trail, conventional_seismic, low_impact_seismic, road_unimproved,
                # natural features
                water, shrub, grass, conifer, broadleaf, mixed_forest) %>% 
  mutate(
    gravel_road = road_gravel_2l + road_gravel_1l,
    seismic_line = conventional_seismic + low_impact_seismic,
    infrastructure_line = transmission_line + pipeline,
    forest = conifer + broadleaf + mixed_forest,
  ) %>%
  rename(wolf = grey_wolf,
         deer = 'white-tailed_deer',
         hare = snowshoe_hare)


# poisson -----------------------------------------------------------------


H0 <- glm(coyote ~ 1,
          data = project_data2,
          family = poisson)

summary(H0)
#Residual deviance: 654.45  on 146  degrees of freedom
#AIC: 926.33

# H1: Coyotes like wide features, coyotes ~ infrastructure_line + gravel_road
H1 <- glm(coyote ~ 
            scale(infrastructure_line) + 
            scale(gravel_road),
          data = project_data2,
          family = poisson)

summary(H1)
#Residual deviance: 541.08  on 144  degrees of freedom
#AIC: 816.96

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ infrastructure_line + gravel_road + infrastructure_line:wolf_tot_det + gravel_road:wolf_tot_det
H2 <- glm(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(infrastructure_line):scale(wolf) +
    scale(gravel_road):scale(wolf),
  data = project_data2,
  family = poisson)

summary(H2)
#Residual deviance: 540.26  on 142  degrees of freedom
#AIC: 820.13

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ infrastructure_line + gravel_road + deer_tot_det + hare_tot_det + moose_tot_det
H3 <- glm(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(deer) +
    scale(hare) +
    scale(moose),
  data = project_data2,
  family = poisson)

summary(H3)
#Residual deviance: 401.59  on 141  degrees of freedom
#AIC: 683.46

# H4: Coyotes like narrow features, coyote ~ trail + seismic_line + road_unimproved
H4 <- glm(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = project_data2,
  family = poisson)

summary(H4)
#Residual deviance: 604.42 on 143  degrees of freedom
#AIC: 882.29

# H5: Coyote use of narrow features depends on presence of a competitor, coyote ~ trail + seismic_line  + road_unimproved + trail:wolf_tot_det + seismic_line:wolf_tot_det + road_unimproved:wolf_tot_det
H5 <- glm(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(trail):scale(wolf) +
    scale(seismic_line):scale(wolf) +
    scale(road_unimproved):scale(wolf),
  data = project_data2,
  family = poisson)

summary(H5)
#Residual deviance: 599.09  on 140  degrees of freedom
#AIC: 882.96

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + seismic_line + road_unimproved + deer_tot_det + hare_tot_det + moose_tot_det
H6 <- glm(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(deer) +
    scale(hare) +
    scale(moose),
  data = project_data2,
  family = poisson)

summary(H6)
#Residual deviance: 424.96  on 140  degrees of freedom
#AIC: 708.83

# H7: Coyotes like both wide and narrow features, coyote ~ infrastructure_line + gravel_road + trail + seismic_line + road_unimproved
H7 <- glm(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = project_data2,
  family = poisson)

summary(H7)
#Residual deviance: 476.83  on 141  degrees of freedom
#AIC: 758.7

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + forest
H8 <- glm(
  coyote ~ 
    scale(water) +
    scale(shrub) +
    scale(grass) +
    scale(forest),
  data = project_data2,
  family = poisson)

summary(H8)  

model.sel(H0,H1,H2,H3,H4,H5,H6,H7,H8)

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
  scale_x_discrete(labels = c('infrastructure lines',
                              'gravel roads',
                              'snowshoe hares',
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

plot_1 


# neg binomial ------------------------------------------------------------

# redoing exploration with total detections and poisson glm

species_presence <- read_csv("data/raw/OSM_2022_species_presence.csv")  %>% 
  setNames(
    names(.) %>% 
      tolower() %>% 
      gsub(" ", "_", .))

project_data3 <- covariates %>% 
  # filter for buffer distance of 1000m only
  filter(buff_dist == 1000) %>% 
  # select all potential features of interest
  dplyr::select(site, array, camera, pipeline, transmission_line, road_gravel_1l, road_gravel_2l, road_paved_undiv_2l, road_paved_undiv_1l, rlwy_sgl_track, road_paved_1l, road_winter, trail, conventional_seismic, low_impact_seismic, vegetated_edge_roads, vegetated_edge_railways, truck_trail, road_unclassified, road_unimproved, lc_class20, lc_class50, lc_class110, lc_class210, lc_class220, lc_class230) %>% 
  # join proportional detection data to covariates
  right_join(species_presence,
             by = 'site') %>% 
  # renaming columns to get some order in place
  rename(water = lc_class20,
         shrub = lc_class50,
         grass = lc_class110,
         broadleaf = lc_class220,
         mixed_forest = lc_class230,
         conifer = lc_class210) %>%
  na.omit()

project_data3 <- project_data3 %>%
  dplyr::select(site, array, camera, 
                # animals
                coyote, snowshoe_hare , moose, `white-tailed_deer`, grey_wolf,
                # linear features
                pipeline, transmission_line, road_gravel_1l, road_gravel_2l, trail, conventional_seismic, low_impact_seismic, road_unimproved,
                # natural features
                water, shrub, grass, conifer, broadleaf, mixed_forest) %>% 
  mutate(
    gravel_road = road_gravel_2l + road_gravel_1l,
    seismic_line = conventional_seismic + low_impact_seismic,
    infrastructure_line = transmission_line + pipeline,
    forest = conifer + broadleaf + mixed_forest,
  ) %>%
  rename(wolf = grey_wolf,
         deer = 'white-tailed_deer',
         hare = snowshoe_hare)


# models

H0 <- glm(coyote ~ 1,
          data = project_data3,
          family = binomial)

summary(H0)
#Residual deviance: 654.45  on 146  degrees of freedom
#AIC: 926.33

# H1: Coyotes like wide features, coyotes ~ infrastructure_line + gravel_road
H1 <- glm(coyote ~ 
            scale(infrastructure_line) + 
            scale(gravel_road),
          data = project_data3,
          family = binomial)

summary(H1)
#Residual deviance: 541.08  on 144  degrees of freedom
#AIC: 816.96

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ infrastructure_line + gravel_road + infrastructure_line:wolf_tot_det + gravel_road:wolf_tot_det
H2 <- glm(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(infrastructure_line):scale(wolf) +
    scale(gravel_road):scale(wolf),
  data = project_data3,
  family = binomial)

summary(H2)
#Residual deviance: 540.26  on 142  degrees of freedom
#AIC: 820.13

# H3: Coyote use of wide features varies with the presence of prey, coyote ~ infrastructure_line + gravel_road + deer_tot_det + hare_tot_det + moose_tot_det
H3 <- glm(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(deer) +
    scale(hare) +
    scale(moose),
  data = project_data3,
  family = binomial)

summary(H3)
#Residual deviance: 401.59  on 141  degrees of freedom
#AIC: 683.46

# H4: Coyotes like narrow features, coyote ~ trail + seismic_line + road_unimproved
H4 <- glm(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = project_data3,
  family = binomial)

summary(H4)
#Residual deviance: 604.42 on 143  degrees of freedom
#AIC: 882.29

# H5: Coyote use of narrow features depends on presence of a competitor, coyote ~ trail + seismic_line  + road_unimproved + trail:wolf_tot_det + seismic_line:wolf_tot_det + road_unimproved:wolf_tot_det
H5 <- glm(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(trail):scale(wolf) +
    scale(seismic_line):scale(wolf) +
    scale(road_unimproved):scale(wolf),
  data = project_data3,
  family = binomial)

summary(H5)
#Residual deviance: 599.09  on 140  degrees of freedom
#AIC: 882.96

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + seismic_line + road_unimproved + deer_tot_det + hare_tot_det + moose_tot_det
H6 <- glm(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(deer) +
    scale(hare) +
    scale(moose),
  data = project_data3,
  family = binomial)

summary(H6)
#Residual deviance: 424.96  on 140  degrees of freedom
#AIC: 708.83

# H7: Coyotes like both wide and narrow features, coyote ~ infrastructure_line + gravel_road + trail + seismic_line + road_unimproved
H7 <- glm(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = project_data3,
  family = binomial)

summary(H7)
#Residual deviance: 476.83  on 141  degrees of freedom
#AIC: 758.7

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + forest
H8 <- glm(
  coyote ~ 
    scale(water) +
    scale(shrub) +
    scale(grass) +
    scale(forest),
  data = project_data3,
  family = binomial)

summary(H8)  

model.sel(H0,H1,H2,H3,H4,H5,H6,H7,H8)

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
  scale_x_discrete(labels = c('infrastructure lines',
                              'gravel roads',
                              'snowshoe hares',
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

plot_1 



# neg binomial total detections!! ------------------------------------------------------------

# models

H0 <- glm.nb(coyote ~ 1,
          data = project_data2,
          link = "log")

summary(H0)


# H1: Coyotes like wide features, coyotes ~ infrastructure_line + gravel_road
H1 <- glm.nb(coyote ~ 
            scale(infrastructure_line) + 
            scale(gravel_road),
          data = project_data2,
          link = "log")

summary(H1)

# H2: Coyote use of wide features depends on the presence of wolf competitors, coyote ~ infrastructure_line + gravel_road + infrastructure_line:wolf_tot_det + gravel_road:wolf_tot_det
H2 <- glm.nb(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(infrastructure_line):scale(wolf) +
    scale(gravel_road):scale(wolf),
  data = project_data2,
  link = "log")

summary(H2)


# H3: Coyote use of wide features varies with the presence of prey, coyote ~ infrastructure_line + gravel_road + deer_tot_det + hare_tot_det + moose_tot_det
H3 <- glm.nb(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(deer) +
    scale(hare) +
    scale(moose),
  data = project_data2,
  link = "log")

summary(H3)
#Residual deviance: 401.59  on 141  degrees of freedom
#AIC: 683.46

# H4: Coyotes like narrow features, coyote ~ trail + seismic_line + road_unimproved
H4 <- glm.nb(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = project_data2,
  link = "log")

summary(H4)
#Residual deviance: 604.42 on 143  degrees of freedom
#AIC: 882.29

# H5: Coyote use of narrow features depends on presence of a competitor, coyote ~ trail + seismic_line  + road_unimproved + trail:wolf_tot_det + seismic_line:wolf_tot_det + road_unimproved:wolf_tot_det
H5 <- glm.nb(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(trail):scale(wolf) +
    scale(seismic_line):scale(wolf) +
    scale(road_unimproved):scale(wolf),
  data = project_data2,
  link = "log")

summary(H5)
#Residual deviance: 599.09  on 140  degrees of freedom
#AIC: 882.96

# H6: Coyote use of narrow features also varies with the presence of prey, coyotes ~ trail + seismic_line + road_unimproved + deer_tot_det + hare_tot_det + moose_tot_det
H6 <- glm.nb(
  coyote ~ 
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved) +
    scale(deer) +
    scale(hare) +
    scale(moose),
  data = project_data2,
  link = "log")

summary(H6)
#Residual deviance: 424.96  on 140  degrees of freedom
#AIC: 708.83

# H7: Coyotes like both wide and narrow features, coyote ~ infrastructure_line + gravel_road + trail + seismic_line + road_unimproved
H7 <- glm.nb(
  coyote ~ 
    scale(infrastructure_line) + 
    scale(gravel_road) +
    scale(trail) +
    scale(seismic_line) +
    scale(road_unimproved),
  data = project_data2,
  link = "log")

summary(H7)
#Residual deviance: 476.83  on 141  degrees of freedom
#AIC: 758.7

# H8: Baseline world without humans, coyotes ~ water + shrub + grass + forest
H8 <- glm.nb(
  coyote ~ 
    scale(water) +
    scale(shrub) +
    scale(grass) +
    scale(forest),
  data = project_data2,
  link = "log")

summary(H8)  

model.sel(H0,H1,H2,H3,H4,H5,H6,H7,H8)

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
  scale_x_discrete(labels = c('infrastructure lines',
                              'gravel roads',
                              'snowshoe hares',
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

plot_1 


# Predicted probability -------------------------------------------------


# inverse logit of coefficients to get probabilities 
plogis(coefficients(H3))

H3_nb_predict <- expand.grid(coyote = seq(min(project_data2$coyote), 
                                               max(project_data2$coyote),
                                               by = 1),
                             infrastructure_line = mean(project_data2$infrastructure_line),
                             gravel_road = mean(project_data2$gravel_road),
                             deer = mean(project_data2$deer),
                             hare = mean(project_data2$hare),
                             moose = mean(project_data2$moose))

head(H3_nb_predict)

# use predict function to get predicted probabilities of coyote total detections based on our model
H3_nb_predict$pred <- predict(H3,
                              type = 'response',
                              newdata = H3_nb_predict)

# look at what we created
head(H3_nb_predict)

# use predict function to get predicted probabilities of cow damage based on our model
H3_nb_predict_2 <- predict(H3,
                           type = 'response',
                           se.fit = TRUE,
                           newdata = H3_nb_predict)

head(H3_nb_predict_2)

# graphings set up
H3_nb_predict_2 <- cbind(H3_nb_predict,
                         H3_nb_predict_2) %>% 
  
  # add column for lower and upper 95% CI using manual calculation from SE
  mutate(lwr = fit - (1.96*se.fit),
         upr = fit + (1.96*se.fit))

head(H3_nb_predict_2)

ggplot(data = H3_nb_predict_2, aes(x = coyote, y = fit)) +
  
  # add line for predicted prob
  geom_line() +
  
  # add error bar
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.5) # changes opacity so you can see the main line


# maddys code -------------------------------------------------------------

project_data2$pred_coyote <- predict(H3, type = "response") # unsure about this/ the math behind it, will look into it 

## infrastructure line ----

inf_line_plot <- ggplot(project_data2, aes(x = infrastructure_line, y = pred_coyote)) +
  geom_point() +  # Use geom_point to plot points
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +  # Add smoothed line based on the GLM
  labs(x = "Infrastructure Line",
       y = "Probability of Coyote Presence") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

inf_line_plot


# Plotting Models ---------------------------------------------------------

plot(H3)
# output graphs in figures folder

# area under curve
c.roccurve <- roc(project_data2$coyote, 
                  predict(H3, type = 'response'))

auc(c.roccurve)

plot.roc(c.roccurve, main="Area Under the Curve for best model (H3)")
