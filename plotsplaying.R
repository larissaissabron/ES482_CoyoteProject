# H3 prep -----
#Coyote use of wide features varies with the presence of prey, coyote ~ infrastructure_line + gravel_road + deer_tot_det + hare_tot_det + moose_tot_det
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

# calculate r^2 
#H3
summary(H3)
1 - (438.37/695.64)
# 0.3698321

model_selection <- model.sel(H0, H1, H2, H3, H4, H5, H6, H7, H8) 
model_selection
## H3 ODDS RATIO PLOT -----
# plot odds ratios for H3
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



# marissas code---- 
# line 1423 onward is useful!
# sticking it here for easy referencing 

dForest_graph <- 
  ggplot() +
  geom_line(data = new_data_dForest, 
            aes(x = dForest_raw, 
                y = pred_c),
            linetype = 'solid',
            size = 1) +
  coord_cartesian(ylim = c(0,1),
                  xlim = c(220,6850)) +
  labs(x = "Distance to Forest (m)",
       y = "Probability of Livestock Damage") +
  annotate("text", 
           x = 6900, 
           y = 1, 
           size = 7.5, 
           label = "(b)") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# view graph

dForest_graph






# Predict probabilities from the model
project_data$pred_coyote <- predict(H3, type = "response") # unsure about this/ the math behind it, will look into it 

## infrastructure line ----

inf_line_plot <- ggplot(project_data, aes(x = infrastructure_line, y = pred_coyote)) +
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

## gravel road ----

gravel_road_plot <- ggplot(project_data, aes(x = gravel_road, y = pred_coyote)) +
  geom_point() +  # Use geom_point to plot points
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +  # Add smoothed line based on the GLM
  labs(x = "Gravel Road",
       y = "Probability of Coyote Presence") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

## deer ----

deer_plot <- ggplot(project_data, aes(x = deer_tot_det, y = pred_coyote)) +
  geom_point() +  # Use geom_point to plot points
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +  # Add smoothed line based on the GLM
  labs(x = "deer",
       y = "Probability of Coyote Presence") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

## hare ----

hare_plot <- ggplot(project_data, aes(x = hare_tot_det, y = pred_coyote)) +
  geom_point() +  # Use geom_point to plot points
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +  # Add smoothed line based on the GLM
  labs(x = "Hare",
       y = "Probability of Coyote Presence") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

## moose ----

moose_plot <- ggplot(project_data, aes(x = moose_tot_det, y = pred_coyote)) +
  geom_point() +  # Use geom_point to plot points
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +  # Add smoothed line based on the GLM
  labs(x = "moose",
       y = "Probability of Coyote Presence") +
  theme(legend.position = "NONE",
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


figure_1 <- ggarrange(inf_line_plot, gravel_road_plot, deer_plot, hare_plot, moose_plot)

figure_1

## #2 infrastructure line -----
pred_data_infrastructure_line <- data.frame(
  infrastructure_line = project_data$infrastructure_line,
  gravel_road = mean(scale(project_data$gravel_road)),  # Set others to mean
  deer_tot_det = mean(scale(project_data$deer_tot_det)),
  hare_tot_det = mean(scale(project_data$hare_tot_det)),
  moose_tot_det = mean(scale(project_data$moose_tot_det))
)
pred_data_infrastructure_line$coyote_presence_prob <- predict(
  H3, 
  newdata = pred_data_infrastructure_line, 
  type = "response"
)

if_line_plot_2 <- ggplot(pred_data_infrastructure_line, aes(x = infrastructure_line, y = coyote_presence_prob)) +
  geom_line() +  
  labs(x = "Infrastructure Line", 
       y = "Probability of Coyote Presence") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1))

if_line_plot_2

## #2 gravel road -----
pred_data_gravel_road <- data.frame(
  gravel_road = project_data$gravel_road,  # Generate scaled sequence
  infrastructure_line = mean(scale(project_data$infrastructure_line)),  # Set others to mean
  deer_tot_det = mean(scale(project_data$deer_tot_det)),
  hare_tot_det = mean(scale(project_data$hare_tot_det)),
  moose_tot_det = mean(scale(project_data$moose_tot_det))
)
pred_data_gravel_road$coyote_presence_prob <- predict(
  H3, 
  newdata = pred_data_gravel_road, 
  type = "response"
)

gravel_road_plot_2 <- ggplot(pred_data_gravel_road, aes(x = gravel_road, y = coyote_presence_prob)) +
  geom_line() +  
  labs(x = "Gravel road", 
       y = "Probability of Coyote Presence") +
  theme_minimal()  + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1))

gravel_road_plot_2

## #2 deer -----
pred_data_deer_tot_det <- data.frame(
  deer_tot_det = project_data$deer_tot_det,
  infrastructure_line = mean(scale(project_data$infrastructure_line)),  # Set others to mean
  gravel_road = mean(scale(project_data$gravel_road)),
  hare_tot_det = mean(scale(project_data$hare_tot_det)),
  moose_tot_det = mean(scale(project_data$moose_tot_det))
)
pred_data_deer_tot_det$coyote_presence_prob <- predict(
  H3, 
  newdata = pred_data_deer_tot_det, 
  type = "response"
)

deer_plot_2 <- ggplot(pred_data_deer_tot_det, aes(x = deer_tot_det, y = coyote_presence_prob)) +
  geom_line() +  
  labs(x = "Deer", 
       y = "Probability of Coyote Presence") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1))

deer_plot_2



## #2 moose -----
pred_data_moose_tot_det <- data.frame(
  moose_tot_det = project_data$moose_tot_det,  # Generate scaled sequence
  infrastructure_line = mean(scale(project_data$infrastructure_line)),  # Set others to mean
  gravel_road = mean(scale(project_data$gravel_road)),
  hare_tot_det = mean(scale(project_data$hare_tot_det)),
  deer_tot_det = mean(scale(project_data$deer_tot_det))
)
pred_data_moose_tot_det$coyote_presence_prob <- predict(
  H3, 
  newdata = pred_data_moose_tot_det, 
  type = "response"
)

moose_plot_2 <- ggplot(pred_data_moose_tot_det, aes(x = moose_tot_det, y = coyote_presence_prob)) +
  geom_line() +  
  labs(x = "moose", 
       y = "Probability of Coyote Presence") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1))

moose_plot_2

###2 hare -----
pred_data_hare_tot_det <- data.frame(
  hare_tot_det = project_data$hare_tot_det, 
                           # max(project_data$hare_tot_det), 
                           # length.out = 100)),  # Generate scaled sequence
  infrastructure_line = mean(scale(project_data$infrastructure_line)),  # Set others to mean
  gravel_road = mean(scale(project_data$gravel_road)),
  moose_tot_det = mean(scale(project_data$moose_tot_det)),
  deer_tot_det = mean(scale(project_data$deer_tot_det))
)
pred_data_hare_tot_det$coyote_presence_prob <- predict(
  H3, 
  newdata = pred_data_hare_tot_det, 
  type = "response"
)

hare_plot_2 <- ggplot(pred_data_hare_tot_det, aes(x = hare_tot_det, y = coyote_presence_prob)) +
  geom_line() +  
  labs(x = "hare", 
       y = "Probability of Coyote Presence") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1))
hare_plot_2

figure_2 <- ggarrange(if_line_plot_2, gravel_road_plot_2, deer_plot_2, hare_plot_2, moose_plot_2)
figure_2

