# 1) Externes Preprocessing-Skript einbinden (dies liest config.yaml intern)
source("clean_code/data_preprocessing.R", local = FALSE)

# 2) Vorverarbeitete Daten holen (alles erfolgt über config.yaml)
hmm_data <- prepareHMMData()
print(hmm_data[40000:103940,])
tail(hmm_data[40000:103940,])
hmm_data_cut = hmm_data[40000:103940,]

# evation und temp müssen noch mal scaliert werden
evation_scaled <- scale(hmm_data_cut$evation)
evation_center_param_new <<- attr(evation_scaled, "scaled:center")  
evation_scale_param_new  <<- attr(evation_scaled, "scaled:scale")
evation_center_param <<- evation_center_param+ evation_center_param_new/evation_scale_param
evation_scale_param <<- evation_scale_param*evation_scale_param_new
hmm_data_cut$evation <- as.vector(evation_scaled)

temp_scaled <- scale(hmm_data_cut$temp)
temp_center_param_new <<- attr(temp_scaled, "scaled:center")  
temp_scale_param_new  <<- attr(temp_scaled, "scaled:scale")
temp_center_param <<- temp_center_param+ temp_center_param_new/temp_scale_param
temp_scale_param <<- temp_scale_param*temp_scale_param_new
hmm_data_cut$temp <- as.vector(temp_scaled)


rds_name = "Deer_summer_20_05_to_end.R"

# hier laden wir die gefitteten paramter aus der HMM die auf den gesammten daten trainiert wurde
# das normale modell muss sich im speicher befinden
hmm_model <- momentuHMM::fitHMM(
  data = hmm_data_cut,
  nbStates = config$nbStates,
  dist = list(step = "gamma"),
  formula = as.formula(config$formula),
  Par0 = list(step =hmm_model$mle$step),
  DM = list(step = list(mean = as.formula(config$emission_mean), sd = ~1, zeromass = ~1)),
  beta0 <- hmm_model$mle$beta,
  nlmPar = list(print.level = 2)
)

saveRDS(hmm_model, file = rds_name)
AIC(hmm_model)

#Nachvertarbeitung der HMM
hmm_data_cut$state <- momentuHMM::viterbi(hmm_model)
sm <- as.matrix(hmm_model$CIreal$step$est)
state_proportions <- prop.table(table(hmm_data_cut$state)) 
print(state_proportions)

#################################################################################
#########Erstelle Plot mit der Dichte############################################
#################################################################################

x_vals <- seq(min(hmm_data_cut$step, na.rm = TRUE), max(hmm_data_cut$step, na.rm = TRUE), length.out = 1000)
density_data <- data.frame(x  = x_vals)

for(i in 1:config$nbStates) {
  density_data[[paste0("State", i)]] <- dgamma(x_vals,
                                               shape = (sm["mean", i] / sm["sd", i])^2,
                                               scale = (sm["sd", i]^2) / sm["mean", i]
  ) * state_proportions[i] * (1 - sm["zeromass", i])
}

density_data$Summe <- rowSums(density_data[ , -1], na.rm = TRUE)

density_long <- density_data %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    names_prefix = "State",
    values_to = "Density"
  )

x_upper <- quantile(hmm_data_cut$step, probs = 0.99, na.rm = TRUE)
x_upper <- x_upper * 1.05
if(config$nbStates==3)
{max_in_hist=0.01}else{
  max_in_hist=0.1
}
# Plot: Histogramm + Dichtekurven
hist_with_density <- ggplot(hmm_data_cut, aes(x = step)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", alpha = 0.6) +
  geom_line(data = density_data, aes(x = x, y = Summe, color = "Summe"), linewidth = 1) +
  geom_line(data = density_long, aes(x = x, y = Density, color = State), linewidth = 1) +
  labs(
    x = config$preprocessing,
    y = "Density",
    color = "State"
  ) +
  scale_color_manual(values = c("All" = "black", "1" = "green", "2" = "blue", "3" = "red", "4" = "orange")) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, max_in_hist), xlim = c(0, x_upper)) +
  geom_vline(xintercept = 0, color = "yellow", linetype = "dashed", size = 1)

print(hist_with_density)

ggsave(
  filename = paste0(rds_name,"_hist", ".png"),
  plot = hist_with_density,
  width = 16,
  height = 9,
  dpi = 1000,
  bg = "white"
)

#################################################################################
#########Erstelle Scatter Plot############################################
#################################################################################
scatter_plot <- ggplot(hmm_data_cut, aes(x = time, y = step, color = factor(state))) +  
  scale_color_manual(values = c("Summe" = "black", "1" = "green", "2" = "blue", "3" = "red", "4" = "orange")) +
  geom_point(size = 1,alpha = 0.2) +
  labs(colour= "State", x = "Time", y = "Activity") +
  theme_minimal()

print(scatter_plot)

ggsave(
  filename = paste0(rds_name,"_scatter_plot", ".png"),
  plot = scatter_plot,
  width = 16,
  height = 9,
  dpi = 1000,
  bg = "white"
)
#################################################################################
#########Uebersicht der Ergebisse in Zahlen############################################
#################################################################################
print(rds_name)
#readRDS(hmm_model, file = rds_name)
sink("deer_summer.txt")
source("clean_code/transition_effects.R", local = FALSE)
source("clean_code/impact_mean.R", local = FALSE)
print(calculate_state_effects("mean_2"))
print(calculate_state_effects("mean_3"))

stationary_effect_all_3state()

print(calculate_transition_effects_for_3_states())
print(hmm_model)

sink()