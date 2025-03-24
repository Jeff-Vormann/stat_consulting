

# Definiere die Funktion zur Berechnung der Effekte für einen angegebenen State
calculate_state_effects <- function(state = "mean_2") {
  
  cov_names <- c("ToD", "season", "evation", "temp")
  
  scale_params <- c(ToD = ToD_scale_param, 
                    season = season_scale_param, 
                    evation = evation_scale_param, 
                    temp = temp_scale_param)
  
  #Multiplikatoren und Szenario-Beschreibungen
  multipliers <- c(ToD = 2, season = 2, evation = 500, temp = 5)
  scenario_labels <- c(ToD = "Unterschied Mitternacht zu Mittag",
                       season = "Unterschied Winter zu Sommer",
                       evation = "500 m Höher",
                       temp = "5 Grad wärmer")
  
  # Extrahiere die Beta-Werte für den gewünschten State (z.B. "mean_2:ToD")
  beta_names <- paste0(state, ":", cov_names)
  beta_vals <- hmm_model$mle$step[1, beta_names]
  
  # Berechne den Effekt bei einer Änderung um 1 SD: exp(β)
  effect_sd <- sapply(cov_names, function(x) {
    beta_val <- hmm_model$mle$step[1, paste0(state, ":", x)]
    return(exp(beta_val))
  })
  
  # Berechne den szenariospezifischen Effekt:
  # Normiere den Beta-Wert (β/Scale), multipliziere mit dem Szenario-Multiplikator und exponentiiere dann.
  scenario_effect <- sapply(cov_names, function(x) {
    beta_val <- hmm_model$mle$step[1, paste0(state, ":", x)]
    multiplier <- multipliers[x]
    return(exp((beta_val / scale_params[x]) * multiplier))
  })
  
  # Erstelle eine Übersichtstabelle mit den Ergebnissen
  result <- data.frame(
    Covariate = cov_names,
    Beta = round(beta_vals, 6),
    Effekt_pro_SD = round(effect_sd, 6),
    Effekt_Szenario = round(scenario_effect, 6),
    Szenario = scenario_labels,
    stringsAsFactors = FALSE
  )
  
  return(result)
}