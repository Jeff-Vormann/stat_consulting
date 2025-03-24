
#Ausgabe der Effekte einer Transiotion
calculate_transition_effects <- function(transition = "1 -> 2") {
  
  cov_names <- c("ToD", "season", "evation", "temp")
  
  scale_params <- c(ToD = ToD_scale_param, 
                    season = season_scale_param, 
                    evation = evation_scale_param, 
                    temp = temp_scale_param)
  
    multipliers <- c(ToD = 2, season = 2, evation = 500, temp = 5)
  
    scenario_labels <- c(ToD = "Unterschied Mitternacht zu Mittag",
                         season = "Unterschied Winter zu Sommer",
                         evation = "500 m Höher",
                         temp = "5 Grad wärmer")

  beta_vals <- hmm_model$mle$beta[cov_names,transition]
  
  # Berechne den Effekt pro Änderung um 1 Standardabweichung:
  # Hier: exp(beta / Scale)
  effect_sd <- exp(beta_vals)
  
  # Berechne den szenariospezifischen Effekt:
  # Zuerst normiere (beta / Scale), multipliziere mit dem jeweiligen Multiplikator und exponentiiere dann
  scenario_effect <- sapply(cov_names, function(x) {
    beta_val <- beta_vals[x]
    multiplier <- multipliers[x]
    return(exp((beta_val / scale_params[x]) * multiplier))
  })
  
  result <- data.frame(
    Covariate        = cov_names,
    Beta             = round(beta_vals, 6),
    Effekt_pro_SD    = round(effect_sd, 6),
    Effekt_Szenario  = round(scenario_effect, 6),
    Szenario         = scenario_labels,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

#Ausgabe der Tranzitionmatrix einer 2-Stage HMM mit fester Kovariate
#  - cov_name: Name der Covariate (z.b. ToD)
#  - sd: gibt an ob ein Punkt über oder unter dem Durchschnitt betrachtet werden soll
# wie betrachten +- sd bei Temp und  evation und die extrempunkte bei ToD und season
calculate_transition_matrix_2state <- function(cov_name,sd) {
  cov_names <- c("ToD", "season", "evation", "temp")
  s= 1
  if(cov_name=="season"){s= 1/season_scale_param}
  if(cov_name=="ToD"){s= 1/ToD_scale_param}
  beta_mat <- hmm_model$mle$beta[c("(Intercept)", "ToD", "season", "evation", "temp"), 
                                 c("1 -> 2", "2 -> 1")]
  if(sd=="+sd"){
  lp_12 <- beta_mat["(Intercept)", "1 -> 2"] + s*beta_mat[cov_name, "1 -> 2"] 
  lp_21 <- beta_mat["(Intercept)", "2 -> 1"] + s*beta_mat[cov_name, "2 -> 1"] 
  }else{
    lp_12 <- beta_mat["(Intercept)", "1 -> 2"] - s*beta_mat[cov_name, "1 -> 2"] 
    lp_21 <- beta_mat["(Intercept)", "2 -> 1"] - s*beta_mat[cov_name, "2 -> 1"] 
  }
  p_12 <- exp(lp_12) / (exp(lp_12) + 1)  
  p_21 <- exp(lp_21) / (exp(lp_21) + 1)  
  
  trans_mat <- matrix(c(1 - p_12, p_12, 
                        p_21,     1 - p_21), 
                      nrow = 2, byrow = TRUE)
  rownames(trans_mat) <- c("State 1", "State 2")
  colnames(trans_mat) <- c("to State 1", "to State 2")
  
  return(trans_mat)
}

#Ausgabe der Tranzitionmatrix einer 3-Stage HMM mit fester Kovariate
#  - cov_name: Name der Covariate (z.b. ToD)
#  - sd: gibt an ob ein Punkt über oder unter dem Durchschnitt betrachtet werden soll
# wie betrachten +- sd bei Temp und  evation und die extrempunkte bei ToD und season
calculate_transition_matrix_3state <- function(cov_name, sd) {
  cov_names <- c("ToD", "season", "evation", "temp")
  s= 1
  if(cov_name=="season"){s= 1/season_scale_param}
  if(cov_name=="ToD"){s= 1/ToD_scale_param}
  beta_mat <- hmm_model$mle$beta[c("(Intercept)", "ToD", "season", "evation", "temp"), 
                                 c("1 -> 2", "1 -> 3", "2 -> 1", "2 -> 3", "3 -> 1", "3 -> 2")]
  
  # Zustand 1: Übergänge 1 -> 2 und 1 -> 3
  if(sd == "+sd"){
    lp_12 <- beta_mat["(Intercept)", "1 -> 2"] + s*beta_mat[cov_name, "1 -> 2"]
    lp_13 <- beta_mat["(Intercept)", "1 -> 3"] + s*beta_mat[cov_name, "1 -> 3"]
  } else {
    lp_12 <- beta_mat["(Intercept)", "1 -> 2"] - s*beta_mat[cov_name, "1 -> 2"]
    lp_13 <- beta_mat["(Intercept)", "1 -> 3"] - s*beta_mat[cov_name, "1 -> 3"]
  }
  denom1 <- 1 + exp(lp_12) + exp(lp_13)
  p11 <- 1 / denom1          # 1 -> 1 
  p12 <- exp(lp_12) / denom1   # 1 -> 2
  p13 <- exp(lp_13) / denom1   # 1 -> 3
  
  # Zustand 2: Übergänge 2 -> 1 und 2 -> 3
  if(sd == "+sd"){
    lp_21 <- beta_mat["(Intercept)", "2 -> 1"] + s*beta_mat[cov_name, "2 -> 1"]
    lp_23 <- beta_mat["(Intercept)", "2 -> 3"] + s*beta_mat[cov_name, "2 -> 3"]
  } else {
    lp_21 <- beta_mat["(Intercept)", "2 -> 1"] - s*beta_mat[cov_name, "2 -> 1"]
    lp_23 <- beta_mat["(Intercept)", "2 -> 3"] - s*beta_mat[cov_name, "2 -> 3"]
  }
  denom2 <- 1 + exp(lp_21) + exp(lp_23)
  p22 <- 1 / denom2          # 2 -> 2
  p21 <- exp(lp_21) / denom2   # 2 -> 1
  p23 <- exp(lp_23) / denom2   # 2 -> 3
  
  # Zustand 3: Übergänge 3 -> 1 und 3 -> 2
  if(sd == "+sd"){
    lp_31 <- beta_mat["(Intercept)", "3 -> 1"] + s*beta_mat[cov_name, "3 -> 1"]
    lp_32 <- beta_mat["(Intercept)", "3 -> 2"] + s*beta_mat[cov_name, "3 -> 2"]
  } else {
    lp_31 <- beta_mat["(Intercept)", "3 -> 1"] - s*beta_mat[cov_name, "3 -> 1"]
    lp_32 <- beta_mat["(Intercept)", "3 -> 2"] - s*beta_mat[cov_name, "3 -> 2"]
  }
  denom3 <- 1 + exp(lp_31) + exp(lp_32)
  p33 <- 1 / denom3          # 3 -> 3
  p31 <- exp(lp_31) / denom3   # 3 -> 1
  p32 <- exp(lp_32) / denom3   # 3 -> 2
  
  # Baue die Übergangsmatrix zusammen: Zeilen = Ausgangszustand, Spalten = Zielzustand
  trans_mat <- rbind(
    "State 1" = c(p11, p12, p13),
    "State 2" = c(p21, p22, p23),
    "State 3" = c(p31, p32, p33)
  )
  colnames(trans_mat) <- c("to State 1", "to State 2", "to State 3")
  
  return(trans_mat)
}


calc_stationary <- function(P) {
  # Berechne die Eigenwerte und Eigenvektoren der transponierten Matrix,
  # denn für die stationäre Verteilung gilt: π * P = π, also ist π ein rechter Eigenvektor von t(P)
  eig <- eigen(t(P))
  # Finde den Index des Eigenwerts, der am nächsten an 1 liegt
  idx <- which.min(abs(eig$values - 1))
  # Extrahiere den zugehörigen Eigenvektor und stelle sicher, dass er reell ist
  stat <- Re(eig$vectors[, idx])
  # Normiere, sodass die Summe 1 ergibt
  stat <- stat / sum(stat)
  names(stat) <- rownames(P)
  return(stat)
}


# Erstellt eine übersichtlich Darstellung der Veränderung der stationären Verteilung
stationary_effect_2state <- function(cov_name) {
  P_plus  <- calculate_transition_matrix_2state(cov_name, "+sd")
  stat_plus <- calc_stationary(P_plus)
  
  P_minus <- calculate_transition_matrix_2state(cov_name, "-sd")
  stat_minus <- calc_stationary(P_minus)
  
  # Prozentuale Veränderung: ((plus - minus) / minus)*100
  perc_change <- ((stat_plus - stat_minus) / stat_plus) * 100
  result <- data.frame(
    State = names(stat_plus),
    Stationary_plus = round(stat_plus * 100, 2),   # in Prozent
    Stationary_minus = round(stat_minus * 100, 2),
    Percentage_change = -round(perc_change, 2)
  )
  if(cov_name == "temp"){
    temp_upper <- temp_center_param + temp_scale_param
    temp_lower <- temp_center_param - temp_scale_param
    names(result)[2] <- paste0("(", round(temp_upper,1), "°C)")
    names(result)[3] <- paste0("(", round(temp_lower,1), "°C)")
    names(result)[4] <- "Percentage_change"
  } else if(cov_name == "evation"){
    evation_upper <- evation_center_param + evation_scale_param
    evation_lower <- evation_center_param - evation_scale_param
    names(result)[2] <- paste0("(", round(evation_upper,1), " m)")
    names(result)[3] <- paste0("(", round(evation_lower,1), " m)")
    names(result)[4] <- "Percentage_change"
  } else if(cov_name == "season"){
    names(result)[2] <- "(Summer)"
    names(result)[3] <- "(Winter)"
    names(result)[4] <- "Percentage_change"
  } else if(cov_name == "ToD"){
    names(result)[2] <- "(Noon)"
    names(result)[3] <- "(Midnight)"
    names(result)[4] <- "Percentage_change"
  } else {
    names(result)[2] <- paste0("Stationary_", cov_name, "_plus")
    names(result)[3] <- paste0("Stationary_", cov_name, "_minus")
    names(result)[4] <- paste0("Percentage_change_", cov_name)
  }
  return(result)
}


# Funktion für 3‑Zustände
stationary_effect_3state <- function(cov_name) {
  P_plus  <- calculate_transition_matrix_3state(cov_name, "+sd")
  stat_plus <- calc_stationary(P_plus)
  
  P_minus <- calculate_transition_matrix_3state(cov_name, "-sd")
  stat_minus <- calc_stationary(P_minus)
  
  perc_change <- ((stat_plus - stat_minus) / stat_plus) * 100
  
  result <- data.frame(
    State = names(stat_plus),
    Stationary_plus = round(stat_plus * 100, 2),   # in Prozent
    Stationary_minus = round(stat_minus * 100, 2),
    Percentage_change = -round(perc_change, 2)
  )
  if(cov_name == "temp"){
    temp_upper <- temp_center_param + temp_scale_param
    temp_lower <- temp_center_param - temp_scale_param
    names(result)[2] <- paste0("(", round(temp_upper,1), "°C)")
    names(result)[3] <- paste0("(", round(temp_lower,1), "°C)")
    names(result)[4] <- "Percentage_change"
  } else if(cov_name == "evation"){
    evation_upper <- evation_center_param + evation_scale_param
    evation_lower <- evation_center_param - evation_scale_param
    names(result)[2] <- paste0("(", round(evation_upper,1), " m)")
    names(result)[3] <- paste0("(", round(evation_lower,1), " m)")
    names(result)[4] <- "Percentage_change"
  } else if(cov_name == "season"){
    names(result)[2] <- "(Summer)"
    names(result)[3] <- "(Winter)"
    names(result)[4] <- "Percentage_change"
  } else if(cov_name == "ToD"){
    names(result)[2] <- "(Noon)"
    names(result)[3] <- "(Midnight)"
    names(result)[4] <- "Percentage_change"
  } else {
    names(result)[2] <- paste0("Stationary_", cov_name, "_plus")
    names(result)[3] <- paste0("Stationary_", cov_name, "_minus")
    names(result)[4] <- paste0("Percentage_change_", cov_name)
  }
  return(result)
}

stationary_effect_all_2state <- function() {
  cov_list <- c("temp", "evation", "season", "ToD")
  results <- list()
  for(cov in cov_list) {
    results[[cov]] <- stationary_effect_2state(cov)
  }
  return(results)
}

stationary_effect_all_3state <- function() {
  cov_list <- c("temp", "evation", "season", "ToD")
  results <- list()
  for(cov in cov_list) {
    results[[cov]] <- stationary_effect_3state(cov)
  }
  return(results)
}



calculate_transition_effects_for_2_states <- function(){
  print("Impact on Odds of 1 -> 2")
  print(calculate_transition_effects("1 -> 2"))
  print("Impact on Odds of 1 -> 2")
  print(calculate_transition_effects("2 -> 1"))
  print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
}

calculate_transition_effects_for_3_states <- function(){
  print("extrem bound in % of 1 -> 2")
  print(calculate_transition_effects("1 -> 2"))
  print("extrem bound in % of 1 -> 3")
  print(calculate_transition_effects("1 -> 3"))
  print("extrem bound in % of 2 -> 1")
  print(calculate_transition_effects("2 -> 1"))
  print("extrem bound in % of 2 -> 3")
  print(calculate_transition_effects("2 -> 3"))
  print("extrem bound in % of 3 -> 1")
  print(calculate_transition_effects("3 -> 1"))
  print("extrem bound in % of 3 -> 2")
  print(calculate_transition_effects("3 -> 2"))
  print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
}