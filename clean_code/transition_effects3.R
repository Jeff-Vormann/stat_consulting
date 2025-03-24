#############################################
# transition_effects3.R
#
# Dieses Skript definiert die Funktion
# calculate_transition_effects(), mit der
# du den Einfluss einzelner Kovariaten auf
# die Übergangswahrscheinlichkeiten
# (z.B. "1 -> 2" oder "2 -> 1") berechnen kannst.
#
# Voraussetzung: Die folgenden globale Variablen
# müssen im aufrufenden Skript bereits definiert sein:
#   - ToD_scale_param, season_scale_param, evation_scale_param, temp_scale_param
#   - multipliers (z.B. c(ToD = 2, season = 2, evation = 500, temp = 5))
#   - scenario_labels (z.B. c(ToD = "Unterschied Mitternacht zu Mittag",
#                              season = "Unterschied Winter zu Sommer",
#                              evation = "500 m Höher",
#                              temp = "5 Grad wärmer"))
#   - hmm_model (mit hmm_model$mle$transition, in dem die Übergangs-Koeffizienten
#                gespeichert sind; Zeilen sollten "ToD", "season", "evation", "temp" heißen)
#############################################

calculate_transition_effects <- function(transition = "1 -> 2") {
  
  # Definiere die Kovariaten, falls nicht global gesetzt
  cov_names <- c("ToD", "season", "evation", "temp")
  
  # Verwende die global definierten Skalierungsparameter
  scale_params <- c(ToD = ToD_scale_param, 
                    season = season_scale_param, 
                    evation = evation_scale_param, 
                    temp = temp_scale_param)
  
  # Verwende die global definierten Multiplikatoren und Szenario-Beschreibungen
  # (Falls diese global nicht existieren, werden hier Defaultwerte verwendet)
  
  multipliers <- c(ToD = 2, season = 2, evation = 500, temp = 5)
  
  scenario_labels <- c(ToD = "Unterschied Mitternacht zu Mittag",
                       season = "Unterschied Winter zu Sommer",
                       evation = "500 m Höher",
                       temp = "5 Grad wärmer")
  
  
  # Extrahiere die Beta-Werte für den gewünschten Übergang
  # Es wird angenommen, dass die Zeilen der Übergangsmatrix den Namen der Kovariaten entsprechen
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
  
  # Erstelle eine Übersichtstabelle mit den Ergebnissen
  result <- data.frame(
    Covariate        = cov_names,
    Beta             = round(beta_vals, 6),
    Effekt_pro_SD_auf_Odds    = round(effect_sd, 6),
    Effekt_Szenario_auf_Odds  = round(scenario_effect, 6),
    Szenario         = scenario_labels,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Beispielaufruf (optional, kann auch auskommentiert werden):
# res_1_2 <- calculate_transition_effects("1 -> 2")
# print(res_1_2)
