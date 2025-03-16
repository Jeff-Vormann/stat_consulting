getInitialParameters <- function() {
  # 1) Config lesen (fester Dateiname "config.yaml")
  config_path <- "config.yaml"
  config <- yaml::yaml.load_file(config_path)
  
  # 2) Notwendige Felder aus der Config extrahieren
  if (!"dataset_name" %in% names(config))
    stop("In der config.yaml fehlt 'dataset_name'!")
  if (!"nbStates" %in% names(config))
    stop("In der config.yaml fehlt 'nbStates'!")
  if (!"preprocessing" %in% names(config))
    stop("In der config.yaml fehlt 'preprocessing'!")
  
  dataset_name <- config$dataset_name
  nbStates     <- config$nbStates
  preproc      <- config$preprocessing
  
  # 3) Definiere den k-Wert (basierend auf praktischen Beobachtungen)
  # Diese k-Werte beschreiben, dass y = k * x gilt
  if (dataset_name == "Ursina") {
    k <- 0.437
  } else if (dataset_name == "Thomas") {
    k <- 0.524
  } else if (dataset_name == "Deer") {
    k <- 0.927
  } else if (dataset_name == "Gams") {
    k <- 0.8
  } else {
    stop("Kein k-Wert für dataset_name ", dataset_name, " definiert!")
  }
  
  # Umrechnungsfaktoren aus der L2-Norm
  # Für die Transformation gilt:
  #   x = L2 / sqrt(1+k^2)   und   x+y = L2 * (1+k)/sqrt(1+k^2)
  conv_x   <- 1 / sqrt(1 + k^2)
  conv_sum <- (1 + k) / sqrt(1 + k^2)
  
  # 4) Fallunterscheidung: je Kombination von nbStates, dataset_name und preproc
  # Für jede Kombination werden zuerst die Originalparameter (L2norm) definiert.
  # Anschließend, falls preproc != "L2norm", werden diese Werte mit dem Umrechnungsfaktor transformiert.
  
  if (nbStates == 2 && dataset_name == "Ursina") {
    # Originalparameter (L2norm) für Ursina, 2 Hidden States
    orig_mu       <- c(0.1, 7)
    orig_sigma    <- c(0.1, 3)
    orig_zeromass <- c(0.99, 0.01)
    
    if (preproc == "L2norm") {
      # L2norm: Nutze die Originalwerte
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      # onlyX: x = L2 / sqrt(1+k^2)
      # => mu0 = orig_mu * conv_x, sigma0 = orig_sigma * conv_x
      mu0    <- orig_mu * conv_x
      sigma0 <- orig_sigma * conv_x
    } else if (preproc == "sum") {
      # average: x+y = L2 * (1+k)/sqrt(1+k^2)
      # => mu0 = orig_mu * conv_sum, sigma0 = orig_sigma * conv_sum
      mu0    <- orig_mu * conv_sum
      sigma0 <- orig_sigma * conv_sum
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
    #return(c(mu0, sigma0, zeromass0))
    
  } else if (nbStates == 2 && dataset_name == "Thomas") {
    # Originalparameter für Thomas, 2 Hidden States
    orig_mu       <- c(0.1, 7)
    orig_sigma    <- c(0.1, 3)
    orig_zeromass <- c(0.99, 0.01)
    
    if (preproc == "L2norm") {
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      mu0    <- orig_mu * (1 / sqrt(1 + 0.524^2))   # Formel: x = L2 / sqrt(1+0.524^2)
      sigma0 <- orig_sigma * (1 / sqrt(1 + 0.524^2))
    } else if (preproc == "sum") {
      mu0    <- orig_mu * ((1+0.524) / sqrt(1 + 0.524^2))  # Formel: x+y = L2 * (1+0.524)/sqrt(1+0.524^2)
      sigma0 <- orig_sigma * ((1+0.524) / sqrt(1 + 0.524^2))
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
    #return(c(mu0, sigma0, zeromass0))
    
  } else if (nbStates == 3 && dataset_name == "Deer") {
    # Originalparameter für Deer, 3 Hidden States
    orig_mu       <- c(0.1, 17, 110)
    orig_sigma    <- c(0.1, 12, 48)
    orig_zeromass <- c(0.9, 0.2, 0.1)
    
    if (preproc == "L2norm") {
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      mu0    <- orig_mu * (1 / sqrt(1 + 0.927^2))  # Formel: x = L2 / sqrt(1+0.927^2)
      sigma0 <- orig_sigma * (1 / sqrt(1 + 0.927^2))
    } else if (preproc == "sum") {
      mu0    <- orig_mu * ((1+0.927) / sqrt(1 + 0.927^2))  # Formel: x+y = L2 * (1+0.927)/sqrt(1+0.927^2)
      sigma0 <- orig_sigma * ((1+0.927) / sqrt(1 + 0.927^2))
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
    #return(c(mu0, sigma0, zeromass0))
    
  } else if (nbStates == 4 && dataset_name == "Deer") {
    # Originalparameter für Deer, 4 Hidden States
    orig_mu       <- c(0.1, 12, 80, 140)
    orig_sigma    <- c(0.1, 11, 35, 35)
    orig_zeromass <- c(0.9, 0, 0.1, 0.1)
    
    if (preproc == "L2norm") {
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      mu0    <- orig_mu * (1 / sqrt(1 + 0.927^2))
      sigma0 <- orig_sigma * (1 / sqrt(1 + 0.927^2))
    } else if (preproc == "sum") {
      mu0    <- orig_mu * ((1+0.927) / sqrt(1 + 0.927^2))
      sigma0 <- orig_sigma * ((1+0.927) / sqrt(1 + 0.927^2))
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
    #return(c(mu0, sigma0, zeromass0))
    
  } else if (nbStates == 3 && dataset_name == "Gams") {
    # Originalparameter für Gams, 3 Hidden States
    orig_mu       <- c(0.1, 9, 109)
    orig_sigma    <- c(0.1, 9, 39)
    orig_zeromass <- c(0.9, 1e-9, 0.1)
    if (preproc == "L2norm") {
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      mu0    <- orig_mu * (1 / sqrt(1 + 0.8^2))   # Formel: x = L2 / sqrt(1+0.8^2)
      sigma0 <- orig_sigma * (1 / sqrt(1 + 0.8^2))
    } else if (preproc == "sum") {
      mu0    <- orig_mu * ((1+0.8) / sqrt(1 + 0.8^2))  # Formel: x+y = L2 * (1+0.8)/sqrt(1+0.8^2)
      sigma0 <- orig_sigma * ((1+0.8) / sqrt(1 + 0.8^2))
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
    #return(c(mu0, sigma0, zeromass0))
    
  } else if (nbStates == 4 && dataset_name == "Gams") {
    # Originalparameter für Gams, 4 Hidden States

    orig_mu       <- c(0.01, 16, 83, 134)
    orig_sigma    <- c(0.01, 18, 22, 35)
    orig_zeromass <- c(1, 0.0, 0, 0.0)
    
    if (preproc == "L2norm") {
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      mu0    <- orig_mu * (1 / sqrt(1 + 0.8^2))
      sigma0 <- orig_sigma * (1 / sqrt(1 + 0.8^2))
    } else if (preproc == "sum") {
      mu0    <- orig_mu * ((1+0.8) / sqrt(1 + 0.8^2))
      sigma0 <- orig_sigma * ((1+0.8) / sqrt(1 + 0.8^2))
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
    #return(c(mu0, sigma0, zeromass0))
    
  } else if (nbStates == 2 ) {
      mu0       <- c(1, 100)
      sigma0    <- c(1, 10)
      zeromass0 <- c(0.99, 0.01)
      #return(c(mu0, sigma0, zeromass0))
  } else {
    stop("Keine Initialparameter für die Konfiguration (", dataset_name, 
         ", nbStates=", nbStates, ", preprocessing=", preproc, ") gefunden!")
  }
  # Erweiterung der mu0-Werte, falls emission_mean nicht "~1" ist:
if (config$emission_mean != "~1") {
  plus_matches <- gregexpr("\\+", config$emission_mean, perl = TRUE)[[1]]
  # Falls kein "+" gefunden wird, sollen keine extra Zeilen hinzugefügt werden (nur der Intercept)
  print(plus_matches)
  numExtra <- if (plus_matches[1] == -1) 0 else length(plus_matches)
  print(numExtra)
  new_mu0 <- c()
  for (i in 1:nbStates) {
    # Für jeden Zustand: der ursprüngliche Intercept gefolgt von numExtra Nullen
    new_mu0 <- c(new_mu0, mu0[i], rep(0, numExtra+1))
  }
  mu0 <- new_mu0
}

  if (config$emission_mean == "~1") {
    final_mat <- rbind(
      matrix(mu0, nrow = 1),
      matrix(sigma0, nrow = 1),
      matrix(zeromass0, nrow = 1)
    )
    rownames(final_mat) <- c("mu0", "sigma0", "zeromass0")
    colnames(final_mat) <- paste("State", 1:nbStates)
  } else {
    p <- length(mu0) / nbStates  # p = 1 + Anzahl der Pluszeichen
    mean_mat <- matrix(mu0, nrow = p, ncol = nbStates)
    final_mat <- rbind(
      mean_mat,
      matrix(sigma0, nrow = 1, ncol = nbStates),
      matrix(zeromass0, nrow = 1, ncol = nbStates)
    )
    rownames(final_mat) <- c(paste0("mu", 1:p), "sigma0", "zeromass0")
    colnames(final_mat) <- paste("State", 1:nbStates)
  }
return(final_mat)
}