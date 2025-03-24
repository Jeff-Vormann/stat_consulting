getInitialParameters <- function() {
  config_path <- "config.yaml"
  config <- yaml::yaml.load_file(config_path)
  
  
  if (!"dataset_name" %in% names(config))
    stop("In der config.yaml fehlt 'dataset_name'!")
  if (!"nbStates" %in% names(config))
    stop("In der config.yaml fehlt 'nbStates'!")
  if (!"preprocessing" %in% names(config))
    stop("In der config.yaml fehlt 'preprocessing'!")
  
  dataset_name <- config$dataset_name
  nbStates     <- config$nbStates
  preproc      <- config$preprocessing
  
  #Die hier gesetzeten paramter wurden nur durch die L2-norm gefittet,
  #zusammen mit dem verhältnis von y zu x wurde versucht die init_paramter für die andern Methoden zu schätzen
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
  conv_x   <- 1 / sqrt(1 + k^2)
  conv_sum <- (1 + k) / sqrt(1 + k^2)
  
  # 4) Fallunterscheidung: je Kombination von nbStates, dataset_name und preproc
  # Für jede Kombination werden zuerst die Originalparameter (L2norm) definiert.
  # Anschließend, falls preproc != "L2norm", werden diese Werte mit dem Umrechnungsfaktor transformiert.
  
  if (nbStates == 2 && dataset_name == "Ursina") {
    orig_mu       <- c(0.4789112, 1.7095093)
    orig_sigma    <- c(-0.4929086, 0.7623653)
    orig_zeromass <- c(2.8855612, -6.9816928)
    
    if (preproc == "L2norm") {
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      mu0    <- orig_mu * conv_x
      sigma0 <- orig_sigma * conv_x
    } else if (preproc == "sum") {
      mu0    <- orig_mu * conv_sum
      sigma0 <- orig_sigma * conv_sum
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
  } else if (nbStates == 2 && dataset_name == "Thomas") {
    orig_mu       <- c(0.5043893, 1.6611546)
    orig_sigma    <- c(-0.5048797, 0.6675502)
    orig_zeromass <- c(2.6334781, -6.8918561)
    
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
    
  } else if (nbStates == 3 && dataset_name == "Deer") {
    orig_mu       <- c(-7.632576  , 2.478063 ,  4.698127)
    orig_sigma    <- c(42.480810  , 2.402027 ,  3.877999)
    orig_zeromass <- c(221.467184 , -2.875361 , -7.521981)
    
    if (preproc == "L2norm") {
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      
      mu0    <- c(-2.660312  , 2.479482  , 4.408424)
      sigma0 <- c(-2.454431  , 2.378860 ,  3.449274)
    } else if (preproc == "sum") {
      mu0    <- orig_mu * ((1+0.927) / sqrt(1 + 0.927^2))  # Formel: x+y = L2 * (1+0.927)/sqrt(1+0.927^2)
      sigma0 <- orig_sigma * ((1+0.927) / sqrt(1 + 0.927^2))
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
    
  } else if (nbStates == 4 && dataset_name == "Deer") {
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
    
  } else if (nbStates == 3 && dataset_name == "Gams") {
    orig_mu       <- c(-3.066023e+01 , 2.738865e+00 , 4.700416e+00)
    orig_sigma    <- c(1.088950e+02 , 2.870825e+00 , 3.663193e+00)
    orig_zeromass <- c(2.949883e+03 ,-2.129570e+00 ,-2.755386e+03)
    if (preproc == "L2norm") {
      mu0    <- orig_mu
      sigma0 <- orig_sigma
    } else if (preproc == "onlyX") {
      mu0    <- c(-2.640805 ,  2.585973  , 4.446839)
      sigma0 <- c(-2.239583  , 2.688234   ,3.367790)
      orig_zeromass <- c(18.420681 ,-20.723409 , -7.412067)
    } else if (preproc == "sum") {
      mu0    <- orig_mu * ((1+0.8) / sqrt(1 + 0.8^2))  # Formel: x+y = L2 * (1+0.8)/sqrt(1+0.8^2)
      sigma0 <- orig_sigma * ((1+0.8) / sqrt(1 + 0.8^2))
    } else {
      stop("Unbekannte Preprocessing-Methode: ", preproc)
    }
    zeromass0 <- orig_zeromass
    
  } else if (nbStates == 4 && dataset_name == "Gams") {

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
    
  } else if (nbStates == 2 ) {
      mu0       <- c(1, 100)
      sigma0    <- c(1, 10)
      zeromass0 <- c(0.99, 0.01)
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

