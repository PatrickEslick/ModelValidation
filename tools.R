
predict_series <- function(data, var_names, coefs, intercept) {
  
  data$Predicted <- 0
  
  for(i in 1:length(var_names)) {
    data$Predicted <- data$Predicted + coefs[i] * data[,var_names[i]]
  }
  data$Predicted <- data$Predicted + intercept
  
  return(data)
  
}

#Say what season a date is intoda
season <- function(date) {
  season_atm <- function(dt) {
    mnth <- month(dt)
    if(mnth %in% c(12, 1, 2)) {
      ssn_atm <- "Winter"
    } else if(mnth %in% c(3, 4, 5)) {
      ssn_atm <- "Spring"
    } else if (mnth %in% c(6, 7, 8)) {
      ssn_atm <- "Summer"
    } else if (mnth %in% c(9, 10, 11)) {
      ssn_atm <- "Fall"
    }
    return(ssn_atm)
  }
  ssn <- sapply(date, season_atm)
  ssn <- ordered(ssn, levels = c("Winter", "Spring", "Summer", "Fall"))
  return(ssn)
}