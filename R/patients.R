library(data.table)

#' Generate synthetic patient cohort
#' 
simulate_patients <- function(
  n_patients,
  seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)

  age = pmin(pmax(35 + 4 * rnorm(n_patients), 25), 45)
  log_amh <- (log(2.5) - 0.10 * (age - 35) + rnorm(n_patients, 0, 0.45))

  data.table(
    patient_id = seq_len(n_patients),
    age = age,
    amh = exp(log_amh),
    patient_frailty = rnorm(n_patients, 0, 0.7)
  )
}
