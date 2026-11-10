library(data.table)

#' Simulate eggs given patients
#'
simulate_eggs <- function(
  patients,
  mean_eggs = 8,
  funs=default_funs) {
  
  patients <- copy(patients)

  patients[, expected_eggs := exp(log(mean_eggs) -
      0.05 * (age - 35) + 
    0.30 * (log(amh) - log(2.5))
  )]

  patients[, n_eggs := rpois(.N, expected_eggs)]

  eggs <- patients[rep(seq_len(.N), n_eggs)]

  eggs[, `:=`(egg_number = seq_len(.N),
    egg_id = seq_len(.N),
    egg_quality = rnorm(.N, 0, 0.6)
  ), by = patient_id]

  eggs[, euploid_prob := funs[["euploid_prob"]](.SD)]
  
  eggs[, euploid := rbinom(.N,1,euploid_prob)]

  eggs

}

#' Simulate eggs given patients
#'
simulate_fert <- function(eggs,funs=default_funs) {
  eggs <- copy(eggs)
  eggs[, day1_prob := funs[["fert_prob"]](.SD)]
  eggs[, day1 := rbinom(.N,1,day1_prob)]
  eggs[, n_day1 := sum(day1),by=patient_id]
  eggs
} 


simulate_day3 <- function(embryos,funs=default_funs) {
  embryos <- copy(embryos)
  embryos[, day3_prob := 0.0]
  embryos[day1 == 1,day3_prob := funs[["day3_prob"]](.SD)]
  embryos[, day3 := rbinom(.N, 1, day3_prob)]
  embryos[, n_day3 := sum(day3),by=patient_id]
  embryos
}


simulate_day5 <- function(embryos,funs=default_funs) {

  grades <- 1:4
  p_grade <- list(
    euploid   = c(0.35, 0.35, 0.20, 0.10),  
    aneuploid = c(0.15, 0.30, 0.30, 0.25) 
  )

  embryos <- copy(embryos)
  embryos[, day5_prob := 0.0]
  embryos[day3 == 1,
          day5_prob := funs[["day5_prob"]](.SD)]
  embryos[, day5 := rbinom(.N, 1, day5_prob)]
  embryos[, n_day5 := sum(day5),by=patient_id]
  embryos[, embryo_quality := NA_real_]

  embryos[day5 == 1, embryo_quality :=
    sample(grades, .N, replace = TRUE,
          prob = if (euploid == 1) p_grade$euploid else p_grade$aneuploid),
    by = euploid
  ]
  embryos
}

