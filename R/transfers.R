
library(data.table)

#' Select and order embryos for transfer
#'
select_order_embryos <- function(embryos,strategy = c("best_first", "random")) {

  strategy <- match.arg(strategy)

  embryos <- copy(embryos)

  embryos <- embryos[available_for_transfer == 1]

  if (strategy == "best_first") {
    setorderv(embryos,
      cols = c("patient_id","embryo_quality"),
      order = c(1, 1))
  } else {
    embryos[,random_order := runif(.N)]
    setorderv(embryos,cols = c("patient_id", "random_order"),
      order = c(1, 1))
    embryos[, random_order := NULL]
  }

  embryos[,transfer_order := seq_len(.N),by = patient_id]

  embryos
}

#' Simulate embryo transfers
#' 
simulate_transfers <- function(
  embryos,
  max_transfers = 3,
  funs = default_funs) {
  
  embryos <- copy(embryos)
  setorder(embryos, patient_id, transfer_order)

  embryos <- embryos[available_for_transfer == 1]
  transfers <- embryos[,head(.SD, max_transfers),by = patient_id]

  if (nrow(transfers) == 0) {
    return(data.table())
  }

  transfers[,transfer_number := seq_len(.N),by = patient_id]
  transfers[,live_birth_prob := funs[["lb_prob"]](.SD)]
  transfers[,live_birth := rbinom(.N,size = 1,prob = live_birth_prob)]
  transfers[, keep := cumsum(live_birth) <= 1L, by = patient_id]
  transfers <- transfers[keep == TRUE]

  # Assign outcomes
  transfers[,outcome := "no_live_birth"]

  transfers[live_birth == 1,outcome := "live_birth"]

  # If no live birth occurred, classify the final observed transfer
  transfers[,had_live_birth := any(live_birth == 1),by = patient_id]

  transfers[!had_live_birth & transfer_number == max(transfer_number),
    outcome := fifelse(
      transfer_number == max_transfers,
      "administrative_censoring",
      "treatment_exhaustion"),
    by = patient_id]

  transfers[,c("live_birth","keep","had_live_birth") := NULL]

  transfers
}

