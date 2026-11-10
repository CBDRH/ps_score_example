library(data.table)

# pgt_sensitivity = P(test says aneuploid | truly aneuploid)
# pgt_specificity = P(test says euploid | truly euploid)
simulate_pgt <- function(embryos,
  pgt_specificity,
  pgt_sensitivity,
  funs=default_funs) {
  
  embryos <- copy(embryos)
  embryos[day5 == 0, prob_pgt := 0.0]
  embryos[day5 == 1, prob_pgt := funs[["do_pgt_prob"]](.SD),by=patient_id]

  pgt_decisions <- embryos[day5 == 1,.(prob_pgt = first(prob_pgt),
      do_pgt = rbinom(1, 1, first(prob_pgt))),by = patient_id]
  
  embryos <- merge(embryos,pgt_decisions[,.(patient_id,do_pgt)],by = "patient_id",all.x=TRUE)

  embryos[day5 == 1, pgt_status := "not_tested"]

  embryos[do_pgt == 1 & euploid == 1 & day5 == 1,
    pgt_status := fifelse(rbinom(.N, 1, pgt_specificity) == 1,
      "euploid","aneuploid")]

  embryos[do_pgt == 1 & euploid == 0 & day5 == 1,
    pgt_status := fifelse(rbinom(.N, 1, pgt_sensitivity) == 1,
      "aneuploid","euploid")]
  
  embryos[, available_for_transfer := fifelse(
    (do_pgt == 0 & day5 == 1) | (do_pgt == 1 & pgt_status == "euploid"),
    1,0)]
  
  embryos

}
