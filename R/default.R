f_euploid_prob <- function(x) {
  base_euploid_prob <- 0.7
  (x$age <= 35) * base_euploid_prob + 
    (x$age > 35) * pmax((base_euploid_prob - 0.065 * (x$age - 35)),0.05)
}

f_fert_prob <- function(x) {
  0.7
}

f_day3_prob <- function(x) {
  0.95
}

f_day5_prob <- function(x,euploid_day5_prob=0.7,aneuploid_day5_prob=0.3) {
  aneuploid_day5_prob * (x$euploid == 0) +
    euploid_day5_prob * (x$euploid == 1)
}

f_pgt_prob <- function(x) {
  0.5
}

f_lb_prob <- function(x,base_lb_prob = 0.45) {

  0.05 * (x$euploid == 0) +
    (x$euploid == 1) * (
    (x$age < 40) * base_lb_prob +  
    (x$age >= 40) * pmin((base_lb_prob - (x$age - 40) * 0.03), 0.15)
    )
}


default_funs <- list(
  euploid_prob = f_euploid_prob,
  fert_prob = f_fert_prob,
  day3_prob = f_day3_prob,
  day5_prob = f_day5_prob,
  do_pgt_prob = f_pgt_prob,
  lb_prob = f_lb_prob
)
