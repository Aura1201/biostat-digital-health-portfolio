inc_rate_ci <- function(events, pt_years, mult = 100) {
  if (pt_years <= 0) stop("Person-time must be > 0")
  
  rate <- (events / pt_years) * mult
  lcl  <- (qchisq(0.025, df = 2 * events) / (2 * pt_years)) * mult
  ucl  <- (qchisq(0.975, df = 2 * (events + 1)) / (2 * pt_years)) * mult
  
  return(
    data.frame(
      events = events,
      pt_years = pt_years,
      rate = rate,
      lcl = lcl,
      ucl = ucl
    )
  )
}
