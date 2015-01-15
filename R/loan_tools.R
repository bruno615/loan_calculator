#' Loan Calculator Functions
#' 


calc_amort_table <- function(size, apr, duration, period){
  payment <- size * apr / 12 / (1 - (1+ apr / 12)^(-duration)) # A rough guesstimate
  interval <- .01
  repeat{
    amort <- refresh_amort_table(1000, .95, 12, payment = payment, period = period)
    remaining_principal <- amort$rem_principal[nrow(amort)]
    if (remaining_principal <= 0) {
      amort <- refresh_amort_table(1000, .95, 12, payment = payment, final_payment = payment + remaining_principal, period = period)
      break
    } else {
      payment = payment + interval
    }
  }
  amort
}

refresh_amort_table <- function(size, apr, duration, payment = calc_monthly_payment(size, apr, duration), final_payment = payment, period) {
  amort <- data.frame(installment = 0:duration, 
                      period = period, 
                      payment = truncate(payment, 2),                                                                     # Truncate Here
                      interest = NA, principal = NA, rem_principal = NA, 
                      row.names = c(0:duration))
  amort$payment[nrow(amort)] <- final_payment
  amort[1,3:6] = c(0,0,0,size)
  for (i in 2:nrow(amort)) {
    amort$interest[i] = truncate(interest_earned(amort$rem_principal[i - 1], apr, amort$period[i]), digits = 2)           # Truncate Here
    amort$principal[i] = amort$payment[i] - amort$interest[i]
    amort$rem_principal[i] = size - sum(amort$principal, na.rm = TRUE)
  }
  amort 
}

interest_earned <- function(size, apr, days) {
  size * apr * days / 365
}

truncate <- function(x, digits = 0) { #because for some reason R does not do this...
  trunc(x * 10^digits) / 10^digits
}


period <- c(0,37,30,31,31,31,30,31,30,31,31,30,31)

amort <- calc_amort_table(1000, .95, 12, period = period)
finance_charge <- sum(amort$interest)


