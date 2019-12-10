###################################################################################################
#
# Given some numerical parameters describing a loan in the dataset, returns its Internal Rate
# of Return.
#
# In the first instance, the function creates a schedule of payments.
# In many cases, the schedule will be extremely simple: a series of 36 or 60 equal instalements.
#
# But in some cases, a loan repayment are accelerated. Therefore the total amount of interest will
# be lower than expected (but this is good for the investor because highe interest rate over
# shorter tenor.).
#
# In other cases, the borrower defaults. Overall payments are less than expected.
#
# Based on the limited information of the dataset, the function makes educated guesses on the exact schedule.
#
# WARNING: THIS IS NOT OPTIMISED. RUNNING THIS FOR ALL LOANS (1.3 MLN OF THEM) TAKES CA.20 HOURS !!!!
#
calculateIRR <- function(loanNumber = 1,
                         loan = 1000,
                         intRate = 0.02,
                         term = 36,
                         totalPaid = 1000,
                         totalPrincipalPaid,
                         totalInterestPaid,
                         recoveries = 0,
                         lateFees = 0,
                         showSchedule = FALSE) {
  require(tidyverse)

  # number of monthly payments.
  # It exceeds 60 months in case recoveries on a 60-month loan takes the schedule after 60 months.
  nMonths <- 90

  # Months after which a loan defaults (normal tenor if no default or early prepayment)
  monthDefault = term

  # Note: *100 /100 to calculate in cent because ceiling cannot specify significant digits.
  installment <-
    ceiling(100 * loan * intRate / 12 / (1 - 1 / (1 + intRate / 12) ^ term)) / 100

  # We create a schedule
  schedule <- tibble(
    month = 0:nMonths,
    monthlyPayment = 0.0,
    totalPandI = 0.0,
    totalI = 0.0,
    totalP = 0.0
  )

  for (i in 2:(nMonths + 2)) {
    # Get situation at the end of previous month
    previousTotalPandI <- as.numeric(schedule[i - 1, "totalPandI"])
    previousTotalP     <- as.numeric(schedule[i - 1, "totalP"])
    previousTotalI     <- as.numeric(schedule[i - 1, "totalI"])

    # This is the beginning of a new month. First and foremost, the borrower is expected to pay the
    # accrued interest on amount of principal outstanding.
    # ceiling doesn't seem accept to accept significative digits.
    accruedInterest <-
      ceiling(100 * (loan - previousTotalP) * intRate / 12) / 100

    # If that amount takes the schedule above the total amount of interest shown in the data set,
    # we should stop the schedule at this point
    if (previousTotalI + accruedInterest > totalInterestPaid) {
      # We stop the normal schedule at this date.
      # Interest is paid (although less than scheduled)
      schedule[i, "monthlyPayment"] <-
        totalInterestPaid - previousTotalI

      # As well as whatever principal is left as per the dataset
      schedule[i, "monthlyPayment"] <-
        schedule[i, "monthlyPayment"] + totalPrincipalPaid - previousTotalP

      # Then 3-month after the last payment date, recoveries and and late fees are paid
      schedule[i + 3, "monthlyPayment"] <-
        schedule[i + 3, "monthlyPayment"] + recoveries + lateFees

      # Not really useful, but for completeness
      schedule[i, "totalPandI"] <- totalPaid
      schedule[i, "totalI"]     <- totalInterestPaid
      schedule[i, "totalP"]     <- totalPrincipalPaid

      # If total principal paid is less than borrower, then it is a default, and the monthDefault
      # is adjusted.
      if (totalPrincipalPaid < loan) {
        monthDefault = i
      }

      # No more payments to add to the schedule
      break()

    } else {
      # Deal with normal schedule
      schedule[i, "monthlyPayment"] <- installment
      schedule[i, "totalPandI"] <-
        schedule[i - 1, "totalPandI"] + installment
      schedule[i, "totalI"]     <-
        schedule[i - 1, "totalI"]   + accruedInterest
      schedule[i, "totalP"]     <-
        schedule[i - 1, "totalP"]   + installment - accruedInterest
    }
  }

  # At this point schedule[, "monthlyPayment"] contains the schedule of all payments, but needs to
  # include the initial loan.
  schedule[1, "monthlyPayment"] <- -loan

  if (showSchedule) {
    schedule %>% view()
  }

  NPV <- function(interest, cashFlow) {
    t = 0:(length(cashFlow) - 1)
    sum(cashFlow / (1 + interest) ^ t)
  }

  IRR <- function(CF) {
    res <- NA
    try({
      res <- uniroot(NPV, c(-0.9, 1), cashFlow = CF)$root
    },
    silent = TRUE)
    return(res)
  }

  return(tibble(
    loanID = loanNumber,
    IRR = round(as.numeric(IRR(
      schedule$monthlyPayment
    ) * 12), digits = 4),
    monthDefault = monthDefault
  ))
}

loanNumberIRR <- function(loanNumber) {
  require(tidyverse)

  l <- loans %>% filter(loanID == loanNumber)
  calculateIRR(
    loanNumber = l$loanID,
    loan = l$funded_amnt, intRate = l$int_rate, term = l$term,
    totalPaid = l$total_pymnt, totalPrincipalPaid = l$total_rec_prncp, totalInterestPaid = l$total_rec_int,
    recoveries = l$recoveries, lateFees = l$total_rec_late_fee,
    showSchedule = TRUE
  )
}
```


```{r IRR-calculation,eval=FALSE,echo=TRUE}
#
# Calculate all the IRRs and month of default for all the loans.
# WARNING: This takes around a full day to run!!!!
#
# The actual data was generated by the Julia version, with cross-checks.
# Julia version takes about 150 sec on the same unoptimised code.
#
local({
  loansIRR <-
    loans %>%
    rowwise() %>%
    do(calculateIRR(loanNumber = .$loanID,
                    loan = .$funded_amnt, intRate = .$int_rate, term = .$term,
                    totalPaid = .$total_pymnt, totalPrincipalPaid = .$total_rec_prncp,
                    totalInterestPaid = .$total_rec_int,
                    recoveries = .$recoveries, lateFees = .$total_rec_late_fee))

  saveRDS(loansIRR, "datasets/lending_club_IRRs.rds")

})
