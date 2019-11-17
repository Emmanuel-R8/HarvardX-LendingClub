####################################################################################################
##
## Prepare datasets
##

## Previously saved from R with:
##     lending_club <- readRDS("lending_club_reformatted.rds")
##     library(readr)
##     write.csv(lending_club, "LendingClub.csv")
##
## WARNING: 1.3GB on disk
##
using CSV, GZip

lendingClub = GZip.open("datasets/LendingClub.csv.gz", "r") do fileHandler
    CSV.read(fileHandler)
end

RATES = GZip.open("datasets/Rates.csv.gz", "r") do fileHandler
    CSV.read(fileHandler)
end



####################################################################################################
##
## NPV calculations
##
## Given some numerical parameters describing a loan in the dataset, returns its Net Present
## Value calculated at the risk-free rate.
##
##

using DataFrames

# number of monthly payments to model
# It exceeds 60 months in case recoveries on a 60-month loan takes the schedule after 60 months.
const nMonths = 90

# Sculpt credit foncier profiles over 36 and 60 months at 20% per annum.
# The profile is expressed as percentage of loan amount
function CreateCreditFoncier(;n = 36, riskFree = 0.0)

  instalment = 1 * riskFree/12 * 1 / (1 - 1 / (1 + riskFree/12) ^ n)

  # We create a schedule
  schedule = DataFrame(month = 0:nMonths, payment = 0.0, principal = 0.0)

  # Add the day 1 principal outlay
  schedule[1,       :payment] = -1
  schedule[2:(n+1), :payment] = instalment

  # Principal schedule
  schedule[1, :principal] = -1
  for m in 2:(n+1)
    # interest component
    interest = -riskFree * sum(schedule[1:(m-1), :principal])
    schedule[m, :principal] = instalment - interest
  end

  return(schedule)
end


# Solve for the credit margin
function loanSchedule(; loanNumber = 1, loan = 1000.0, intRate = 0.05, term = 36,
  totalPaid = 1000.0, totalPrincipalPaid = 700.0, totalInterestPaid = 50.0,
  recoveries = 0.0, lateFees = 0.0,
  riskFree = 0.01,
  showSchedule = false)

  # Months after which a loan defaults (normal tenor if no default or early prepayment)
  monthDefault = term

  # Monthly instlment
  instalment = ceil(loan * intRate/12 / (1 - 1 / (1 + intRate/12) ^ term), digits = 2)

  # Create a blank schedule
  schedule = DataFrame(month = 0:nMonths, monthlyPayment = 0.0,
                       principalPayment = 0.0,
                       totalPandI = 0.0, totalI = 0.0, totalP = 0.0)

  for i in 2:(nMonths + 1)
    # Get situation at the end of previous month
    previousTotalPandI = schedule[i - 1, :totalPandI]
    previousTotalP     = schedule[i - 1, :totalP]
    previousTotalI     = schedule[i - 1, :totalI]

    # This is the beginning of a new month. First and foremost, the borrower is expected to pay the
    # accrued interest on amount of principal outstanding.
    # The instalment is expected to cover that amount of interest and the rest goes to
    # reducing the principal due outstanding.
    accruedInterest = ceil((loan - previousTotalP) * intRate/12; digits = 2)
    decreasePrincipal = instalment - accruedInterest

    # If that amount takes the schedule above the total amount of interest shown in the data set,
    # we should stop the schedule at this point
    # This is a shortcut since we could have a payment higher than the interest due, but not enough
    # to cover the expected principal repayment. However, it works well in practice.
    if previousTotalI + accruedInterest > totalInterestPaid

      # We stop the normal schedule at this date.
      # Interest is paid (although less than scheduled)
      schedule[i, :monthlyPayment] = totalInterestPaid - previousTotalI

      # Whatever principal is left as per the dataset
      decreasePrincipal = totalPrincipalPaid - previousTotalP
      schedule[i, :monthlyPayment] += decreasePrincipal

      # Then 3-month after the last payment date, recoveries and and later fees are paid
      schedule[i + 3, :monthlyPayment] += recoveries + lateFees

      # Not really useful, but for completeness
      schedule[i, :totalPandI] = totalPaid
      schedule[i, :totalI]     = totalInterestPaid
      schedule[i, :totalP]     = totalPrincipalPaid

      # If total principal paid is less than borrower, then it is a default, and the monthDefault
      # is adjusted.
      if (totalPrincipalPaid < loan)
        monthDefault = i
      end

      # No more payments to add to the schedule
      break

    else
      # Deal with normal schedule
      schedule[i, :monthlyPayment]   = instalment
      schedule[i, :principalPayment] = decreasePrincipal
      schedule[i, :totalPandI]       = schedule[i-1, :totalPandI] + instalment
      schedule[i, :totalI]           = schedule[i-1, :totalI]     + accruedInterest
      schedule[i, :totalP]           = schedule[i-1, :totalP]     + decreasePrincipal
    end
  end

  # At this point schedule[, :monthlyPayment] contains the schedule of all payments, but needs to
  # include the initial loan.
  schedule[1, :monthlyPayment] = -loan
  schedule[1, :principalPayment] = -loan

  if (showSchedule)
    println("Payments")
    println(schedule)
  end
  # Rescale to 1,000

  schedule[!, :monthlyPayment]   = schedule[:, :monthlyPayment] ./ loan .* 1000
  schedule[!, :principalPayment] = schedule[:, :principalPayment] ./ loan .* 1000
  schedule[!, :totalPandI]       = schedule[:, :totalPandI] ./ loan .* 1000
  schedule[!, :totalI]           = schedule[:, :totalI] ./ loan .* 1000
  schedule[!, :totalP]           = schedule[:, :totalP] ./ loan .* 1000

  return(schedule)
end



# Solve for the credit margin
function NPV(; loanNumber = 1, loan = 1000.0, intRate = 0.05, term = 36,
  totalPaid = 1000.0, totalPrincipalPaid = 700.0, totalInterestPaid = 50.0,
  recoveries = 0.0, lateFees = 0.0,
  riskFree = 0.01,
  showSchedule = false)

  # Months after which a loan defaults (normal tenor if no default or early prepayment)
  monthDefault = term

  # Monthly instlment
  instalment = ceil(loan * intRate/12 / (1 - 1 / (1 + intRate/12) ^ term), digits = 2)

  # Create a blank schedule
  schedule = DataFrame(month = 0:nMonths, monthlyPayment = 0.0,
                       principalPayment = 0.0,
                       totalPandI = 0.0, totalI = 0.0, totalP = 0.0)

  for i in 2:(nMonths + 1)
    # Get situation at the end of previous month
    previousTotalPandI = schedule[i - 1, :totalPandI]
    previousTotalP     = schedule[i - 1, :totalP]
    previousTotalI     = schedule[i - 1, :totalI]

    # This is the beginning of a new month. First and foremost, the borrower is expected to pay the
    # accrued interest on amount of principal outstanding.
    # The instalment is expected to cover that amount of interest and the rest goes to
    # reducing the principal due outstanding.
    accruedInterest = ceil((loan - previousTotalP) * intRate/12; digits = 2)
    decreasePrincipal = instalment - accruedInterest

    # If that amount takes the schedule above the total amount of interest shown in the data set,
    # we should stop the schedule at this point
    # This is a shortcut since we could have a payment higher than the interest due, but not enough
    # to cover the expected principal repayment. However, it works well in practice.
    if previousTotalI + accruedInterest > totalInterestPaid

      # We stop the normal schedule at this date.
      # Interest is paid (although less than scheduled)
      schedule[i, :monthlyPayment] = totalInterestPaid - previousTotalI

      # Whatever principal is left as per the dataset
      decreasePrincipal = totalPrincipalPaid - previousTotalP
      schedule[i, :monthlyPayment] += decreasePrincipal

      # Then 3-month after the last payment date, recoveries and and later fees are paid
      schedule[i + 3, :monthlyPayment] += recoveries + lateFees

      # Not really useful, but for completeness
      schedule[i, :totalPandI] = totalPaid
      schedule[i, :totalI]     = totalInterestPaid
      schedule[i, :totalP]     = totalPrincipalPaid

      # If total principal paid is less than borrower, then it is a default, and the monthDefault
      # is adjusted.
      if (totalPrincipalPaid < loan)
        monthDefault = i
      end

      # No more payments to add to the schedule
      break

    else
      # Deal with normal schedule
      schedule[i, :monthlyPayment]   = instalment
      schedule[i, :principalPayment] = decreasePrincipal
      schedule[i, :totalPandI]       = schedule[i-1, :totalPandI] + instalment
      schedule[i, :totalI]           = schedule[i-1, :totalI]     + accruedInterest
      schedule[i, :totalP]           = schedule[i-1, :totalP]     + decreasePrincipal
    end
  end

  # At this point schedule[, :monthlyPayment] contains the schedule of all payments, but needs to
  # include the initial loan.
  schedule[1, :monthlyPayment] = -loan
  schedule[1, :principalPayment] = -loan

  if (showSchedule)
    println("Payments")
    println(schedule)
  end

  # Principal profile on the borrowing side
  creditFoncier = round.(CreateCreditFoncier(n = term, riskFree = riskFree)[:, :payment] .* loan;
                                             digits = 2)


  # Net final cashflow is:
  #     all monthly payments on the lending side
  # less
  #     borrowing profile
  cashFlow = schedule[:, :monthlyPayment] .- creditFoncier

  return((
    loanID = loanNumber,
    NPV =  sum(cashFlow ./ (1 + riskFree/12) .^ (0:nMonths))
  ))
end


####################################################################################################
##
## Quick check
##
NPV(loanNumber = 1, loan = 5600, intRate = 0.1299, term = 36,
             totalPaid = 6791.72, totalPrincipalPaid = 5600, totalInterestPaid = 1191.72,
             recoveries = 0, lateFees = 0,
             riskFree = 0.02, showSchedule = false)

NPV(loanNumber = 1, loan = 35000, intRate = 0.1820, term = 60,
             totalPaid = 26600.1, totalPrincipalPaid = 3874.72, totalInterestPaid = 5225.38,
             recoveries = 17500, lateFees = 0.0,
             riskFree = 0.02, showSchedule = true)

NPV(loanNumber = 1734666, loan = 35000, intRate = 0.0797, term = 36,
             totalPaid = 1057.04, totalPrincipalPaid = 863.83, totalInterestPaid = 193.72,
             recoveries = 0, lateFees = 0,
             riskFree = 0.02, showSchedule = true)

loanSchedule(loanNumber = 1, loan = 5600, intRate = 0.1299, term = 36,
    totalPaid = 6791.72, totalPrincipalPaid = 5600, totalInterestPaid = 1191.72,
    recoveries = 0, lateFees = 0,
    riskFree = 0.02, showSchedule = false)




##
## Look for the loans which have gone to their end
##
indextmp = (lendingClub.loan_status .== "Fully Paid") .|
           (lendingClub.loan_status .== "Charged Off") .|
           (lendingClub.loan_status .== "Does not meet the credit policy. Status:Charged Off") .|
           (lendingClub.loan_status .== "Does not meet the credit policy. Status:Fully Paid")

## Select relevant variables to calculate profitability
## Column1 contains the loanID's
cols = [:loanID, :issue_d, :funded_amnt, :int_rate, :term,
        :total_pymnt, :total_rec_prncp, :total_rec_int,
        :recoveries, :total_rec_late_fee]

## Create the dataset we will use - Should be the same as lending_club_reformatted_paid.rds
lc = select(lendingClub[indextmp, :], cols)

## Interest rates as percentage
lc[!, :int_rate] = lc[:, :int_rate] ./ 100

## Add the risk-free LIBOR rates
lc = join(lc, RATES, on = :issue_d => :DATE, kind = :left)

# Select the appropriate (sort of) depending on tenor.
lc[!, :risk_free] = ifelse.((lc[:, :term] .== 36) .== 1, lc[:, :RATE3Y], lc[:, :RATE5Y])

## New data frame to store the results
NPV_Result = DataFrame(loanID = zeros(Int64, nrow(lc)), NPV = zeros(Float64, nrow(lc)),
                                      pct = zeros(Float64, nrow(lc)))


# 360 sec. to do the whole dataset
@time for i in 1:nrow(lc)
  global NPV_Result

  # Use multiple-return-value
  # 1h17m runtime
  (NPV_Result[i, :loanID],
   NPV_Result[i, :NPV]) =
      NPV(loanNumber = lc[i, :loanID],
          loan = lc[i, :funded_amnt], intRate = lc[i, :int_rate], term =lc[i, :term],
          totalPaid = lc[i, :total_pymnt], totalPrincipalPaid = lc[i, :total_rec_prncp],
          totalInterestPaid = lc[i, :total_rec_int],
          recoveries = lc[i, :recoveries], lateFees = lc[i, :total_rec_late_fee],
          riskFree = lc[i, :risk_free],
          showSchedule = false)
end


# NPV as percentage of the amount lent
NPV_Result[!, :pct] = NPV_Result[:, :NPV] ./ lc[:, :funded_amnt]

CSV.write("datasets/NPVs.csv", NPV_Result)
