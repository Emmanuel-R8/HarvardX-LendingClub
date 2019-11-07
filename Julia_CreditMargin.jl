####################################################################################################
##
## Prepare datasets
##

## Previously saved from R with:
##     lending_club <- readRDS("lending_club.rds"); write.csv(lending_club, "lending_club.rds")
##
## WARNING: 1.7GB on disk
##
using CSV
lendingClub = CSV.read("datasets/lending_club.csv"; delim = ",")



####################################################################################################
##
## CREDIT MARGIN
##
## This method of approximating the credit margin is far less sophisticated than what FI's do.
##
## We need to calculate what the credit margin should be on a defaulted loan to get a nil NPV.
##
## On a risk free loan the CF will be P+I at risk-free on _both_ borrowing and lending sides.
##
##  On a defaulted loan, the CF will be:
##      borrowing unchanged = P&I at risk-free
##    and
##      lending P&I at (risk-free + credit margin) until before default, then recoveries+fees.
##
## The principal amortisation profile depends on the credit margin used. We will arbitrarily use
## 20% which is a conservative assumption.
##
## credit risk on that CF should be nil with the right margin when discounted at risk-free.
##
## The function is very similar to the IRR calculation.
##

using DataFrames, Roots

# number of monthly payments to model
# It exceeds 60 months in case recoveries on a 60-month loan takes the schedule after 60 months.
const nMonths = 90



# Sculpt credit foncier profiles over 36 and 60 months at 20% per annum.
# The profile is expressed as percentage of loan amount
function CreateCreditFoncier(;n = 36, riskFree = 0.0)

  instalment = 1 * riskFree/12 * 1 / (1 - 1 / (1 + riskFree/12) ^ n)

  # We create a schedule
  schedule = DataFrame(month = 0:nMonths, payment = 0.0)

  # Add the day 1 principal outlay
  schedule[1,       :payment] = -1
  schedule[2:(n+1), :payment] = instalment

  return(schedule)
end


# Solve for the credit margin
function CreditMargin(; loanNumber = 1, loan = 1000.0, intRate = 0.05, term = 36,
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
      schedule[i, :monthlyPayment] += totalPrincipalPaid - previousTotalP

      # Then 3-month after the last payment date, recoveries and and later fees are paid
      schedule[i + 3, :principalPayment] += recoveries + lateFees

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
  schedule[1, :principalPayment] = -loan

  if (showSchedule)
    println("Payments")
    println(schedule)
  end

  # Principal profile on the borrowing side
  creditFoncier = round.(CreateCreditFoncier(n = term, riskFree = riskFree)[:, :payment] .* loan;
                                             digits = 2)

  # For a given margin, calculate the _net_ NPV between the what is borrowed at the risk-free rate
  # and what is earned on the loan principal profile (possibly shortned because of default)
  # carrying an interest of risk-free + credit margin
  function NetNPV(margin)
    # We need to store the calculated interest
    interestSchedule = DataFrame(month = 0:nMonths, interestPayment = 0.0)

    # For each month, calculate the amount of interest with the credit margin
    for i in 2:(monthDefault + 1)
      outstandingPrincipal = sum(schedule[1:(i - 1), :principalPayment])
      interestSchedule[i, :interestPayment] =
            -round((riskFree + margin)/12 * outstandingPrincipal; digits = 2)
    end

    # Net final cashflow is:
    #     total principal and interest cashflow on the lending side
    # less
    #     borrowing profile
    cashFlow = schedule[:, :principalPayment] .+ interestSchedule[:, :interestPayment]
    cashFlow = cashFlow .- creditFoncier

    return sum(cashFlow ./ (1 + riskFree/12) .^ (0:nMonths))
  end

  # rootInterest = round(find_zero(NetNPV, (-0.5, 10), Bisection()); digits = 6)
  rootInterest = try
                   rootInterest = round(find_zero(NetNPV, (-0.5, 10), Bisection()); digits = 6)
                 catch e
                   NaN
                 end


  return((
    loanID = loanNumber,
    creditMargin = rootInterest,
    monthDefault = monthDefault
  ))
end


####################################################################################################
##
## Quick check
##
CreditMargin(loanNumber = 1, loan = 5600, intRate = 0.1299, term = 36,
             totalPaid = 6791.72, totalPrincipalPaid = 5600, totalInterestPaid = 1191.72,
             recoveries = 0, lateFees = 0,
             riskFree = 0.02, showSchedule = false)

CreditMargin(loanNumber = 1, loan = 35000, intRate = 0.1820, term = 60,
             totalPaid = 26600.1, totalPrincipalPaid = 3874.72, totalInterestPaid = 5225.38,
             recoveries = 17500, lateFees = 0.0,
             riskFree = 0.02, showSchedule = true)

CreditMargin(loanNumber = 1734666, loan = 35000, intRate = 0.0797, term = 36,
             totalPaid = 1057.04, totalPrincipalPaid = 863.83, totalInterestPaid = 193.72,
             recoveries = 0, lateFees = 0,
             riskFree = 0.02, showSchedule = true)




##
## Look for the loans which have gone to their end
##
indextmp = (lendingClub.loan_status .== "Fully Paid") .|
           (lendingClub.loan_status .== "Charged Off") .|
           (lendingClub.loan_status .== "Does not meet the credit policy. Status:Charged Off") .|
           (lendingClub.loan_status .== "Does not meet the credit policy. Status:Fully Paid")

## Create the dataset we will use - Should be the same as lending_club_reformatted_paid.rds
lc = lendingClub[indextmp, :]

## Select relevant variables to calculate profitability
## Column1 contains the loanID's
cols = [:Column1, :funded_amnt, :int_rate, :term,
        :total_pymnt, :total_rec_prncp, :total_rec_int,
        :recoveries, :total_rec_late_fee]

lc = select(lc, cols)

## Interest rates as percentage
lc[!, :int_rate] = lc[!, :int_rate] ./ 100

## Create a new column
lc[:tenor] = 0

## that will record the official loan tenor as a number (instead of string)
lc[startswith.( lc[!, :term], " 36"), :tenor] .= 36
lc[startswith.( lc[!, :term], " 60"), :tenor] .= 60

## New data frame to store the results
creditMargin_Result = DataFrame(loanID = zeros(Int64, nrow(lc)),
                                creditMargin = zeros(Float64, nrow(lc)),
                                monthDefault = zeros(Int64, nrow(lc)))


# 4,700 sec. to do the whole dataset
@time for i in 1:nrow(lc)
  global creditMargin_Result

  # Use multiple-return-value
  # 1h17m runtime
  (creditMargin_Result[i, :loanID],
   creditMargin_Result[i, :creditMargin],
   creditMargin_Result[i, :monthDefault]) =
      CreditMargin(
          loanNumber = lc[i, :Column1],
          loan = lc[i, :funded_amnt], intRate = lc[i, :int_rate], term =lc[i, :tenor],
          totalPaid = lc[i, :total_pymnt], totalPrincipalPaid = lc[i, :total_rec_prncp],
          totalInterestPaid = lc[i, :total_rec_int],
          recoveries = lc[i, :recoveries], lateFees = lc[i, :total_rec_late_fee],
          riskFree = 0.02,
          showSchedule = false)
end


creditMargin_Result[1:10,:]

CSV.write("datasets/CreditMargins.csv", creditMargin_Result)
