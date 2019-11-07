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
## IRR calculations
##
## Given some numerical parameters describing a loan in the dataset, returns its Internal Rate
## of Return.
##
## In the first instance, the function creates a schedule of payments.
## In many cases, the schedule will be extremely simple: a series of 36 or 60 equal instalements.
##
## But in some cases, a loan repayment are accelerated. Therefore the total amount of interest will
## be lower than expected (but this is good for the investor because highe interest rate over
## shorter tenor.).
##
## In other cases, the borrower defaults. Overall payments are less than expected.
##
## Based on the limited information of the dataset, the function makes educated guesses on the exact
## schedule.
##
using DataFrames, Roots

function calculateIRR(; loanNumber = 1, loan = 0.0, intRate = 0.0, term = 36,
  totalPaid = 0.0, totalPrincipalPaid = 0.0, totalInterestPaid = 0.0,
  recoveries = 0.0, lateFees = 0.0,
  showSchedule = false)

  # number of monthly payments.
  # It exceeds 60 months in case recoveries on a 60-month loan takes the schedule after 60 months.
  nMonths = 90

  # Months after which a loan defaults (normal tenor if no default or early prepayment)
  monthDefault = term

  # Note: *100 /100 to calculate in cent because ceiling cannot specify significant digits.
  installment = ceil(loan * intRate / 12 / (1 - 1 / (1 + intRate / 12) ^ term), digits = 2)

  # We create a schedule
  schedule = DataFrame(month = 0:nMonths, monthlyPayment = 0.0,
                       totalPandI = 0.0, totalI = 0.0, totalP = 0.0)

  for i in 2:(nMonths + 1)
    # Get situation at the end of previous month
    previousTotalPandI = schedule[i - 1, :totalPandI]
    previousTotalP     = schedule[i - 1, :totalP]
    previousTotalI     = schedule[i - 1, :totalI]

    # This is the beginning of a new month. First and foremost, the borrower is expected to pay the
    # accrued interest on amount of principal outstanding.
    # The installment is expected to cover that amount of interest and the rest goes to
    # reducing the principal due outstanding.
    accruedInterest = ceil((loan - previousTotalP) * intRate / 12; digits = 2)
    decreasePrincipal = installment - accruedInterest

    # If that amount takes the schedule above the total amount of interest shown in the data set,
    # we should stop the schedule at this point
    # This is a shortcut since we could have a payment higher than the interest due, but not enough
    # to cover the expected principal repayment. However, it works well in practice.
    if previousTotalI + accruedInterest > totalInterestPaid

      # We stop the normal schedule at this date.
      # Interest is paid (although less than scheduled)
      schedule[i, :monthlyPayment] = totalInterestPaid - previousTotalI

      # As well as whatever principal is left as per the dataset
      schedule[i, :monthlyPayment] = schedule[i, :monthlyPayment] + totalPrincipalPaid - previousTotalP

      # Then 3-month after the last payment date, recoveries and and later fees are paid
      schedule[i + 3, :monthlyPayment] = schedule[i + 3, :monthlyPayment] + recoveries + lateFees

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
      schedule[i, :monthlyPayment] = installment
      schedule[i, :totalPandI]     = schedule[i - 1, :totalPandI] + installment
      schedule[i, :totalI]         = schedule[i - 1, :totalI]     + accruedInterest
      schedule[i, :totalP]         = schedule[i - 1, :totalP]     + installment - accruedInterest
    end
  end

  # At this point schedule[, :monthlyPayment] contains the schedule of all payments, but needs to
  # include the initial loan.
  schedule[1, :monthlyPayment] = -loan

  if (showSchedule)
    print(schedule)
  end

  cashFlow = schedule[:,:monthlyPayment]

  ##
  ## Finding the IRR is equivalent to finding the root such that the NPV of the cash flow is zero.
  ## Julia has a function (see below) called `find_zero` to do that which requires a function to
  ## be zeroed. This helper function is defined as NPV.
  ##
  function NPV(interest)
    t = 0:(length(cashFlow) - 1)

    ## If you are new to Julia, note the dot before the operation. This indicates that the
    ## operation has to be done element-wise (called `broadcasting` in Julia-ese).
    ## Otherwise, Julia would try to divide one vector by another vector, which makes no sense.
    ## This is also exactly the approach taken in Matlab/Octave.
    return sum(cashFlow ./ (1 + interest) .^ t)
  end

  ## Finds the root, catching any problems which would instead return the R equivalent of NA
  rootInterest = try
                  round(12 * find_zero(NPV, (-0.9, 1.0), Bisection(); xatol = 0.000001); digits = 4)
                catch e
                  NaN
                end

  return((
    loanID = loanNumber,
    IRR = rootInterest,
    monthDefault = monthDefault
  ))
end


##
## Calculate the IRR and repayment schedule of a particular loan identified by its loanID
##
function loanNumberIRR(loanNumber)
  l = lc[ lc[:, :Column1] .== loanNumber, :]
  global lc
  calculateIRR(loanNumber = l[1, :Column1],
               loan = l[1, :funded_amnt], intRate = l[1, :int_rate], term =l[1, :tenor],
               totalPaid = l[1, :total_pymnt], totalPrincipalPaid = l[1, :total_rec_prncp],
               totalInterestPaid = l[1, :total_rec_int],
               recoveries = l[1, :recoveries], lateFees = l[1, :total_rec_late_fee],
               showSchedule = true)
end


####################################################################################################
##
## Quick check
##
calculateIRR(loanNumber = 1, loan = 5600, intRate = 0.1299, term = 36,
             totalPaid = 6791.72, totalPrincipalPaid = 5600, totalInterestPaid = 1191.72,
             recoveries = 0, lateFees = 0,
             showSchedule = true)

calculateIRR(loanNumber = 1, loan = 35000, intRate = 0.1820, term = 60,
             totalPaid = 26600.1, totalPrincipalPaid = 3874.72, totalInterestPaid = 5225.38,
             recoveries = 17500, lateFees = 0.0,
             showSchedule = false)

calculateIRR(loanNumber = 1734666, loan = 35000, intRate = 0.0797, term = 36,
             totalPaid = 1057.04, totalPrincipalPaid = 863.83, totalInterestPaid = 193.72,
             recoveries = 0, lateFees = 0,
             showSchedule = false)




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
lc[:, :int_rate] = lc[:, :int_rate] ./ 100

## Create a new column
lc[:tenor] = 0

## that will record the official loan tenor as a number (instead of string)
lc[startswith.( lc[:, :term], " 36"), :tenor] .= 36
lc[startswith.( lc[:, :term], " 60"), :tenor] .= 60

## New data frame to store the results
IRR_Result = DataFrame(loanID = zeros(Int64, nrow(lc)),
                       IRR = zeros(Float64, nrow(lc)),
                       monthDefault = zeros(Int64, nrow(lc)))


# ~150 sec. to do the whole dataset
@time for i in 1:nrow(lc)
  global IRR_Result

  # Use multiple-return-value
  (IRR_Result[i, :loanID], IRR_Result[i, :IRR], IRR_Result[i, :monthDefault]) =
      calculateIRR(
          loanNumber = lc[i, :Column1],
          loan = lc[i, :funded_amnt], intRate = lc[i, :int_rate], term =lc[i, :tenor],
          totalPaid = lc[i, :total_pymnt], totalPrincipalPaid = lc[i, :total_rec_prncp],
          totalInterestPaid = lc[i, :total_rec_int],
          recoveries = lc[i, :recoveries], lateFees = lc[i, :total_rec_late_fee],
          showSchedule = false)
end


IRR_Result[1:10,:]
# Check
loanNumberIRR(171)

CSV.write("datasets/loanIRR.csv", IRR_Result)
