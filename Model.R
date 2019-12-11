#########################################################################################
##
## THIS IS A COPY OF CleanLoad.Rmd
##
#########################################################################################


# Load libraries
library(tidyverse)
library(lubridate)

library(gridExtra)
library(kableExtra)
library(lubridate)

library(dslabs)
library(knitr)
library(bookdown)

###################################################################################################
##
## LendingClub data
## This file is only used to create the working datasets
##
if (!exists("lending_club")) {
  lending_club <- readRDS("datasets/lending_club_reformatted.rds")
}

if (!exists("loans")) {
  loans <- readRDS("datasets/lending_club_reformatted_paid.rds")
}

###################################################################################################
##
## Response variables
##
if (!exists("LoansNPV")) {
  LoansNPV <- read.csv(gzfile("datasets/LoansNPV.csv.gz")) %>%
    as_tibble()
}

if (!exists("LoansIRR")) {
  LoansIRR <- read.csv(gzfile("datasets/LoansIRR.csv.gz")) %>%
    as_tibble()
}

if (!exists("LoansMargin")) {
  LoansMargin <-
    read.csv(gzfile("datasets/LoansCreditMargin.csv.gz")) %>%
    as_tibble() %>%

    # Remove monthDefault since already in IRR dataframe
    select(-monthDefault)
}

###################################################################################################
##
## Interest rate swaps
##
if (!exists("RATES")) {
  RATES <- readRDS("datasets/rates.rds")
}

if (!exists("RATES3Y")) {
  RATES3Y <- readRDS("datasets/rates3Y.rds")
}

if (!exists("RATES5Y")) {
  RATES5Y <- readRDS("datasets/rates5Y.rds")
}


###################################################################################################
##
## ZIP/FIPS code data
##
if (!exists("zips")) {
  zips <- readRDS("datasets/zips.rds")
}

if (!exists("zipfips")) {
  zipfips <- readRDS("datasets/kaggleCodes.rds")
}

###################################################################################################
##
## Macro data
##
if (!exists("medianIncome")) {
  medianIncome <- readRDS("datasets/medianincome.rds")
}

if (!exists("personalIncome")) {
  personalIncome <- readRDS("datasets/personalincome.rds")
}

if (!exists("unemploymentRate")) {
  unemploymentRate <- readRDS("datasets/unemployment.rds")
}


###################################################################################################
##
## Create the dataset joining LendingClub and IRR/NPV calculations
##

nSamples <- nrow(loans)

loansWorkingSet <- loans %>%
  # add IRR and credit margins calculations
  left_join(LoansIRR, by = "loanID") %>%
  left_join(LoansMargin, by = "loanID") %>%
  left_join(LoansNPV, by = "loanID") %>%
  rename(NPVLossPCT = pct) %>%
  # add a % principal loss variable
  mutate(principal_loss_pct = (funded_amnt - total_rec_prncp) / funded_amnt)



###################################################################################################
##
## Mostly generated from a LibreOffice spreadsheet for convenience purposes, then reformatted
## by RStudio.
##

LC_variable <-
  tibble(
    variable_name = 'loanID' ,
    description = 'NOTE THIS IS NOT AN ORIGINAL VARIABLE. IT WAS ADDED FOR THE PURPOSE OF TRACKING LOANS INDIVIDUALLY AS AND WHEN NEEDED.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'loan_amnt' ,
    description = 'The listed amount of the loan applied for by the borrower. If at some point in time, the credit department reduces the loan amount, then it will be reflected in this value.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'funded_amnt' ,
    description = 'The total amount committed to that loan at that point in time.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'funded_amnt_inv' ,
    description = 'The total amount committed by investors for that loan at that point in time.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'term' ,
    description = 'The number of payments on the loan. Values are in months and can be either 36 or 60.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'int_rate' ,
    description = 'Interest Rate on the loan',
    inModel = TRUE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'installment' ,
    description = 'The monthly payment owed by the borrower if the loan originates.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'grade' ,
    description = 'LC assigned loan grade',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sub_grade' ,
    description = 'LC assigned loan subgrade',
    inModel = TRUE,
    inPrediction = TRUE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'emp_title' ,
    description = 'The job title supplied by the Borrower when applying for the loan.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'emp_length' ,
    description = 'Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years. ',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'home_ownership' ,
    description = 'The home ownership status provided by the borrower during registration or obtained from the credit report. Our values are: RENT, OWN, MORTGAGE, OTHER, NONE',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'annual_inc' ,
    description = 'The self-reported annual income provided by the borrower during registration. NOT USED AS A VARIABLE SINCE JOINT INCOME ALREADY INCLUDES IT.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'verification_status' ,
    description = 'Indicates if income was verified by LC, not verified, or if the income source was verified',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'issue_d' ,
    description = 'The month which the loan was funded',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'loan_status' ,
    description = 'Current status of the loan',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'pymnt_plan' ,
    description = 'Indicates if a payment plan has been put in place for the loan',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'url' ,
    description = 'URL for the LC page with listing data.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'desc' ,
    description = 'Loan description provided by the borrower',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'purpose' ,
    description = 'A category provided by the borrower for the loan request. ',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'title' ,
    description = 'The loan title provided by the borrower',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'zip_code' ,
    description = 'The first 3 numbers of the zip code provided by the borrower in the loan application.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'addr_state' ,
    description = 'The state provided by the borrower in the loan application',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'dti' ,
    description = 'A ratio calculated using the borrower s total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower s self-reported monthly income. NOT USED AS A VARIABLE. ONLY USE JOINT DTI.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'delinq_2yrs' ,
    description = 'The number of 30+ days past-due incidences of delinquency in the borrower s credit file for the past 2 years',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'earliest_cr_line' ,
    description = 'The month the borrower s earliest reported credit line was opened',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'inq_last_6mths' ,
    description = 'The number of inquiries in past 6 months (excluding auto and mortgage inquiries)',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mths_since_last_delinq' ,
    description = 'The number of months since the borrower s last delinquency.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mths_since_last_record' ,
    description = 'The number of months since the last public record.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'open_acc' ,
    description = 'The number of open credit lines in the borrower s credit file.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'pub_rec' ,
    description = 'Number of derogatory public records',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'revol_bal' ,
    description = 'Total credit revolving balance',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'revol_util' ,
    description = 'Revolving line utilization rate, or the amount of credit the borrower is using relative to all available revolving credit.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'total_acc' ,
    description = 'The total number of credit lines currently in the borrower s credit file',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'initial_list_status' ,
    description = 'The initial listing status of the loan. Possible values are – W, F',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'out_prncp' ,
    description = 'Remaining outstanding principal for total amount funded. NOTE ONCE A LOAN IS REPAID OR CHARGED OFF, THIS AMOUNT BECOMES 0. ',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'out_prncp_inv' ,
    description = 'Remaining outstanding principal for portion of total amount funded by investors. NOTE ONCE A LOAN IS REPAID OR CHARGED OFF, THIS AMOUNT BECOMES 0. ',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'total_pymnt' ,
    description = 'Payments received to date for total amount funded',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'total_pymnt_inv' ,
    description = 'Payments received to date for portion of total amount funded by investors',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'total_rec_prncp' ,
    description = 'Principal received to date. NOTE THIS AMOUNT WILL SHOW WHETHER A BORROWER DID NOT REPAY IN FULL',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'total_rec_int' ,
    description = 'Interest received to date',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'total_rec_late_fee' ,
    description = 'Late fees received to date',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'recoveries' ,
    description = 'Post charge off gross recovery. NOTE IF A LOAN IS REPAID, THIS AMOUNT IS 0. ',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'collection_recovery_fee' ,
    description = 'Post charge off collection fee',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'last_pymnt_d' ,
    description = 'Last month payment was received',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'last_pymnt_amnt' ,
    description = 'Last total payment amount received',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'next_pymnt_d' ,
    description = 'Next scheduled payment date',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'last_credit_pull_d' ,
    description = 'The most recent month LC pulled credit for this loan',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'collections_12_mths_ex_med' ,
    description = 'Number of collections in 12 months excluding medical collections',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mths_since_last_major_derog' ,
    description = 'Months since most recent 90-day or worse rating',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'policy_code' ,
    description = 'Publicly available policy_code=1 / New products not publicly available policy_code=2',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'application_type' ,
    description = 'Indicates whether the loan is an individual application or a joint application with two coborrowers',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'annual_inc_joint' ,
    description = 'The combined self-reported annual income provided by the coborrowers during registration',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'dti_joint' ,
    description = 'A ratio calculated using the coborrowers total monthly payments on the total debt obligations, excluding mortgages and the requested LC loan, divided by the coborrowers combined self-reported monthly income',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'verification_status_joint' ,
    description = 'Indicates if income was verified by LC, not verified, or if the income source was verified',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'acc_now_delinq' ,
    description = 'The number of accounts on which the borrower is now delinquent.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'tot_coll_amt' ,
    description = 'Total collection amounts ever owed',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'tot_cur_bal' ,
    description = 'Total current balance of all accounts',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'open_acc_6m' ,
    description = 'Number of open trades in last 6 months',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'open_act_il' ,
    description = 'Number of currently active installment trades',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'open_il_12m' ,
    description = 'Number of installment accounts opened in past 12 months',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'open_il_24m' ,
    description = 'Number of installment accounts opened in past 24 months',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'mths_since_rcnt_il' ,
    description = 'Months since most recent instalment accounts opened',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'total_bal_il' ,
    description = 'Total current balance of all installment accounts',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'il_util' ,
    description = 'Ratio of total current balance to high credit/credit limit on all install acct',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'open_rv_12m' ,
    description = 'Number of revolving trades opened in past 12 months',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'open_rv_24m' ,
    description = 'Number of revolving trades opened in past 24 months',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'max_bal_bc' ,
    description = 'Maximum current balance owed on all revolving accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'all_util' ,
    description = 'Balance to credit limit on all trades',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'total_rev_hi_lim' ,
    description = 'Total revolving high credit/credit limit',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'inq_fi' ,
    description = 'Number of personal finance inquiries',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'total_cu_tl' ,
    description = 'Number of finance trades',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'inq_last_12m' ,
    description = 'Number of credit inquiries in past 12 months',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'acc_open_past_24mths' ,
    description = 'Number of trades opened in past 24 months.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'avg_cur_bal' ,
    description = 'Average current balance of all accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'bc_open_to_buy' ,
    description = 'Total open to buy on revolving bankcards.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'bc_util' ,
    description = 'Ratio of total current balance to high credit/credit limit for all bankcard accounts.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'chargeoff_within_12_mths' ,
    description = 'Number of charge-offs within 12 months',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'delinq_amnt' ,
    description = 'The past-due amount owed for the accounts on which the borrower is now delinquent.',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'mo_sin_old_il_acct' ,
    description = 'Months since oldest bank instalment account opened',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mo_sin_old_rev_tl_op' ,
    description = 'Months since oldest revolving account opened',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mo_sin_rcnt_rev_tl_op' ,
    description = 'Months since most recent revolving account opened',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mo_sin_rcnt_tl' ,
    description = 'Months since most recent account opened',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mort_acc' ,
    description = 'Number of mortgage accounts.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mths_since_recent_bc' ,
    description = 'Months since most recent bankcard account opened.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mths_since_recent_bc_dlq' ,
    description = 'Months since most recent bankcard delinquency',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mths_since_recent_inq' ,
    description = 'Months since most recent inquiry.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'mths_since_recent_revol_delinq' ,
    description = 'Months since most recent revolving delinquency.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_accts_ever_120_pd' ,
    description = 'Number of accounts ever 120 or more days past due',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_actv_bc_tl' ,
    description = 'Number of currently active bankcard accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_actv_rev_tl' ,
    description = 'Number of currently active revolving trades',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_bc_sats' ,
    description = 'Number of satisfactory bankcard accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_bc_tl' ,
    description = 'Number of bankcard accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_il_tl' ,
    description = 'Number of installment accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_op_rev_tl' ,
    description = 'Number of open revolving accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_rev_accts' ,
    description = 'Number of revolving accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_rev_tl_bal_gt_0' ,
    description = 'Number of revolving trades with balance >0',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_sats' ,
    description = 'Number of satisfactory accounts',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_tl_120dpd_2m' ,
    description = 'Number of accounts currently 120 days past due (updated in past 2 months)',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_tl_30dpd' ,
    description = 'Number of accounts currently 30 days past due (updated in past 2 months)',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_tl_90g_dpd_24m' ,
    description = 'Number of accounts 90 or more days past due in last 24 months',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'num_tl_op_past_12m' ,
    description = 'Number of accounts opened in past 12 months',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'pct_tl_nvr_dlq' ,
    description = 'Percent of trades never delinquent',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'percent_bc_gt_75' ,
    description = 'Percentage of all bankcard accounts > 75% of limit.',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'pub_rec_bankruptcies' ,
    description = 'Number of public record bankruptcies',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'tax_liens' ,
    description = 'Number of tax liens',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'tot_hi_cred_lim' ,
    description = 'Total high credit/credit limit',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'total_bal_ex_mort' ,
    description = 'Total credit balance excluding mortgage',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'total_bc_limit' ,
    description = 'Total bankcard high credit/credit limit',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'total_il_high_credit_limit' ,
    description = 'Total installment high credit/credit limit',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'revol_bal_joint' ,
    description = 'Total credit revolving balance',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_earliest_cr_line' ,
    description = 'Earliest credit line at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_inq_last_6mths' ,
    description = 'Credit inquiries in the last 6 months at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_mort_acc' ,
    description = 'Number of mortgage accounts at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_open_acc' ,
    description = 'Number of open trades at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_revol_util' ,
    description = 'Ratio of total current balance to high credit/credit limit for all revolving accounts. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_open_act_il' ,
    description = 'Number of currently active installment trades at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_num_rev_accts' ,
    description = 'Number of revolving accounts at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_chargeoff_within_12_mths' ,
    description = 'Number of charge-offs within last 12 months at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_collections_12_mths_ex_med' ,
    description = 'Number of collections within last 12 months excluding medical collections at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'sec_app_mths_since_last_major_derog' ,
    description = 'Months since most recent 90-day or worse rating at time of application for the secondary applicant. VARIABLE NOT USED. WE RELY ON THE MAIN BORROWER IN THE FIRST INSTANCE.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'hardship_flag' ,
    description = 'Flags whether or not the borrower is on a hardship plan',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_type' ,
    description = 'Describes the hardship plan offering',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'hardship_reason' ,
    description = 'Describes the reason the hardship plan was offered',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'hardship_status' ,
    description = 'Describes if the hardship plan is active, pending, cancelled, completed, or broken',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'deferral_term' ,
    description = 'Amount of months that the borrower is expected to pay less than the contractual monthly payment amount due to a hardship plan',
    inModel = FALSE,
    inPrediction = TRUE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_amount' ,
    description = 'The interest payment that the borrower has committed to make each month while they are on a hardship plan',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_start_date' ,
    description = 'The start date of the hardship plan period',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_end_date' ,
    description = 'The end date of the hardship plan period',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'payment_plan_start_date' ,
    description = 'The day the first hardship plan payment is due. For example, if a borrower has a hardship plan period of 3 months, the start date is the start of the three-month period in which the borrower is allowed to make interest-only payments.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_length' ,
    description = 'The number of months the borrower will make smaller payments than normally obligated due to a hardship plan',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_dpd' ,
    description = 'Account days past due as of the hardship plan start date',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_loan_status' ,
    description = 'Loan Status as of the hardship plan start date',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'orig_projected_additional_accrued_interest' ,
    description = 'The original projected additional interest amount that will accrue for the given hardship payment plan as of the Hardship Start Date. This field will be null if the borrower has broken their hardship payment plan.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_payoff_balance_amount' ,
    description = 'The payoff balance amount as of the hardship plan start date',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'hardship_last_payment_amount' ,
    description = 'The last payment amount as of the hardship plan start date',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = TRUE
  ) %>%
  add_row(
    variable_name = 'disbursement_method' ,
    description = 'The method by which the borrower receives their loan. Possible values are: CASH, DIRECT_PAY',
    inModel = TRUE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'debt_settlement_flag' ,
    description = 'Flags whether or not the borrower, who has charged-off, is working with a debt-settlement company.',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'debt_settlement_flag_date' ,
    description = 'The most recent date that the Debt_Settlement_Flag has been set  ',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'settlement_status' ,
    description = 'The status of the borrower’s settlement plan. Possible values are: COMPLETE, ACTIVE, BROKEN, CANCELLED, DENIED, DRAFT',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'settlement_date' ,
    description = 'The date that the borrower agrees to the settlement plan',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'settlement_amount' ,
    description = 'The loan amount that the borrower has agreed to settle for',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'settlement_percentage' ,
    description = 'The settlement amount as a percentage of the payoff balance amount on the loan',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  ) %>%
  add_row(
    variable_name = 'settlement_term' ,
    description = 'The number of months that the borrower will be on the settlement plan',
    inModel = FALSE,
    inPrediction = FALSE,
    inPricing = FALSE
  )




#########################################################################################
##
## Make room
##
rm(lending_club, loans)



#########################################################################################
##
## select the variables that might be used to create the training+test set
##

modelVarsIn <- c(LC_variable[LC_variable$inModel == TRUE, "variable_name"])$variable_name
modelVarsIn <- c(modelVarsIn,
                 "grade_num", "sub_grade_num",
                 "principal_loss_pct", "creditMargin", "monthDefault")

# Make sure that some variables are NOT in included in the final training set
modelVarsOut <- c("grade_num", "sub_grade_num",
                  "principal_loss_pct", "creditMargin", "monthDefault",
                  "zip_code")


#########################################################################################
##
## Prepare a dataset with ONLY the predictors NOT removing NA's
##

loansPredictors <-
  loansWorkingSet %>%

  # Keep the chosen predictors
  # Use tidyselect::one_of() to avoid errors if column does not exist
  select(one_of(modelVarsIn)) %>%

  ##
  ## Dates to numeric, in 'decimal' years since 2000
  ##
  mutate_at(c("issue_d", "earliest_cr_line"), function(d) {
    return(year(d) - 2000 + (month(d) - 1) / 12)
  }) %>%

  ## Add polynomials of the dates to model the time-trend shape
  mutate(
    issue_d2 = issue_d^2,
    issue_d3 = issue_d^3,
    earliest_cr_line2 = earliest_cr_line^2,
    earliest_cr_line3 = earliest_cr_line^3
  ) %>%

  ## Create a logical flag TRUE for non-defaulted (good) loans
  mutate(isGoodLoan = (principal_loss_pct < 0.001)) %>%

  select(-tidyselect::one_of(modelVarsOut))






#########################################################################################
##
## Create training / test sets 80%/20%
##
proportionTraining <- 0.8
set.seed(42)

nSamples <- nrow(loansPredictors)

sampleTraining <- sample(1:nSamples, floor(nSamples * proportionTraining),
                         replace = FALSE)
loansTraining <- loansPredictors %>% slice(sampleTraining)
loansTest <- loansPredictors %>% slice(-sampleTraining)

# Subsets of the training set
set.seed(42)
nSamplesTraining <- nrow(loansTraining)


# 20%
sample20 <- sample(1:nSamplesTraining, floor(nSamplesTraining * 0.20),
                   replace = FALSE)
loans20 <- loansTraining %>% slice(sample20)


#########################################################################################
##
## Make room
##
rm(loansWorkingSet, loansPredictors, LoansNPV, LoansIRR, LoansMargin, RATES, RATES3Y, RATES5Y)


#########################################################################################
##
## Load binner package
##


# Ensure that `binner` is here and available
if ("package:binner" %in% search()) {
  detach("package:binner", unload = TRUE, force = TRUE)
}

if (!("binner" %in% installed.packages()[,1])) {
  devtools::install_github("Emmanuel-R8/SMBinning")
}

library(binner)


#########################################################################################
##
## Loop through all variables to create bins
##
loansBinning <- loansTraining %>%
  mutate(
    home_ownership = as_factor(home_ownership),
    emp_length = as_factor(emp_length),
    grade = as_factor(grade)
  )

#########################################################################################
##
## New tibble to store the list of bins + Weight of Evidences factors
##
listBins <-
  tibble(
    variable = "",
    type = "",
    IV = 0.0,
    WoE = list(),
    .rows = 0
  )

# About 500 sec wall-time
startTime <- proc.time()

for (n in names(loansBinning)) {

  # We don't test the response with itself
  if (n != "isGoodLoan") {

    # For categorical variable
    if (class(loansBinning[[1, n]]) == "factor") {
      # cat(" is a factor, ")
      result <- WoETableCategorical(
        df = loansBinning,
        x = n,
        y = "isGoodLoan",
        maxCategories = 100)

    } else {
      # For continuous variable
      result <- WoETableContinuous(df = loansBinning,
                                   x = n,
                                   y = "isGoodLoan",
                                   p = 0.05)
    }

    tryCatch({
      if (is.na(result)) {
        # In case no WoE table is create (of not enough bins)
        add_row(
          listBins,
          variable = n,
          type = NA,
          IV = NA
        )
      } else {

        listBins <- listBins %>%
          add_row(
            variable = n,
            type = result$type,
            IV = result$IV,
            WoE = list(result$table)
          )

      }
    },
    finally = {})
  }
}
cat("-- Lapsed time: ", proc.time() - startTime)


# SAVE TO SPEED UP ITERATIVE EXPLORATION
# saveRDS(listBins, "datasets/listBins100.rds")


#########################################################################################
##
## Select relevant variables
##
bestBins <- listBins %>% filter(IV >= 0.02)

bestBins <-
  bestBins %>%
  filter(!(variable %in% c(
    "loanID", "term", "int_rate", "creditMargin", "loan_status",
    "grade", "sub_grade", "grade_num", "sub_grade_num",
    "emp_length", "home_ownership", "monthDefault",
    "principal_loss_pct", "creditMargin", "monthDefault",
    "isGoodLoan")))

# saveRDS(bestBins, "datasets/bestBins100.rds")


#########################################################################################
##
## Create data table with one-hot encoding
##

# Those variable will contain all the best characteristics. Every continuous variable is
# reformatted into factors reflecting the appropriate bins.

allFactorsAsCharacteristics <- loansTraining[,"loanID"]
allFactorsAsBins <- loansTraining[,"loanID"]

for (index in 1:nrow(bestBins)) {
  #for (index in 1:27) {
  name <- bestBins$variable[index][[1]]

  cat("--- Variable No. ",
      index,
      "-- Name: ",
      name, "\n")

  ltIndex <- which(names(loansTraining) == name)

  characteristic <- categoriseFromWoE(df = loansTraining[, ltIndex],
                                      varName =  name,
                                      woeTable = bestBins$WoE[index][[1]])

  bins <- categoriseFromWoE.Wide(df = loansTraining[, ltIndex],
                                 varName =  name,
                                 woeTable = bestBins$WoE[index][[1]])

  allFactorsAsCharacteristics <-
    allFactorsAsCharacteristics %>%
    cbind(characteristic)


  allFactorsAsBins <-
    allFactorsAsBins %>%
    cbind(bins)

}


# saveRDS(allFactorsAsCharacteristics, "datasets/allCharacteristics100.rds")
# saveRDS(allFactorsAsBins, "datasets/allBins100.rds")

#########################################################################################
##
## Logistic Regression
##

# Cleanup and reload

# Training set to speed up analysis
if (!exists("loansTraining")) {
  loansTraining <- readRDS("datasets/LoansTraining.rds")
}

if (!exists("allFactorsAsBins")) {
  allFactorsAsBins <- readRDS("datasets/allBins100.rds")
}


#########################################################################################
##
## All bins + response variable
##

loanSampleBins <-
  allFactorsAsBins %>%
  select(-loanID) %>%
  cbind(loansTraining$isGoodLoan) %>%
  rename(isGoodLoan = "loansTraining$isGoodLoan") %>%
  mutate(isGoodLoan = if_else(isGoodLoan, 1, 0)) %>%
  as.data.frame()

rm(allFactorsAsBins)



if (!exists("allFactorsAsCharacteristics")) {
  allFactorsAsCharacteristics <-
    readRDS("datasets/allCharacteristics100.rds")
}

loanSampleCharacteristics <-
  allFactorsAsCharacteristics %>%
  select(-loanID) %>%
  cbind(loansTraining$isGoodLoan) %>%
  rename(isGoodLoan = "loansTraining$isGoodLoan") %>%
  mutate(isGoodLoan = if_else(isGoodLoan, 1, 0)) %>%
  as.data.frame()


#########################################################################################
##
## Make room
##
rm(allFactorsAsCharacteristics)
rm(loansTraining)



#########################################################################################
##
## Remove identical bins
##
namesCharacteristics <- names(loanSampleCharacteristics)
namesBins <- names(loanSampleBins)


# Starting list of names, calculate a hash value for each column with that name and
# identify duplicates.
duplicateNames <- names(
  loanSampleBins[
    duplicated(
      lapply(loanSampleBins, digest::digest))])

# Remove any categories uniformaly constant (only 0)
#
# This is important when working on a small extract of the dataset for variables that are
# seldom used (e.g. customers from certain states).
zeroColumnsNames <- loanSampleBins[, colSums(loanSampleBins) == 0] %>% names()


#########################################################################################
##
## First model - complete dataset
##

# About 700 sec wall-time to complete training dataset
# The dataset is the sample less duplicate, less zero-ed columns
{
  doMC::registerDoMC(cores = 1) # Too many processes push over 32GB
  startTime <- proc.time()

  loansData <- loanSampleBins

  SGLM_B_train <- speedglm::speedglm(isGoodLoan ~ .,
                                     data = loansData,
                                     family = binomial())

  doMC::registerDoMC(cores = NULL)
  cat("-- Lapsed time: ", proc.time() - startTime)
}

# saveRDS(SGLM_B_train, "datasets/SGLM_B_train.rds")


NAsFirstTraining <-
  # Take the results (just the estimated coefficients) from the model
  tibble(
    name = rownames(summary(SGLM_B_train)$coefficient),
    estimate = summary(SGLM_B_train)$coefficient$Estimate
  ) %>%

  # Reformat the names to be the same as in the bins
  mutate(name = stringr::str_remove_all(name, "\`")) %>%

  # Make sure that repsonse is not deleted by mistake and list all NAs
  filter(name != "isGoodLoan" & is.na(estimate))

NAsFirstTraining <- NAsFirstTraining$name



#########################################################################################
##
## Second training - fitted model less colinear bins
##

# Start the model training again
{
  startTime <- proc.time()
  doMC::registerDoMC(cores = 2) # Too many processes push over 32GB if more than 2

  loansData <- loanSampleBins %>%
    select(-one_of( c(duplicateNames,
                      zeroColumnsNames,
                      NAsFirstTraining)))

  SGLM_B_retrain <- speedglm::speedglm(isGoodLoan ~ .,
                                       data = loansData,
                                       family = binomial())

  doMC::registerDoMC(cores = NULL)
  cat("-- Lapsed time: ", proc.time() - startTime)
}

# saveRDS(SGLM_B_retrain, "datasets/SGLM_B_retrain.rds")


NAsSecondTraining <-
  tibble(
    name = rownames(summary(SGLM_B_retrain)$coefficient),
    estimate = summary(SGLM_B_retrain)$coefficient$Estimate,
    zValue = abs(summary(SGLM_B_retrain)$coefficient$"z value")
  ) %>%

  mutate(name = stringr::str_remove_all(name, "\`")) %>%
  filter(name != "isGoodLoan") %>%

  # Remove stray NAs or anything less than 2 sigmas
  filter(is.na(estimate) | zValue < 2)

NAsSecondTraining <- NAsSecondTraining$name



#########################################################################################
##
## Third training - second fitted model less non-significant bins
##

# Start the model training again. About 350 sec.
{

  startTime <- proc.time()
  doMC::registerDoMC(cores = 2)

  loansData <- loanSampleBins %>%
    select(-one_of( c(
      duplicateNames,
      zeroColumnsNames,
      NAsFirstTraining,
      NAsSecondTraining
    )))

  SGLM_B_reretrain <- speedglm::speedglm(isGoodLoan ~ .,
                                         data = loansData,
                                         family = binomial())

  doMC::registerDoMC(cores = NULL)
  cat("-- Lapsed time: ", proc.time() - startTime)
}

# saveRDS(SGLM_B_reretrain, "datasets/SGLM_B_reretrain.rds")

NAsThirdTraining <-
  tibble(
    name     = rownames(summary(SGLM_B_reretrain)$coefficient),
    estimate = summary(SGLM_B_reretrain)$coefficient$Estimate,
    zValue   = abs(summary(SGLM_B_reretrain)$coefficient$"z value")
  ) %>%
  mutate(name = stringr::str_remove_all(name, "\`")) %>%
  filter(name != "isGoodLoan") %>%

  # Remove stray NAs (Shouldn't be any) or less than 2 sigmas
  filter(is.na(estimate) | zValue < 2)

NAsThirdTraining <- NAsThirdTraining$name



tibble(
  "Criteria" = c("AIC", "Log likelihood"),
  "Training #1" = c(
    speedglm:::summary.speedglm(SGLM_B_train)$aic,
    speedglm:::summary.speedglm(SGLM_B_train)$logLik
  ),

  "Training #2" = c(
    speedglm:::summary.speedglm(SGLM_B_retrain)$aic,
    speedglm:::summary.speedglm(SGLM_B_retrain)$logLik
  ),

  "Training #3" = c(
    speedglm:::summary.speedglm(SGLM_B_reretrain)$aic,
    speedglm:::summary.speedglm(SGLM_B_reretrain)$logLik
  )
) %>%

  kable(caption = "Training AIC and Log-likelihood", digits = 3) %>%
  kableExtra::kable_styling(latex_options = "HOLD_position")


rm(loansData)

rm(loansTraining,
   allFactorsAsCharacteristics,
   allFactorsAsBins,
   loanSampleBins,
   loanSampleCharacteristics)



# Saved at the end of the modeling file
SGLM_B_train <- readRDS("datasets/SGLM_B_train.rds")
SGLM_B_retrain <- readRDS("datasets/SGLM_B_retrain.rds")
SGLM_B_reretrain <- readRDS("datasets/SGLM_B_reretrain.rds")



#########################################################################################
##
## Model results
##

### Final list of variables

# Model to use
GLModel <- SGLM_B_retrain


# List of model variables
modelNames <-
  attr(GLModel$coefficients, "names") %>%
  enframe(x = .) %>%
  rename(variableName = "value") %>%
  select(variableName) %>%
  mutate(variableName = str_remove_all(variableName, "\`"))


# and their coefficients in the model (the summary function for speedglm objects is not exported and
# bookdown seems to have a problem with that).
GLMCoefficients <-
  speedglm:::summary.speedglm(GLModel)$coefficients %>%
  as_tibble() %>%
  cbind(modelNames) %>%
  rename(
    zValue = "z value",
    pValue = "Pr(>|z|)",
    stdErr = "Std. Error"
  ) %>%

  # reorder columns to have names first
  select(variableName, everything())


# The list of characteristics is extracted from the list of bins names.
numberOfCharacteristics <-

  modelNames %>%

  # List of all names excluding the intercept which is not part of the scoring calculations (it
  # would mean giving points for free as a base line)
  filter(variableName != "(Intercept)") %>%

  # Extract strings ("[a-zA-Z0-9-_]*") preceded by an opening bracket ("(?<=\\()") and followed by a
  # closing bracket ("(?=\\))"). (The column names were formatted this way for that purpose.) See
  # "https://github.com/rstudio/cheatsheets/blob/master/strings.pdf" for details on the regex.
  mutate(characteristic = str_match(variableName, "(?<=\\()[a-zA-Z0-9-_]*(?=\\))")) %>%

  # We are only interested in how many distinct characteristic there are.
  distinct(characteristic) %>%
  nrow()



#########################################################################################
##
## Score parameters
##

ProbDefaultAtAnchor <- 1/20

# The model is trained so that the 'Good' outcome is that a loan does not default. The odds that
# therefore calculated on the probability of no default.
ProbAtAnchor <- 1 - ProbDefaultAtAnchor
OddsAnchor <- ProbAtAnchor / (1 - ProbAtAnchor)
ScoreAnchor <- 2000

# Doubling odds = 100 points
DoubleOdds <- 100

ScoreFactor <- OddsAnchor / log(2)

# 5,000 points is 20:1 odds of default
ScoreOffset <- ScoreAnchor - ScoreFactor * log(OddsAnchor)


# Score at the intercept
Intercept <- summary(GLModel)$coefficients["(Intercept)", "Estimate"]

ScorePerVariable <- (ScoreFactor * Intercept + ScoreOffset) / numberOfCharacteristics
InterceptPerVariable <- Intercept * ScoreFactor / numberOfCharacteristics


# Convert coefficients to scores
GLMScores <-
  GLMCoefficients %>%
  mutate(
    weight = Estimate * ScoreFactor,
    weightScaled = weight + ScorePerVariable,
    points = round(weightScaled)
  )

GLMScores[1, "points"] <-  0



# Reload the right datasets
allFactorsAsBins <- readRDS("datasets/allBins100.rds")
loansTraining <- readRDS("datasets/LoansTraining.rds")
loansTest <- readRDS("datasets/LoansTest.rds")


# Remove every variable that is not in the list of variables in the model then convert into a matrix
allMatrix <-
  allFactorsAsBins[, !is.na(match(
    names(allFactorsAsBins),
    str_remove_all(GLMCoefficients$variableName, "\`")
  ))] %>%
  as.matrix()


# Add a column of 1s for the intercept
allMatrix <-
  cbind(as.vector(rep.int(
    x = 1, times = dim(allMatrix)[1]
  )), allMatrix)


# Done with this dataset
rm(allFactorsAsBins)


CoefficientsVector <- GLMCoefficients$Estimate %>% as.matrix()

# Score per variable
TrainingScorecard <- allMatrix %*% ( GLMScores$points %>% as.matrix() )
dim(allMatrix)
dim(GLMScores$points)

# CHECK: Any variable with NAs?
# GLMCoefficients$modelName[which(is.na(CoefficientsVector))]
# CoefficientsVector[is.na(CoefficientsVector)] <- 0
# dim(CoefficientsVector)

TrainingLogit <- allMatrix %*% CoefficientsVector
TrainingLogit <-
  enframe(TrainingLogit[, 1]) %>%
  mutate(oddsGood = exp(value),
         p = 1 / (1 + oddsGood)) %>%
  cbind(TrainingScorecard)






# Prepare the full test set
bestBins <- readRDS("datasets/bestBins100.rds")

predictionCategories <- loansTest[, "loanID"]

for (index in 1:length(bestBins$variable)) {
  binned <-
    binner::categoriseFromWoE.Wide(
      df = loansTest,
      varName = bestBins$variable[index],
      woeTable = bestBins$WoE[[index]]
    )

  predictionCategories <- cbind(predictionCategories, binned)
}



# Retain only the relevant scorecard categories
predictionMatrix <-
  predictionCategories[, !is.na(match(
    names(predictionCategories),
    str_remove_all(GLMCoefficients$variableName, "\`")
  ))] %>%
  as.matrix()



predictionMatrix <- cbind(as.vector(rep.int(x = 1, times = dim(predictionMatrix)[1])), predictionMatrix)

TestLogit <- predictionMatrix %*% CoefficientsVector
TestLogit <-
  tibble::enframe(TestLogit[, 1]) %>%
  mutate(
    p = 1 / (1 + exp(-value)),
    oddsGood = if_else(is.infinite(p / (1 - p)), 1e10, p / (1 - p)))

predictionScorecard <- predictionMatrix %*% ( GLMScores$points %>% as.matrix() )



## Confusion matrix

tCM <- loansTest %>%
  cbind(TestLogit) %>%
  select(p, isGoodLoan) %>%

  mutate(p = if_else(p >= 0.50, "GOOD", "BAD"),
         isGoodLoan = if_else(isGoodLoan, "GOOD", "BAD")) %>%
  rename(Predicted = p,
         Actual = isGoodLoan) %>%

  table() %>%
  caret::confusionMatrix(positive = "GOOD")

tCM


## ROC Curve

ROCRPrediction <- ROCR::prediction(TestLogit$p, loansTest$isGoodLoan)
ROCR::performance(ROCRPrediction,
                  measure = "tpr",
                  x.measure = "fpr") %>%
  ROCR::plot(colorize = TRUE)


ROCR::performance(ROCRPrediction,
                  measure = "prec",
                  x.measure = "rec") %>%
  ROCR::plot(colorize = TRUE)


ROCR::performance(ROCRPrediction,
                  measure = "sens",
                  x.measure = "spec") %>%
  ROCR::plot(colorize = TRUE)


ROCRPerformance <- ROCR::performance(ROCRPrediction,
                  measure = "lift",
                  x.measure = "rpp") %>%
  ROCR::plot(colorize = TRUE)

