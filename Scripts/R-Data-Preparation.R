#
# STEP 1: Download the dataset
#
#   Got to https://www.kaggle.com/wendykan/lending-club-loan-data
#
#   Download into the 'datasets' subdirectory
#   Unzip the file.
#   WARNING: The unzipping will be about 2.4GB
#
#   Name the sql database "datasets/lending_club.sqlite"
#

#
# STEP 2: Prepare the dabase as a tibble
#

##
## WARNING: THIS ASSUMES A 'datasets' DIRECTORY WAS CREATED
##
library(RSQLite)
db_conn <-
  dbConnect(RSQLite::SQLite(), "datasets/lending_club.sqlite")
dbListTables(db_conn)

# Returns a 2.96GB data frame
lending_club <- dbGetQuery(db_conn, "SELECT * FROM loan")
lending_club <- as_tibble(lending_club)

# Close the database
dbDisconnect(db_conn)

# Compressed to ca.285MB on disk
saveRDS(lending_club, "datasets/lending_club.rds")


library(tidyverse)
library(lubridate)
library(hablar)

# Before reformat in case the previous step was already done
# lending_club <- readRDS("datasets/lending_club.rds")
#
# str(lending_club)
#

# Leave the original dataset untouched and work with a copy.
lc <- lending_club

lc <- lc %>%

  # Add loan identification number to track the loans across calculations
  mutate(loanID = row_number()) %>%

  # Remove useless strings
  mutate(
    term       = str_remove(term, " months"),
    emp_length = str_replace(emp_length, "<1", "0"),
    emp_length = str_replace(emp_length, "10+", "10"),
    emp_length = str_remove(emp_length, "years")
  ) %>%

  # Creates dates out of strings - Parse errors will be raised when no dates.
  mutate(
    debt_settlement_flag_date = as_date(dmy(str_c(
      "1-", debt_settlement_flag_date
    ))),
    earliest_cr_line          = as_date(dmy(str_c(
      "1-", earliest_cr_line
    ))),
    hardship_start_date       = as_date(dmy(str_c(
      "1-", hardship_start_date
    ))),
    hardship_end_date         = as_date(dmy(str_c(
      "1-", hardship_end_date
    ))),
    issue_d                   = as_date(dmy(str_c("1-", issue_d))),
    last_credit_pull_d        = as_date(dmy(str_c(
      "1-", last_credit_pull_d
    ))),
    last_pymnt_d              = as_date(dmy(str_c("1-", last_pymnt_d))),
    next_pymnt_d              = as_date(dmy(str_c("1-", next_pymnt_d))),
    payment_plan_start_date   = as_date(dmy(str_c(
      "1-", payment_plan_start_date
    ))),
    sec_app_earliest_cr_line  = as_date(dmy(str_c(
      "1-", sec_app_earliest_cr_line
    ))),
    settlement_date           = as_date(dmy(str_c(
      "1-", settlement_date
    )))
  ) %>%

  # Bulk type conversion with convert from the `hablar` package
  convert(
    # Strings
    chr(emp_title, title, url, zip_code),

    # Factors
    fct(
      addr_state,
      application_type,
      debt_settlement_flag,
      desc,
      disbursement_method,
      grade,
      hardship_flag,
      hardship_loan_status,
      hardship_reason,
      hardship_status,
      hardship_type,
      home_ownership,
      id,
      initial_list_status,
      loan_status,
      member_id,
      policy_code,
      purpose,
      pymnt_plan,
      settlement_status,
      sub_grade,
      verification_status,
      verification_status_joint
    ),

    # Integers
    int(
      acc_now_delinq,
      acc_open_past_24mths,
      chargeoff_within_12_mths,
      collections_12_mths_ex_med,
      deferral_term,
      delinq_2yrs,
      emp_length,
      hardship_dpd,
      hardship_length,
      inq_fi,
      inq_last_12m,
      inq_last_6mths,
      mo_sin_old_il_acct,
      mo_sin_old_rev_tl_op,
      mo_sin_rcnt_rev_tl_op,
      mo_sin_rcnt_tl,
      mort_acc,
      mths_since_last_delinq,
      mths_since_last_major_derog,
      mths_since_last_record,
      mths_since_rcnt_il,
      mths_since_recent_bc,
      mths_since_recent_bc_dlq,
      mths_since_recent_inq,
      mths_since_recent_revol_delinq,
      num_accts_ever_120_pd,
      num_actv_bc_tl,
      num_actv_rev_tl,
      num_bc_sats,
      num_bc_tl,
      num_il_tl,
      num_op_rev_tl,
      num_rev_accts,
      num_rev_tl_bal_gt_0,
      num_sats,
      num_tl_120dpd_2m,
      num_tl_30dpd,
      num_tl_90g_dpd_24m,
      num_tl_op_past_12m,
      open_acc,
      open_acc_6m,
      open_act_il,
      open_il_12m,
      open_il_24m,
      open_rv_12m,
      open_rv_24m,
      sec_app_chargeoff_within_12_mths,
      sec_app_collections_12_mths_ex_med,
      sec_app_inq_last_6mths,
      sec_app_mort_acc,
      sec_app_mths_since_last_major_derog,
      sec_app_num_rev_accts,
      sec_app_open_acc,
      sec_app_open_act_il,
      term
    ),

    # Floating point
    dbl(
      all_util,
      annual_inc,
      annual_inc_joint,
      avg_cur_bal,
      bc_open_to_buy,
      bc_util,
      collection_recovery_fee,
      delinq_amnt,
      dti,
      dti_joint,
      funded_amnt,
      funded_amnt_inv,
      hardship_amount,
      hardship_last_payment_amount,
      hardship_payoff_balance_amount,
      il_util,
      installment,
      int_rate,
      last_pymnt_amnt,
      loan_amnt,
      max_bal_bc,
      orig_projected_additional_accrued_interest,
      out_prncp,
      out_prncp_inv,
      pct_tl_nvr_dlq,
      percent_bc_gt_75,
      pub_rec,
      pub_rec_bankruptcies,
      recoveries,
      revol_bal,
      revol_bal_joint,
      revol_util,
      sec_app_revol_util,
      settlement_amount,
      settlement_percentage,
      tax_liens,
      tot_coll_amt,
      tot_cur_bal,
      tot_hi_cred_lim,
      total_acc,
      total_bal_ex_mort,
      total_bal_il,
      total_bc_limit,
      total_cu_tl,
      total_il_high_credit_limit,
      total_pymnt,
      total_pymnt_inv,
      total_rec_int,
      total_rec_late_fee,
      total_rec_prncp,
      total_rev_hi_lim
    )
  ) %>%

  # Converts some values to 1/-1 (instead of Boolean)
  mutate(
    pymnt_plan =           if_else(pymnt_plan == "y",           1, -1),
    hardship_flag =        if_else(hardship_flag == "Y",        1, -1),
    debt_settlement_flag = if_else(debt_settlement_flag == "Y", 1, -1)
  ) %>%

  # Some values are percentages
  mutate(
    int_rate = int_rate / 100,
    dti = dti / 100,
    dti_joint = dti_joint / 100,
    revol_util = revol_util / 100,
    il_util = il_util / 100,
    all_util = all_util / 100,
    bc_open_to_buy = bc_util / 100,
    pct_tl_nvr_dlq = pct_tl_nvr_dlq / 100,
    percent_bc_gt_75 = percent_bc_gt_75 / 100,
    sec_app_revol_util = sec_app_revol_util / 100
  ) %>%

  # Create quasi-centered numerical grades out of grade factors with "A" = 3 down to "G" = -3
  mutate(grade_num = 4 - as.integer(grade)) %>%

  # Ditto with sub_grades. "A1" = +3.4, "A3" = +3.0, down to "G3" = -3.0, "G5" = -3.4
  mutate(sub_grade_num = 3.6 - as.integer(sub_grade) / 5) %>%

  # Keep the first 3 digits of the zipcode as numbers
  mutate(zip_code = as.integer(str_sub(zip_code, 1, 3))) %>%

  # order by date
  arrange(issue_d) %>%

  # Remove empty columns
  select(-id, -member_id, -url)

saveRDS(lc, "datasets/lending_club_reformatted.rds")


# Select loans which have matured or been terminated
past_loans <- lc %>%
  filter(
    loan_status %in% c(
      "Charged Off",
      "Does not meet the credit policy. Status:Charged Off",
      "Does not meet the credit policy. Status:Fully Paid",
      "Fully Paid"
    )
  )

saveRDS(past_loans, "datasets/lending_club_reformatted_paid.rds")
