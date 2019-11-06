# Libraries
library(tidyverse)
library(lubridate)

# Load data
if (!exists("loans")) loans <- readRDS("datasets/lending_club_reformatted_paid.rds")
if (!exists("loansIRR")) loansIRR <- read.csv("datasets/loanIRR.csv") %>% as_tibble()
nSamples <- nrow(loans)


loans <- loans %>%
  # add IRR calculations
  left_join(loansIRR, by = "loanID") %>%

  # add a % principal loss variable
  mutate(principal_loss_pct = (funded_amnt - total_rec_prncp) / funded_amnt)

# Select the variables checked in 01-startup.Rmd
varList <- c(LC_variable[LC_variable$inModel == TRUE, "variable_name"])$variable_name


#################################################################################################
##
## Prepare a dataset with ONLY the predictors
##
loansPredictors <-
  loans %>%

  # Keep the chosen predictors
  select(varList) %>%

  # [TODO] FOR THE MOMENT UNTIL MACRODATA IS FIXED
  select(-"addr_state") %>%

  ##
  ## Dates to numeric
  ##
  mutate_at(c("issue_d", "earliest_cr_line"), function(d) {
    return(year(d) + month(d) / 12)
  }) %>%

  # Fill missing values
  mutate_all(~replace(., is.na(.), 0))


#################################################################################################
##
## One-hot encoding of factors (in case model do not mix continuous and cat. variables)
##
modelX <-
  loansPredictors %>%

  # Binary categories into 0/1
  mutate(disbursement_method = if_else(disbursement_method == "Cash", 1, 0)) %>%
  rename(cashDisbursement = disbursement_method) %>%

  mutate(application_type = if_else(application_type == "Individual", 0, 1)) %>%
  rename(applicationJoint = application_type) %>%

  ## Widen the categorical datas
  # Verification
  mutate(verifiedSource      = if_else(verification_status       == "Source Verified", 1, 0),
         verified            = if_else(verification_status       == "Not Verified", 0, 1),
         verifiedJointSource = if_else(verification_status_joint == "Source Verified", 1, 0),
         verifiedJoint       = if_else(verification_status_joint %in% c("", "Not Verified"), 0, 1)) %>%
  select(-verification_status, -verification_status_joint) %>%

  # Home ownership
  mutate(homeANY = if_else(home_ownership == "ANY", 1, 0),
         homeMORTGAGE = if_else(home_ownership == "MORTGAGE", 1, 0),

         # Delete beacuse useless
         homeNONE = if_else(home_ownership == "NONE", 1, 0),
         homeOTHER = if_else(home_ownership == "OTHER", 1, 0),
         homeOWN = if_else(home_ownership == "OWN", 1, 0),
         homeRENT = if_else(home_ownership == "RENT", 1, 0)) %>%
  select(-home_ownership) %>%

  # Purpose
  mutate(purpCAR = if_else(purpose == "car", 1, 0),
         purpCREDITCARD = if_else(purpose == "credit_card", 1, 0),
         purpCDEBTCONSO = if_else(purpose == "debt_consolidation", 1, 0),
         purpEDUCATION = if_else(purpose == "educational", 1, 0),
         purpHOMEIMPROV = if_else(purpose == "home_improvement", 1, 0),
         purpHOUSE = if_else(purpose == "house", 1, 0),
         purpPURCHASE = if_else(purpose == "major_purchase", 1, 0),
         purpMEDICAL = if_else(purpose == "medical", 1, 0),
         purpMOVING = if_else(purpose == "moving", 1, 0),
         purpOTHER = if_else(purpose == "other", 1, 0),
         purpRENEWABLE = if_else(purpose == "renewable_energy", 1, 0),
         purpSMALLBUS = if_else(purpose == "small_business", 1, 0),
         purpVACATION = if_else(purpose == "vacation", 1, 0),
         purpWEDDING = if_else(purpose == "wedding", 1, 0)) %>%
  select(-purpose)


# Seed
set.seed(42)
sample005 <- sample(1:nSamples, floor(nSamples * 0.005), replace = FALSE)
sample01  <- sample(1:nSamples, floor(nSamples * 0.01), replace = FALSE)
sample05  <- sample(1:nSamples, floor(nSamples * 0.05), replace = FALSE)
sample10  <- sample(1:nSamples, floor(nSamples * 0.10), replace = FALSE)
sample20  <- sample(1:nSamples, floor(nSamples * 0.20), replace = FALSE)

loans005 <- loansPredictors %>% slice(sample005)
loans01  <- loansPredictors %>% slice(sample01)
loans05  <- loansPredictors %>% slice(sample05)
loans10  <- loansPredictors %>% slice(sample10)
loans20  <- loansPredictors %>% slice(sample20)

model005 <- modelX %>% slice(sample005)
model01  <- modelX %>% slice(sample01)
model05  <- modelX %>% slice(sample05)
model10  <- modelX %>% slice(sample10)
model20  <- modelX %>% slice(sample20)

G01 <- loans %>% select(sub_grade_num) %>% slice(sample01)
G05 <- loans %>% select(sub_grade_num) %>% slice(sample05)
G10 <- loans %>% select(sub_grade_num) %>% slice(sample10)
G20 <- loans %>% select(sub_grade_num) %>% slice(sample20)

loss01 <- loans %>% select(principal_loss_pct) %>% slice(sample01)
loss05 <- loans %>% select(principal_loss_pct) %>% slice(sample01)
loss10 <- loans %>% select(principal_loss_pct) %>% slice(sample01)
loss20 <- loans %>% select(principal_loss_pct) %>% slice(sample01)


#
# This method of approximating the credit margin is far less sophisticated than what FI's do.
# Need to calculate what should the credit margin be on a defaulted loan
#
# CF principal is -L the amortising P
#
# On a risk free loan the CF will be P+LIBOR on borrowing side / ditto on lending side
#
# On a defaulted loan, the CF will be:
#  borrowing = P + P&I @ LIBOR / lending P + P&I @ (LIBOR + credit margin) for less than tenor + decreased P
#
# The P amortisation profile depends on the credit margin used. We will arbitrarily use 20%.
#
# credit risk on that CF should be nil with the right margin.
#




library(cluster)
library(factoextra)
library(caret)

trainKMEANS <- train(x = model01, y = Y01$sub_grade_num,
                     method = "knn",
                     tuneGrid = data.frame(k = seq(3, 9, 2)))

# Random Search
# set.seed(seed)
# rf_random <- train(Class~., data = dataset, method = "rf", metric = metric, trControl = control)
# print(rf_random)
# plot(rf_random)


###################################################################################################
##
## RANDOM FOREST
##
## Use Rborist since it accepts factors
##


# 17000 sec.
# Best result = 32 predictors
{
  require(caret)
  require(Rborist)
  require(doParallel)

  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)

  tictoc::tic()

  trainRF <- train(x = loans01, y = loss01$principal_loss_pct,
                   method = "Rborist",
                   nSamp = 2500,
                   trControl = trainControl(method = "cv"))

  tictoc::toc()
  stopCluster(cl)

  print(trainRF)

  #################################################################################################
  # Random Forest
  #
  # IF RESPONSE IS THE SUB-GRADE
  #
  # 13063 samples
  # 63 predictor
  #
  # No pre-processing
  # Resampling: Cross-Validated (10 fold, repeated 1 times)
  # Summary of sample sizes: 11756, 11757, 11757, 11757, 11758, 11757, ...
  # Resampling results across tuning parameters:
  #
  #   predFixed  RMSE       Rsquared   MAE
  # 2         1.0217915  0.4787036  0.8070386
  # 32         0.9217696  0.4984839  0.7231087
  # 63         0.9235622  0.4944814  0.7241667
  #
  # Tuning parameter 'minNode' was held constant at a value of 3
  # RMSE was used to select the optimal model using the smallest value.
  # The final values used for the model were predFixed = 32 and minNode = 3.
  #

  #################################################################################################
  # Random Forest
  #
  # IF RESPONSE IS THE PRINCIPAL LOSS (in %)
  #
  # 13063 samples
  # 65 predictor
  #
  # No pre-processing
  # Resampling: Cross-Validated (10 fold)
  # Summary of sample sizes: 11756, 11757, 11757, 11758, 11756, 11756, ...
  # Resampling results across tuning parameters:
  #
  #   predFixed  RMSE       Rsquared    MAE
  # 2         0.2824622  0.09468031  0.2166095
  # 33         0.2812666  0.09767208  0.2120904
  # 65         0.2820000  0.09485043  0.2124572
  #
  # Tuning parameter 'minNode' was held constant at a value of 3
  # RMSE was used to select the optimal model using the smallest value.
  # The final values used for the model were predFixed = 33 and minNode = 3.
  #
}






# .
# Best result = 32 predictors
{
  require(caret)
  require(Rborist)
  require(doParallel)

  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)

  tictoc::tic()

  trainRF <- train(x = loans01, y = Y01$sub_grade_num,
                   method = "Rborist",
                   predFixed = 32,
                   nSamp = 2500)

  tictoc::toc()
  stopCluster(cl)

  print(trainRF)
}



ggplot(trainRF)

cc <- cor(modelX)
ggcorrplot::ggcorrplot(cc, hc.order = TRUE, type = "upper", outline.color = "white")



tmp01 <-
  loans %>%
  slice(sample01) %>%
  select(funded_amnt, int_rate, installment, term, total_pymnt, total_rec_prncp)









