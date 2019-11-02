# Libraries
library(tidyverse)
library(lubridate)

# Load data
if (!exists("loans")) loans <- readRDS("datasets/lending_club_reformatted_paid.rds")
nSamples <- nrow(loans)


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
## Reformatting/wrangling of factors (in case model do not mix continuous and cat. variables)
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
sample01 <- sample(1:nSamples, floor(nSamples * 0.01), replace = FALSE)
sample05 <- sample(1:nSamples, floor(nSamples * 0.05), replace = FALSE)
sample10 <- sample(1:nSamples, floor(nSamples * 0.10), replace = FALSE)
sample20 <- sample(1:nSamples, floor(nSamples * 0.20), replace = FALSE)

loans01 <- loansPredictors %>% slice(sample01)
loans05 <- loansPredictors %>% slice(sample05)
loans10 <- loansPredictors %>% slice(sample10)
loans20 <- loansPredictors %>% slice(sample20)

model01 <- modelX %>% slice(sample01)
model05 <- modelX %>% slice(sample05)
model10 <- modelX %>% slice(sample10)
model20 <- modelX %>% slice(sample20)

Y01 <- loans %>% select(sub_grade_num) %>% slice(sample01)
Y05 <- loans %>% select(sub_grade_num) %>% slice(sample05)
Y10 <- loans %>% select(sub_grade_num) %>% slice(sample10)
Y20 <- loans %>% select(sub_grade_num) %>% slice(sample20)

library(cluster)
library(factoextra)
library(caret)

trainKMEANS <- train(x = model01, y = Y01$sub_grade_num,
                     method = "knn",
                     tuneGrid = data.frame(k = seq(3, 9, 2)))

# Random Search
set.seed(seed)
rf_random <- train(Class~., data=dataset, method="rf", metric=metric, trControl=control)
print(rf_random)
plot(rf_random)


###################################################################################################
##
## RANDOM FOREST
##
## Use Rborist since it accepts factors
##

{
  require(caret)
  require(Rborist)
  require(doParallel)

  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)

  tictoc::tic()

  trainRF <- train(x = loans01, y = Y01$sub_grade_num,
                   method = "Rborist",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10,
                                            repeats = 3))

  tictoc::toc()
  stopCluster(cl)
}

print(trainRF)
ggplot(trainRF)


cc <- cor(modelX)
ggcorrplot::ggcorrplot(cc, hc.order = TRUE, type = "upper", outline.color = "white")








d <- dist(l01)
h <- hclust(d)

plot(h, cex = 0.6)

subgroup <- cutree(h, k = 5)
rect.hclust(h, k = 5, border = 2:5)
fviz_cluster(list(data = model01, cluster = subgroup))

fviz_nbclust(model01, FUN = hcut, method = "wss")
