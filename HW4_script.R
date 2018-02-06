getwd()
setwd('/Users/jessie/Desktop/Bittiger/Month1')
list.files()
loan <- read.csv('../Data/loan.csv', stringsAsFactors = FALSE)

# feature engineering
loan <- loan[,-which(colnames(loan) %in% c('id', 'member_id', 'url'))]
loan$dti <- ifelse(!is.na(loan$dti_joint), loan$dti_joint, loan$dti)
loan$annual_inc <- ifelse(!is.na(loan$annual_inc_joint), loan$annual_inc_joint, loan$annual_inc)
loan$home_ownership <- ifelse(loan$home_ownership %in% c('ANY', 'NONE', 'OTHER'), 'OTHER',
                              loan$home_ownership)
int_state <- by(loan, loan$addr_state, function(x) {
  return(mean(x$int_rate))
})
loan$state_mean_int <-
  ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.25))], 
         'low', ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.5))],
                       'lowmedium', ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.75))], 
                                           'mediumhigh', 'high')))
num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
remain.col <- names(num.NA)[which(num.NA <= 0.8 * dim(loan)[1])]
loan <- loan[, remain.col]

#inputation
num.NA <- sort(sapply(loan, function(x) { sum(is.na(x))} ), decreasing = TRUE)
for(col.i in names(num.NA)[which(num.NA > 0)]) {
  loan[which(is.na(loan[,col.i])), col.i] <- median(loan[,col.i], na.rm = TRUE)
}

# split training and test data
set.seed(1212)
train.ind <- sample(1:dim(loan)[1], 0.7 * dim(loan)[1])
train <- loan[train.ind, ]
test <- loan[-train.ind, ]

train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc')]

# fit model
library(glmnet)

  