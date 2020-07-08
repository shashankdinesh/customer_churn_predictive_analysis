# Setting current working directory for script
setwd("/home/shashank/workspace/datascience")

# Read dataset
customer_churn<-read.csv("Customer_Churn.csv" , stringsAsFactors = T)

library(caTools)

# Split dataset in train and test sets
sample.split(customer_churn, SplitRatio = 0.75)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

# Preparing final dataset for training by filtering out independent columns

# Checking dependency between continuous data (MonthlyCharges) vs categorical 
# data (Churn) using logistic regression. If columns found independent, we will
# filter out MonthlyCharges from dataset.
mod_log <- glm(Churn~MonthlyCharges, data=train, family = "binomial")
predict(mod_log,newdata=test,type="response")->result_log
tab = table(test$Churn, result_log> 0.35)
accuracy = (tab[[1]]+tab[[4]])/(tab[[1]]+tab[[2]]+tab[[3]]+tab[[4]])
accuracy

# Checking dependency between continuous data (tenure) vs categorical 
# data (Churn) using logistic regression. If columns found independent, we will
# filter out tenure from dataset.
mod_log <- glm(Churn~tenure, data=train, family = "binomial")
predict(mod_log,newdata=test,type="response")->result_log
tab = table(test$Churn, result_log> 0.35)
accuracy = (tab[[1]]+tab[[4]])/(tab[[1]]+tab[[2]]+tab[[3]]+tab[[4]])
accuracy

# Checking dependency between continuous data (TotalCharges) vs categorical 
# data (Churn) using logistic regression. If columns found independent, we will
# filter out TotalCharges from dataset.
mod_log <- glm(Churn~TotalCharges, data=train, family = "binomial")
predict(mod_log,newdata=test,type="response")->result_log
tab = table(test$Churn, result_log> 0.35)
accuracy = (tab[[1]]+tab[[4]])/(tab[[1]]+tab[[2]]+tab[[3]]+tab[[4]])
accuracy

# Fetching numerical column names
numericalcol = colnames(dt)[!grepl('factor|logical|character',sapply(dt,class))]

# Fetching categorical column names
categoricalcol = colnames(dt)[grepl('factor|logical|character',sapply(dt,class))]
numerical = c("SeniorCitizen","tenure","MonthlyCharges","TotalCharges")
categorical = c("customerID","gender","Partner", "Dependents","PhoneService","MultipleLines",
         "InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport",
         "StreamingTV","StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod")

# Setting target column
target =  "Churn"


# Tests of independence (categorical vs categorical)
# CHI-SQUARE TEST OF INDEPENDENCE
# After test we will filter out independent columns

library(vcd) 
mytable <- xtabs(~SeniorCitizen+Churn, data=customer_churn)
chisq.test(mytable)
depedentcol=c("Partner","Dependents","MultipleLines","InternetService","OnlineSecurity","OnlineBackup",
              "DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod")

# strength of the relationships present in dependent columns
assocstats(mytable)

#{"Partner":0.15,"Dependents":0.16,"MultipleLines":0.04,"InternetService":0.307,"OnlineSecurity":0.328,"OnlineBackup":0.281}
#{"DeviceProtection":0.271,"TechSupport":0.324,"StreamingTV":0.225,"StreamingMovies":0.225,"Contract":0.38,"PaperlessBilling":0.192}
#{"PaymentMethod":0.3}
finalcatcolset = c("SeniorCitizen","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection",
                   "TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod", "Churn",
                   "tenure","MonthlyCharges","TotalCharges")

# Split dataset in train and test sets, for training over filtered 
# dependent columns
sample.split(customer_churn[finalcatcolset], SplitRatio = 0.75)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test
mod_log <- glm(Churn~MonthlyCharges, data=train, family = "binomial")
predict(mod_log,newdata=test,type="response")->result_log
tab = table(test$Churn, result_log> 0.35)
accuracy = (tab[[1]]+tab[[4]])/(tab[[1]]+tab[[2]]+tab[[3]]+tab[[4]])
accuracy


# My findings
# Customer churn highly affected by tenure, MonthlyCharges and TotalCharges
# as compared to other categorical columns.

