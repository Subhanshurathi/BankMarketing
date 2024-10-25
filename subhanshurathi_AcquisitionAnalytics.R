############################## Bank Marketing Analysis #################################



## Data Preparation and Business Understanding:- Prospect Profiling##


rm(list=ls())

#Loding Required Packages

library(ggplot2)

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response      #0.1126542


# Adding Prospect ID column to the banking dataset to identify the unique customers

bank_data$prospectID<-seq(1,nrow(bank_data),by=1)


# Checking missing values

sum(is.na(bank_data))  


# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))



ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

  

# Checking structure of dataset

str(bank_data)

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.
# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  


# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")


# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")

#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]     #removed default variable

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)

plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Nxet is "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable
# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-23]

## Definitely the outlier is present in the dataset
# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()





summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)

quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)
levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

summary(bank_data$previous)
plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)
plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------

#----Economic Infomation
#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

#----------------------------------------------------------------------------

# Taking a backup of dataset
bank_data_bkup <- bank_data

#-------------------------------------------------------------
## Building Logistic Regression Model without Duarion Variable
#-------------------------------------------------------------

# Required Packages

library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)

#---------------------------------------------------------    

# Removing binning variables 

str(bank_data)
bank_data <- bank_data[, c(-22)]
View(head(bank_data))

#creating dummy variables

bank_data$response <- as.integer(bank_data$response)
k1 <- bank_data
bank_data <- dummy.data.frame(bank_data)
bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#---------------------------------------------------------    

# splitting into train and test data

set.seed(1)
split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)
train <- bank_data[split_indices, ]
test  <- bank_data[!split_indices, ]

#Eliminating the prospect ID and Duration from the training dataset

train<-train[,c(-45,-62)]
train$response <- as.factor(ifelse(train$response == "yes", 1, 0))

remaining_data <- bank_data[,c(45,62)]

test<-test[,-c(45,62)]

nrow(train)/nrow(bank_data)      #0.7000097
nrow(test)/nrow(bank_data)       #0.2999903

final_data <- bank_data[,-c(45,62)]


#---------------------------------------------------------    
### Model 1: Logistic Regression
#---------------------------------------------------------  

Model_1 <- glm(response ~ ., family = "binomial", data = train)

summary(Model_1)


# Using stepwise algorithm for removing insignificant variables 

Model_2 <- stepAIC(Model_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

Model_3 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed + `previousMore than_3_times`, family = "binomial", data = train)

# checking vif for Model_3
vif(Model_3)
summary(Model_3)


# removing "jobadmin" since variable is not significant 

Model_4 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed + `previousMore than_3_times`, family = "binomial", data = train)

vif(Model_4)
summary(Model_4)

# Removing "`previousMore than_3_times`" variable as it is not significant 

Model_5 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)

vif(Model_5)
summary(Model_5)

# Removing "maritaldivorced" as it is not significant 

Model_6 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)

vif(Model_6)
summary(Model_6)

# Removing "day_of_weekfri" as it is not significant 

Model_7 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)

vif(Model_7)
summary(Model_7)

# Removing "educationTertiary_Education" as it is not significant 

Model_8 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)

vif(Model_8)
summary(Model_8)

# Removing "jobtechnician" as it is not significant 

Model_9 <- glm(formula = response ~ jobretired + jobstudent +  
                    educationPrimary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed, family = "binomial", data = train)

vif(Model_9)
summary(Model_9)

# Removing "monthoct" as it is not significant 

Model_10 <- glm(formula = response ~ jobretired + jobstudent +  
                     educationPrimary_Education + 
                     contactcellular + monthapr + monthjul + monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     nr.employed, family = "binomial", data = train)

vif(Model_10)
summary(Model_10)


# Removing "monthjul" as it is not significant 

Model_11 <- glm(formula = response ~ jobretired + jobstudent +  
                     educationPrimary_Education + 
                     contactcellular + monthapr + monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     nr.employed, family = "binomial", data = train)

vif(Model_11)
summary(Model_11)

# Removing "monthapr" as it is not significant 

Model_12 <- glm(formula = response ~ jobretired + jobstudent +  
                     educationPrimary_Education + 
                     contactcellular + monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                     nr.employed, family = "binomial", data = train)

vif(Model_12)
summary(Model_12)

# Removing "nr.employed" as it is not significant 

Model_13 <- glm(formula = response ~ jobretired + jobstudent +  
                     educationPrimary_Education + 
                     contactcellular + monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx
                   , family = "binomial", data = train)

vif(Model_13)
summary(Model_13)


Model_14 <- glm(formula = response ~ jobretired +   
                     educationPrimary_Education + 
                     contactcellular + monthjun + monthmar + 
                     monthmay + monthnov +  day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx
                   , family = "binomial", data = train)

vif(Model_14)
summary(Model_14)

# Finalizing the model
FinalModel <- Model_14


#---------------------------------------------------------    
# Model Evaluation Metrics
# Predicting probabilities of responding for the test data

predictions_logit <- predict(FinalModel, newdata = test[,-60], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.
predicted_response <- factor(ifelse(predictions_logit >= 0.50, 'yes', 'no'))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf

#---------------------------------------------------------    
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 



# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))




cutoff <- s[which(abs(OUT[,1]-OUT[,2])< 0.01)]
View(cutoff)

# Let's choose a cutoff value of 7.3% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.073, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]




#---------------------------------------------------------    
# ------Model Evaluation----------------------------------

# Appending the probabilities and response variables to the Final data

#---------------------------------------------------------    
# Model Evaluation Metrics
# Predicting probabilities of responding for the test data

predictions_logit <- predict(FinalModel, newdata = final_data[,-60], type = "response")

summary(predictions_logit)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.073, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, final_data$response, positive = "yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
sens
spec

final_data$predicted_probs <- predictions_logit
final_data$predicted_response <- predicted_response

#---------------------------------------------------------    

# Creating new dataframe "final_predictions"

final_predictions <- final_data[, c("response", "predicted_probs", "predicted_response")]

summary(final_predictions$response)
summary(final_predictions$predicted_response)

response_rate <- table(final_data$response)[2]/(table(final_data$response)[1] + table(final_data$response)[2])

# sorting the probabilities in decreasing order 
final_predictions <- final_predictions[order(final_predictions$predicted_probs, decreasing = T), ]

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

final_predictions$response <- as.factor(ifelse(final_predictions$response=="yes",1,0))
LG = lift(final_predictions$response, final_predictions$predicted_probs, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")


plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# Total Cost incur throught direct telemarketing 



final_data<-cbind(final_data, remaining_data)
#Sorting the test data according to the probability
final_data<-final_data[order(final_data$predicted_probs,decreasing=TRUE),]


#Extracting the customers upto the 5th Decile

final_5th_dec <-head(final_data,n=20595)

#Calculating the cost of call for each of these customers


final_5th_dec$Costofcall<- round(0.033*(final_5th_dec$duration) + 0.8,2)







