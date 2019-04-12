getwd()
setwd("C:\\Users\\Shravan-Harshini\\Desktop\\CAPSTONE FINAL SUBMIT")
data=read.csv("sampletelecomfinal.csv",stringsAsFactors = T)


#data quality report

#install.packages("dataQualityR")
library(dataQualityR)
checkDataQuality(data,out.file.num="Numeric.csv",out.file.cat="Character.csv")

View(data)
colnames(data)

# removing variables having greater than 15% missing values

dataRM <- data[,-c(46,47,48,49,52,55,61,62,63,64,66,72)]
dim(dataRM)

# Frequency Distribution of the binary dependent variable 
View(data)
table(dataRM$churn)
table(dataRM$churn)/nrow(dataRM)

#Missing Value treatment of "retdays" and Creating Dummy Variable
sort(unique(dataRM$retdays), na.last = F)
dataRM$retdays1 <- ifelse(is.na(dataRM$retdays)==TRUE, 0, 1)
str(dataRM$retdays1)
summary(dataRM$retdays1)


#derived variables (required in answering questions)
# mean no.of completed voice calls/Mean number of attempted voice calls placed
# this gives completed voice calls
dataRM$comp_per <- dataRM$comp_vce_Mean/dataRM$plcd_vce_Mean
colnames(dataRM)

# mean data overage revenue/total revenue
# this helps in knowing if the customers are selecting the plan correctly or not
dataRM$non_optimal <- dataRM$ovrrev_Mean/dataRM$totrev
dataRM$non_optimal_rateplan <- ifelse(dataRM$ovrrev_Mean>0,dataRM$non_optimal,0)
dim(dataRM)
dataRM <- select(dataRM,-non_optimal)
dim(dataRM)

# removing retdays as we have already created new column for that
dim(dataRM)
View(dataRM)
colnames(dataRM)
dataRM <- dataRM[,-c(48)]
dim(dataRM)

# creating dummy variables for 2 level variables
#refurb_new
dataRM$New_refurb = ifelse(dataRM$refurb_new=="R",1,0)
dim(dataRM)
dataRM <- select(dataRM,-refurb_new) # removing original column
dim(dataRM)

#asl_flag 
dataRM$asl_flag <- ifelse(dataRM$asl_flag=="Y",1,0)
dim(dataRM)

#Checking for missing values in all the columns
colSums(is.na(dataRM))
sum(is.na(dataRM))

#install.packages("VIM",dependencies = T)
#install.packages("abind",dependencies = T)
library(abind)
library(VIM)
library(dplyr)

dataRM = kNN(dataRM) # missing value imputation in dataRM

sum(is.na(dataRM))
dim(dataRM)
dataRM = select(dataRM,-c(70:138))

dim(dataRM)


# creating dummy variables

#   for area
# table(dataRM$ethnic,dataRM$churn)
summary(dataRM$area)
dataRM$areagp1 <- ifelse(dataRM$area=="NORTHWEST/ROCKY MOUNTAIN AREA" | dataRM$area=="CALIFORNIA NORTH AREA" |
                           dataRM$area=="PHILADELPHIA AREA" | dataRM$area=="SOUTH FLORIDA AREA",1,0)

dataRM$areagp2 <- ifelse(dataRM$area=="CHICAGO AREA" | dataRM$area=="DALLAS AREA" |
                           dataRM$area=="DC/MARYLAND/VIRGINIA AREA" | dataRM$area=="NORTH FLORIDA AREA"
                         | dataRM$area=="NEW YORK CITY AREA",1,0)

dataRM$areagp3 <- ifelse(dataRM$area=="NEW ENGLAND AREA" | dataRM$area=="LOS ANGELES AREA" |
                           dataRM$area=="ATLANTIC SOUTH AREA" | dataRM$area=="HOUSTON AREA"
                         | dataRM$area=="SOUTHWEST AREA",1,0)
dim(dataRM)
dataRM <- select(dataRM,-area)
dim(dataRM)

# dummies for ethnic
# table(dataRM$ethnic,dataRM$churn)
dataRM$ethnicgp1 <- ifelse(dataRM$ethnic=="X"|dataRM$ethnic=="C"|dataRM$ethnic=="P"|dataRM$ethnic=="M"|dataRM$ethnic=="Z",1,0)

dataRM$ethnicgp2 <- ifelse(dataRM$ethnic=="F"|dataRM$ethnic=="N"|dataRM$ethnic=="B"|dataRM$ethnic=="I"|dataRM$ethnic=="J"|dataRM$ethnic=="U"|dataRM$ethnic=="H"|dataRM$ethnic=="S"|dataRM$ethnic=="G"|dataRM$ethnic=="R",1,0)

dataRM$ethnicgp3 <- ifelse(dataRM$ethnic=="D"|dataRM$ethnic=="O",1,0)

dim(dataRM)
dataRM <- select(dataRM,-ethnic)
dim(dataRM)


# for marital
#table(tele$marital,tele$churn)
dataRM$maritalA <- ifelse(dataRM$marital=="A",1,0)
dataRM$maritalB <- ifelse(dataRM$marital=="B",1,0)
dataRM$maritalM <- ifelse(dataRM$marital=="M",1,0)
dataRM$maritalS <- ifelse(dataRM$marital=="S",1,0)
dataRM$maritalU <- ifelse(dataRM$marital=="U",1,0)

dim(dataRM)
dataRM <- select(dataRM,-marital)
dim(dataRM)

# for car_buy
#table(tele$car_buy,tele$churn)
dataRM$car_buy1 <- ifelse(dataRM$car_buy=="New",1,0)
dataRM$car_buy2 <- ifelse(dataRM$car_buy=="UNKNOWN",1,0)

dim(dataRM)
dataRM <- select(dataRM,-car_buy)
dim(dataRM)

# for hnd_webcap
#table(tele$hnd_webcap,tele$churn)
dataRM$hnd_webcap1 <- ifelse(dataRM$hnd_webcap=="UNKW",1,0)
dataRM$hnd_webcap2 <- ifelse(dataRM$hnd_webcap=="WC",1,0)
dataRM$hnd_webcap3 <- ifelse(dataRM$hnd_webcap=="WCMB",1,0)

dim(dataRM)
dataRM <- select(dataRM,-hnd_webcap)
dim(dataRM)

# for prizm_social_one
#table(tele$prizm_social_one,tele$churn)
dataRM$prizm_socialSUTC <- ifelse(dataRM$prizm_social_one=="S"|dataRM$prizm_social_one=="U"|dataRM$prizm_social_one=="T"|dataRM$prizm_social_one=="C",1,0)
dataRM$prizm_socialR <- ifelse(dataRM$prizm_social_one=="R",1,0)

dim(dataRM)
dataRM <- select(dataRM,-prizm_social_one)
dim(dataRM)

# removing crclscod as it is having many levels
dataF <- dataRM[,-c(31)]
dim(dataRM)
View(dataF)

# sampling

set.seed(200)
sampling <- sort(sample(nrow(dataF), nrow(dataF)*.7))

length(sampling)

dim(train)
dim(test)
#Row subset and create training and validation samples
train <- dataF[sampling,]
test <- dataF[-sampling,]
dim(train)
dim(test)

# Checking the frequency Distribution of the target variable 
table(dataF$churn)/13259
table(dataF$churn)/9281
table(dataF$churn)/3978

# finding highly corelated variables
#install.packages("corrplot",dependencies = T)
library(corrplot)
colnames(train)
traincor<-cor(train)
#install.packages("corrgram")
library(corrgram)
?corrgram #The corrgram function produces a graphical 
#display of a correlation matrix, called a correlogram. 
#The cells of the matrix can be shaded or colored to show the correlation value.

cormat<-corrgram(traincor)
write.csv(cormat,"Correlation.csv")

#install.packages("caret")
library(caret)
correlated <- findCorrelation(traincor,cutoff = 0.75)
correlated
# from the above, we get the following variables as highly corelated
# 1 25 29 26 27 30 28 24 53 11 19 21 52 54  4 22 20 48  3 10 77 73 50 42 37 79
# but removing all these wouldn't give accurate model as some of them may be important.
# therefore,removing only those which are not of much use(i.e., manually from their definition) and corelated


# BUILDING THE MODEL

# using logistic regression
colnames(train)
myresult <- glm(data=train,churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + rev_Mean + change_mou + drop_blk_Mean
                + drop_vce_Range + drop_vce_Mean + owylis_vce_Range + mou_opkv_Range + months + totcalls + income + eqpdays
                + custcare_Mean + callwait_Mean + callwait_Range + adjqty + ovrrev_Mean + iwylis_vce_Mean + ccrndmou_Range
                + ovrmou_Mean + asl_flag + prizm_socialSUTC +areagp1 + areagp2 + comp_vce_Mean + plcd_vce_Mean + avg3mou
                + avgmou + avg3qty + avgqty + avg6mou + avg6qty +uniqsubs + roam_Mean + hnd_webcap1 + hnd_webcap2 + maritalA
                + maritalB + maritalM + maritalS + ethnicgp1 + ethnicgp2 + age1 + age2 + models + hnd_price + actvsubs 
                + forgntvl + opk_dat_Mean +adjmou +totrev + adjrev + avgrev+ mtrcycle + truck + recv_sms_Mean + blck_dat_Mean 
                + mou_pead_Mean + car_buy1 + da_Mean + da_Range + datovr_Range + datovr_Mean + drop_dat_Mean + retdays1
                + New_refurb +comp_per + non_optimal_rateplan, family=binomial)
summary(myresult)


library(car)
?vif
vif(myresult)

# from the vif considering only those which are not highly corelated.
# also, monitor the obtained corelated variables once and then remove them.

myresult1 <- glm(data=train,churn ~ drop_blk_Mean + drop_vce_Range + drop_vce_Mean + owylis_vce_Range + mou_opkv_Range
                 + income + eqpdays + custcare_Mean + callwait_Mean + callwait_Range + iwylis_vce_Mean + ccrndmou_Range
                 + asl_flag + prizm_socialSUTC + areagp1 + areagp2 + uniqsubs + hnd_webcap1 + hnd_webcap2 + maritalA
                 + maritalB + maritalM + maritalS + ethnicgp1 + ethnicgp2 + age1 + age2 + models + hnd_price + actvsubs
                 + forgntvl + opk_dat_Mean + mtrcycle + truck + recv_sms_Mean + blck_dat_Mean + mou_pead_Mean + car_buy1
                 + da_Mean + da_Range + drop_dat_Mean + retdays1 + New_refurb + comp_per + avgrev + non_optimal_rateplan,family=binomial)
summary(myresult1)

reduced <- step(myresult1,direction="backward")

# from the above summary of the model
# considering lowest AIC : 9808.96

myresult1 <- glm(data=train, churn ~ drop_vce_Mean + owylis_vce_Range + eqpdays + iwylis_vce_Mean + 
                   asl_flag + prizm_socialSUTC + areagp1 + areagp2 + uniqsubs + 
                   hnd_webcap2 + ethnicgp1 + ethnicgp2 + age1 + models + hnd_price + 
                   blck_dat_Mean + mou_pead_Mean + retdays1 + New_refurb + comp_per + 
                   avgrev + non_optimal_rateplan, family = binomial)

summary(myresult2)


#Finding Predicted Values

myresult2$fitted.values

train$predicted <- myresult2$fitted.values
train$predicted

# Compare with actual data
head(train$churn)
head(train$predicted)

#Confusion Matrix
train$predclass<-ifelse(train$predicted>0.5,1,0)
table(train$predclass,train$churn)

#True Positive+ True Negative should be high. 

# Accuracy = (TP+TN)/(P+N)
(7035+50)/(7035+64+50+2132)

# For different cutoff probabilities, the confusion matrix will be different
# To find accuracies for different cut-off probabilities
# There are a lot of performance parameters available in ROCR package

#install.packages("ROCR")
library(ROCR)
#Reciever Operating Characteristic Curve = ROCR

# The prediction function of the ROCR library basically creates 
# a structure to validate our predictions with actual values

pred<-prediction(train$predicted,train$churn)
class(pred)

?performance

perf <- performance(pred,"acc") #acc considers accuracy for all possible cut-offs
class(perf)
perf
# x values contain the cut-off probabilities
#use @ to access the slots

class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))
cutoffs <- data.frame(cutoffprob, accuracies )

# In the decreasing order of accuracy
cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]
View(cutoffs)

# Pick cutoff for which Accuracy is highest 
train$predclass <- ifelse(train$predicted>0.5319685,1,0)

# Kappa values and Confusion Matrix from caret package
#install.packages("caret")
library(caret)
#install.packages("irr")
library(irr)
kappa2(data.frame(train$churn,train$predclass)) 
#install.packages("e1071")
library(e1071)
confusionMatrix(as.factor(train$churn),as.factor(train$predclass), positive = "1")

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
?  performance
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
# Receiver Operating Characteristic Curve (ROC) a plot of TPR versus FPR 
# for the possible cut-off classification probability values.
# A good ROC curve should be almost vertical in the beginning and 
# almost horizontal in the end.
# "tpr" and "fpr" are arguments of the "performance" function 
# indicating that the plot is between the true positive rate and 
# the false positive rate.

?abline
# Draw a straight line with intercept 0 and slope = 1
# lty is the line type (dotted or dashed etc.)
# The straight line is a random chance line
# ROC curve should be higher than the AB line

abline(0,1, lty = 8, col = "blue")

# Area under the curve should be more than 50%
auc<-performance(pred,"auc")
auc

#Creating a Gains chart
#install.packages("gains")
#install.packages("gains")
library(gains)
#The model may throw lots of numbers like 1000 people subscribing to a campaign
#Cannot take action on all 1000, for example we need only top 20 or 10 people.

gains(as.numeric(train$churn),train$predicted, groups =10)
quantile(train$predicted, seq(0,1,0.1))  #divides into 10 parts


# testing the model on test set

# To obtain predictions from the model, use the predict() function.
?predict()
test$pred <- predict(myresult2, type = "response",newdata = test)
test$pred

# The value 'response' to the parameter type would make sure 
# that these predictions are returned as probability of events.
#testing the model with test sample

# Compare with actual data
head(test$churn)
head(test$pred)

#Confusion Matrix
test$predclass<-ifelse(test$pred>0.5,1,0)
table(test$predclass,test$churn)
#True Positive+ True Negative should be high. 
# Accuracy = (TP+TN)/(P+N)
(3027+28)/(3027+28+29+894)

# For different cutoff probabilities, the confusion matrix will be different
# To find accuracies for different cut-off probabilities
# There are a lot of performance parameters available in ROCR package

#install.packages("ROCR")
library(ROCR)
#Reciever Operating Characteristic Curve = ROCR

# The prediction function of the ROCR library basically creates 
# a structure to validate our predictions with actual values

pred1<-prediction(test$pred,test$churn)
class(pred1)
?performance
perf <- performance(pred1,"acc") #acc considers accuracy for all possible cut-offs
class(perf)
perf
# x values contain the cut-off probabilities
#use @ to access the slots
# creating data frame with cutoff probabilities and accuracy
class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))
cutoffprob

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))
cutoffs <- data.frame(cutoffprob, accuracies ) 

# In the decreasing order of accuracy
cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]
View(cutoffs)

# Pick cutoff for which Accuracy is highest 
test$predclass <- ifelse(test$pred>0.4897700,1,0)

# Kappa values and Confusion Matrix from caret package
#install.packages("caret")
library(caret)
#install.packages("irr")
library(irr)
kappa2(data.frame(test$churn,test$predclass)) 

#install.packages("e1071")
library(e1071)
confusionMatrix(as.factor(test$churn),as.factor(test$predclass), positive = "1")

# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
?  performance
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
# Receiver Operating Characteristic Curve (ROC) a plot of TPR versus FPR 

?abline
# Drawing a straight line with intercept 0 and slope = 1
# lty is the line type (dotted or dashed etc.)
# ROC curve should be higher than the AB line

abline(0,1, lty = 8, col = "blue")

# Area under the curve should be more than 50%
auc<-performance(pred,"auc")
auc

#Creating a Gains chart
#install.packages("gains")
library(gains)

# to take action on particular segment of people we use gain chart
gains(as.numeric(train$churn),train$predicted, groups =10)
quantile(train$predicted, seq(0,1,0.1))  #divides into 10 parts



# QUESTIONS

# QUESTION 1 : What are the top five factors driving likelihood of churn at Mobicom?

head(sort(abs(myresult2$coefficients),decreasing = T),10)
summary(myresult2)

# Variable            non_optimal_rateplan       comp_per            retdays1        ethnicgp1          hnd_webcap1            
# Beta coefficient       4.9212352              -1.3179515          0.9069381       -0.8921357          -0.8119502             

# the above are the variables having high beta co.eficients. 
# Hence they are the top five factorsdriving likelihood of churn at Mobicom


#   QUESTION 2 : Validation of survey findings. 

# a) Whether "cost and billing" and "network and service quality" are important factors 
# influencing churn behaviour.  

# The following variables explain "cost and billing" and "network and service quality"

# Variables 
# (1) totmrc_Mean i.e. base plan charge : cost to customer on their plan not on min used
# (2) rev_Range i.e. Range of Revenue(charge amount)  i.e., billing amount
# (3) ovrrev_Mean : 'Mean overage revenue' (sum of data and voice overage revenues)
#           ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN
#           representing the overage revenue earned from customers after billing them
#           overage : When a user goes over the minutes allowed under the particular post-paid cell phone plan, they are charged separately for the extra minutes.   
# (4) totrev i.e. 'Total revenue' representing total revenue earned from customers

# the above variables can be explained with their beta co.efficients as follows

# (1) totmrc_Mean has beta coefficient value of -0.00488482 meaning a unit increase in this variable is causing 
# decrease in churn by -0.0053095 

# (2) rev_Range has beta coefficient value of -0.00135284 meaning a unit increase in this variable is causing 
# decrease in churn by -0.00135284

# (3) ovrrev_Mean has beta coefficient value of 0.00335597 meaning a unit increase in this variable is causing 
# increase in churn by 0.00335597

# (4) totrev has beta coefficient value of   0.00177397  meaning a unit increase in this variable is causing 
# increase in churn by   0.00177397 

# (5) non_optimal_rateplan: 4.9212352  # mean of complete call/mean of total no.of calls

# As "non_optimal_rateplan" has the highest significance to the churn, we can conclude "cost and billing" is one of the critical factor influencing churn


# # The following variables explain "network and service quality" 

# change_mou : -0.00047781 : Percentage change in monthly minutes of use vs previous three month average 
#                   displaying beta coefficient since continuous decrease may covey customer dis-satisfaction
# drop_blk_Mean   : 0.00316646 : sum of blocked data and voice calls and dropped data and voice calls
#                   since this may signify network issues
# drop_vce_Range  : 0.00122351 : Range of number of dropped (failed) voice calls  
# mou_opkv_Range  : -0.00008831 : Range of unrounded minutes of use of off-peak voice calls
# iwylis_vce_Mean : -0.00136145 : Mean number of inbound wireless to wireless voice calls (since netwok is needed)
# avg6mou         : 0.00032225 : Average monthly minutes of use over the previous six months
# comp_per : -1.3179515   # mean of no.of completed calls/total calls attempted

# As "comp_per" has the highest significance to the churn, we can conclude "network and service quality" is one of the critical factor influencing churn


#** (b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 

# let's take variables meaningfully describing data usage with respect to this problem

#   opk_dat_Mean - Mean number of off-peak data calls : 0.01160182
#   blck_dat_Mean - Mean no. of blocked / failed data calls : 0.02
#   datovr_Mean - Mean revenue of data overage : 0.14241252
#   datovr_Range - Range of revenue of data overage : -0.06262868
#   drop_dat_Mean - Mean no. of dropped / failed data calls : 0.03148416 

quantile(dataF$opk_dat_Mean ,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(dataF$blck_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(dataF$datovr_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(dataF$datovr_Range,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(dataF$drop_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))


# This problem could be a matter of concern since the global market survey report shows "Subscribers who 
#   have switched operators in recent months reported two key information sources in their decision:
#   the Internet and recommendation of family and friends..
# The Data Quality Report for the above variables show that only 10% to 15% customers are actualy 
#   making data calls or using the internet.
# In this case, Mean revenue of data overage found to be more significant for churn than the other variables. 
# Though data usage connectivity is leading to the churn as the usage itself is very low, we can conclude this is not turning out to costly.


# QUESTION 3 : Would you recommend rate plan migration as a proactive retention strategy?

# Identify the variables which are related to data usage connectivity issues. 
non_optimal_rateplan : 4.9212352 
# Non- optimal rate plan is the direct variable related to rate plan and has the highest significance(4.9212352) in the entire model 
# hence would recommend to identify these customers and proactively migrate their plan to retain them

#  QUESTION 4. What would be your recommendation on how to use this churn model for prioritisation
#   of customers for a proactive retention campaigns in the future?

# Solution:

# Selecting Customers with high churn rate
test$prob<-predict(myresult2,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# Top 20% of the probabilities lie between 0.3002649 and 0.7533089  (i.e., has high probablity)

# Applying cutoff value to predict customers who Will Churn
pred4<-predict(myresult2, type="response", newdata=test)
pred4<-ifelse(pred4>=0.3002649 , 1, 0)    # taking greater than 0.3002649 probablity of churn
table(pred4,test$churn)

# retrieving customers having high probability of churn
Targeted <- test[test$prob > 0.3002649 & test$prob <= 0.7533089 & test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)

# saving the targeted customers into a file
write.csv(Targeted,"Target_Customers.csv",row.names = F)

#   Thus this model(myresult2) can be used to predict customers with high probability of Churn by extracting the 
#   target list using their "Customer ID". 



# 5. What would be the target segments for proactive retention campaigns? 
# Falling ARPU forecast is also a concern and therefore, Mobicom would like to save their high revenue 
# customers besides managing churn. Given a budget constraint of a contact list of 20% of the subscriber pool, 
# which subscribers should prioritized if "revenue saves" is also a priority besides controlling churn. 
# In other words, controlling churn is the primary objective and revenue saves is the secondary objective.

# Solution:

# ARPU : Average Revenue Per User
pred5 <- predict(myresult2, type="response", newdata=test)
test$prob <- predict(myresult2,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6 <- ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))

# these values are derived from the quantile function above
# Low_Score : in the above we considered 0.20 as it contains the least probability
# Medium_Score : in the above we considered 0.20 to o.30 as it contains the middle level probability
# High_Score : in the above we considered greater than 0.30 as it contains the top probability

table(pred6,test$churn)   # gives tabular format of above ifelse condition

# for revenue
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<670.076,"Low_Revenue",ifelse(test$totrev>=670.076 & 
                                                                  test$totrev<1124.269,"Medium_Revenue","High_Revenue"))

# these values are derived from the quantile function above
# Low_Revenue : in the above we considered 670.076 as it contains the least probability
# Medium_Revenue : in the above we considered 670.076 to 1124.269 as it contains the middle level probability
# High_Revenue : in the above we considered greater than 1124.269 as it contains the top probability

table(Revenue_Levels)

table(pred6,Revenue_Levels)

#** Thus this table can be used to select the levels of customers are to be targeted
#** and the Target list can be extracted as follows:

test$prob_levels<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                       test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)

# the above file has customer id's of customers to be targeted
# Categorize the customer into 3 categories each based on the
# Probability to churn 
# Source of High Revenue
# Based on the categorization and the objective initial target segment would be  high probability to churn segment and High revenue segment. As the budget is only for 20% subscribers which comes to 800 subscribers.
# High Probability subscribers total add up to 802 so only this subscribers can be targeted











