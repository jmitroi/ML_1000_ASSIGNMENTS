### ML1000 - Assignment 1 - Data exploration for German Credit dataset ###

#Import packages

install.packages('dplyr')
library(dplyr)
install.packages('data.table')
library(data.table)
install.packages('gmodels')
library(gmodels)

#Import dataset - germa_credit.csv  #numeric data from the UCI website has been converted to csv with column names manually, in Excel
german_credit <- read.table(file.choose(), sep = ",", header = TRUE)

#Check # of rows
ncol(german_credit);
nrow(german_credit);

#Preview data structure
head(german_credit)

#Check for missing data points
sum(is.na(german_credit))  # returns zero -- no missing data in this dataset

#Normalize column names to remove spaces
colnames(german_credit) <- sub(" ","",colnames(german_credit))

#Get a list of column names
colnames(german_credit)

#Set the name of dataset to be german_credit so that it does not have to be mentioned every time
attach(german_credit) 

#Target variable, Creditability, is binary/categorical. 
#Proportions for Creditable (1) and Non-creditable (0) are 70% and 30% respectively
prop.table(table(Creditability))

#Distribution of the target variable, Creditability
counts <- table(Creditability)
barplot(counts, col="darkblue", xlab="Creditability", ylab="Frequency", names.arg=c("0 (Bad)", "1 (Good)")) # produces bar chart


#Frequency tables for the continuous variables
freq_tbl1=table(Duration.of.Credit..month.)
head(freq_tbl1)

freq_tbl2=table(Credit.Amount)
head(freq_tbl2)

freq_tbl3=table(Age..years.)
head(freq_tbl3)


#Distribution of the continuous variables in the German credit data ##

#Distribution of the Duration of Credit (Month) variable
brksDuration <- seq(0, 80, 10) # Bins for histogram
hist(Duration.of.Credit..month., breaks=brksDuration, col="darkgreen", xlab = "Credit month", ylab = "Frequency", main = " ", cex=0.4) # produces histogram
boxplot(Duration.of.Credit..month., bty="n", col="lightgreen", xlab = "Credit month", ylab = "Median Duration (thick line)", cex=0.4) # produces boxplot
#adding mean to boxplot
abline(h=mean(Duration.of.Credit..month.))
points(mean(Duration.of.Credit..month.), col="red",pch=18)
text(mean(Duration.of.Credit..month.) + 0.06)


#Distribution of the Amount of Credit variable
brksCredit <- seq(0, 19000, 1000) # Bins for histogram
hist(Credit.Amount, breaks=brksCredit, col="darkgreen", xlab = "Credit amount", ylab = "Frequency", main = " ", cex=0.4) # produces histogram
boxplot(Credit.Amount, bty="n", col="lightgreen", xlab = "Credit amount", ylab = "Median Credit Amount (thick line)", cex=0.4) # For boxplot
#adding mean to boxplot
abline(h=mean(Credit.Amount))
points(mean(Credit.Amount), col="red",pch=18)
text(mean(Credit.Amount) + 0.06)


#Distribution of the Age (of Applicant) variable
brksAge <- seq(0, 80, 10) # Bins for histogram
hist(Age..years., breaks=brksAge, col="darkgreen", xlab = "Age in years", ylab = "Frequency", main = " ", cex=0.4) # produces histogram
boxplot(Age..years., bty="n", col="lightgreen", xlab = "Age in years", ylab = "Median Age (thick line)", cex=0.4) # produces boxplot
#adding mean to boxplot
abline(h=mean(Age..years.))
points(mean(Age..years.), col="red",pch=18)
text(mean(Age..years.) + 0.08)


#Summary statistics of the continuous variables in the German credit data ##
#Produces the following statistics: min, 1st quartile, median, mean, 3rd quartile, max
summary(Duration.of.Credit..month.)
summary(Credit.Amount)
summary(Age..years.)


#Calculating proportions of applicants belonging to each classification of a categorical variable

#Proportions for Account.Balance

german_credit %>%
  group_by(Account.Balance) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Payment.Status.of.Previous.Credit

german_credit %>%
  group_by(Payment.Status.of.Previous.Credit) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Value.Savings.Stocks

german_credit %>%
  group_by(Value.Savings.Stocks) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

#Proportions for Length.of.current.employment

german_credit %>%
  group_by(Length.of.current.employment) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Instalment.per.cent

german_credit %>%
  group_by(Instalment.per.cent) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Sex...Marital.Status

german_credit %>%
  group_by(Sex...Marital.Status) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Guarantors

german_credit %>%
  group_by(Guarantors) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Duration.in.Current.address

german_credit %>%
  group_by(Duration.in.Current.address) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Most.valuable.available.asset

german_credit %>%
  group_by(Most.valuable.available.asset) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Concurrent.Credits

german_credit %>%
  group_by(Concurrent.Credits) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for No.of.Credits.at.this.Bank

german_credit %>%
  group_by(No.of.Credits.at.this.Bank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Occupation

german_credit %>%
  group_by(Occupation) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for No.of.dependents

german_credit %>%
  group_by(No.of.dependents) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Telephone

german_credit %>%
  group_by(Telephone) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Foreign.Worker

german_credit %>%
  group_by(Foreign.Worker) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


#Proportions for Purpose

german_credit %>%
  group_by(Purpose) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


# Cross-tabulation of potential predictor variables with Creditability
# Predictors to be selected based on the significance (p) value of the Chi-squared test and t-tests below
# Only significant predictors are to be included in the development of our classification model

#crosstabulation of Account.Balance and Creditability -- Chi-square P-value < 0.001 -- significant
CrossTable(Creditability, Account.Balance, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Payment.Status.of.Previous.Credit and Creditability -- Chi-square P-value < 0.001 -- significant
CrossTable(Creditability, Payment.Status.of.Previous.Credit, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Value.Savings.Stocks and Creditability -- Chi-square P-value < 0.001 -- significant
CrossTable(Creditability, Value.Savings.Stocks, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Length.of.current.employment and Creditability -- Chi-square P-value < 0.001 -- significant
CrossTable(Creditability, Length.of.current.employment, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Instalment.per.cent and Creditability -- Chi-square P-value = 0.14 -- significant
CrossTable(Creditability, Instalment.per.cent, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Sex...Marital.Status and Creditability -- Chi-square P-value = 0.01 -- significant
CrossTable(Creditability, Sex...Marital.Status, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Guarantors and Creditability -- Chi-square P-value = 0.98 -- not significant
CrossTable(Creditability, Guarantors, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Duration.in.Current.address and Creditability -- Chi-square P-value = 0.86 -- not significant
CrossTable(Creditability, Duration.in.Current.address, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Most.valuable.available.asset and Creditability -- Chi-square P-value < 0.001 -- significant
CrossTable(Creditability, Most.valuable.available.asset, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)              

#crosstabulation of Concurrent.Credits and Creditability -- Chi-square P-value < 0.001 -- significant
CrossTable(Creditability, Concurrent.Credits, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of No.of.Credits.at.this.Bank and Creditability -- Chi-square P-value = 0.15 -- not significant
CrossTable(Creditability, No.of.Credits.at.this.Bank, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Occupation and Creditability -- Chi-square P-value = 0.42 -- not significant
CrossTable(Creditability, Occupation, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of No.of.dependents and Creditability -- Chi-square P-value = 0.92 -- not significant
CrossTable(Creditability, No.of.dependents, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Telephone and Creditability -- Chi-square P-value = 0.28 -- not significant
CrossTable(Creditability, Telephone, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Foreign.Worker and Creditability -- Chi-square P-value = 0.01 -- significant
CrossTable(Creditability, Foreign.Worker, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

#crosstabulation of Purpose and Creditability -- Chi-square P-value < 0.001 -- significant
CrossTable(Creditability, Purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)


# Welch Two Sample t-tests for the continuous variables

t.test(Duration.of.Credit..month.~Creditability) #t-test P-value < 0.001 -- significant
t.test(Credit.Amount~Creditability) #t-test P-value < 0.001 -- significant
t.test(Age..years.~Creditability) #t-test P-value = 0.003 -- significant
