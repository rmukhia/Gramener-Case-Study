library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
 
# Clean workspace
rm(list = ls())

# Read from 'raw' csv which has not been cleaned
loan_raw <- read.csv("loan.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

# Analyse the data at hand
str(loan_raw)

# A function to check if a column has a single unique value
isSingleDataColumn <- function(col_data) {
  unique_data <- length(unique(col_data))
  return (unique_data == 1)
}
# The good side effect is that a columns which only has NA's or 0's will also appear as a column
# with a single unique value

# Get a list of columns which has a single unique value
columns_with_single_data <- sapply(loan_raw, isSingleDataColumn)

# Remove columns with single unique values, all NAs and all 0s from our dataset
loan_data <- loan_raw[, !columns_with_single_data]

# Make percentage as numeric by removing the % character
loan_data<-separate(loan_data,int_rate,c("int_rate"),sep="%",remove=TRUE)
loan_data$int_rate<-as.numeric(loan_data$int_rate)
loan_data<-separate(loan_data, revol_util, c("revol_util"), sep="%",remove=TRUE)
loan_data$revol_util<-as.numeric(loan_data$revol_util)

# Don't need the url of the application for analysis, remove it
loan_data <- subset(loan_data, select = -url)

# Show columns which has NA values
colWithNA <- function(loan_data) {
  col <- colSums(is.na(loan_data))
  return (col[col > 0])
}

colWithNA(loan_data)

# There are 2453 NA values in emp_title, convert those to 'Other'
loan_data$emp_title[is.na(loan_data$emp_title)] <- 'Other'

# Description is can be removed
loan_data <- subset(loan_data, select = -desc)

# Set NA values in loan 'title' to 'Other'
loan_data$title[is.na(loan_data$title)] <- 'Other'


# The payment dates lies in the range of 8 - 16 day of the month, ie 8 day range.
unique(loan_data$last_pymnt_d)
# We are going to assume that 8 days of gap does not effect the payment, and remove the payment 
# date information
loan_data <- subset(loan_data, select = -last_pymnt_d)
loan_data <- subset(loan_data, select = -next_pymnt_d)
# Also remove last_credit_pull_d
loan_data <- subset(loan_data, select = -last_credit_pull_d)

# Check the values of tax liens
unique(loan_data$tax_liens)
# Has only 0 and NAs
# We are going to assume that NA also means no tax liens, i.e the person has paid his taxes properly.
# Since all values are either 0 or NAs, we can remove this column
loan_data <- subset(loan_data, select = -tax_liens)

# Collection with 12 months just has 0 and NAs and can be remove
unique(loan_data$collections_12_mths_ex_med)
loan_data <-subset(loan_data, select = -collections_12_mths_ex_med)

# Chargeoff within 12 months just has 0 and Nas and can be removed
unique(loan_data$chargeoff_within_12_mths)
loan_data <- subset(loan_data, select = -chargeoff_within_12_mths)

# Maintain NA values in 'Months since last delinq', 'Months since last record',
# Revloving line utilization rate', and 'public record bankruptcies' for analysis

# Check if loan status can be converted into factor
unique(loan_data$loan_status)
# Yes, it can be. Convert into factor.
loan_data$loan_status <- factor(loan_data$loan_status)

# Do the same thing for grade, sub_grade
unique(loan_data$grade)
unique(loan_data$sub_grade)
loan_data$grade <- factor(loan_data$grade)
loan_data$sub_grade <- factor(loan_data$sub_grade)

# Do the same thing for addr_state
unique(loan_data$addr_state)
loan_data$addr_state <- factor(loan_data$addr_state)

# Check and convert into factors for term, emp_length, verification_status, purpose,
# home ownership, pub_rec_bankruptcies, pub_rec, inq_last_6mths, delinq_2yrs
unique(loan_data$term)
unique(loan_data$emp_length)
unique(loan_data$verification_status)
unique(loan_data$purpose)
unique(loan_data$home_ownership)
unique(loan_data$pub_rec_bankruptcies)
unique(loan_data$pub_rec)
unique(loan_data$inq_last_6mths)
unique(loan_data$delinq_2yrs)
loan_data$term <- factor(loan_data$term)
loan_data$emp_length <- factor(loan_data$emp_length)
loan_data$verification_status <- factor(loan_data$verification_status)
loan_data$purpose <- factor(loan_data$purpose)
loan_data$home_ownership <- factor(loan_data$home_ownership)
loan_data$pub_rec_bankruptcies <- factor(loan_data$pub_rec_bankruptcies)
loan_data$pub_rec <- factor(loan_data$pub_rec)
loan_data$inq_last_6mths <- factor(loan_data$inq_last_6mths)
loan_data$delinq_2yrs <- factor(loan_data$delinq_2yrs)

# Determine the proportion of Charged Off loans
# Plot pie chart
temp <- summarize(group_by(loan_data, loan_status), value = n())
str(temp)
temp <- mutate(temp, loan_status  = factor(loan_status, levels = rev(loan_status)),
               cumulative = cumsum(value),
               midpoint = cumulative - value / 2,
               label = factor(paste0(round(value / sum(value) * 100, 1), "%")))


ggplot(temp, aes(x="",  weight = value, fill = loan_status)) + 
  geom_bar(width=1, position = "stack") +
  coord_polar(theta = "y") +
  labs(title = "Total Loan Status" , fill= "Loan status") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_void()



# Performing univariate and segmented univariate analysis with categorical variables

# 1 - Loan Status vs Loan Grade
ggplot(loan_data,aes(x=grade,fill=loan_status))+geom_bar(position="fill")+
  labs(x="Grade",title=" Grade")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

# 2 - Loan Status vs Loan Sub-grade
ggplot(loan_data,aes(x=sub_grade,fill=loan_status))+geom_bar(position="fill")+
  labs(x="Sub Grade",title="Loan Sub Grade")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

# 3 - Loan Status vs state
ggplot(loan_data,aes(x=addr_state))+geom_bar(stat = "count") + 
  geom_text(stat ='count', aes(label = ..count..), vjust = -1)
# Now plot the proportion of charged off loans
ggplot(loan_data,aes(x=addr_state,fill= loan_status))+geom_bar(position = "fill")+
  labs(x="State",title="State")+
  labs(fill="Loan Status")+
  labs(y="Proportion")
# State NE has very few loans to make a call on the high proportion of charged-off loans

# 4 Loan Status vs Term
ggplot(loan_data,aes(x=term,fill=loan_status))+geom_bar(position = "fill")+
  labs(x="Term",title="Loan Term")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 
# Loan with Term of 60 months have a higher proportion of defaulting

# 5 Loan Status vs Emp Length
ggplot(loan_data,aes(x=emp_length,fill=loan_status))+geom_bar(position = "fill") 

# 6 Loan Status vs Verification Status
ggplot(loan_data,aes(x=verification_status,fill=loan_status))+
  geom_bar(position = "fill")

# 7 Loan Status vs Purpose
ggplot(loan_data,aes(x=purpose,fill=loan_status))+
  geom_bar(position = "fill")+
  labs(x="Purpose",title="Purpose")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 

# 8 Loan Status vs Home ownership
ggplot(loan_data,aes(x=home_ownership,fill=loan_status))+
  geom_bar(position = "fill")

# 9 Loan Status vs Public Record Bankrupcies
ggplot(loan_data,aes(x=pub_rec_bankruptcies,fill=loan_status))+
  geom_bar(position = "fill")+
  labs(x="Public Record Bankrupcies",title="Public Record Bankrupcies")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 

# 10 Loan Status vs Public Records
ggplot(loan_data,aes(x=pub_rec,fill=loan_status))+
  geom_bar(position = "fill")+
  labs(x="Public Records",title="Public Records")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 

# 11 Loan status vs inq_last_6mths
ggplot(loan_data,aes(x=inq_last_6mths,fill=loan_status))+
  geom_bar(position = "fill")+
  labs(x="Six Month Inquiries",title="Six Month Inquiries")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 

# 12 Loan status vs delinq_2yrs
ggplot(loan_data,aes(x=delinq_2yrs,fill=loan_status))+
  geom_bar(position = "fill")+
  labs(x="Delinquencies",title="Delinquencies In Last Two Year")+
  labs(fill="Loan Status")+
  labs(y="Proportion")


# Performing univariate quantitative variables  
  
# 13 Loan Status vs dti
ggplot(loan_data,aes(x=dti,fill=loan_status))+
  geom_histogram(colour = "black", binwidth = 0.5,position = "fill")+
  labs(x="DTI",title="DTI")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

# 14 Loan Status vs Funded Amount
bin_size = (max(loan_data$funded_amnt) - min(loan_data$funded_amnt)) / 50
ggplot(loan_data,aes(x=funded_amnt,fill=loan_status))+
  geom_histogram(colour = "black", binwidth = bin_size, position = "fill")+
  labs(x="Funded Amount",title="Funded Amount")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

# 15 Loan Status vs Annual Income
bin_size = (max(loan_data$annual_inc) - min(loan_data$annual_inc)) / 50
ggplot(loan_data,aes(x=annual_inc,fill=loan_status))+
  geom_histogram(colour = "black", binwidth = bin_size, position = "fill")+
  labs(x="Annual Income",title="Annual Income")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

# 16 Loan Status vs Installment
bin_size = (max(loan_data$installment ) - min(loan_data$installment)) / 50
ggplot(loan_data,aes(x=installment,bin_width = bin_size, fill=loan_status))+
  geom_histogram(colour = "black", position = "fill")+
  labs(x="Installment",title="Installment")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

#* int_rate :
# 17 creating rate slots:
loan_data$int_rate_Slots = rep(1,nrow(loan_data))
loan_data$int_rate_Slots[ which( loan_data$int_rate  >= 5 & loan_data$int_rate < 7.5 ) ] = "Slot1 5 - 7.5" ;
loan_data$int_rate_Slots[ which( loan_data$int_rate  >= 7.5 & loan_data$int_rate < 10 ) ] = "Slot2 7.5 - 10" ;
loan_data$int_rate_Slots[ which( loan_data$int_rate  >= 10 & loan_data$int_rate < 12.5 ) ] = "Slot3 10 - 12.5" ;
loan_data$int_rate_Slots[ which( loan_data$int_rate  >= 12.5 & loan_data$int_rate < 15 ) ] = "Slot4 12.5 - 15" ;
loan_data$int_rate_Slots[ which( loan_data$int_rate  >= 15 & loan_data$int_rate < 17.5 ) ] = "Slot5 15 - 17.5" ;
loan_data$int_rate_Slots[ which( loan_data$int_rate  >= 17.5 & loan_data$int_rate < 20 ) ] = "Slot6 17.5 - 20" ;
loan_data$int_rate_Slots[ which( loan_data$int_rate  >= 20 & loan_data$int_rate < 22.5 ) ] = "Slot7 20 - 22.5" ;
loan_data$int_rate_Slots[ which( loan_data$int_rate  >= 22.5 & loan_data$int_rate < 25 ) ] = "Slot8 22.5 - 25" ;

ggplot( loan_data, aes(x = int_rate_Slots, fill = factor(loan_status))) + geom_bar(stat = "count" ) + labs(x ="Interest Rate SLots",title="Total Loans per Slot") + labs(y ="Count of Loans") + labs(fill="Loan Status")



#here it shows that majority loans are given in 10 - 15 rate of interest.


#calculating only charged off loans vs state
charged_off <- subset(loan_data, loan_status == "Charged Off")
ggplot( charged_off, aes(x = int_rate_Slots)) + geom_bar(stat = "count" ) + labs(x ="Interest Rate SLots",title="Defaulted Loans per Slot") + labs(y ="Count of Defaulted Loans")
#This comparision shows the loans given in 12.5-15% slot have seen highest defaulters.

# Performing segmented univariate analysis
# Identifying a derived metric as proportion of charged off in total population

proportions <- summarise(group_by(loan_data, loan_status), total = n())
proportions$percentage <- proportions$total / sum(proportions$total)
proportions
ggplot(proportions, aes(x = loan_status, y= percentage)) + geom_bar(stat="identity")

# Evaluate if the proportion metric is higher in any segment

# 18 Mortage and lower grade loan

homeOwner <- subset(loan_data, loan_data$home_ownership=='MORTGAGE' &
                      ((loan_data$grade=='B')| (loan_data$grade=='C')|
                       (loan_data$grade=='D')| (loan_data$grade=='E')|
                       (loan_data$grade=='F')))

proportions <- summarise(group_by(homeOwner, loan_status), total = n())
proportions$percentage <- proportions$total / sum(proportions$total)
proportions
ggplot(proportions, aes(x = loan_status, y= percentage)) + geom_bar(stat="identity")



# 19 High Funded value and high DTI

temp <- subset(loan_data,funded_amnt>25000 & dti > 20)

proportions <- summarise(group_by(temp, loan_status), total = n())
proportions$percentage <- proportions$total / sum(proportions$total)
proportions
ggplot(proportions, aes(x = loan_status, y= percentage)) + geom_bar(stat="identity")


# 20 High Funded value and employment term less than 1 year

temp <- subset(loan_data,funded_amnt>25000 & loan_data$emp_length == '< 1 year')

proportions <- summarise(group_by(temp, loan_status), total = n())
proportions$percentage <- proportions$total / sum(proportions$total)
proportions
ggplot(proportions, aes(x = loan_status, y= percentage)) + geom_bar(stat="identity")


# 21 Annual Income greater than 350K and income source not verified

temp <- subset(loan_data,annual_inc>350000 & factor(loan_data$verification_status) == 'Not Verified')

proportions <- summarise(group_by(temp, loan_status), total = n())
proportions$percentage <- proportions$total / sum(proportions$total)
proportions
ggplot(proportions, aes(x = loan_status, y= percentage)) + geom_bar(stat="identity")

# Write the cleaned data to file
write.csv(loan_data, "loan_clean.csv",row.names = FALSE)

