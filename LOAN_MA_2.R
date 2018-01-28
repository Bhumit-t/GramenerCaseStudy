library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

#setwd("D:/Data Science/Course 2 - Statistics & Exploratory Data Analytics/Group Project - Loan Default")
input <- read.csv("loan.csv", stringsAsFactors = FALSE)

#remove columns with all NAs
input_NoNAs <-  input[, colSums(is.na(input)) < nrow(input)]

#added Bhumit
#removing columns which are of no use (like most of those are 0 values, values of no importance)
input_subset <- subset( input_NoNAs, select = -c(pymnt_plan,url,initial_list_status,collections_12_mths_ex_med,policy_code, application_type, acc_now_delinq, chargeoff_within_12_mths,delinq_amnt, tax_liens))

#added Bhumit
#removing few more columns
input_subset = subset( input_subset, select = - c(id, member_id,emp_title,title,zip_code) )

# Make percentage as numeric by removing the % character
input_subset<-separate(input_subset,int_rate,c("int_rate"),sep="%",remove=TRUE)
input_subset$int_rate<-as.numeric(input_subset$int_rate)

#str(input_subset)

#Determine the proportion of Charged Off loans
ggplot(input_subset, aes(x = factor(loan_status))) + geom_bar(stat = "count")+ 
  geom_text(stat ='count', aes(label = ..count..), vjust = -1)+
  labs(x="Loan Status",title="Loan Status")


#added Bhumit
#* int_rate :
#creating rate slots:
input_subset$int_rate_Slots = rep(1,nrow(input_subset))
input_subset$int_rate_Slots[ which( input_subset$int_rate  >= 5 & input_subset$int_rate < 7.5 ) ] = "Slot1 5 - 7.5" ;
input_subset$int_rate_Slots[ which( input_subset$int_rate  >= 7.5 & input_subset$int_rate < 10 ) ] = "Slot2 7.5 - 10" ;
input_subset$int_rate_Slots[ which( input_subset$int_rate  >= 10 & input_subset$int_rate < 12.5 ) ] = "Slot3 10 - 12.5" ;
input_subset$int_rate_Slots[ which( input_subset$int_rate  >= 12.5 & input_subset$int_rate < 15 ) ] = "Slot4 12.5 - 15" ;
input_subset$int_rate_Slots[ which( input_subset$int_rate  >= 15 & input_subset$int_rate < 17.5 ) ] = "Slot5 15 - 17.5" ;
input_subset$int_rate_Slots[ which( input_subset$int_rate  >= 17.5 & input_subset$int_rate < 20 ) ] = "Slot6 17.5 - 20" ;
input_subset$int_rate_Slots[ which( input_subset$int_rate  >= 20 & input_subset$int_rate < 22.5 ) ] = "Slot7 20 - 22.5" ;
input_subset$int_rate_Slots[ which( input_subset$int_rate  >= 22.5 & input_subset$int_rate < 25 ) ] = "Slot8 22.5 - 25" ;

ggplot( input_subset, aes(x = int_rate_Slots, fill = factor(loan_status))) + geom_bar(stat = "count" ) + labs(x ="Interest Rate SLots",title="Total Loans per Slot") + labs(y ="Count of Loans") + labs(fill="Loan Status")



#here it shows that majority loans are given in 10 - 15 rate of interest.


#calculating only charged off loans vs state
charged_off <- subset(input_subset, loan_status == "Charged Off")
ggplot( charged_off, aes(x = int_rate_Slots)) + geom_bar(stat = "count" ) + labs(x ="Interest Rate SLots",title="Defaulted Loans per Slot") + labs(y ="Count of Defaulted Loans")
#This comparision shows the loans given in 12.5-15% slot have seen highest defaulters.




#1 - Loan Status vs Loan Grade
ggplot(input_subset,aes(x=factor(grade),fill=factor(loan_status)))+geom_bar(position="fill")+
  labs(x="Grade",title=" Grade")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

#2 - Loan Status vs Loan Sub-grade
ggplot(input_subset,aes(x=factor(sub_grade),fill=factor(loan_status)))+geom_bar(position="fill")+
  labs(x="Sub Grade",title="Loan Sub Grade")+
  labs(fill="Loan Status")+
  labs(y="Proportion")

#3 - Loan Status vs state
ggplot(input_subset,aes(x=factor(addr_state)))+geom_bar(stat = "count") + 
  geom_text(stat ='count', aes(label = ..count..), vjust = -1)
#Now plot the proportion of charged off loans
ggplot(input_subset,aes(x=factor(addr_state),fill=factor(loan_status)))+geom_bar(position = "fill")+
  labs(x="State",title="State")+
  labs(fill="Loan Status")+
  labs(y="Proportion")
# State NE has very few loans to make a call on the high proportion of charged-off loans

#4 Loan Status vs Term
ggplot(input_subset,aes(x=factor(term),fill=factor(loan_status)))+geom_bar(position = "fill")+
  labs(x="Term",title="Loan Term")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 
#Loan with Term of 60 months have a higher proportion of defaulting

#5 Loan Status vs Emp Length
ggplot(input_subset,aes(x=factor(emp_length),fill=factor(loan_status)))+geom_bar(position = "fill") 

#6 Loan Status vs Verification Status
ggplot(input_subset,aes(x=factor(verification_status),fill=factor(loan_status)))+
  geom_bar(position = "fill")

#7 Loan Status vs Purpose
ggplot(input_subset,aes(x=factor(purpose),fill=factor(loan_status)))+
  geom_bar(position = "fill")+
  labs(x="Purpose",title="Purpose")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 

#8 Loan Status vs Home ownership
ggplot(input_subset,aes(x=factor(home_ownership),fill=factor(loan_status)))+
  geom_bar(position = "fill")

#9 Loan Status vs Public Record Bankrupcies
ggplot(input_subset,aes(x=factor(pub_rec_bankruptcies),fill=factor(loan_status)))+
  geom_bar(position = "fill")+
  labs(x="Public Record Bankrupcies",title="Public Record Bankrupcies")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 

#10 Loan Status vs Public Records
ggplot(input_subset,aes(x=factor(pub_rec),fill=factor(loan_status)))+
  geom_bar(position = "fill")+
  labs(x="Public Records",title="Public Records")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 

#11 Loan status vs inq_last_6mths
ggplot(input_subset,aes(x=factor(inq_last_6mths),fill=factor(loan_status)))+
  geom_bar(position = "fill")+
  labs(x="Six Month Inquiries",title="Six Month Inquiries")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 

#12 Loan status vs delinq_2yrs
ggplot(input_subset,aes(x=factor(delinq_2yrs),fill=factor(loan_status)))+
  geom_bar(position = "fill")+
  labs(x="Delinquencies",title="Delinquencies In Last Two Year")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 



#Performing univariate quantitative variables  
  
#13 Loan Status vs dti
ggplot(input_subset,aes(x=dti,fill=factor(loan_status)))+geom_histogram(binwidth = 0.5,position = "fill")+
  labs(x="DTI",title="DTI")+
  labs(fill="Loan Status")+
  labs(y="Proportion") 


#Performing segmented univariate analysis

#Identifying a derived metric as proportion of charged off in total population
TotalLoans<-sum(summary(factor(input_subset$loan_status)))
ChargedOffPopulationProportion<-(summary(factor(input_subset$loan_status))[1])/TotalLoans
CurrentPopulationProportion<-(summary(factor(input_subset$loan_status))[2])/TotalLoans
FullyPaidPopulationProportion<-(summary(factor(input_subset$loan_status))[3])/TotalLoans

ChargedOffPopulationProportion
CurrentPopulationProportion
FullyPaidPopulationProportion

#Evaluate if the proportion metric is higher in any segment

#14 Mortage and lower grade loan

HomeOwner <- subset(input_subset,factor(input_subset$home_ownership)=='MORTGAGE' &
                      ((input_subset$grade=='B')| (input_subset$grade=='C')|
                       (input_subset$grade=='D')| (input_subset$grade=='E')|
                       (input_subset$grade=='F')))


TotalLoansSegment<-sum(summary(factor(HomeOwner$loan_status)))
ChargedOffSegmentProportion<-(summary(factor(HomeOwner$loan_status))[1])/TotalLoansSegment
CurrentSegmentProportion<-(summary(factor(HomeOwner$loan_status))[2])/TotalLoansSegment
FullyPaidSegmentProportion<-(summary(factor(HomeOwner$loan_status))[3])/TotalLoansSegment

ChargedOffSegmentProportion
CurrentSegmentProportion
FullyPaidSegmentProportion


#14 High Funded value and high DTI

Temp <- subset(input_subset,funded_amnt>25000 & dti > 20)


TotalLoansSegment<-sum(summary(factor(Temp$loan_status)))
ChargedOffSegmentProportion<-(summary(factor(Temp$loan_status))[1])/TotalLoansSegment
CurrentSegmentProportion<-(summary(factor(Temp$loan_status))[2])/TotalLoansSegment
FullyPaidSegmentProportion<-(summary(factor(Temp$loan_status))[3])/TotalLoansSegment

ChargedOffSegmentProportion
CurrentSegmentProportion
FullyPaidSegmentProportion


#15 High Funded value and employment term less than 1 year

Temp <- subset(input_subset,funded_amnt>25000 & factor(input_subset$emp_length) == '< 1 year')


TotalLoansSegment<-sum(summary(factor(Temp$loan_status)))
ChargedOffSegmentProportion<-(summary(factor(Temp$loan_status))[1])/TotalLoansSegment
CurrentSegmentProportion<-(summary(factor(Temp$loan_status))[2])/TotalLoansSegment
FullyPaidSegmentProportion<-(summary(factor(Temp$loan_status))[3])/TotalLoansSegment

ChargedOffSegmentProportion
CurrentSegmentProportion
FullyPaidSegmentProportion


#16 Annual Income greater than 350K and income source not verified

Temp <- subset(input_subset,annual_inc>350000 & factor(input_subset$verification_status) == 'Not Verified')


TotalLoansSegment<-sum(summary(factor(Temp$loan_status)))
ChargedOffSegmentProportion<-(summary(factor(Temp$loan_status))[1])/TotalLoansSegment
CurrentSegmentProportion<-(summary(factor(Temp$loan_status))[2])/TotalLoansSegment
FullyPaidSegmentProportion<-(summary(factor(Temp$loan_status))[3])/TotalLoansSegment

ChargedOffSegmentProportion
CurrentSegmentProportion
FullyPaidSegmentProportion


write.csv(input_subset, "loan_clean.csv",row.names = FALSE)




