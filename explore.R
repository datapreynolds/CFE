############################################################
# CFE Data Explore
# 
# Darren Reynolds (darrenpreynolds@knights.ucf.edu)
# created on December 2018
############################################################

library(readr)
train <- read_csv("~/Grad School/Data Comp/train.csv")
View(train)

#Clean the Data
#####
train$LTV <- as.numeric(train$LTV)

train$CoMonthlyLiability  <- is.na(as.numeric(train$CoMonthlyLiability),0 )
train$CoMonthlyLiability <- replace(as.numeric(train$CoMonthlyLiability)) 

train$CoMonthlyRentt  <- is.na(as.numeric(train$CoMonthlyRent),0 )
train$CoMonthlyRent <- replace(as.numeric(train$CoMonthlyRent)) 
#####


months = train$EmployedMonths + train$PrevEmployedMonths
co_months = train$CoEmployedMonths + train$CoPrevEmployedMonths
num_months = train$EmployedMonths + train$PrevEmployedMonths + train$CoEmployedMonths + train$CoPrevEmployedMonths

hist(num_months, main = "Total Months", xlab = "Months (with Co-Applicant)", xlim = c(0,1000),las=1 ,breaks = 5)

hist(months, main = "Total Months", xlab = "Months (w/o Co-Applicant)", xlim = c(0,500),las=1 ,breaks = 5)

hist(co_months, main = "Total Months", xlab = "Months (just Co-Applicant)", xlim = c(0,500),las=1 ,breaks = 5)

summary(num_months)
summary(months)
summary(co_months)


library(ggplot2)


options(scipen = 999)






gg <- ggplot(train, aes(x=EmployedMonths + PrevEmployedMonths 
                        , y=CoEmployedMonths + CoPrevEmployedMonths)) +
      #geom_point(col=train$LoanStatus)+
      geom_smooth(method="loess", se=F) + xlim(c(0, 2200)) + ylim(c(0, 250)) + 
      labs( y="Co-Employment Months", 
            x="Employment Months", 
            title="Total months of Employment", 
            caption = "Employent plus Coapplicant")
plot(gg)


# cut off with 500
# Employment_Co.png
qplot(x=EmployedMonths + PrevEmployedMonths, 
      y=CoEmployedMonths + CoPrevEmployedMonths, 
      data= train, color=LoanStatus, 
      xlim=(c(0,500)), ylim=(c(0,500)), 
      xlab="Employment", 
      ylab= "Co-Employment", 
      main="Total months of Employment")

# log with cut off of 500
# cut off with 10
# Log Employment_Co.png
qplot(x=log(EmployedMonths + PrevEmployedMonths), 
      y=log(CoEmployedMonths + CoPrevEmployedMonths), 
      data= train, color=LoanStatus, 
      xlim=(c(0,7.5)), ylim=(c(0,7.5)), 
      xlab="Employment", 
      ylab= "Co-Employment", 
      main="Logged Total months of Employment")

############################################################################


rev <- (table(train$NumberOfOpenRevolvingAccounts, train$LoanStatus))
rev <- default
rev$rate <- rev[2] / rev[3]
# revolving accounts
qplot(x=NumberOfOpenRevolvingAccounts, 
      y=NumberOfOpenRevolvingAccounts, 
      data= train, color=LoanStatus, 
      xlim=(c(0,100)), ylim=(c(0,100)), 
      xlab="# of open rev accounts", 
      ylab= "", 
      main="Accept/ Decline Rev accounts")

# mod credit score and num rev accounts
qplot(x=ModifiedCreditScore, 
             y=NumberOfOpenRevolvingAccounts, 
             data= train, color=LoanStatus, 
             xlim=(c(0,900)), ylim=(c(0,100)), 
             xlab="mod credit score", 
             ylab= "", 
             main="Accept/ Decline Rev accounts")

# LOG mod credit score and num rev accounts
qplot(x=log(ModifiedCreditScore), 
      y=log(NumberOfOpenRevolvingAccounts), 
      data= train, color=LoanStatus, 
      xlim=(c(0, 10)), ylim=(c(0, 5)), 
      xlab="mod credit score", 
      ylab= "Revolving account number", 
      main="LOG Accept/ Decline Rev accounts")

############################################################################

# num rev accounts and 
qplot(x=ModifiedBankruptcyScore, 
      y=NumberOfOpenRevolvingAccounts, 
      data= train, color=LoanStatus, 
      xlim=(c(0,900)), ylim=(c(0,100)), 
      xlab="mod bankrupt score", 
      ylab= "", 
      main="Accept/ Decline Rev accounts")

# LOG mod credit score and num rev accounts
qplot(x=log(ModifiedBankruptcyScore), 
      y=log(NumberOfOpenRevolvingAccounts), 
      data= train, color=LoanStatus, 
      xlim=(c(0, 10)), ylim=(c(0, 5)), 
      xlab="mod bankrupt score", 
      ylab= "Revolving account number", 
      main="LOG Accept/ Decline Rev accounts")


###############################################################################

# Applicant with Co | Credit/ Bankruptcy Score



newdata <- train[ which(train$CoApplicantIndicator=='Y'), ]

newdata <- newdata[ which((newdata$ModifiedCreditScore > 0) 
                    & (newdata$ModifiedBankruptcyScore > 0)),]

table(newdata$LoanStatus)




boxplot( ModifiedCreditScore ~ LoanStatus, data = newdata,
           xlab = "Loan Status", ylab = "Modified Credit Score",
           main = "Loan Status that has Co-Applicants")

summary(newdata$ModifiedCreditScore[which(newdata$LoanStatus == 'Approved')])
summary(newdata$ModifiedCreditScore[which(newdata$LoanStatus == 'Declined')])


boxplot( newdata$ModifiedBankruptcyScore ~ LoanStatus, data = newdata,
         xlab = "Loan Status", ylab = "Modified Bankruptcy Score",
         main = "Loan Status that has Co-Applicants")

summary(newdata$ModifiedBankruptcyScore[which(newdata$LoanStatus == 'Approved')])
summary(newdata$ModifiedBankruptcyScore[which(newdata$LoanStatus == 'Declined')])


# new_Approve <- newdata[ which((newdata$LoanStatus=='Approved') & (newdata$ModifiedCreditScore > 0 )), ]
# 
# new_Decline <- newdata[ which((newdata$LoanStatus=='Declined') & (newdata$ModifiedCreditScore > 0 )), ]



###############################################################################


# Loan Application of All Applicants without Zeros

# Based off of Modified Credit Score
boxplot( ModifiedCreditScore ~ LoanStatus, data = train,
         xlab = "Loan Status", ylab = "Modified Credit Score",
         main = "Loan Status of All Applicants")


ggplot(train, aes(x=LoanStatus, y=ModifiedCreditScore, 
              fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()


summary(train$ModifiedCreditScore[which(train$LoanStatus == 'Approved')])
summary(train$ModifiedCreditScore[which(train$LoanStatus == 'Declined')])

# Based off of Modified Bankruptcy Score
boxplot( ModifiedBankruptcyScore ~ LoanStatus, data = train,
         xlab = "Loan Status", ylab = "Modified Bankruptcy Score",
         main = "Loan Status of All Applicants")


ggplot(train, aes(x=LoanStatus, y=ModifiedBankruptcyScore, 
                  fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()

summary(train$ModifiedBankruptcyScore[which(train$LoanStatus == 'Approved')])
summary(train$ModifiedBankruptcyScore[which(train$LoanStatus == 'Declined')])

########################################################################################

# Loan Application of All Applicants without Zeros

# Based off of Modified Credit Score
boxplot( ModifiedCreditScore ~ LoanStatus, data = train[which(train$ModifiedCreditScore > 0),],
         xlab = "Loan Status", ylab = "Modified Credit Score",
         main = "Loan Status of All Applicants")

ggplot(train[which(train$ModifiedCreditScore > 0),], aes(x=LoanStatus, y=ModifiedCreditScore, 
                  fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()


summary(train$ModifiedCreditScore[which((train$LoanStatus == 'Approved') & (train$ModifiedCreditScore > 0))], )
summary(train$ModifiedCreditScore[which((train$LoanStatus == 'Declined') & (train$ModifiedCreditScore > 0))], )

# Based off of Modified Bankruptcy Score
boxplot( ModifiedBankruptcyScore ~ LoanStatus, data = train[which(train$ModifiedBankruptcyScore > 0),],
         xlab = "Loan Status", ylab = "Modified Bankruptcy Score",
         main = "Loan Status of All Applicants")


ggplot(train[which(train$ModifiedCreditScore > 0),], aes(x=LoanStatus, y=ModifiedBankruptcyScore, 
                  fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()

summary(train$ModifiedBankruptcyScore[which((train$LoanStatus == 'Approved') & (train$ModifiedBankruptcyScore > 0))], )
summary(train$ModifiedBankruptcyScore[which((train$LoanStatus == 'Declined') & (train$ModifiedBankruptcyScore > 0))], )

############################################################################################

# Take a look at zeros for Credit and Bankruptcy score

Credit_0 <- train[which(train$ModifiedCreditScore == 0),]
Bank_0 <- train[which(train$ModifiedBankruptcyScore == 0),]

View(Credit_0)
View(Bank_0)

summary(Credit_0[which(Credit_0$LoanStatus == 'Approved'),])
summary(Bank_0[which(Bank_0$LoanStatus == 'Approved'),])


#################################################################################################




#train <- train %>% mutate(EstimatedNetIncome = 
#TotalMonthlyIncome - PrimeMonthlyLiability - CoMonthlyLiability 
#- PrimeMonthlyRent - CoMonthlyRent - TotalMonthlyDebtBeforeLoan - EstimatedMonthlyPayment,
#'IsBalanced' = EstimatedNetIncome > 0)

#visuals for new variable created by Quinton Roberts
# This variable is just to show what someones Estimated Net Income is

## NOTE TO SELF Quinton has made this visual as it has a clear pattern associated with it

########################################################################################################3

## LTV

#> BigLTV <- train[which(train$LTV > 1), ]
#> View(BigLTV)
#> summary(BigLTV)

#> table(BigLTV$LoanStatus)

# Approved Declined 
# 21447    27788 
# > table(train$LoanStatus)
# 
# Approved Declined 
# 49466    60388 
# > plot(train$TotalVehicleValue, train$AmountRequested )


qplot(x= TotalVehicleValue, 
      y= AmountRequested, 
      data= train, color=LoanStatus, 
      xlim= c(0,100000) , ylim=c(0,100000) , 
      xlab="Total Vechile Value", 
      ylab= "Amount Requested", 
      main="Vehicle Value vs. Amount Requested")

qplot(x= log(TotalVehicleValue), 
      y= log(AmountRequested), 
      data= train, color=LoanStatus, 
      xlim= c(0,20) , ylim=c(0,20) , 
      xlab="Total Vechile Value", 
      ylab= "Amount Requested", 
      main="LOG Vehicle Value vs. Amount Requested")


#################################################################

#LTV and DTI


qplot(x= LTV, 
      y= DTI, 
      data= train, color=LoanStatus, 
      xlim= c(0,2.5) , ylim= c(0,2.5) , 
      xlab="Loan to Vehicle Ratio", 
      ylab= "Debt to Income Ratio", 
      main="LTV vs. DTI")



qplot(x= log(LTV), 
      y= log(DTI), 
      data= train, color=LoanStatus, 
      xlim=  , ylim= , 
      xlab="Loan to Vehicle Ratio", 
      ylab= "Debt to Income Ratio", 
      main="Log LTV vs. DTI")




################################################################

# DTI and Estimated Net Income

qplot(x= TotalMonthlyIncome - PrimeMonthlyLiability - CoMonthlyLiability - PrimeMonthlyRent - CoMonthlyRent - TotalMonthlyDebtBeforeLoan - EstimatedMonthlyPayment, 
      y= DTI, 
      data= train, color=LoanStatus, 
      xlim= c(0,2.5) , ylim= c(-100000, 100000) , 
      xlab="Estimated Net Income", 
      ylab= "Debt to Income Ratio", 
      main="Estimated Net Income vs. DTI")


###################################################################

# Justin Requested Plots

#DTI check
#LTV check
#ModifiedCreditScore: be sure the quartiles are correct (it seems like some of the data is missing because the quartiles don't overlap): See attached spreadsheet Credit Score Tab
#ModifiedBankRuptcy score
#OpenRevolving Accounts check
#Scatter plot for: Open Revolving Accounts vs. Credit Score with Approved / Declined (feel free to exclude certain high-outliers)

# DTI

summary(train$DTI)
ggplot(train, aes(x=LoanStatus, y=DTI, 
                  fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()

#1st Approval after outliers is at 1.77345
# Max of 4375 
# Na's of 198

# 42.8 - >> 208.33...

ggplot(train[which(train$DTI < 2),], aes(x=LoanStatus, y=DTI, 
                 fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() 


# LTV

summary(train$LTV)
ggplot(train, aes(x=LoanStatus, y=LTV, 
                  fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()

# 1st approval after outliers is at 2.8301
# Max of 30102
# Na's 10822

# 89.2655 - >> 114.3424

ggplot(train[which(train$LTV < 2.9),], aes(x=LoanStatus, y=LTV,
                fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() 




# Number of Open Revolving Accounts
ggplot(train, aes(x=LoanStatus, y=NumberOfOpenRevolvingAccounts, 
        fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()

# open revolving account
summary(train$NumberOfOpenRevolvingAccounts)
ggplot(train, aes(x=LoanStatus, y=train$NumberOfOpenRevolvingAccounts, 
                  fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()

# Open Revolving Acounts vs Credit Score

qplot(x= NumberOfOpenRevolvingAccounts, 
      y= train$ModifiedCreditScore, 
      data= train, color=LoanStatus, 
      xlim=  , ylim=  , 
      xlab="Number of Open Revolving Accounts", 
      ylab= "Modified Credit Score", 
      main="Number if Open Revolving Accounts vs. Modified Credit Scores")



# removed zeros----
# R doc for geom_boxplot
# R doc for boxplots.stats
# R doc for quantile

CREDIT = ggplot(train[which(train$ModifiedCreditScore > 0),], aes(x=LoanStatus, y=ModifiedCreditScore, 
                fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()

ggplot_build(CREDIT)$data

BANKRUPTCY = ggplot(train[which(train$ModifiedBankruptcyScore > 0),], aes(x=LoanStatus, y=ModifiedBankruptcyScore, 
                fill = LoanStatus)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()

ggplot_build(BANKRUPTCY)$data