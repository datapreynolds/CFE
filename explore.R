############################################################
# CFE Data Explore
# 
# Darren Reynolds (darrenpreynolds@knights.ucf.edu)
# created on December 2018
############################################################

library(readr)
train <- read_csv("~/Grad School/Data Comp/train.csv")
View(train)


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







gg <- ggplot(train, aes(x=train$EmployedMonths + train$PrevEmployedMonths 
                        , y=train$CoEmployedMonths + train$CoPrevEmployedMonths)) +
      #geom_point(col=train$LoanStatus)+
      geom_smooth(method="loess", se=F) + xlim(c(0, 2200)) + ylim(c(0, 250)) + 
      labs( y="Co-Employment Months", 
            x="Employment Months", 
            title="Total months of Employment", 
            caption = "Employent plus Coapplicant")
plot(gg)



qplot(x=train$EmployedMonths + train$PrevEmployedMonths, 
      y=train$CoEmployedMonths + train$CoPrevEmployedMonths, 
      data= train, color=train$LoanStatus, 
      xlim=(c(0,2200)), ylim=(c(0,1400)), 
      xlab="Employment", 
      ylab= "Co-Employment", 
      main="Total months of Employment")




#gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
#  geom_point(aes(col=state, size=popdensity)) + 
#  geom_smooth(method="loess", se=F) + 
#  xlim(c(0, 0.1)) + 
#  ylim(c(0, 500000)) + 
#  labs(subtitle="Area Vs Population", 
#       y="Population", 
#       x="Area", 
#       title="Scatterplot", 
#       caption = "Source: midwest")



#hist(AirPassengers, 
#main="Histogram for Air Passengers", 
#xlab="Passengers", 
#border="blue", 
#col="green",
#xlim=c(100,700),
#las=1, 
#breaks=5)