#requires the tidyverse package

library(tidyverse)
library(lubridate)

fp <- ""		#Insert the filepath of the training data
train <- read_csv(fp)

monthly_data <- train %>%
	mutate(AppReceiveMonth = floor_date(as.Date(AppReceiveDate, format = '%m/%d/%Y'), unit = 'month')) %>%
	group_by(AppReceiveMonth) %>%
	summarize(ApprovalRate = mean(LoanStatus == 'Approved'))

ggplot(monthly_data, mapping = aes(AppReceiveMonth, ApprovalRate)) +
	geom_line() +
	coord_cartesian(ylim = c(0,1), expand = FALSE)
