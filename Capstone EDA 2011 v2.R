library(ggplot2)
library(dplyr)
library(tidyr)

#Convert year & region to factor
AllYearsNoCert$Year<- as.factor(AllYearsNoCert$Year)
AllYearsNoCert$region <- as.factor(AllYearsNoCert$region)
AllYearsNoCert$sch_deg2 <- as.factor(AllYearsNoCert$sch_deg)

#DISTRIBUTION OF SCHOOLS BY REGION
qplot(x = AllYearsNoCert$region, xlab = 'region')+ scale_x_discrete()

#summary of admission rates by geographic characteristics
by(AllYearsNoCert$ADM_RATE_ALL, AllYearsNoCert$region, summary, na.rm = TRUE) #fix
ggplot(data = AllYearsNoCert, aes(x = region, y = ADM_RATE_ALL)) + geom_boxplot()

#Pell recipients by year
by(AllYearsNoCert$PCTPELL, AllYearsNoCert$Year, summary)
ggplot(data = subset(AllYearsNoCert, PCTPELL > 0.0), aes(x = Year, y = PCTPELL)) + geom_boxplot(fill = 'orange')

#by median debt - Public average cost
ggplot(aes(x = NPT4_PUB, y = DEBT_MDN), data = subset(AllYearsNoCert, NPT4_PUB >=0)) + 
  geom_point(aes(alpha = 1/20, position = 'jitter', colour = 'red')) +
  scale_x_continuous(breaks = seq(0, 30000, 5000)) + scale_y_sqrt()
summary(AllYearsNoCert$NPT4_PUB)
by(AllYearsNoCert$DEBT_MDN, AllYearsNoCert$NPT4_PUB, summary)
cor.test(AllYearsNoCert$DEBT_MDN, AllYearsNoCert$NPT4_PUB)

#by median debt - Private average cost
ggplot(aes(x = NPT4_PRIV, y = DEBT_MDN), data = subset(AllYearsNoCert, NPT4_PRIV >=0)) + 
  geom_point(alpha = 1/20, position = 'jitter', color = 'orange') +
  scale_x_continuous(breaks = seq(0, 75000, 10000)) + scale_y_log10()
summary(AllYearsNoCert$NPT4_PRIV)
by(AllYearsNoCert$DEBT_MDN, AllYearsNoCert$NPT4_PRIV, summary)
cor.test(AllYearsNoCert$DEBT_MDN, AllYearsNoCert$NPT4_PRIV)


#2 year default rate versus median debt
ggplot(aes(x = CDR2, y = DEBT_MDN), data = AllYearsNoCert) + 
  geom_point(alpha = 1/10, position = 'jitter', colour = 'red') +
  scale_x_log10(breaks = seq(0, 0.5, 0.1)) + scale_y_continuous()
summary(AllYearsNoCert$CDR2)
cor.test(AllYearsNoCert$CDR2, AllYearsNoCert$DEBT_MDN)
#weak, negative correlation --> does not intuitively make sense

#3 year default rate versus median debt
ggplot(aes(x = CDR3, y = DEBT_MDN), data = AllYearsNoCert) + 
  geom_point(alpha = 1/10, position = 'jitter', colour = 'red') +
  scale_x_continuous() + scale_y_continuous()+
  ylim(0, 35000)
summary(AllYearsNoCert$CDR3)
cor.test(AllYearsNoCert$CDR3, AllYearsNoCert$DEBT_MDN)
#weak, negative correlation --> does not intuitively make sense

#Low income median debt against 3 year default rate
ggplot(aes(x = CDR3, y = LO_INC_DEBT_MDN), data = AllYearsNoCert) + 
  geom_point(alpha = 1/10, position = 'jitter', color = "blue") +
  scale_x_continuous() + scale_y_continuous(breaks = seq(0, 35000, 5000)) +
  ylim(0, 35000)
summary(AllYearsNoCert$LO_INC_DEBT_MDN)
cor.test(AllYearsNoCert$CDR3, AllYearsNoCert$DEBT_MDN)

#median debt versus low income median debt
ggplot(aes(x = DEBT_MDN, y = LO_INC_DEBT_MDN), data = AllYearsNoCert) + 
  geom_point(alpha = 1/20, position = 'jitter', color = "blue") +
  scale_x_continuous() + scale_y_continuous(breaks = seq(0, 35000, 5000)) +
  ylim(0, 35000)

#4-year public cost verus median debt - low income
ggplot(aes(x = NPT4_PUB, y = LO_INC_DEBT_MDN), data = AllYearsNoCert) + 
  geom_point(alpha = 1/5, position = 'jitter', color = "blue") +
  scale_x_continuous() + scale_y_continuous(breaks = seq(0, 35000, 5000)) +
  ylim(0, 35000)

#Total cost 4 year academic calendar
ggplot(aes(x = COSTT4_A, y = mn_earn_wne_p10), data = AllYearsNoCert) + 
  geom_point(alpha = 1/10, position = 'jitter', color = "blue") +
  scale_x_continuous() + scale_y_log10(breaks = seq(0, 100000, 10000))
by(AllYearsNoCert$mn_earn_wne_p10, AllYearsNoCert$COSTT4_A, summary)

hiCost <- subset(AllYearsNoCert, COSTT4_A >= 30000 & COSTT4_A <= 60000)

#4 year private cost versus median debt - low income
ggplot(aes(x = NPT4_PRIV, y = LO_INC_DEBT_MDN), data = AllYearsNoCert) + 
  geom_point(alpha = 1/5, position = 'jitter', color = "blue") +
  scale_x_continuous(breaks = seq(0, 55000, 5000)) + scale_y_continuous() + xlim(0, 55000)

#earnings versus median debt - 10 years post graduation
ggplot(aes(x = DEBT_MDN, y = md_earn_wne_p10), data = AllYearsNoCert) + 
  geom_point(alpha = 1/10, position = 'jitter', color = "purple") +
  scale_x_log10() + scale_y_log10()

ggplot(aes(x = DEBT_MDN, y = md_earn_wne_p6), data = AllYearsNoCert) + 
  geom_point(alpha = 1/5, position = 'jitter', color = "orange") +
  scale_x_log10() + scale_y_log10()
summary(AllYearsNoCert$md_earn_wne_p6)

AllYearsNoCert$LO_INC_DEBT_MDN <- as.numeric(AllYearsNoCert$LO_INC_DEBT_MDN)
AllYearsNoCert$LO_INC_DEBT_MDN[is.na(AllYearsNoCert$LO_INC_DEBT_MDN)] <- mean(AllYearsNoCert$LO_INC_DEBT_MDN, na.rm = TRUE)  
summary(AllYearsNoCert$AllYearsNoCert$DEBT_MDN[is.na(AllYearsNoCert$DEBT_MDN)] <- mean(AllYearsNoCert$DEBT_MDN, na.rm = TRUE))

ggplot(aes(x = NPT41_PUB, y = LO_INC_DEBT_MDN), data = AllYearsNoCert) + 
  geom_point(alpha = 1/5, position = 'jitter') + scale_y_continuous() +
  xlab('Avg net price')

  
  
  
  
