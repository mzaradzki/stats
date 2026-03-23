###### Natalia Mikhailova  #####
#####Logistic regression #####

library("haven")
wvs6<-read_spss("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav")
wvs6_rus<-wvs6[wvs6$V2==51,]
dim(wvs6_rus)
attributes(wvs6$V2)

#Voting in elections
attributes(wvs6_rus$V227)
table(wvs6_rus$V227)
#table(as_factor(wvs6_rus$V227)) #to see with labels
#Creating a bivariate variable
wvs6_rus$vote <- ifelse(wvs6_rus$V227<3, 1, 0)
table(wvs6_rus$V227, wvs6_rus$vote)
wvs6_rus$vote <- labelled(wvs6_rus$vote , c("Not vote" = 0, "Vote" = 1))
table(as_factor(wvs6_rus$V227), as_factor(wvs6_rus$vote))

### Dependent variable in binary logistic regression is not the variable itself
# but the LOGIT of this variable

# Logit(Z) = ln(P(Y=1)/ P(1-P(Y=1))) 
# P(Y=1) - probability of success (the event happened)
# 1-P(Y=1) - probability of fail (the event NOT happened)
# P(Y=1)/ P(1-P(Y=1)) - odds ratio of the event to happen

### The regression coefficients show:
#b0 (intercept) - what is the logarithm of the odds ratio of occurrence of event Y, 
#regarding that all X (all independent variables) are equal to 0
#bi (coef of IVs) - how the logarithm of the odds ratio of occurrence of event Y change, 
#with an increase of Xi by one and the non-change of other X (other independent variables)

# The value of the logarithm is abstract and not convenient for interpretation
# That is why there is an alternative!
#The binary logistic regression model is often expressed in exponential form, 
#where the DV is directly the odds ratio of occurrence of the event of interest Y

# How to interpret the regression coefficients?
# If b0>0 , then e^b0 > 1,
#therefore, for all IVs = 0, the odds ratio of an event occurring 
#are greater than 1 => the probability of an event occurring 
#(in the number of odds ratio) is greater than the probability of an event not occurring 
#(in the denominator of odds ratio)
# If b0<0 , then e^b0 < 1 => opposite case

# If b0=0, then eb0 = 1,
#therefore, for all Xi= 0, the odds ratio of an event occurring are 1 
#=> both outcomes are equally likely to happen.


wvs6_rus$age<-wvs6_rus$V242
wvs6_rus$gnd<-droplevels(as_factor(wvs6_rus$V240))
class(wvs6_rus$gnd)
wvs6_rus$trgov <- wvs6_rus$V138
wvs6_rus$trparty <- wvs6_rus$V139
wvs6_rus$trparliam <- wvs6_rus$V140

binomial <- glm(vote ~ trgov + trparty + trparliam + age + gnd, family="binomial", data= wvs6_rus)
summary(binomial)
#pseudo R^2 is an abalog of R^2 in linear regression
library(fmsb)
NagelkerkeR2(binomial) # pseudo R^2 Nagelkerke
# Intercept: -0.343870
#e= 2.71828
2.71828^(-0.343870)
#among Russian males (reference group due to dummy) 
# odds ratio of voting is less than 1, 
# so the probability of not-voting is higher

binomial2 <- glm(vote ~ trgov + trparty + age + gnd, family="binomial", data= wvs6_rus)
summary(binomial2)
AIC(binomial); AIC(binomial2)
#the lower AIC shows that the model is statistically better

####################################################
# Summary regarding interpretation:
# if you have the reg coef as b0 = -3, b1=0.5, b2=-1.1, then
# the binary logistic equation is
# DV = e^(b0 + b1*x1 + b2*x2) = e^b0 * (e^b1)^x1 * (e^b2)^x2
# Logit(Z) = ln(P(Y=1)/ P(1-P(Y=1))) = 
# = e^(-3+0.5*x1-1.1*x2) = e^(-3) * (e^0.5)^x1 * (e^(-1.1))^x2 =
2.71828^(-3); 2.71828^0.5; 2.71828^(-1.1)
# = 0.0498 * 1.6487^x1 * 0.3329^x2

#CA
# Make assumptions of what else may influence the willingness to vote
# Compare models with different sets of IVs
# Choose the final one, interpret the results

