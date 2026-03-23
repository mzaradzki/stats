###### Natalia Mikhailova  #####
#####Regression #####
##### Research question: how age influences subjective well-being in Russia

# http://jse.amstat.org/v10n1/kennedy.html 
# http://jse.amstat.org/v22n2/ludlow.pdf
# https://link.springer.com/article/10.1023/A:1026595011371 
library("scales")
library("sjPlot")
library("car")
library("lm.beta")
library("scales")
library("psych")
library("Hmisc")

# Let`s run regression to test the link between subjective well-being and age `

library("haven")
wvs6<-read_spss("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav")

# We will use several countries, therefore transform data for the whole dataset 
# We will start with Russia 

#### Creating an index of subjective well-being
# Subjective well-being. 
# In general, SBW reflects an overall assessment of life as a whole.
# A lot of scales. 
# In huge projects most common - happiness and life satisfaction.
# Happiness - affective satisfaction
# Satisfaction  - cognitive satisfaction
# Index of SWB - combination of happiness and satisfaction
# Happiness + life satisfaction
# Happiness - 4 point, reversed
# life satisfaction  - 10 point.

wvs6$age<-wvs6$V242

library("scales")
attributes(wvs6$V10)

wvs6$hapr<-round(scales::rescale(as.numeric(wvs6$V10), to=c(1,0)),4) # 1,0 - for reverse order
# as.numeric to avoid problems with double haven type
table(wvs6$hapr, wvs6$V10) # to check recoding 
table(wvs6$hapr)
table(wvs6$V10)
table(as_factor(wvs6$V10), wvs6$hapr)
attributes(wvs6$V23) #life satisfaction
wvs6$lsatr<-round(scales::rescale(as.numeric(wvs6$V23), to=c(0,1)),4) 
table(wvs6$V23, wvs6$lsatr)
cor(wvs6$lsatr, wvs6$V23, use = "pairwise.complete.obs") # another way to check recoding 

wvs6$swb2<-rowMeans(wvs6[c("lsatr", "hapr")], na.rm = TRUE) # na.rm = FALSE by default (more NA)
wvs6$swb<-rowMeans(wvs6[c("lsatr", "hapr")],na.rm = FALSE) # na.rm = FALSE by default (more NA)
table(wvs6$swb2, useNA = "ifany") # NA, if all items NA, for calculation of mean uses valid number of cases
table(wvs6$swb, useNA = "ifany") # more NA
psych::alpha(wvs6[, c("lsatr", "hapr")])

# We will use sbw2 with less NA

# Select Russia to check the quality of SWB index 

wvs6_rus<-wvs6[wvs6$V2==643,]
wvs6_sw <- wvs6[wvs6$V2==752,]
attributes(wvs6$V2)
dim(wvs6_rus)
library(Hmisc)
Hmisc::rcorr(as.matrix(wvs6_rus[, c("hapr", "lsatr", "swb2")])) 
library(psych)
psych::alpha(wvs6_rus[, c("hapr", "lsatr")])
psych::alpha(wvs6_sw[, c("hapr", "lsatr")])
# Bivariate regression
# inspect data first 
summary(wvs6_rus$swb2)
hist(wvs6_rus$swb2)
mean(wvs6_rus$swb2, na.rm = TRUE)

cor_swb_age<-cor.test(wvs6_rus$age, wvs6_rus$swb2, use = "pairwise.complete.obs")
summary(cor_swb_age) # information about elements of the output
# Useful for selection of specific elements
# We need point estimate for correlation 
# This is estimate, the 4th element 
cor_swb_age
cor_swb_age$estimate
cor_swb_age$estimate^2 # R squared, we should see it in the bivariate regression

library("car")
m0<-lm(swb2~1, data = wvs6_rus) # null model. only intercept (= mean for null model)
m0
summary(m0)
confint(m0)

# std.err - 0.004
# t-value  = Estimate/Std. Error 
# lci = mean  - std.err*1.96
# uci = mean  - std.err*1.96
# df = valid sample -1, 1 - estimate 1 parameter - intercept 

sum(table(wvs6_rus$swb2))
# we calculated only mean - 1 parameter
sum(table(wvs6_rus$swb2)) - 1
summary(m0)

# bivariate regression
cor_swb_age$estimate
cor_swb_age$estimate^2
0.7292163-0.0028301-0.0028301-0.0028301
# sbw = 0.7292163 + (-0.0028301)*age + e
m1<-lm(swb2~age, data = wvs6_rus)
summary(m1)
 
#let's add another IV
wvs6_rus$health<-round(scales::rescale(as.numeric(wvs6_rus$V11), to=c(1,0)),4)
table(droplevels(as_factor(wvs6_rus$V11)), wvs6_rus$health)

m1a<-lm(swb2~age+health, data = wvs6_rus)
summary(m1a)
confint(m1a)
# if conclude at 5% sig level:
# swb = 0.480090 - 0.000556*age + 0.318124*health +e
# if conclude at 1% sig level:
# swb = 0.480090 +  0.318124*health +e 

# two estimates - intercept, slope for age
# df = valid n-2
# Squared = R Pearson ^ 2 - share of explained variance, density of a link
# F  - shows the overall quality of a model. 
# (If at least one independent (explanatory) variable is significant) 
# the change in dep var(y) for 1 unit change in indep var(x)


# CA! Repeat this model for Sweden
# Interpret R^2
# Interpret Intercept
# Intepret Slope
wvs6_sw$health<-round(scales::rescale(as.numeric(wvs6_sw$V11), to=c(1,0)),4)
table(droplevels(as_factor(wvs6_sw$V11)), wvs6_sw$health)
m1_sw<-lm(swb2~age, data = wvs6_sw)
summary(m1_sw)
m1a_sw<-lm(swb2~age+health, data = wvs6_sw)
summary(m1a_sw)
# swb = 0.5225337 + 0.0013787*age + health*0.2534569 + e

################################
# multiple regression 
# Add control variables according to the literature 
# Additive affects 
# This is not a combination of parameters 

# subjective health, V11
# economic situation, savings, V237
# generalized trust, V105
# particularized trust, V102
# locus of control, V55. 
  # internal lc- I myself responsible for my life, 
   # external  - some other forces determine my life 
   # Question from WVS. How much freedom of choice and control over own life
# gender, V240
# education, V248
# marriage, V57
# city size, V253
# All variables will be recoded into 0 and 1


# recode the whole dataset wvs6 
attributes(wvs6$V11)
wvs6$health<-round(scales::rescale(as.numeric(wvs6$V11), to=c(1,0)),4) 
cor(wvs6$health, wvs6$V11, use = "pairwise.complete.obs")
table(as_factor(wvs6$health),wvs6$V11) 

# recoding
# for the whole data set 

wvs6$gnd<-droplevels(as_factor(wvs6$V240)) 
table(wvs6$gnd,wvs6$V240)

# Education
# later we will compare Russia and Sweden.
# Education should be recoded to provide comparability in two countries 
# I will recode education as dummy, no high education (1:7), high education (8,9) 
attributes(wvs6$V248)
wvs6$edu<-ifelse(wvs6$V248>7, "high_edu", "no_high_edu")
# if V248 >7 (8,9) assign "high edu", if not - assign "no high edu"
table(as_factor(wvs6$V248), wvs6$edu) # check 

# V105 - generalized trust
attributes(wvs6$V105) # reverse coding
wvs6$gntr<-round(scales::rescale(as.numeric(wvs6$V105), to=c(1,0)),4)
cor(wvs6$gntr, wvs6$V105, use = "complete.obs")
table(as_factor(wvs6$gntr), wvs6$V105)

# V104 - particularized trust
attributes(wvs6$V104) # reverse coding
wvs6$parttr<-round(scales::rescale(as.numeric(wvs6$V104), to=c(1,0)),4)
cor(wvs6$gntr, wvs6$V105, use = "complete.obs")
table(as_factor(wvs6$parttr), wvs6$V104)

# V55 - locus of control 
attributes(wvs6$V55)
table(wvs6$V55)
wvs6$locus<-round(scales::rescale(as.numeric(wvs6$V55), to=c(0,1)),4)
cor(wvs6$locus, wvs6$V55, use = "complete.obs")

# V253 - city size 
attributes(wvs6$V253)
wvs6$city<-round(scales::rescale(as.numeric(wvs6$V253), to=c(0,1)),4)
cor(wvs6$city, wvs6$V253, use = "complete.obs")
wvs6$city_cat<-droplevels(as_factor(wvs6$V253))
table(wvs6$city, as_factor(wvs6$V253))

# V237 - savings 
attributes(wvs6$V237) # reverse coding
wvs6$save<-round(scales::rescale(as.numeric(wvs6$V237), to=c(1,0)),4)
cor(wvs6$save, wvs6$V237, use = "complete.obs")
table(as_factor(wvs6$save), wvs6$V237)

# V57 - marital status
# combine separated and divorced
# married and living together 1,2
attributes(wvs6$V57)
# with labels
# another way of assigning labels
wvs6$mar[wvs6$V57==1]<-"Married"
wvs6$mar[wvs6$V57==2]<-"Married"
wvs6$mar[wvs6$V57==3]<-"Divorced"
wvs6$mar[wvs6$V57==4]<-"Divorced"
wvs6$mar[wvs6$V57==5]<-"Widowed"
wvs6$mar[wvs6$V57==6]<-"Single"
class(wvs6$mar) # character, not numeric
table(wvs6$mar, droplevels(as_factor(wvs6$V57))) # ok


# Use Russia and check distributions first
wvs6_rus<-wvs6[wvs6$V2==643,]
dim(wvs6)
dim(wvs6_rus)
table(wvs6_rus$gnd, useNA = "ifany")
table(wvs6_rus$edu, useNA = "ifany")
table(wvs6_rus$mar, useNA = "ifany")
table(wvs6_rus$city, useNA = "ifany")
table(wvs6_rus$save, useNA = "ifany")
table(wvs6_rus$health, useNA = "ifany")
table(wvs6_rus$gntr, useNA = "ifany")
table(wvs6_rus$parttr, useNA = "ifany")
table(wvs6_rus$locus, useNA = "ifany")

# multiple regression
# let`s add our main control variable - health `

rcorr(as.matrix(na.omit(wvs6_rus[,c( "swb2", "health", "age")])))
library("ppcor")
pcor<-na.omit(wvs6_rus[,c("swb2", "age", "health")])
pcor.test(pcor$swb2, pcor$age, pcor$health, method = "spearman")

pcor2<-na.omit(wvs6_rus[,c("lsatr", "age", "health")])
pcor.test(pcor2$lsatr, pcor2$age, pcor2$health, method = "spearman") #for comparison from the previous class

m2<-lm(swb2~age+health, data = wvs6_rus)
m2
summary(m2)

m2ls<-lm(lsatr~age+health, data = wvs6_rus) # for comparison 
m2ls
summary(m2ls)

#install.packages("stargazer") #to create tables with results of different models (of 1 model also)
library(stargazer)
stargazer(m0, m1, m2, m2ls, type="text")
# STOP HERE
# no dummies and nominal variables yet
m3<-lm(swb2~age+health+save+locus+gntr+city, data = wvs6_rus)
summary(m3)
# thus, at 5% sig level  the equation of the model is:
# swb = 0.34 -0.0005342*age + 0.2715849*health + 0.0543463*savings + 0.2237450*locus +e
m4<-lm(swb2~age+health+save+locus+parttr+city, data = wvs6_rus)
summary(m4)

m5<-lm(swb2~age+health+save+locus+gntr+parttr+city, data = wvs6_rus)
summary(m5) 

stargazer(m1, m2, m3, m4, m5, type="text")

# export models 
# https://web.northeastern.edu/econpress/wp-content/uploads/2016/04/Stargazer.pdf
# https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf 
library("stargazer")
getwd()

setwd("/Users/Natasha/Downloads")
stargazer(m1, m2, m3, m4, m5, out ="/Users/Natasha/Downloads/sbw1.html",  type = "html")

# stargazer(m0,m1,m2,m3, out ="/Users/Natasha/Downloads/sbw2.html", type = "html", 
          # covariate.labels = c("write labels here, be careful"), 
          # dep.var.caption = c("Subjective well-being"), dep.var.labels.include = F,
          # ci = TRUE, digits =3)

# Dummy variables in regression 
# EDUCATION 
table(wvs6_rus$edu)

# Check swb in educational groups
tapply(wvs6_rus$swb2, wvs6_rus$edu, mean, na.rm=T)
tapply(wvs6_rus$swb2, wvs6_rus$edu, shapiro.test)

library(ggpubr)
ggqqplot(data=wvs6_rus[complete.cases(wvs6_rus$swb2),], 
                         x = "swb2", color = "edu", facet.by = "edu")

wilcox.test(wvs6_rus$swb2~wvs6_rus$edu)

mld<-lm(swb2~edu, data=wvs6_rus) 
summary(mld)
# Intercept  - mean swb high education
# Slope for no high edu - difference in means of swb between 
#people with high education and without high education

0.631214-0.047009 # swb for low education
# You can change reference category 

mld2<-lm(swb2~I(edu=="high_edu"), data=wvs6_rus) 
summary(mld2)
# Opposite situation 
# Intercept  - mean swb for low education
# slope - difference between high education and low education

# now we can add dummies and nominal variables 
summary(m5)
m6<-lm(swb2 ~ age + health + save + locus + gntr + parttr + 
       city+edu+gnd+mar, data = wvs6_rus)
summary(m6)

stargazer(m1, m5,m6, out ="/Users/Natasha/Downloads/sbw1.html",  type = "html")
stargazer(m1, m5,m6, out ="/Users/Natasha/Downloads/sbw1.html",  type = "text")

# Comparison of models 
# Nested models:
# Models when one model is a part of larger model
# m0 swb2~1; nested in m1-m6
# m1 swb2 ~ age; nested in m2-m6
# m2 swb2 ~ age + health; nested in m3-m6
# m3 swb2 ~ age + health + save + locus + gntr + city; nested in m5-m6
# m5 swb2 ~ age + health + save + locus + gntr + parttr+city; nested in m6
# m6 swb2 ~ age01 + health + save + locus + gntr + parttr + city + edu + gnd + mar

# Non-nested models 
# m4 swb2 ~ age + health + save + locus + parttr + city
# m3  swb2 ~ age + health + save + locus + gntr + city

# Problem!!!! Missing values
# Compare models with same valid sample size
# largest model with all variables 

wvs6_rus2<-na.omit(wvs6_rus[, c('swb2', 'age', 'health', 'gntr', 'parttr',
                                'locus', 'city', 'save', 
                                'edu', 'gnd', 'mar')])
dim(wvs6_rus)
dim(wvs6_rus2)
m1_m<-update(m1, data=wvs6_rus2)
summary(m1_m)
stargazer(m1, m1_m, type="text")

m3_m<-update(m3, data=wvs6_rus2)
summary(m3_m)

stargazer(m3, m3_m, type="text")

m4_m<-update(m4,data=wvs6_rus2)
summary(m4_m)

m5_m<-update(m5,data=wvs6_rus2)
summary(m5_m)

stargazer(m1_m, m4_m, m5_m, m6, type="text")
stargazer(m1_m, m4_m, m5_m, m6, out ="sbw2.html",  type = "text")


# Run m6 for Sweden 

m7<-lm(swb2 ~ age + health + save + locus + gntr + parttr + 
         city+edu+gnd+mar, data=wvs6[as_factor(wvs6$V2)=="Sweden",])
summary(m7)
# Also useful to check R2 for age in Russia and Sweden separately

m1r<-lm(swb2 ~ age, data=wvs6[as_factor(wvs6$V2)=="Russia",])
m1sw<-lm(swb2 ~ age, data=wvs6[as_factor(wvs6$V2)=="Sweden",])

stargazer(m1r,m1sw,m6,m7, type = "text")
stargazer(m1r,m1sw,m6,m7, out ="/Users/Natasha/Downloads/sbw3.html",  type = "html")


# Comparison of coefficients in the model
# Only possible when all indep. variables have the same scale range 
# Way 1. Standardization
# Transformation into the scale with mean -0, SD=1 
summary(m6)
# scale function
# Full std
m6_std_f<-lm(scale(swb2) ~ scale(age) + scale(health) + scale(save) + scale(locus)+
           scale(gntr) + scale(parttr) + scale(city)+edu+gnd+mar, data = wvs6_rus)
# standardization of dummies is not recommended
summary(m6_std_f)

#Only independent vars 
# swb - no scale function

m6_std_x<-lm(swb2 ~ scale(age) + scale(health) + scale(save) + scale(locus)+
             scale(gntr) + scale(parttr) + scale(city)+edu+gnd+mar, data = wvs6_rus)
summary(m6_std_x)

all_coef<-cbind(coef(m6),coef(m6_std_f), coef(m6_std_x))
colnames(all_coef)<-c("Unstd", "Full_std_xy", "X_std")
all_coef<-apply(all_coef,2, function(x) round(x,3))
all_coef

library(lm.beta)
summary.lm.beta(lm.beta(m6))

# WAY2 - recode all variables into the same range 
apply(wvs6_rus[, c("swb2", "age", "health", "save", "locus", "gntr", "parttr", "city")],2, summary)
wvs6_rus$age01<-scales::rescale(as.numeric(wvs6_rus$V242), to=c(0,1))
cor(wvs6_rus$age01, wvs6_rus$V242, use="pairwise.complete.obs")
plot(wvs6_rus$age01, wvs6_rus$V242)
summary(wvs6_rus$age01)
m6_res<-lm(swb2 ~ age01 + health + save + locus + gntr + parttr + 
         city+edu+gnd+mar, data = wvs6_rus)
summary(m6_res)
# Now in the model we see the maxim possible impact of each variable 
# While comparing coefficients take into account confidence intervals
# This is a first step
# To draw conclusions of the equality of the coefficients you need more strict tests

linearHypothesis(m6_res, "health - locus = 0") # not equal
linearHypothesis(m6_res, "save - parttr = 0") # equal 

# or the same 

linearHypothesis(m6_res, "health=locus") # not equal
linearHypothesis(m6_res, "save=parttr") # equal 

# Interactions 

table(wvs6$age)

wvs6_rusw<-wvs6[as_factor(wvs6$V2)=="Russia"| as_factor(wvs6$V2)=="Sweden",]
dim(wvs6_rusw)
wvs6_rusw$cntr<-droplevels(as_factor(wvs6_rusw$V2))
table(wvs6_rusw$age, wvs6_rusw$cntr)
wvs6_rusw$age01<-scales::rescale(as.numeric(wvs6_rusw$age), to=c(0,1))
cor(wvs6_rusw$age01, wvs6_rusw$V242)
table(wvs6_rusw$age01, wvs6_rusw$cntr)

# We will be using variables recoded from 0 to 1 including age 

# start with polled data for two countries
tapply(wvs6_rusw$swb2, wvs6_rusw$cntr, mean, na.rm=T)

m0rs<-lm(swb2 ~ cntr, data=wvs6_rusw)
summary(m0rs) # means of swb across countries 

m0rsb<-lm(swb2 ~ I(cntr=="Russia"), data=wvs6_rusw)
summary(m0rsb)# means of swb across countries

stargazer(m0rs,m0rsb, type="text")

m1rs<-lm(swb2 ~ age01, data=wvs6_rusw)
summary(m1rs)
m2rs<-lm(swb2 ~ age01+cntr, data=wvs6_rusw)
summary(m2rs) # this model shows average effect for two countries

m3rs<-lm(swb2 ~ age01*cntr, data=wvs6_rusw)
summary(m3rs) # coefficient for age = moderator = 0 (Russia)
plot_model(m3rs, type="int")
# Why do we see only one coefficient for age while at the graph we see two lines? 
# Age coefficient  = coefficient when moderator=0
# Russia=0 since it's a base category
# Want to see age partial slope for Sweden?
# Change reference category

m3rsb<-lm(swb2 ~ age01*I(cntr=="Russia"), data=wvs6_rusw)
summary(m3rsb) # coefficient for age when moderator = 0 (Sweden)
stargazer(m3rs, m3rsb, type="text")

# Let`s add control variables to see if the effect of age is robust`

mfin_rs<-lm(swb2 ~ age01*cntr + health + save + locus + gntr + parttr + 
               city+edu+gnd+mar, data=wvs6_rusw)
summary(mfin_rs) # country specific affect for age, average effect for all other variables
plot_model(mfin_rs, type="int") #type = "int" - Marginal effects of interaction terms in model.

# Same model with unrecoded scale of age 
mfin_rs0<-lm(swb2 ~ age*cntr + health + save + locus + gntr + parttr + 
                city+edu+gnd+mar, data=wvs6_rusw)
summary(mfin_rs0)
plot_model(mfin_rs0, type="int")

# Want to see age partial slope for Sweden?
# Change base category
mfin_rsb<-lm(swb2 ~ age01*relevel(cntr, ref="Sweden")+ health + save + locus + gntr + parttr + 
                city+edu+gnd+mar, data=wvs6_rusw)
summary(mfin_rsb)

# Important note! 
# Country eff - also the coef when moderator is 0

# according to the graph the difference in the sbw across 
# Sweden and Russia exists only after appr. 35 years
# 0 age for recoded scale is 18
# when we reverse the scale 1-age01
# we get 0 for the oldest group
# now country coef is significant 


# What to do with insignificant variables 
# Exclude or not to exclude?
# YES, exclude!
# Simple models are better (fewer variables)
# Less degrees of freedom 
# less number of missing values 

# Insignificant coefficients provide important information 
# They show that this concept has nothing to do with dependent variable
# insignificant result may be interesting for other scholars. 
# You can leave your main explanatory variable
# After you exclude some insignificant variables effects may change

# You may exclude minor control variables which are less relevant to your study 
# and rarely used as control variables in other studies of the topic

summary(mfin_rs)

mfin_rs_exl<-lm(formula = swb2 ~ age01 * cntr + health + save + locus + 
                   parttr + gnd + mar, data = wvs6_rusw)

summary(mfin_rs_exl)
plot_model(mfin_rs_exl, type="int")

stargazer(mfin_rs, mfin_rs_exl, type="text")

# Regression assumptions and regression diagnostic

# Linear relationship
# No or little multicollinearity (high correlation between IV)
# Normal distribution of errors (residuals)
# Homoscedasticity (constant variance of errors (residuals)) 
# No auto-correlation
# Independence of observations from each other (not clustered data)
# No influential outliers 
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
# https://cran.r-project.org/web/packages/olsrr 
# https://rforpoliticalscience.com/2020/10/29/check-linear-regression-assumptions-with-gvlma-package-in-r/ 

summary(mfin_rs_exl)
wvs6_rusw$pred_final<-predict(mfin_rs_exl) # predicted values
wvs6_rusw$res_final<-resid(mfin_rs_exl) # residuals
# problems with missing values 
# get rid of missing values

wvs6_rusw_v<-na.omit(wvs6_rusw[,c("swb2", "age01", "cntr", "health", "save", "locus",
                                  "parttr", "gnd", "mar")])

wvs6_rusw_v$pred_final<-predict(mfin_rs_exl) # predicted values
wvs6_rusw_v$res_final<-resid(mfin_rs_exl) # residuals

mfin_rs_exlv<-update(mfin_rs_exl, data = wvs6_rusw_v)
summary(mfin_rs_exlv)
stargazer(mfin_rs_exl,mfin_rs_exlv, type="text")

# For checking regression assumptions we will need:
# Predicted values
# Residuals (error, diff between observed and predicted)
wvs6_rusw_v$pred_fin<-predict(mfin_rs_exlv)
wvs6_rusw_v$res_fin<-resid(mfin_rs_exlv)
hist(wvs6_rusw_v$pred_fin)
hist(wvs6_rusw_v$res_fin)

# Normality 
library(ggpubr)
ggqqplot(data=wvs6_rusw_v, x = "res_fin")
library(olsrr)
ols_plot_resid_qq(mfin_rs_exlv)
ols_plot_resid_hist(mfin_rs_exlv)
shapiro.test(wvs6_rusw_v$res_fin)

# Heteroskedasticity
ols_plot_resid_fit(mfin_rs_exlv) 
ols_test_breusch_pagan(mfin_rs_exlv)
ols_test_breusch_pagan(mfin_rs_exlv, rhs = TRUE, multiple = TRUE, p.adj = 'bonferroni')
lmtest::bptest(mfin_rs_exlv)
#Null Hypothesis: Equal/constant variances

# Solution  - Robust standard errors
# Add other important predictors
# Add interactions 

# Multicollinearity
# High correlation between predictors 
ols_vif_tol(mfin_rs_exlv)
#VIF - variance inflation factor 
#High values of VIF demonstrate multicollinearity (more than 5 is tricky, more than 10 is extreme)

# Solutions 
# add highly correlated predictor separately
# If this is theoretically possible combine them into one additive index 


# Omitted variables bias 
# What if you omit some important explanatory variables? 
# Important explanatory variables:
# Influence the share of explained variance
# May change the magnitude, sign and significance of other explanatory variables 
# Supression effects in regression (see links at the beginning of the script)
# False correlations

# Variance of omitted explanatory variables is stored in the error term 
# Error term should not correlate with regressors 
# error term - residuals
# check correlations between residuals and explanatory variables 

library(Hmisc)
rcorr(as.matrix(wvs6_rusw_v[,c("res_final", "age01", "health", "save", "locus",
                               "parttr")]))


#CA. 
# 1) Select German subsample
# 2) Create an index of generalized trust (strangers V105, another nationality V107, another religion V106). 
# 3) Discuss descriptive statistics for age V242, income V239, religion importance V9, state of health V11, sex V240
# 4) Check association between age and income. Choose and conduct correct test, draw conclusions.
# 5) Is the average age different among males and females? Choose and conduct correct test, draw conclusions
# 6) Check association between religion importance and income. Choose and conduct correct test, draw conclusions
# 7) Check association between age and importance of religion. Choose and conduct correct test, draw conclusions




