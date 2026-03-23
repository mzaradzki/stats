# HA 9 correct answers 


library("haven")
wvs5<-read_sav("/Users/Natasha/Desktop/TEACHING/2025_QDAR/December EXAM/WV5_Data_Spss_v20180912.sav")
wvs5_rus<-wvs5[wvs5$V2==643, ]
table(droplevels(as_factor(wvs5_rus$V2)))

#1.	Calculate the descriptive statistics for the variables regarding the scale types.
## V187 (Religious Person),
attributes(wvs5_rus$V187)
#Nominal scale, shares are representative descriptive statistics in this case
addmargins(table(as_factor(wvs5_rus$V187), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V187), useNA="ifany")))*100,2)

## V186 (Attend), 
attributes(wvs5_rus$V186)
# Quasi-ordinal scale, since distance between points is not the same
# Reversed
# summary is rather useless as there are not so many scale points
addmargins(table(as_factor(wvs5_rus$V186), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V186), useNA="ifany")))*100,2)

## V192 (God important), 
attributes(wvs5_rus$V192)
# Ordinal
summary(wvs5_rus$V192)
addmargins(table(as_factor(wvs5_rus$V192), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V192), useNA="ifany")))*100,2)

## V253 (Income), 
attributes(wvs5_rus$V253)
# Ordinal
summary(wvs5_rus$V253)
addmargins(table(as_factor(wvs5_rus$V253), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V253), useNA="ifany")))*100,2)

## V237 (Age), 
attributes(wvs5_rus$V237)
# Metric
summary(wvs5_rus$V237)

## V238 (Education), 
attributes(wvs5_rus$V238)
# Ordinal scale
# summary is rather useless
addmargins(table(as_factor(wvs5_rus$V238), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V238), useNA="ifany")))*100,2)

## V235 (Sex), 
attributes(wvs5_rus$V235)
# Nominal, binary
# Summary is not meaningful
addmargins(table(as_factor(wvs5_rus$V235), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V235), useNA="ifany")))*100,2)

## Institutional confidence (V136-V140).
##    V136 - The police
attributes(wvs5_rus$V136)
# Ordinal
# Reversed
summary(wvs5_rus$V136)
addmargins(table(as_factor(wvs5_rus$V136), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V136), useNA="ifany")))*100,2)

##    V137 - The courts
attributes(wvs5_rus$V137)
# Ordinal
# Reversed
summary(wvs5_rus$V137)
addmargins(table(as_factor(wvs5_rus$V137), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V137), useNA="ifany")))*100,2)

##    V138 - The government
attributes(wvs5_rus$V138)
# Ordinal
# Reversed
summary(wvs5_rus$V138)
addmargins(table(as_factor(wvs5_rus$V138), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V138), useNA="ifany")))*100,2)

##    V139 - Political parties
attributes(wvs5_rus$V139)
# Ordinal
# Reversed
summary(wvs5_rus$V139)
addmargins(table(as_factor(wvs5_rus$V139), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V139), useNA="ifany")))*100,2)

##    V140 - Parliament
attributes(wvs5_rus$V140)
# Ordinal
# Reversed
summary(wvs5_rus$V140)
addmargins(table(as_factor(wvs5_rus$V140), useNA="ifany"))
round(addmargins(prop.table(table(as_factor(wvs5_rus$V140), useNA="ifany")))*100,2)

#2.	Recode variables with reverse coding. 
#Maximum value should be equal to maximum numeric code, minimum – to minimum. 
#Get rid of small groups if it is necessary.
library("scales")
# the later task is to create an index, so I will rescale the indicators from 0 to 1 also

# V186 (Attend)
wvs5_rus$att<-round(scales::rescale(as.numeric(wvs5_rus$V186), to=c(1,0)),4)
table(wvs5_rus$att, useNA="ifany")
table(as_factor(wvs5_rus$V186), wvs5_rus$att, useNA="ifany")

#    V136 - The police
wvs5_rus$pol<-round(scales::rescale(as.numeric(wvs5_rus$V136), to=c(1,0)),4)
table(wvs5_rus$pol, useNA="ifany")
table(as_factor(wvs5_rus$V136), wvs5_rus$pol, useNA="ifany")

#    V137 - The courts
wvs5_rus$cou<-round(scales::rescale(as.numeric(wvs5_rus$V137), to=c(1,0)),4)
table(wvs5_rus$cou, useNA="ifany")
table(as_factor(wvs5_rus$V137), wvs5_rus$cou, useNA="ifany")

#    V138 - The government
wvs5_rus$gov<-round(scales::rescale(as.numeric(wvs5_rus$V138), to=c(1,0)),4)
table(wvs5_rus$gov, useNA="ifany")
table(as_factor(wvs5_rus$V138), wvs5_rus$gov, useNA="ifany")

#    V139 - Political parties
wvs5_rus$par<-round(scales::rescale(as.numeric(wvs5_rus$V139), to=c(1,0)),4)
table(wvs5_rus$par, useNA="ifany")
table(as_factor(wvs5_rus$V139), wvs5_rus$par, useNA="ifany")

#    V140 - Parliament
wvs5_rus$parl<-round(scales::rescale(as.numeric(wvs5_rus$V140), to=c(1,0)),4)
table(wvs5_rus$parl, useNA="ifany")
table(as_factor(wvs5_rus$V140), wvs5_rus$parl, useNA="ifany")

##### Recode other variables just for convenience

# V187 (Religious Person),
attributes(wvs5_rus$V187)
table(wvs5_rus$V187, useNA="ifany")
wvs5_rus$relg[wvs5_rus$V187==1]<-"A religious person"
wvs5_rus$relg[wvs5_rus$V187==2]<-"Not a religious person"
wvs5_rus$relg[wvs5_rus$V187==3]<-"A convinced atheist"
wvs5_rus$relg[is.nan(wvs5_rus$V187)]<-NA
class(wvs5_rus$relg)
table(wvs5_rus$relg, useNA="ifany")
table(as_factor(wvs5_rus$V187), wvs5_rus$relg, useNA="ifany")

# V192 (God important), 
wvs5_rus$god<-round(scales::rescale(as.numeric(wvs5_rus$V192), to=c(0,1)),4)
table(wvs5_rus$god, useNA="ifany")
table(as_factor(wvs5_rus$V192), wvs5_rus$god, useNA="ifany")

# V253 (Income), 
wvs5_rus$inc<-round(scales::rescale(as.numeric(wvs5_rus$V253), to=c(0,1)),4)
table(wvs5_rus$inc, useNA="ifany")
table(as_factor(wvs5_rus$V253), wvs5_rus$inc, useNA="ifany")

# V237 (Age), 
wvs5_rus$age<-round(scales::rescale(as.numeric(wvs5_rus$V237), to=c(0,1)),4)
cor(wvs5_rus$V237, wvs5_rus$age)

# V238 (Education), 
wvs5_rus$edu<-round(scales::rescale(as.numeric(wvs5_rus$V238), to=c(0,1)),4)
table(wvs5_rus$edu, useNA="ifany")
table(as_factor(wvs5_rus$V238), wvs5_rus$edu, useNA="ifany")

# V235 (Sex), 
wvs5_rus$sex <- as.numeric(wvs5_rus$V235)-1
wvs5_rus$sex <-labelled(wvs5_rus$sex, c("Male" = 0, "Female" = 1))
table(wvs5_rus$sex, useNA="ifany")
table(as_factor(wvs5_rus$V235), as_factor(wvs5_rus$sex), useNA="ifany")


#3. Calculate index of institutional confidence

wvs5_rus$ins<-round(rowMeans(wvs5_rus[c("pol","cou", "gov", "par", "parl")], na.rm=T),4)
wvs5_rus$ins[is.nan(wvs5_rus$ins)]<-NA
#View(wvs5_rus[,c("pol","cou", "gov", "par", "parl", "ins")])
summary(wvs5_rus$ins)
hist(wvs5_rus$ins)
plot(wvs5_rus$ins, wvs5_rus$pol)
plot(wvs5_rus$ins, wvs5_rus$cou)
plot(wvs5_rus$ins, wvs5_rus$gov)
plot(wvs5_rus$ins, wvs5_rus$par)
plot(wvs5_rus$ins, wvs5_rus$parl)

#4.	Is institutional confidence is associated with religiosity (V187)? 
#Choose correct test, justify your choice, interpret results in terms of 
#significance and direction of the relations.

# Institutional confidence
# Checking normality of distribution
hist(wvs5_rus$ins)
# Looks not normal with histogram
library(ggpubr)
ggqqplot(data=wvs5_rus, x = "ins")
# Looks not normal with QQ-plot
options(scipen=999, digits = 8)
shapiro.test(wvs5_rus$ins)
# W = 0.96901, p-value < 0.000000000000000222
# With 99% confidence level, distribution of institutional trust is not normally distributed

# Religiosity is a nominal variable
# Test for equality of groups is necessary

# Checking normality of distributions of institutional trust for all religiosity groups
addmargins(table(wvs5_rus$relg, useNA="ifany"))
hist(wvs5_rus$ins[wvs5_rus$relg=="A convinced atheist"])
# Looks not normal
hist(wvs5_rus$ins[wvs5_rus$relg=="A religious person"])
# Looks not normal
hist(wvs5_rus$ins[wvs5_rus$relg=="Not a religious person"])
# Looks not normal
ggqqplot(data=wvs5_rus[complete.cases(wvs5_rus$ins),], x = "ins", color = as_factor("relg"), facet.by = as_factor("relg"))
# Every group seems to be not normally distributed
tapply(wvs5_rus$ins, wvs5_rus$relg, shapiro.test)
# All p-values are below 0.01 (1% significance level)
# p-value for atheists is 0.00038062, other p-values are much <0.001
# Distribution of institutional trust is not normal for each religious group with 99% probability

# Distributions are not normal, 3 groups - Kruskal-Wallis test fits better
kruskal.test(wvs5_rus$ins~wvs5_rus$relg)
# chi-squared = 19.3949, df = 2, p-value = 0.000061439
# p-value is below 0.05 - difference is significant on the 95% confidence level (99% as well)
# At least one religiosity group has statistically significantly different institutional trust level in Russia

# Post hoc comparison is necessary
pairwise.wilcox.test(wvs5_rus$ins, wvs5_rus$relg, method="holm")
# The p-value for difference between religious and atheist groups is 0.000481, 
# for other pairs p-value is larger than 0.01 sig level (p-value=0.015939 each)
# Post hoc test tells that difference in institutional confidence level
# between groups religious and atheist groups is significant at 1% significance level 
#(with 99% probability)

#5.	Is gender associated with importance of God? 
#Choose correct test, justify your choice, interpret results in terms of significance. 
# Test for comparing two independent groups is needed

# Checking normality of distribution of importance of God for two sexes
hist(wvs5_rus$god[wvs5_rus$sex==0])
# Looks not normal
hist(wvs5_rus$god[wvs5_rus$sex==1])
# Looks not normal
ggqqplot(data=wvs5_rus[complete.cases(wvs5_rus$god),], x = "god", color = as_factor("sex"), facet.by = as_factor("sex"))
# Both do not look normal
tapply(wvs5_rus$god, as_factor(wvs5_rus$sex), shapiro.test)
# All p-values are very close to 0
# Distribution of importance of God is not normal across each sex group with 99% probability

# Distributions for both groups are not normal, samples are independent - Mann-Whitney U-test is necessary
tapply(wvs5_rus$god, as_factor(wvs5_rus$sex), summary)
# Median importance of God for males is 0.44, mean importance is 0.46
# Median importance of God for females is 0.67, mean importance is 0.63
# I can only assume that probably the indicators are different
wilcox.test(wvs5_rus$god~wvs5_rus$sex)
# W = 297776, p-value < 0.000000000000000222
# p-value is close to 0, difference is significant at 1% significance level
# Russian females on average have statistically significantly higher level of God importance 
#than Russian males with 99% probability
# So, there is a statistically significant association between gender and importance of God

#6.	Is gender associated with church attendance? 
#Choose correct test, justify your choice, interpret results.

# Test for comparing two independent groups is needed

# Checking normality of distribution of church attendance for two sexes
hist(wvs5_rus$att[wvs5_rus$sex==0])
# Looks not normal
hist(wvs5_rus$att[wvs5_rus$sex==1])
# Looks not normal
ggqqplot(data=wvs5_rus[complete.cases(wvs5_rus$att),], x = "att", color = as_factor("sex"), facet.by = as_factor("sex"))
# Both do not look normal
tapply(wvs5_rus$att, as_factor(wvs5_rus$sex), shapiro.test)
# All p-values are very close to 0
# Distribution of church attendance is not normal for each sex with 99% probability

# Distributions for both groups are not normal, they are independent - Mann-Whitney U-test is necessary
tapply(wvs5_rus$att, as_factor(wvs5_rus$sex), summary)
# Median church attendance for males is 0.17, mean attendance is 0.2
# Median church attendance for females is 0.33, mean attendance is 0.34
# Considering that 0 is lowest possible attendance among the sample and 1 is the highest possible among the sample
# Probably different, statistical test is needed to justify it
wilcox.test(wvs5_rus$att~wvs5_rus$sex)
# W = 328128, p-value < 0.000000000000000222
# p-value is close to 0, difference is significant difference at 1% significance level
# Females on average have statistically significantly higher church attendance in Russia with 99% probability

# There is a statistically significant association between gender and church attendance

