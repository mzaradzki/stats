#####A. Almakaeva, N. Mikhailova#####
######## class 14 ######
######### ANOVA ########
#class 15.2 starts wiyj 244 line

# to conduct Anova manually we need:
#1. Total sum of squares (SST), SST=sum(x^2)-n*mean(x)^2
#2. Between group sum of squares (SSB), SSB=sum(nk*(mean(xk)-mean(x))^2)
#3. Within group sum of squares (SSW), SSW=SST-SSB
#4. DFB=k-1
#5. DFW=n-k, n -sample size, k-numer of groups
#6. F=mean square between/mean square within, F=(SSB/DFB)/(SSW/DFW)

library("haven")
wvs6<-read_spss("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav")
dim(wvs6)
class(wvs6$V242) #age
summary(wvs6$V242)
table(wvs6$V242)
attributes(wvs6$V57) #marital status
wvs6_rus<-wvs6[wvs6$V2==643, ]

#just to remember that you can
# save your subset as a new file if it's needed
write.csv2(wvs6_rus, "/Users/Natasha/Downloads/WVS/WV6_rus.csv", sep=";") #in csv format
write_sav(wvs6_rus, "/Users/Natasha/Downloads/WVS/WV6_rus.sav") #in sav (spss) format
dim(wvs6_rus)

# let's look at the sample data
# marutal status (V57) and age (V242)
library("Hmisc")
attach(wvs6_rus) # allows not to use $, use for description, not for recording!!!
attributes(V57)

aggregate(V242, by=list(as_factor(V57)), FUN= mean, na.rm=TRUE)
format(aggregate(V242, by=list(as_factor(V57)), FUN=mean, na.rm=TRUE), 
       width=10, digits=5) # format()  to make it visually nicer
wvs6_rus$age<-wvs6_rus$V242 # for convenience
wvs6_rus$mar<-droplevels(as_factor(wvs6_rus$V57)) # for convenience
library(knitr)
kable(aggregate(V242, by=list(as_factor(V57)), FUN=mean, na.rm=TRUE), col.names=c("marital", "mean age"))
detach(wvs6_rus); attach(wvs6_rus) # reattach after recoding
cor(age, V242) # correlation, if 1, then variables are identical
table(V57, mar)

library("Rmisc")
# data for ggplot visualization
mar_age<-summarySE(data=wvs6_rus, measurevar = c("age"), 
                     groupvars = c("mar"), na.rm = T); mar_age
mar_age<-mar_age[-7,] # exclude NA
mar_age

library("ggplot2")
gr_mar_age<-ggplot(mar_age,aes(x=as_factor(mar),y=age, color=as_factor(mar)))+
  geom_point(size=4)+scale_y_continuous("mean age")+ 
  geom_errorbar(aes(ymin=age-1.96*se, ymax=age+1.96*se),width=.3)+
  theme(legend.text=element_text(size=14), legend.position="bottom")+theme(legend.title=element_blank())+
  ggtitle("Mean Age and Marriage Status")+theme(plot.title = element_text(hjust = 0.5))+
    geom_text(aes(label=round(age,2)),hjust=0.5, vjust=-0.5, size=4.5, color="black", check_overlap = TRUE)
gr_mar_age  


#install.packages("gplots")
library(gplots)

plotmeans(wvs6_rus$age~wvs6_rus$mar,xlab="Marriage",
          ylab="Age", main="Age/95% CI", col = "red", lwd = 3) #lwd -Width of connecting lines
#but this type of plot is not relevant in this case!!!
#we see just it's creating here

# ANOVA

model1<- aov(age~as_factor(mar), data=wvs6_rus)
summary(model1)
summary(aov(age~as_factor(mar), data=wvs6_rus))
# Important!!! don`t forget to specify as_factor() for correct calculation`
#Unless DF will be incorrect

# Calculate df manually to check if you have created a correct model
# Degrees of freedom 
levels(droplevels(as_factor(wvs6_rus$mar))) # 6 categories 
length(levels(droplevels(as_factor(wvs6_rus$mar)))) # 6 categories 
addmargins(table(age, mar))
sum(table(age, mar)) # valid observations for mutual dist of age and religiosity
length(levels(droplevels(as_factor(wvs6_rus$mar))))

# DFB 6-1=5
# DFW  = valid cases sum(table(age, mar))  - 6 (number of marital categories), 2485-6=2479

summary(model1)
# MSW=SSW/DFW
496730 /2479
# MSB=SSB/DFB
255911/5
# F value MSB/MSW
51182.2/200.3752

# or 
F_obs<-summary(model1)[[1]][1,3]/summary(model1)[[1]][2,3] # observed F value
F_obs
summary(model1)[[1]][1,3]
summary(model1)[[1]][2,3]

# critical value of F for 95% 
qf(0.95,6-1, 2485-6)
# probability of a mistake (alpha)
1-pf(255.4, 6-1, 2485-6)

# another way for conducting anova
summary(model1)

model1_a<- oneway.test(age~mar, data=wvs6_rus, var.equal=FALSE) # unequal variances by default 
model1_a # DFW are calculated with corrections

model1_b<- oneway.test(age~mar, data=wvs6_rus, var.equal=TRUE) 
model1_b 

#####
#equality of variances test
attach(wvs6_rus) # let not to use $, don`t use for recoding, reattach data after recoding`

tapply(age, mar, var, na.rm=T)

# Levene`s test (H0: equal variances)
library("car")
leveneTest(age, mar, center = mean)# by default center is median
leveneTest(age,V240, center = mean)# by default center is median, for two groups

# Fligner-Killeen test (H0: equal variances)
fligner.test(age, mar, data=wvs6_rus)

#Bartlett`s Test 

#install.packages("stats") 
library("stats") 
bartlett.test(age, mar)

# for two groups (independent samples t-test), you can use F test
# V240  - gender

options(scipen=999)
table(as_factor(V240), useNA = "ifany")
var.test(wvs6_rus$age~wvs6_rus$V240)
t.test(wvs6_rus$age~wvs6_rus$V240, var.equal=TRUE)
t.test(wvs6_rus$age~wvs6_rus$V240, var.equal=FALSE)
vars<-tapply(wvs6_rus$age, wvs6_rus$V240, var, na.rm=T); vars
vars[1]/vars[2]

#Post-hoc tests
options(digits=7)
summary(model1) # doesn`t take into account unequal variances `
TukeyHSD(model1)
plot(TukeyHSD(model1), las=1)

pairwise.t.test(wvs6_rus$age, wvs6_rus$mar,p.adj= "none", var.equal=TRUE)
pairwise.t.test(wvs6_rus$age, wvs6_rus$mar,p.adj= "bonferroni", var.equal=FALSE) 
#see help (p.adjust) to learn about the corrections
pairwise.t.test(wvs6_rus$age, wvs6_rus$mar,p.adj= "hochberg", var.equal=FALSE)

#install.packages("DescTools")
library("DescTools")
DunnettTest(wvs6_rus$age ~ wvs6_rus$mar) #for equal var, equal sample size
table(as_factor(mar))
DunnettTest(wvs6_rus$age~relevel(wvs6_rus$mar, ref="Single")) #ref - reference group
# relevel(wvs6_rus$mar, ref="Single")
#install.packages("rstatix")
library("rstatix")

games_howell_test(data=wvs6_rus, age~mar, conf.level = 0.95) #for unequal variances, unequal sample size

# library("PMCMRplus") very good package 
# https://rdrr.io/cran/PMCMRplus/ 
#library(PMCMR) # old version
#gamesHowellTest(wvs6_rus$age, wvs6_rus$mar) #for unequal var, unequal sample size

# Testing normality of distribution 
# Graphs 
library(ggplot2)
wvs6_rus$mar<-as_factor(wvs6_rus$mar)
class(wvs6_rus$mar) 

age_gr_hist_gr<-ggplot(data=wvs6_rus[complete.cases(wvs6_rus$mar),], aes(x=age))+geom_histogram(binwidth=2)+
  facet_wrap(~mar)
age_gr_hist_gr

age_gr_hist_qq<-ggplot(data=wvs6_rus[complete.cases(wvs6_rus$mar),], aes(sample=age))+
  stat_qq(aes(color = mar))+ stat_qq_line(size=0.9)+facet_wrap(~mar)
age_gr_hist_qq

# QQ plot checks the heretical normal distribution and existing sample. 
# If they fit each other perfectly the graph looks like a straight line 
# https://rdrr.io/cran/qqplotr/ 
# https://cran.r-project.org/web/packages/qqplotr/qqplotr.pdf

# two groups have distribution close to normal
shapiro.test(wvs6_rus$age)

# or ("sw" for the Shapiro-Wilk test)
sw_std<-tapply(wvs6_rus$age, wvs6_rus$mar, function(x) normalTest(scale(x), method = "sw", na.rm = T))
sw_unst<-tapply(wvs6_rus$age, wvs6_rus$mar, function(x) normalTest(x, method = "sw", na.rm = T))

sw_std$Married
sw_unst$Married

sw_std$Divorced
sw_unst$Divorced

sw_std$Separated
sw_unst$Separated


#Shapiro-Wilk's method is widely recommended for normality test and it provides better power than K-S. 
#It is based on the correlation between the data and the corresponding normal scores.
#http://www.sthda.com/english/wiki/normality-test-in-r 
# other normality tests https://rdrr.io/cran/fBasics/man/test-normalityTests.html 

### CA. ###
# Make ANOVA for age among different countries (Sweden, Russia, France, Germany)

ca <- wvs6[as_factor(wvs6$V2)=="Russia" | as_factor(wvs6$V2)=="Sweden" | as_factor(wvs6$V2)=="Germany",]
dim(ca)

fit1 <- aov(V242 ~as_factor(V2), data=ca)
summary(fit1)
# interpret the results
# p-value = 0.00000000132 < 0.01 (1% sig level) => reject H0 of equality of means among the groups
# At least 1 pair of groups has a significant difference in means at any sig level (p-val<0.000).

# Check the normality and equality of variances of the distributions. What are the results?
hist(ca$V242)
shapiro.test(wvs6_rus$V242)
#the distribution of age is not normal (Shapiro-Will p-value<0.000)
LeveneTest(ca$V242, as_factor(ca$V2))
# the variances are not equal at any sig level (Levene's p-value<0.000)
# that's why post-hoc for unequal VARs and unequal Ns

# In what countries does the average age differ?
games_howell_test(data=ca, V242~V2, conf.level = 0.95)
# there is a difference in average age in Russia and Germany (Game-Howell test p-val<0.000)
#there is a difference in average age in Sweden and Germany (Game-Howell test p-val<0.005)
# there is a no significant difference in average age in Sweden and Russia
#(Game-Howell test p-val<0.122) at 1% sig level

########### CLASS 15.2

# If samples have non-normal distribution use non-parametric tests. 

# Non-parametric tests use idea of ranks 
# rank  - is a place of a unit (respondent, country etc) after sorting
# The lowest values - 1st rank (place), highest  rank - last (place) 

# example 1, 10 respondents 

e1_ranks<-sample(wvs6_rus$age, 10) # random sample of 10 respondents from Russian sample of the wvs
e1_ranks
sort(e1_ranks)
e1_ranks<-data.frame(id=1:10, age=sort(e1_ranks), rank=1:10, row.names = 1)
e1_ranks # youngest respondent - 1st place, oldest - last 10th place

# example 2, 20 respondents 
set.seed(1010)# select same respondents each time 
e2_ranks<-sample(wvs6_rus$age, 20, replace = TRUE) 
# random sample of 20 respondents from Russian sample of the wvs
# with replacement, same respondent can be selected several times 

sort(e2_ranks) # we see cases with similar age
# we face the situation of tied ranks 
# tied ranks - equal ranks
# by default  - mean of ranks

e2_ranks<-data.frame(id=1:20, age=sort(e2_ranks), rank=1:20, row.names = 1)
e2_ranks

# we need to assign the correct rank, taking into account equal cases
e2_ranks$ranks_tied<-rank(e2_ranks$age)
e2_ranks

# min - takes first smallest rank
# max - takes last highest rank

# Age variable, the whole Russian sample 
t(t(table(wvs6_rus$age)))
# 18 years - 40 respondents
# ranks from 1 to 40 (1+39)
# tied rank - 20.5 
sum(1:40)/40

# 19 years - 35 respondents
# ranks from 41 to 41+34=75 
# tied rank = 58
sum(41:75)/35

wvs6_rus$age_rank<-rank(wvs6_rus$age)
table(wvs6_rus$age_rank)

# Non-parametric methods 

# two independent samples 
# Wilcoxon’s rank-sum test/Mann–Whitney test
# Stages: 
# 1. Sort the whole sample from lowest (1st rank) to highest (last rank)
# 2. If there is no difference across groups 
# the number of highest and lowest ranks will be almost the same
# and total sum of ranks will also be almost the same 
# 3. correct ranks by group size by calculation mean rank for each group
# 4. Subtract the mean rank from sum of ranks for each group
# Select the difference for the smallest group as W observed
# Compare observed W with critical W
# This is logic of Wilcoxon test

# However in wilcox.test you see Mann-Whitney U Test results
# https://www.statisticshowto.com/mann-whitney-u-test/ 
# 1. Name the sample with the smaller ranks sample1 
# and the sample with the larger ranks sample2. 
# 2. Take the first observation in sample 1. 
# Count how many observations in sample 2 are smaller than it. 
# If the observations are equal, count it as one half. 
# 3. Repeat Step 2 for all observations in sample 1.
# 4. Add up all of your totals from Steps 2 and 3. This is the U statistic.

# Start with regular t-test for independent samples to compare results. 

table(as_factor(wvs6_rus$V240), useNA = "ifany") 
wvs6_rus$gnd<-droplevels(as_factor(wvs6_rus$V240))
table(wvs6_rus$V240, wvs6_rus$gnd)
library(ggpubr)
ggplot(data=wvs6_rus[complete.cases(wvs6_rus$gnd),], aes(x=age))+geom_histogram(binwidth=2)+
  facet_wrap(~gnd)
age_gnd_ggplot<-ggqqplot(data=wvs6_rus[complete.cases(wvs6_rus$gnd),], 
                         x = "age", color = "gnd", facet.by = "gnd")
age_gnd_ggplot

t.test(wvs6_rus$V242~droplevels(as_factor(wvs6_rus$V240)),var.equal=T) # regular t-test
var.test(wvs6_rus$V242~droplevels(as_factor(wvs6_rus$V240)))

tapply(wvs6_rus$age, as_factor(wvs6_rus$V240), shapiro.test)
wvs6_rus$gnd<-as_factor(wvs6_rus$V240) #for convenience
table(wvs6_rus$gnd,wvs6_rus$V240)


# Mann-Whitney U Test for independent samples
# wilcox.test(x, y = NULL,
# alternative = c("two.sided", "less", "greater"),
# mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
# conf.int = FALSE, conf.level = 0.95, ...)

wilcox.test(wvs6_rus$V242~droplevels(as_factor(wvs6_rus$V240)))

# addmargins(table(wvs6_rus$age_rank, wvs6_rus$gnd))
# tapply(wvs6_rus$age_rank, wvs6_rus$gnd, sum)
# tapply(wvs6_rus$age_rank, wvs6_rus$gnd, mean)
# tapply(wvs6_rus$age_rank, wvs6_rus$gnd, function(x) sum(x) - mean(x) )
# 
# # or
# tapply(wvs6_rus$age_rank, wvs6_rus$gnd, sum)-
# tapply(wvs6_rus$age_rank, wvs6_rus$gnd, mean)

# Paired samples
#V23 - satisfaction with life as a whole
#V59 - financial satisfaction

# 1. Calculate between two variables
# 2. Rank difference
# 3. Assign positive or negative sign depending on difference, exclude zero
# 4. Sum up negative ranks and positive ranks
# 5. For test statistic use smaller of the values


wvs6_rus$finsat<-wvs6_rus$V59
cor(wvs6_rus$finsat,wvs6_rus$V59, use="complete.obs")
wvs6_rus$lifesat<-wvs6_rus$V23
cor(wvs6_rus$lifesat, wvs6_rus$V23, use="complete.obs")

t.test(wvs6_rus$finsat,wvs6_rus$lifesat, paired=T) 

wvs6_rus$satdif<-wvs6_rus$finsat-wvs6_rus$lifesat # check normality of difference
shapiro.test(wvs6_rus$satdif)

wilcox.test(wvs6_rus$finsat, wvs6_rus$lifesat, paired=TRUE)

summary(wvs6_rus$finsat)
summary(wvs6_rus$lifesat) 

# Kruskal-Wallis test
# Uses mean ranks
# H0: mean ranks in all groups are equal to each other
# H1: at least two ranks are not equal to each other

attach(wvs6_rus)

kruskal.test(age~mar, data=wvs6_rus)
tapply(wvs6_rus$age_rank, wvs6_rus$mar, mean, na.rm=T)


# Pairwise comparisons 
# Dunn`s test 
#https://cran.r-project.org/web/packages/dunn.test/dunn.test.pdf 
# Dunn's test may be understood as a test for median difference 

options(scipen=999, digits = 6)

pairwise.wilcox.test(as.numeric(wvs6_rus$age), wvs6_rus$mar, method="BH")

#install.packages("PMCMRplus")
#library("PMCMRplus")
kwAllPairsDunnTest(wvs6_rus$age~wvs6_rus$mar, p.adjust.method="BH")


# CA.
# 1) Check if age when completed the education (V249) is associated with being a Religious person (V147) in Germany
# Explain why you choose this method
# Make a conclusion
# In what groups age is different?

# 2) Check if gender (V240) is associated with church attendance (V145) in Russia
# Explain why you choose this method
# Make a conclusion
# Who attend church more frequently in Russia?


# 3) Recode happiness (V10) into 3-point variable (unhappy, rather happy, happy). Check recoding, assign labels.
# Check if there is a difference between people with different happiness levels in Russia 
# in terms of church attendance (V145). 
# If yes, which groups are different?
# Explain why you choose this method
# Draw conclusion.

# 4) Check if age is different among 3 happiness groups
# Explain why you choose this method
# Make a conclusion
# In what groups age is different?



