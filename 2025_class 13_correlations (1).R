###### Correlations#####
#####N. Mikhailova#######

# Part 1  

library("haven")
wvs6<-read_spss("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav")
dim(wvs6)
wvs6_rus<-wvs6[(wvs6$V2)==643, ]
dim(wvs6_rus)
attributes(wvs6_rus$V23)
table(wvs6_rus$V23, useNA = "ifany")
hist(wvs6_rus$V23)

plot(wvs6_rus$V242, wvs6_rus$V23, xlab = "Age", ylab = "Life satisfaction", 
     main = "life satisfaction and Age") # scatter plot for individual-level data rarely shows patterns

# life satisfaction is ordered  (pseudo-metric)
cor1<-cor(wvs6_rus$V242, wvs6_rus$V23, method="pearson", use = "pairwise.complete.obs")
cor1
cor.test(wvs6_rus$V242, wvs6_rus$V23, method="pearson", use = "pairwise.complete.obs")
cor.test(wvs6_rus$V242, wvs6_rus$V23, method="kendall", use = "pairwise.complete.obs")
cor.test(wvs6_rus$V242, wvs6_rus$V23, method="spearman", use = "pairwise.complete.obs")

# better to use correct correlation

# as.numeric() turns into numeric type. Use it if you face problems with double data
#If your variable is a factor, use this option to calculate correlation

cor.test(wvs6_rus$V242, wvs6_rus$V23, method="pearson", conf.level=0.90)
cor.test(wvs6_rus$V242, wvs6_rus$V23, method="pearson", conf.level=0.99)
summary(cor.test(wvs6_rus$V242, wvs6_rus$V23, method="pearson", conf.level=0.99))

library("Hmisc")
attach(wvs6_rus)
rcorr(wvs6_rus$V242, wvs6_rus$V23, type = "spearman")
rcorr(wvs6_rus$V242, wvs6_rus$V23, type = "pearson")

#http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
# https://gist.github.com/aL3xa/887249
# combine stars and values

# correlations by groups 

wvs6_rusw <- wvs6[as_factor(wvs6$V2)=="Russia" | as_factor(wvs6$V2)=="Sweden",]
dim(wvs6_rusw)

library(dplyr)
wvs6_rusw %>%
  group_by(V2) %>%
  summarize(COR=cor(V242, V23, method="pearson", use = "pairwise.complete.obs"))

wvs6_rusw %>%
  group_by(V2) %>%
  summarize(COR=cor.test(V242, V23, 
                         method="pearson", use = "pairwise.complete.obs")$statistic)

wvs6_rusw %>%
  group_by(V2) %>%
  summarize(COR=cor.test(V242, V23, 
                         method="pearson", use = "pairwise.complete.obs")$p.value)


# more than two parameters
# correlation matrices 
# V10 happiness
# V23 - life satisfaction
# V242 - age

# Recode and rename variables 
attributes(wvs6_rus$V10)
wvs6_rus$happiness<-5-wvs6_rus$V10 # happiness has a reverse coding
cor(wvs6_rus$happiness,wvs6_rus$V10,use="complete.obs") #check recoding
wvs6_rus$lsatisfaction<-wvs6_rus$V23
cor(wvs6_rus$lsatisfaction,wvs6_rus$V23,use="complete.obs")
wvs6_rus$age<-wvs6_rus$V242 
cor(wvs6_rus$age,wvs6_rus$V242,use="complete.obs")

Hmisc::rcorr(as.matrix(wvs6_rus[, c("happiness","lsatisfaction","age")]), type = "spearman")
Hmisc::rcorr(as.matrix(wvs6_rus[, c("happiness","lsatisfaction","age")]), type = "pearson")

#Correlograms
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# https://www.r-graph-gallery.com/correlogram.html
# https://rpkgs.datanovia.com/ggcorrplot/
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
# https://www.blopig.com/blog/2019/06/a-brief-introduction-to-ggpairs/
# http://www.sthda.com/english/articles/32-r-graphics-essentials/130-plot-multivariate-continuous-data
# https://ggobi.github.io/ggally/reference/ggpairs.html
library(corrplot)
for_matrix <- wvs6_rus[, c("happiness","lsatisfaction","age")]
dim(for_matrix)
cor2 <- cor(for_matrix, use="complete.obs"); cor2
corrplot(cor2)
corrplot(cor2, method='color')
corrplot.mixed(cor2, lower = 'shade', upper = 'pie')

#### Part 2 
# 2.1 Partial correlation 
# 2.2 Alpha Cronbach`s 

# partial correlation 
# zero order correlation 
# V23 - life satisfaction
# V11  - subjective health
# V242 - age 

rcorr(V242, V23, type = "spearman") # zero-order correlation

# negative significant correlation between age and life satisfaction
# life satisfaction decreases with age and vice verse
# Why? Bad health - one of possible explanations
# Low financial status - another explanation
# If this is true, health should absorb (decrease) the negative impact of age
# V11 - subjective health
# V237 - savings 


# Attention! Subjective health has a reverse coding, bad health - 4, good - 1
# It leads to misinterpretation! 
# Recode health 

attributes(wvs6_rus$V11)
attributes(wvs6_rus$V11)$labels
t(attributes(wvs6_rus$V11)$labels) # for convenience 
addmargins(table(droplevels(as_factor(wvs6_rus$V11), useNA = "ifany")))
wvs6_rus$health<-5-wvs6_rus$V11
table(droplevels(as_factor(wvs6_rus$V11)), wvs6_rus$health)

# recode savings
attributes(wvs6_rus$V237)
wvs6_rus$save<-5-wvs6_rus$V237
table(wvs6_rus$save,wvs6_rus$V237)
rcorr(wvs6_rus$save,wvs6_rus$V237)

rcorr(wvs6_rus$lsatisfaction, wvs6_rus$age, type = "spearman") # zero-order correlation
rcorr(wvs6_rus$age, wvs6_rus$health, type = "spearman") # zero-order correlation
rcorr(wvs6_rus$lsatisfaction, wvs6_rus$health, type = "spearman") ## zero-order correlation
rcorr(wvs6_rus$lsatisfaction, wvs6_rus$save, type = "spearman") 
rcorr(wvs6_rus$age, wvs6_rus$save, type = "spearman") 
cor.test(wvs6_rus$age, wvs6_rus$health, type = "spearman")
Hmisc::rcorr(as.matrix(wvs6_rus[, c("lsatisfaction","health", "age")]), type = "spearman")
# H0: correlation coeff of population = 0 (no association)

#install.packages("ppcor")
library("ppcor")
#pcor.test() for Partial correlation for two variables given a third variable.
wvs6_rus$lsatisfaction<-wvs6_rus$V23
pcor.test(wvs6_rus$lsatisfaction, wvs6_rus$age, wvs6_rus$health) # does not work with missing data 
# we deleted missing values for all 3 vars, therefore correlation is slightly different
sat2<-na.omit(wvs6_rus[, c("lsatisfaction","health", "age")]) # omitting NA
dim(sat2)

rcorr(sat2$lsatisfaction, sat2$age)
pcor.test(sat2$lsatisfaction, sat2$age, sat2$health) 
sat3<-na.omit(wvs6_rus[, c("lsatisfaction","age", "save")])
dim(sat3)
pcor.test(sat3$lsatisfaction, sat3$age, sat3$save) 

# after controlling for subjective health
# the link between age and life satisfaction turned to be insignificant at 95 cl. 

# Cronbach`s Alpha 
#install.packages("psych")
library("psych")

# cronbach`s alpha`
# https://rpubs.com/hauselin/reliabilityanalysis

# Generalized trust 

attributes(wvs6_rus$V105)$label
attributes(wvs6_rus$V106)$label
attributes(wvs6_rus$V107)$label

table(droplevels(as_factor(wvs6_rus$V105)), useNA = "ifany")
table(droplevels(as_factor(wvs6_rus$V106)), useNA = "ifany")
table(droplevels(as_factor(wvs6_rus$V107)), useNA = "ifany")

rcorr(as.matrix(wvs6_rus[, c("V105","V106", "V107")]), type=c("spearman"))
rcorr(as.matrix(wvs6_rus[, c("V105","V106", "V107")]), type=c("pearson"))

alpha(wvs6_rus[, c("V105","V106", "V107")])
# to measure internal consistency 
# if raw_alpha higher than 0.7, then there is internal consistency
# The overall α (raw_alpha) is 0.84. 
# ! Reliability if an item is dropped:
# i.e. raw_alpha of V107 = 0.69 (< 0.84 => dropping V107 will decrease the overall α of the index)
# so, it reflects worse reliability => no need to keep it in the index
# Also, we can see that 0.69 tells us about questionable level of internal consistency

# Particularized trust

attributes(wvs6_rus$V102)$label
attributes(wvs6_rus$V103)$label
attributes(wvs6_rus$V104)$label

table(droplevels(as_factor(wvs6_rus$V102)), useNA = "ifany") # low variation
table(droplevels(as_factor(wvs6_rus$V103)), useNA = "ifany")
table(droplevels(as_factor(wvs6_rus$V104)), useNA = "ifany")

rcorr(as.matrix(wvs6_rus[, c("V102","V103", "V104")]), type=c("spearman"))
rcorr(as.matrix(wvs6_rus[, c("V102","V103", "V104")]), type=c("pearson"))

alpha(wvs6_rus[, c("V102","V103", "V104")])
# dropping V102 from the index would increase reliability (0.54 > 0.5)

alpha(wvs6_rus[, c("V103", "V104")])

#CA.
# Create an index of subjective well-being which includes 2 indicators:
# V23 - life satisfaction
# V10 - happiness
# Check internal consistency (reliability) of the index


