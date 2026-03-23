
# --- 0. Load Libraries ---
library(haven)      # For reading .sav files
library(scales)     # For rescaling variables
library(psych)      # For descriptive stats/alpha
library(stargazer)  # For regression tables
library(lm.beta)    # For standardized coefficients
library(car)        # For linearHypothesis and recoding
library(ggplot2)    # For visualization

# 1. Data Selection
# Selection: Central Federal District (FED_OKR == 30)
df <- read_sav("~/Downloads/Final exam (1).sav")
df_sub <- df[df$FED_OKR == 30, ]

# 2. Variables descriptions (Requirement: names, description, scale types)
var_info <- data.frame(
  Variable_name = c("R_DEN", "H01_01", "H01_02", "I01_10", "MNG_SEM", "H00_07"),
  Description = c("Wage per year", "Sex", "Age", "Education", "Large Family", "Settlement size"),
  Scale_type = c("Ratio", "Nominal", "Interval", "Ordinal", "Nominal", "Ordinal")
)
print(var_info)
#   Variable_name     Description Scale_type
# 1         R_DEN   Wage per year      Ratio
# 2        H01_01             Sex    Nominal
# 3        H01_02             Age   Interval
# 4        I01_10       Education    Ordinal
# 5       MNG_SEM    Large Family    Nominal
# 6        H00_07 Settlement size    Ordinal

# 3. Descriptive Statistics
# TO DO

# 4. Visualization: Age distribution by Sex
# H01_01: 1=Male, 2=Female (Assuming based on typical coding)
ggplot(df_sub, aes(x = H01_02, fill = as_factor(H01_01))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("1" = "darkgreen", "2" = "red"), 
                    labels = c("Male", "Female")) +
  labs(title = "Age Distribution by Gender", x = "Age", fill = "Gender") +
  theme_minimal()

# 5. Average wage of working population
mean(df_sub$R_DEN[df_sub$R_DEN > 0], na.rm = TRUE)
# 535274.5

# ------------------------------------------------------------------------------
#                        Recoding
# ------------------------------------------------------------------------------

# 1. Recode education

# Inspect current attributes to build recoding map
attributes(df_sub$I01_10)$labels

# Recode Education (I01_10) into 4 points
# Logic depends on original coding; assuming high numbers = high edu
# Recoding based on the precise labels and codes from your attributes output
df_sub$edu4 <- car::recode(as.numeric(df_sub$I01_10), 
                           "1 = 'postgraduate'; 
     2:4 = 'higher'; 
     5:8 = 'secondary'; 
     9 = 'no education'")

# Set the levels as a factor to ensure the correct order (from low to high)
# when we do regression analysis
df_sub$edu4 <- factor(df_sub$edu4, 
                      levels = c("no education", "secondary", "higher", "postgraduate"))

df_sub$edu4_scale <- as.numeric(df_sub$edu4) # a numerical version for regression models

# Verification table
table(as_factor(df_sub$I01_10), df_sub$edu4_scale)

# Calculate Spearman correlation
# We use as.numeric() on the factor to get the underlying rank (1, 2, 3, 4)
cor_test <- cor.test(as.numeric(df_sub$I01_10), 
                     as.numeric(df_sub$edu4), 
                     method = "spearman")
print(cor_test)

# 2. Inspecting ordering and possible recoding of all variables

# For each variable we use "attributes(...)$labels" to inspect the ordering
# of the current data labels. We want their numerical label to be increasing
# when the underlying data category (eg level of education) increases too.
# Otherwise we apply a re-coding/re-ordering using "car::recode".

# 2.a Check ordering of : Occupation
class(df_sub$R_8_1)[1]
# "haven_labelled"   <= categorical
attributes(df_sub$R_8_1)$labels
# Director                                                                         1 
# Specialists of the highest qualification level                                   2 
# Mid-level specialists                                                            3 
# Employees engaged in the preparation and execution of documentation, accounting  4 
# Employees of the service and trade sector, protection of citizens and co         5 
# Qualified workers of agriculture and forestry                                    6 
# Operators of production plants and machines                                      7 
# Unqualified worker                                                               8 

# We see the initial "occupation" is in the reverse order if we want to study income
df_sub$occ <- car::recode(as.numeric(df_sub$R_8_1), 
                          "1=8; 2=7; 3=6; 4=5; 5=4; 6=3; 7=2; 8=1")

# Calculate Spearman correlation
# We use as.numeric() on the factor to get the underlying rank (1, 2, 3, 4)
cor_test <- cor.test(as.numeric(df_sub$R_8_1), as.numeric(df_sub$occ),method = "spearman")
print(cor_test)
# -1 as expected as we reversed the scale

# 2.b Check ordering of : Settlement Size
class(df_sub$H00_07)[1]
# "haven_labelled"   <= categorical
attributes(df_sub$H00_07)$labels
# urban less than 50.0        urban 50.0 – 99.9      -----      rural more than 5000 
#                    1                        2                                   10

# Here again we see the original data is not correctly sorted
# For example the 6th one is "urban 1 million and more" ie the biggest settlement size
df_sub$settle_size <- car::recode(as.numeric(df_sub$H00_07), 
                                  "7=1; 8=2; 9=3; 10=4; 1=5; 2=6; 3=7; 4=8; 5=9; 6=10")
 
table(as_factor(df_sub$H00_07), df_sub$settle_size)
#                              1     2     3     4     5     6     7     8     9    10
# urban less than 50.0         0     0     0     0 19357     0     0     0     0     0
# urban 50.0 – 99.9            0     0     0     0     0  4028     0     0     0     0
# Urban 100.0 – 249.9          0     0     0     0     0     0  5160     0     0     0
# Urban 250.0 – 499.9          0     0     0     0     0     0     0  8676     0     0
# Urban 500.0 – 999.9          0     0     0     0     0     0     0     0  2951     0
# urban 1 million and more     0     0     0     0     0     0     0     0     0 24106
# rural 200 or less         3467     0     0     0     0     0     0     0     0     0
# rural 201 - 1000             0  9475     0     0     0     0     0     0     0     0
# rural 1001 - 5000            0     0  6056     0     0     0     0     0     0     0
# rural more than 5000         0     0     0   831     0     0     0     0     0     0

# 2.c Check ordering of : Employment
class(df_sub$R_10_1)[1]
# "haven_labelled"   <= categorical
attributes(df_sub$R_10_1)$labels
# Employed in the economy (working) Not employed in the economy (not working) 
#                                 1                                         2 
# We want to transform so that:
#   Working (1)     => 1
#   Not Working (2) => 0

# Recode: 1 stays 1, 2 becomes 0
df_sub$employed <- car::recode(as.numeric(df_sub$R_10_1), "1=1; 2=0")

# Verify with Cross-tabulation
table(as_factor(df_sub$R_10_1), df_sub$employed)
#                                                 0       1
# Employed in the economy (working)               0   38747
# Not employed in the economy (not working)   33931       0

# 2d Inspect : Age
class(df_sub$H01_02)[1]
# "numeric"   <= numerical
# As expected Age is already numerical so there is no need to recode it

# Check the summary to see the Min and Max age
summary(df_sub$H01_02)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.00   22.00   44.00   42.22   62.00   99.00

# 2e Inspect : Large Family
class(df_sub$MNG_SEM)[1]
# "haven_labelled"   <= categorical
attributes(df_sub$MNG_SEM)$labels
# large family (3+ children) 
#                          1
# This one is a bit different as there is only one value (or NA)
# it suggests it is a binary coding.
# Lets inspect more :
table(df_sub$MNG_SEM, useNA = "always")
#    1  <NA> 
# 3904 37178  
# It seems it is a 10% vs 90% split between "1" and missing
# Recode with a the binary dummy: 1 if they have a large family, 0 otherwise
df_sub$large_family <- ifelse(is.na(df_sub$MNG_SEM), 0, 1)

# Double-check the new distribution
table(df_sub$large_family)
#     0     1 
# 37178  3904 

# 2f Inspect : Sex
# Here we expect the data to be split between 2 values
attributes(df_sub$H01_01)$labels
# Male Female 
#   1      2 
table(df_sub$H01_01, useNA = "always")
#      1     2  <NA> 
#  18081 23001     0 

# Recode: 1 (Male) -> 0; 2 (Female) -> 1
df_sub$female <- car::recode(as.numeric(df_sub$H01_01), "1=0; 2=1")

# Check if the mapping is correct (Cross-tabulation)
table(as_factor(df_sub$H01_01), df_sub$female)

# ------------------------------------------------------------------------------
#                        Bivariate Associations
# ------------------------------------------------------------------------------

# 1. Wage (R_DEN) and Large Family (large_family)
# Nature of variables: Ratio (Wage) vs. Binary/Nominal (Large Family)
# Correct tests to consider:
#   - Mann-Whitney U Test           (if non-normal)
#   - t-test (Student's or Welch's) (if normal)
# => Run Shapiro-Wilk to decide

# --- restrict to employed (wage > 0) ---
df_working <- df_sub[df_sub$R_DEN > 0, ]

# --- Pre-check: normality of wage per group ---
shapiro.test(df_working$R_DEN[df_working$large_family == 0])
shapiro.test(df_working$R_DEN[df_working$large_family == 1])
# => Wage is non-normal in both groups

# => Correct test: Mann-Whitney U (non-parametric alternative to independent t-test)

# Run the test on the filtered data
wilcox.test(R_DEN ~ large_family, data = df_working)
# data:  R_DEN by large_family
# W = 11331320, p-value = 0.6412
# alternative hypothesis: true location shift is not equal to 0

# p-value is high we do not reject the null hypothesis

# direction check on filtered data
tapply(df_working$R_DEN, df_working$large_family, median, na.rm = TRUE)
#        0        1 
# 465179.7 455172.4 

# consistent with the high p-value we observe similar median for the 2 groups
# we do not reject the hypothesis that there is no relationship between wage and large family 

# 2. Settlement Size (settle_size) and Wage (R_DEN)
# Nature of variables: Ordinal (10-point scale) vs. Ratio (Wage)
# Correct Test: Spearman’s Rank Correlation

cor.test(df_working$settle_size, df_working$R_DEN, method = "spearman")
# S = 3.6843e+12, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.5674481 

# p-value is very small we reject the "null hypothesis" in favor of the "alternative hypothesis"
# furthermore we note the estimated correlation is positive (56%) suggesitng a significant
# positive association between the size and the settlement on wages

# 3. Occupation (occ) and Education Level (edu4)
# Nature of variables: Ordinal (Occupation rank) vs. Ordinal (Education categories).
# Correct Test: Spearman’s Rank Correlation

cor.test(as.numeric(df_sub$occ), as.numeric(df_sub$edu4), method = "spearman")
# S = 3.5297e+12, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.5856024 

# p-value of the test is very low so we reject the "null hypothesis" in favor of the "alternative hypothesis"
# further more rho is about 60% so the association between educ level and level of occuption is high

# 4. Occupation (occ) and Sex (female)
# Nature of variables: Ordinal (Occupation) vs. Nominal (Sex)
# Correct tests to consider:
# - Mann-Whitney U Test
# - t-test (either Student's  or Welsh's)

# Shapiro-Wilk requires n ≤ 5000 — sample a subset
set.seed(42)  # for reproducibility

shapiro.test(sample(df_sub$occ[df_sub$female == 0], 1000))
# W = 0.88582, p-value < 2.2e-16
# => we reject normality
# => this justifies the choice of Mann-Whitney U over an independent t-test

shapiro.test(sample(df_sub$occ[df_sub$female == 1], 1000))
# Shapiro-Wilk normality test
# data:  sample(df_sub$occ[df_sub$female == 1], 1000)
# W = 0.83179, p-value < 2.2e-16
# => we reject normality
# => this justifies the choice of Mann-Whitney U over an independent t-test

# => occ is non-normal in both groups
# => Mann-Whitney U is the correct test (non-parametric alternative to independent t-test)

wilcox.test(occ ~ female, data = df_sub)
# W = 131348503, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

# p-value is small so we reject the "null hypothesis" in favor of the "alternative hypothesis"
# ie there is an association between gender and occupation

tapply(df_sub$occ, df_sub$female, median, na.rm = TRUE)
#   0   1
#   4   6
#
# WARNING: occ was RECODED so that higher values = HIGHER status occupations
#   1 = Unqualified worker (lowest status)
#   8 = Director (highest status)
#
# => Men   (0): median occ = 4
# => Women (1): median occ = 6

# => Women have a higher median occupation status than men on the recoded scale.
# => This is a statistically significant difference (p < 2.2e-16),
#    though the direction is perhaps surprising — it may reflect
#    the occupational composition of the Central Federal District sample.


# 5. Wage (R_DEN) and Sex (female)
# Nature of variables: Ratio (Wage) vs. Nominal (Sex)
# Correct tests to consider:
# - Mann-Whitney U Test           (if non-normal)
# - t-test (Student's or Welch's) (if normal)

set.seed(42)
shapiro.test(sample(df_working$R_DEN[df_working$female == 0], 5000))
# p-value < 2.2e-16 => Non-Normal

shapiro.test(sample(df_working$R_DEN[df_working$female == 1], 5000))
# p-value < 2.2e-16 => Non-Normal

# Wage is non-normal in both groups => Mann-Whitney U selected
wilcox.test(R_DEN ~ female, data = df_working)
# W = 213016752, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

# p-value is very small we reject the "null hypothesis" (equality of wage) in favor
# of the "alternative hypothesis" : median wages are different for men and women

# Direction check: median wage by gender
tapply(df_working$R_DEN, df_working$female, median, na.rm = TRUE)
#        0        1
# 532440.0 388218.4 

# Furthermore inspecting the median for group 0 and 1 shows that median wage for mean
# are greated than median wages for women (about 30% difference)

# 6. Employment (employed) and Sex (female)
# Nature of variables: Nominal (Binary) vs. Nominal (Binary)
# Correct Test: Chi-Square Test of Independence
chisq_test <- table(df_sub$employed, df_sub$female)

chisq.test(chisq_test)$expected
#          0        1
# 0 14470.09 19460.91
# 1 16523.91 22223.09
#
# => All four expected counts are well above 5
# => Chi-square approximation is valid, we can proceed

chisq.test(chisq_test)
# data:  chisq_test
# X-squared = 1348.4, df = 1, p-value < 2.2e-16

# p-value is very small so we reject the null hypothesis : there is a relation between gender and employment status

# We use a proportion table:
prop.table(chisq_test, 2) * 100
#          0        1
# 0 38.80428 52.54774
# 1 61.19572 47.45226

# This is showing that women (column 1) are much more likely to be unemployed (row 0) than men
# with a 52% vs 39% proportion

# ------------------------------------------------------------------------------
#                        Regression analysis
# ------------------------------------------------------------------------------

# Hypothesis 1: Gender gap in wage controlling for age, education, settlement, occupation.

m1 <- lm(R_DEN ~ female + H01_02 + edu4_scale + settle_size + occ, data = df_working)
summary(m1)
# Coefficients:
#                Estimate Std. Error t value     Pr(>|t|)    
#   (Intercept)  -53564.2     8534.8  -6.276 3.51e-10 ***
#   female      -171912.1     2680.0 -64.145  < 2e-16 ***
#   H01_02        -1179.1      113.6 -10.383  < 2e-16 ***
#   edu4_scale   112333.2     3039.3  36.960  < 2e-16 ***
#   settle_size   40438.5      486.6  83.096  < 2e-16 ***
#   occ           36255.2      770.0  47.084  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 250900 on 37104 degrees of freedom
# (46997 observations deleted due to missingness)
# Multiple R-squared:  0.4157,	Adjusted R-squared:  0.4156 
# F-statistic:  5279 on 5 and 37104 DF,  p-value: < 2.2e-16

# We observe the model has a large R2 meaning it explained 40% of the wage variability
# this large R2 is consistent with the very low p-value for the overall F-stat.
# Furthemore all the variables have also very low p-value for there individual t-test
# so we conclude rach of the variables have explanatory power in the model.
# In particular variable "female" has a strong negative beta meaning being a woman
# is associated to a decrease in salary of 170k after taking into account other variables
# such as the city of the city and the type of occuption and education level !
# We also observe the beta for education is positive and strong : higher education lead to higher wage
# We also confirm that living in a bigger city is associated with higher wage
# Lastly a higher level of occupation also lead to a higher wage.
# Being older is associated to a small decrease in wage.

# The negative impact of being a woman is higher in absolute term than the effect of gaining "one step"
# in our 4 level education scale ! (170k vs 110k)
# Meaning a woman with a higher education degree will make less money than a man with high school degree if all the
# other variables are the same.

# Hypothesis 2 : Occupation status affects wage more than age

# We run a model where Wage is regressed on both Occupation and Age 
m2 <- lm(R_DEN ~ occ + H01_02, data = df_working)

summary(m2)
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 355504.9     7285.7   48.80   <2e-16 ***
#   occ          60900.6      736.0   82.74   <2e-16 ***
#   H01_02       -2810.9      134.2  -20.94   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 298800 on 37107 degrees of freedom
# Multiple R-squared:  0.1712,	Adjusted R-squared:  0.1712 
# F-statistic:  3833 on 2 and 37107 DF,  p-value: < 2.2e-16

# We see the F-statistic p-value is low so we reject the null hypothesis
# this is consistent with the significant R2 at 17% of wage variance being explained
# Looking at the 2 variables individually we see their slope have low p-value
# so we reject the respective null hypothesis "slope=0" for both and conclude
# that both occupation and age and explanatory power on wage.

# To decide which of the 2 is more important we rely on the model Standardized Coefficients
library(lm.beta)
lm.beta(m2)
# Standardized Coefficients::
#  (Intercept)         occ      H01_02 
#           NA  0.39269144 -0.09938589 

# Using the absolute value we see that occupation is more important to explain wage than age.

# Hypothesis 3 : Impact of a large family

m3 <- lm(R_DEN ~ female + H01_02 + edu4_scale + settle_size + occ + large_family, data = df_working)
summary(m3)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   -56623.0     8553.5  -6.620 3.64e-11 ***
#   female       -171600.0     2679.9 -64.033  < 2e-16 ***
#   H01_02         -1150.1      113.7 -10.118  < 2e-16 ***
#   edu4_scale    112460.7     3038.4  37.013  < 2e-16 ***
#   settle_size    40498.2      486.6  83.221  < 2e-16 ***
#   occ            36269.7      769.8  47.118  < 2e-16 ***
#   large_family   50956.7    10113.5   5.038 4.71e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 250800 on 37103 degrees of freedom
# (46997 observations deleted due to missingness)
# Multiple R-squared:  0.4161,	Adjusted R-squared:  0.416 
# F-statistic:  4406 on 6 and 37103 DF,  p-value: < 2.2e-16

# We see a high R2 of 40% and consistantly a low p-value for the F-stat
# we conclude in the overall significance of the model
# This is as expected given we added an extra variable to the previous model
# which itself was significant.
# All the variables that were present and significant are still showing similar beta/slopes
# and very low p-values so they are still significant in the model

# The new variable "large_family" has a positive beta/slope of 50k and a very low p-value.
# So we accept it in the model and conclude a large familly is associated to 50k higher wage.
# This is as expected as some family will only try to have an extra child if they can afford it
# and conversely parents with higher expense will try to earn extra money.
