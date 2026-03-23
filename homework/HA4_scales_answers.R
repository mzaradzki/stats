# Home Assignment 4 Scales

setwd("/Users/Natasha/Downloads")
getwd()

#1. Use R script to store your results
#2. Download the 6th wave of the WVS?[http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp](http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp)

#3. Read this data in R using haven.
library("haven")
wvs6<-read_sav("WV6_Data_sav_v20201117.sav")

#4. Find two questions with nominal scales.
as_factor(head(wvs6[1:10])) # or we could download questionnaire (task 2) and search it there
attributes(wvs6$V2) 
attributes(wvs6$V57)
# Questions with nominal scales: Q2 and Q57

#5. Show values and labels for these questions.
print_labels(wvs6$V2)
print_labels(wvs6$V57)
#or
attributes(wvs6$V2); attributes(wvs6$V57)

#6. Use frequency tables for these questions to demonstrate the distribution. 
# Do not forget about missing values.
table(as_factor(wvs6$V2), useNA = "ifany")
table(as_factor(wvs6$V57), useNA = "ifany")
table(droplevels(as_factor(wvs6$V57), useNA = "ifany")) 
#in %
round(prop.table(table(as_factor(wvs6$V2), useNA = "ifany"))*100, 2)
round(prop.table(table(as_factor(wvs6$V57), useNA = "ifany"))*100, 2)
#or 
as.data.frame(table(as_factor(wvs6$V2), useNA = "ifany"))
as.data.frame(round(prop.table(table(as_factor(wvs6$V2), useNA = "ifany"))*100, 2))
as.data.frame(table(as_factor(wvs6$V57), useNA = "ifany"))
as.data.frame(round(prop.table(table(as_factor(wvs6$V57), useNA = "ifany"))*100, 2))

#7. Find two ordered scales.
as_factor(head(wvs6[1:10])) #or search it in questionnaire paper
attributes(wvs6$V4)
attributes(wvs6$V5)
# Questions with ordered scales: Q4 and Q5

#8. Show values and labels for these questions. 
# Use frequency tables for these questions to demonstrate the distribution. 
# Do not forget about missing values.
print_labels(wvs6$V4)
table(droplevels(as_factor(wvs6$V4), useNA = "ifany"))
round(prop.table(table(droplevels(as_factor(wvs6$V4), useNA = "ifany")))*100,2)

print_labels(wvs6$V5)
table(droplevels(as_factor(wvs6$V5), useNA = "ifany"))
round(prop.table(table(droplevels(as_factor(wvs6$V5), useNA = "ifany")))*100,2)

#9. If scales have reversed order, recode them. Check recoding
print_labels(wvs6$V4)
wvs6$V4rev <- 5-as.numeric(wvs6$V4)
table(wvs6$V4, wvs6$V4rev, useNA = "ifany")
print_labels(wvs6$V5)
wvs6$V5rev <- 5-as.numeric(wvs6$V5)
table(wvs6$V5, wvs6$V5rev, useNA = "ifany")


#10. Assign new labels to recoded scales.
wvs6$V4rev<-labelled(wvs6$V4rev, c("Not at all important" = 1, "Not very important" = 2, 
                                   "Rather important" = 3, "Very important" = 4))
wvs6$V5rev<-labelled(wvs6$V5rev, c("Not at all important" = 1, "Not very important" = 2, 
                                   "Rather important" = 3, "Very important" = 4))

#11. Check the class for newly created variables with labels.
class(wvs6$V4rev)
class(wvs6$V5rev)

#12. Check the recoding for these two questions
table(as_factor(wvs6$V4), as_factor(wvs6$V4rev), useNA = "ifany")
table(as_factor(wvs6$V5), as_factor(wvs6$V5rev), useNA = "ifany")

#13. Find real metric scales. As many as possible.
attributes(wvs6$V241) # Can you tell me your year of birth, please? 19____
attributes(wvs6$V242) #  This means you are ____ years old (write in age in two digits).
attributes(wvs6$V249) # At what age did you (or will you) complete your full time education, 
              # either at school or at an institution of higher education? Please exclude apprenticeships 
              # [NOTE: if respondent indicates to be a student, code highest level s/he expects to complete]:
              # ________ (write in age in two digits)

# Questions with metric scales: Q241, Q242 and Q249

#14. Show frequencies for these scales.
table(wvs6$V241, useNA = "ifany")
round(prop.table(table(as_factor(wvs6$V241), useNA = "ifany"))*100, 2)
as.data.frame(round(prop.table(table(as_factor(wvs6$V241), useNA = "ifany"))*100, 2))
table(wvs6$V242, useNA = "ifany")
as.data.frame(round(prop.table(table(as_factor(wvs6$V242), useNA = "ifany"))*100, 2))
table(wvs6$V249, useNA = "ifany")
as.data.frame(round(prop.table(table(as_factor(wvs6$V249), useNA = "ifany"))*100, 2))

#15. Create a table describing all chosen scales (original, not recorded versions)
question <- c("V2", "V57", "V4", "V5", "V241", "V242", "V249")
wording <- c(attributes(wvs6$V2)$label, attributes(wvs6$V57)$label, 
             attributes(wvs6$V4)$label, attributes(wvs6$V5)$label, 
             attributes(wvs6$V241)$label,
             attributes(wvs6$V242)$label, attributes(wvs6$V249)$label)
type <- c(rep(c("nominal", "ordered", "metric"), each=2), "metric")
n_points <- c(length(attributes(wvs6$V2)$labels), "6","4", 
              "4", length(unique(wvs6$V241)),
              length(unique(wvs6$V242)), length(unique(wvs6$V249)))
table(wvs6$V4); table(wvs6$V5)
min_n <- c("Not applicable", "Not applicable", 
         min(wvs6$V4, na.rm=T), min(wvs6$V5, na.rm=T), min(wvs6$V241, na.rm=T),
         min(wvs6$V242, na.rm=T), min(wvs6$V249, na.rm=T))
max_n <- c("Not applicable", "Not applicable", 
                    max(wvs6$V4, na.rm=T), max(wvs6$V5, na.rm=T), max(wvs6$V241, na.rm=T),
                    max(wvs6$V242, na.rm=T), max(wvs6$V249, na.rm=T))
valid <- c(length(na.omit(wvs6$V2)), length(na.omit(wvs6$V57)), 
           length(na.omit(wvs6$V4)), length(na.omit(wvs6$V5)), length(na.omit(wvs6$V241)),
           length(na.omit(wvs6$V242)), length(na.omit(wvs6$V249)))
missing <- c(sum(is.na(wvs6$V2)), sum(is.na(wvs6$V57)), 
                        sum(is.na(wvs6$V4)), sum(is.na(wvs6$V5)), sum(is.na(wvs6$V241)),
                        sum(is.na(wvs6$V242)), sum(is.na(wvs6$V249)))
desc_table <- data.frame(question, wording, type, n_points, min_n, max_n, valid, missing)
desc_table

