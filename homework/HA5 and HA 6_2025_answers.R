### HA 5 ####

# 1.	Read the 6th wave of the WVS.
library('haven')
wvs6<-read_sav("/Users/Natasha/Downloads/WV6_Data_Spss_v20180912.sav") 
dim(wvs6) 
# 89565 respondents,   440 variables

#2. Select Russian and Sweden sample in a new data set
print_labels(wvs6$V2) # to see the country codes
wvs6_rusw <- wvs6[wvs6$V2 == 643 | wvs6$V2 == 752, ]
wvs6_rusw <- wvs6[as_factor(wvs6$V2)=="Russia" | as_factor(wvs6$V2)=="Sweden" , ]
dim(wvs6_swru)
#3706 respondents,  440 variables (still)

# 3.	Select variables trust in strangers (V105), education (V248), 
# financial satisfaction (V59), family savings (V237) 
wvs6_rusw <- wvs6_rusw[, c("V2", "V105", "V248", "V59", "V237")]
dim(wvs6_rusw)
#3706 respondents,  5 variables (now)

# 4.	Reverse scales matching the maximum level to the maximum numeric code 
# and minimum levels to the minimum numeric code.  
attributes(wvs6_rusw$V105)$labels #check the scale ->it's reversed
wvs6_rusw$trstr<-5-as.numeric(wvs6_rusw$V105)
wvs6_rusw$trstr<-labelled(wvs6_rusw$trstr,c("Do not trust at all"=1, "Do not trust very much"=2, 
                                            "Trust somewhat"=3, "Trust completely"=4 ))
table(as_factor(wvs6_rusw$trstr), as_factor(wvs6_rusw$V105)) #check recoding

attributes(wvs6_rusw$V248)$labels #the scale is not reversed
attributes(wvs6_rusw$V59)$labels #the scale is not reversed
attributes(wvs6_rusw$V237)$labels

# 5.	Recode financial satisfaction into 5-point scale, assign labels, check recoding
library(car)
wvs6_rusw$fin_sat_rec<-car::recode(wvs6_rusw$V59, "1:2=1; 3:4=2; 5:6=3; 7:8=4; 9:10=5")
table(wvs6_rusw$V59, wvs6_rusw$fin_sat_rec, useNA="ifany")
wvs6_rusw$fin_sat_rec<- labelled(wvs6_rusw$fin_sat_rec, 
                                c("Dissatisfied" = 1, "Rather Dissatisfied" = 2, "Neither" = 3, "Rather Satisfied" = 4, 
                                  "Satisfied" = 5))
table(as_factor(wvs6_rusw$fin_sat_rec))
table(droplevels(as_factor(wvs6_rusw$V59)), as_factor(wvs6_rusw$fin_sat_rec), useNA = "ifany")

#6.	Recode education into dummy variable (1- a university education and higher, 
# 0 – no university education). Assign labels, check recoding.  
wvs6_rusw$educ_rec<-Recode(wvs6_rusw$V248, "1:7=0; 8:9=1")
table(wvs6_rusw$V248,wvs6_rusw$educ_rec, useNA="ifany")
wvs6_rusw$educ_rec<-labelled(wvs6_rusw$educ_rec, 
                             c("No university educ" = 0, "University educ" = 1))
table(as_factor(wvs6_rusw$educ_rec))
table(droplevels(as_factor(wvs6_rusw$V248)), as_factor(wvs6_rusw$educ_rec), useNA = "ifany")

#7.	Recode trust in strangers into dummy (1-high trust, 0 –low trust), assign labels, check recoding.
wvs6_rusw$trust_rec<-Recode(wvs6_rusw$trstr, "1:2=0; 3:4=1") #trstr from task 4
table(wvs6_rusw$trust_rec,wvs6_rusw$trstr,useNA="ifany")
wvs6_rusw$trust_rec<-labelled(wvs6_rusw$trust_rec, 
                              c("High trust" = 1, "Low trust" = 0))
table(as_factor(wvs6_rusw$trust_rec))
table(droplevels(as_factor(wvs6_rusw$V105)), as_factor(wvs6_rusw$trust_rec), useNA = "ifany") #to check labels I am using the original (not reversed) scale
table(wvs6_rusw$V105, wvs6_rusw$trust_rec)

#8.	Recode family savings into 3-point scales. Assign labels, check recoding.
wvs6_rusw$savings_rec<-Recode(wvs6_rusw$V237, "1=1; 2=2; 3:4=3")
table(wvs6_rusw$savings_rec,wvs6_rusw$V237,useNA = "ifany" )
wvs6_rusw$savings_rec<-labelled(wvs6_rusw$savings_rec, 
                                c("Save money" = 1, "Just get by" = 2, 
                                  "Spent savings and/or borrowed money" = 3))
table(droplevels(as_factor(wvs6_rusw$V237)), as_factor(wvs6_rusw$savings_rec), useNA = "ifany")

#####################################
############### HA 6 ################
#####################################

#1.	Visualize the distributions for each newly created variable. 
#Select the correct visualization method. Include all needed information to make 
#your plot informative. Use any nice colors from HSE brand book (https://www.hse.ru/en/info/brandbook/).
library(ggplot2)

#a) financial satisfaction
finsat <- ggplot(wvs6_rusw, aes(x = as_factor(fin_sat_rec), fill = as_factor(V2))) +
  geom_bar(position = "dodge") +
  labs(title = 'Financial satisfaction in Sweden and Russia',
       x = 'Satisfaction with Finance', 
       y = 'Frequeny',
       subtitle = 'sample: Sweden and Russia',
       caption = 'Data: WVS, 6 wave') +
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#1a4784","Sweden"="#C20061")) +
  theme_minimal()
print(finsat)
finsat
#b) family savings

savings <-  ggplot(wvs6_rusw, aes(x = as_factor(savings_rec), fill = as_factor(V2))) + 
  geom_bar(position = "dodge") +
  labs(title = 'Family Savings in Sweden and Russia',
       x = 'Savings',
       y = 'Frequency', 
       subtitle = 'sample: Sweden and Russia', 
       caption = 'Data: WVS, 6 wave') +
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#1a4784","Sweden"="#C20061")) +
  theme_minimal()
print(savings)

#c) education level
edu <- ggplot(wvs6_rusw, aes(x = as_factor(educ_rec), fill = as_factor(V2))) + 
  geom_bar(position = "dodge") +
  labs(title = 'Education level in Sweden and Russia', 
       x = 'Education', 
       y = 'Frequency', 
       subtitle = 'sample: Sweden and Russia',
       caption = 'Data: WVS, 6 wave') +
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#1a4784","Sweden"="#C20061")) +
  theme_minimal()

print(edu)

#d) trust to strangers (generalized trust)
trust <- ggplot(wvs6_rusw, aes(x = as_factor(trust_rec), fill = as_factor(V2))) + 
  geom_bar(position = "dodge") +
  labs(title = 'Generalized trust in Sweden and Russia',
       x = 'Trust to strangers', 
       y = 'Frequency', 
       subtitle = 'sample: Sweden and Russia',
       caption = 'Data: WVS, 6 wave') +
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#1a4784","Sweden"="#C20061")) +
  theme_minimal()
print(trust)

#2.	Create a picture which includes all your plots from task 9.
library(ggpubr)
ggarrange(finsat, savings, edu, trust)

#3. Create graphs for distributions of importance of family (V4) and importance of friends (V5)
# for Russia and Sweden (recode variables if the scale is reverse for better logic of
# visualization). Add categorization by countries to the graph.
wvs6_swru<-wvs6_1[as_factor(wvs6_1$V2)=="Russia" | as_factor(wvs6_1$V2)=="Sweden" , ]
dim(wvs6_swru)
attributes(wvs6_swru$V4); attributes(wvs6_swru$V5) #-> scales are reversed

wvs6_swru$fam<-5-as.numeric(wvs6_swru$V4)
wvs6_swru$fam<-labelled(wvs6_swru$fam, c("Not at all important"=1, "Not very important"=2, 
                                            "Rather important"=3, "Very important "=4 ))
table(as_factor(wvs6_swru$V4), as_factor(wvs6_swru$fam)) #check recoding

wvs6_swru$friend<-5-as.numeric(wvs6_swru$V5)
wvs6_swru$friend<-labelled(wvs6_swru$friend, c("Not at all important"=1, "Not very important"=2, 
                                         "Rather important"=3, "Very important "=4 ))
table(as_factor(wvs6_swru$V5), as_factor(wvs6_swru$friend)) #check recoding

ggplot(wvs6_swru, aes(x = fam, fill = as_factor(V2))) +  
  geom_bar(position = "fill") + scale_fill_brewer(palette = "Pastel1") +   
  labs(title = 'Important in life: Family', 
       subtitle = 'pooled samle: Russia and Sweden', 
       caption = 'Data: WVS, 6 wave',     
       x = "Indicate how important it is in your life",   
       y = 'Share',
       color = 'Country')

ggplot(wvs6_swru, aes(x = friend, fill = as_factor(V2))) +  
  geom_bar(position = "dodge") + scale_fill_brewer(palette = "Pastel2") +   
  labs(title = 'Important in life: Friends', 
       subtitle = 'pooled samle: Russia and Sweden', 
       caption = 'Data: WVS, 6 wave',     
       x = "Indicate how important it is in your life",   
       y = 'Share',
       color = 'Country')

#4. Create a graph of distribution of happiness (V10) in Russia and Sweden on one graph. Add
#all needed signs and names. Assign nice colors which you like.
attributes(wvs6_swru$V10)
wvs6_swru$happy<-5-as.numeric(wvs6_swru$V10)
wvs6_swru$happy<-labelled(wvs6_swru$happy, c("Not at all happy"=1, "Not very happy"=2, 
                                         "Rather happy"=3, "Very happy"=4 ))
table(as_factor(wvs6_swru$V10), as_factor(wvs6_swru$happy)) #check recoding

ggplot(wvs6_swru, aes(x = happy, fill = as_factor(V2))) +  
  geom_bar() + scale_fill_brewer(palette = "Accent") +   
  labs(title = 'Level of happiness', 
       subtitle = 'pooled samle: Russia and Sweden', 
       caption = 'Data: WVS, 6 wave',     
       x = "Are you happy?",   
       y = 'Frequency / N')
