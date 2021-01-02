setwd("~/Desktop/R/WWS TF")
library(dplyr)

load("WVS_Longitudinal_1981_2014_R_v2015_04_18.rdata")

#V37, V39, V41, V44, V46, V60-66, V69, V72, V84-95, V102-107, V108-130, 133, 136, 138, 140-42, 144-156, 170-80, 


#(V181-86 for context)
#V186 is KEY
#189, 210-215, 217-228, 229-234, 238-248, 251, 252, 262, 265, 

#y003 --> autonomy index, 

#SACSECVAL, Overall Secular Values

# S003: The country
# S009: The country code (alphanumeric)
# S025: The country and year
# S017: The original weight
# S019: The original weight equilibrated to an homogeneous N of 1500 for each country
# TRADRAT5/SURVSELF: The cultural map dimensions


surveydata <- WVS_Longitudinal_1981_2014_R_v2015_04_18 %>% filter(H006_06 >= 0)

#H006_01	Worries: Losing my job or not finding a job
#H006_02	Worries: Not being able to give one's children a good education
#H006_03	Worries: A war involving my country
#H006_04	Worries: A terrorist attack
#H006_05	Worries: A civil war
#H006_06	Worries: Government wire-tapping or reading my mail or email

#E197	Opinion on terrorism
#E198	Using violence for political goals not justified


#the ultimate question, from a counter-terrorism standpoint what is the benefit to governments for incorporating trust?

UKsurveydata <- WVS_Longitudinal_1981_2014_R_v2015_04_18 %>% filter(S003 == 2100)

summary(surveydata$H006_06)

summary(surveydata$S003)
library(haven)
ZA4804_v3_0_0 <- read_dta("~/Downloads/ZA4804_v3-0-0.dta/ZA4804_v3-0-0.dta")

MergedSurvey <- rbind(ZA4804_v3_0_0, WVS_Longitudinal_1981_2014_R_v2015_04_18)

eusurveydata <- ZA4804_v3_0_0 %>% filter(H006_06 >= 0)

summary(ZA4804_v3_0_0$H006_06)