library(readr)
library(tidyverse)

# importing the data
heartData <- read_csv("heartData.csv")
View(heartData)

# see the structure and the over all of the data
str(heartData)
summary(heartData)

# ******* Data description ********

# age in years
summary(heartData$age)
# sex (1 = male; 0 = female)
summary(heartData$sex)

# cp (chest pain type)
# There are 4 type
# Typical Angina (1), Atypical angina (2), Non-anginal Pain (3) and Asymptomatic (0)
summary(heartData$cp)

# trestbps (Resting blood pressure (in mm Hg))
summary(heartData$trestbps)

# chol (serum cholestoral in mg/dl)
summary(heartData$chol)

# fasting blood sugar  > 120 mg/dl (1), <= 120 (0)
summary(heartData$fbs)

# resting (electrocardiographic results (values 0,1,2) 
# 0 = normal 
# 1 = having ST-T wave abnormality
# 2 = showing probable or definite left ventricular hypertrophy
summary(heartData$restecg)

# thalach (Maximum Heart Rate Achieved)
summary(heartData$thalach)

# exang (Exercise induced angina) yes (1), no (0)
summary(heartData$exang)

# oldpeak (ST depression induced by exercise relative to rest)
summary(heartData$oldpeak)

# slope (the slope of the peak exercise ST segment) 
# (Value 1: upsloping, Value 2: flat, Value 3: downsloping)
summary(heartData$slope)

# ca (Number of Major Vessels (1-3) Colored by Fluoroscopy)
summary(heartData$ca)

# thal (Thalassemia)
# 1(3) = normal; 2(6) = fixed defect; 3(7) = reversable defect
summary(heartData$thal)

# target (Heart disease (0 = no, 1 = yes))
summary(heartData$target)

# ******* Data tranformation ********
# 1.check for Null value
colSums(is.na(heartData))
# There are no Null value in this dataset

# 2.tranfrom data for future visualization and modeling
heartData %>%
  mutate(sex = as.factor(ifelse(sex == 1,'Male','Female')),
         cp = as.factor(ifelse(cp == 1,'Typical Angina',
                                  ifelse(cp == 2,'Atypical angina',
                                                                  ifelse(cp == 3,'Non-anginal Pain','Asymptomatic')))),
         fbs = as.factor(ifelse(fbs == 1,'>120','<=120')),
         restecg = as.factor((ifelse(restecg == 0,'Normal',
                                        ifelse(restecg == 1,'ST-T wave Abnormality','Probable or definite')))),
         exang = as.factor(ifelse(exang == 1,'Yes','No')),
         slope = as.factor(ifelse(slope == 1,'Upsloping',ifelse(slope == 2,'Flat','Downsloping'))),
         ca = as.factor(ca),
         thal = as.factor(ifelse(thal == 1,'Normal',ifelse(thal == 2,'Fixed defect','Reversable defect'))),
         target = as.factor(ifelse(target == 1,'Yes','No')))-> heartData_Modify

summary(heartData_Modify)

# 2.check for outliers
# from chol has the significant outliers, so we have to remove them
# Let plot graph to find the outliers
ggplot(heartData_Modify,aes(x = chol, fill = target)) +
  geom_histogram(binwidth = 10)

# from the graph it can tell that the the outliers have the value over 550 
# where as the other is less than about 430
# we can filter in out
heartData_Modify %>%
  filter(chol < 500) -> heartData_Modify

ggplot(heartData_Modify,aes(x = chol, fill = target)) +
  geom_histogram(binwidth = 10)




### For future model ###

# heartData %>%
#   mutate(age = as.factor(age),
#          sex = as.character(ifelse(sex == 1,'Male','Female')),
#          cp = as.character(ifelse(cp == 1,'Typical Angina',ifelse(cp == 2,'Atypical angina',ifelse(cp == 3,'Non-anginal Pain','Asymptomatic')))),
#          trestbps = as.factor(trestbps),
#          chol = as.factor(chol),
#          fbs = as.character(ifelse(fbs == 1,'>120','<=120')),
#          restecg = as.character((ifelse(restecg == 0,'Normal',
#                                         ifelse(restecg == 1,'ST-T wave Abnormality','Probable or definite')))),
#          thalach = as.factor(thalach),
#          exang = as.character(ifelse(exang == 1,'Yes','No')),
#          oldpeak = as.factor(oldpeak),
#          slope = as.character(ifelse(slope == 1,'Upsloping',ifelse(slope == 2,'Flat','Downsloping'))),
#          ca = as.factor(ca),
#          thal = as.character(ifelse(thal == 1,'Normal',ifelse(thal == 2,'Fixed defect','Reversable defect'))),
#          target = as.character(ifelse(target == 1,'Yes','No'))) -> heartData_Modify

















