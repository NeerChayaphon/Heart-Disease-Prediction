library(tidyverse)


#target
ggplot(heartData_Modify, aes(target, fill=target)) + 
  geom_bar() + labs(title = "Bar chart of Heart Disease", x = 'Heart Disease', y = 'Number of patients', fill = 'Heart Disease')

ggplot(heartData_Modify, aes(target, fill=sex)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Heart Disease by Sex", x = 'Heart Disease', y = 'Number of patients')

#age
ggplot(heartData_Modify, aes(age, fill=target)) +
  geom_histogram(binwidth=1) + labs(title = "Age with Heart Disease", x = 'Age', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify ,aes(x = target, y = age, fill = target)) + 
  geom_boxplot() + labs(title = "Distribution of Heart Disease by Age", x = 'Heart Disease', y = 'Age', fill = 'Heart Disease') 
ggplot(heartData_Modify ,aes(x = target,y = age,fill = sex)) + geom_boxplot() +
  labs(title = "Distribution of Heart Disease by Age and Sex", x = 'Heart Disease', y = 'Age')

#sex
ggplot(heartData_Modify, aes(sex, fill=target)) +
  geom_bar(position = 'fill') + labs(title = "Bar chart of Heart Disease by Sex", x = 'Sex', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(sex, fill=target)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Heart Disease by Sex", x = 'Sex', y = 'Number of patients', fill = 'Heart Disease')

#cp
ggplot(heartData_Modify, aes(cp, fill=target)) +
  geom_bar(position = 'fill') + labs(title = "Bar chart of Heart Disease by Chest Pain Type", x = 'Chest Pain', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(cp, fill=target)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Heart Disease by Chest Pain Type", x = 'Chest Pain', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(cp, fill=sex)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Chest Pain Type by Sex", x = 'Chest Pain', y = 'Number of patients', fill = 'Sex')
ggplot(heartData_Modify ,aes(x = cp,y = age,fill = sex)) + geom_boxplot() + labs(title = "Distribution of Chest Pain Type by Age and Sex", x = 'Chest Pain', y = 'Age', fill = 'Sex')
#skip
ggplot(heartData_Modify ,aes(x = cp,y = age,fill = target)) + geom_boxplot() + labs(title = "Distribution of Heart Disease by Chest Pain Type and Age", x = 'Chest Pain', y = 'Age', fill = 'Heart Disease')

#trestbps
ggplot(heartData_Modify, aes(trestbps, fill=target)) +
  geom_histogram(binwidth=3) + labs(title = "Bar chart of Heart Disease by Resting blood pressure", x = 'Blood pressure(mm Hg)', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify ,aes(x = target, y = trestbps, fill = target)) + 
  geom_boxplot() + labs(title = "Distribution of Heart Disease by Resting blood pressure", x = 'Heart Disease', y = 'Blood pressure(mm Hg)', fill = 'Heart Disease')
ggplot(heartData_Modify ,aes(x = target, y = trestbps, fill = sex)) + 
  geom_boxplot() + labs(title = "Distribution of Heart Disease by Resting blood pressure and Sex",x = 'Heart Disease', y = 'Blood pressure(mm Hg)', fill = 'Sex')
ggplot(heartData_Modify) + 
  geom_point(aes(age,trestbps)) + 
  geom_smooth(aes(age,trestbps),method = 'lm') + labs(title = "Relationship between Resting blood pressure and Age",x = 'Age', y = 'Blood pressure(mm Hg)')

#chol
ggplot(heartData_Modify, aes(chol, fill=target)) +
  geom_histogram(binwidth=10) + labs(title = "Cholesterol with Heart Disease", x = 'Cholesterol', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify ,aes(x = target, y = chol, fill = target)) + 
  geom_boxplot() + labs(title = "Distribution of Heart Disease by Cholesterol", x = 'Heart Disease', y = 'Cholesterol', fill = 'Heart Disease')
ggplot(heartData_Modify ,aes(x = target, y = chol, fill =sex)) + 
  geom_boxplot() + labs(title = "Distribution of Heart Disease by Cholesterol and Sex", x = 'Heart Disease', y = 'Cholesterol', fill = 'Sex')
ggplot(heartData_Modify) + 
  geom_point(aes(age,chol)) + 
  geom_smooth(aes(age,chol),method = 'lm') + labs(title = "Relationship between Cholesterol and Age", x = 'Age', y = 'Cholesterol')


#fbs
ggplot(heartData_Modify, aes(fbs, fill=target)) +
  geom_bar(position = 'fill') + labs(title = "Bar chart of Heart Disease by Sugar level", x = 'Sugar level(120 mg/dl)', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(fbs, fill=target)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Heart Disease by Sugar level", x = 'Sugar level(120 mg/dl)', y = 'Number of patients', fill = 'Heart Disease')

ggplot(heartData_Modify, aes(fbs, fill=sex)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Sugar level by Sex", x = 'Sugar level(120 mg/dl)', y = 'Number of patients', fill = 'Sex')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = fbs, y = age, fill = target)) + labs(title = "Distribution of Heart Disease by Sugar level and Age", x = 'Sugar level(120 mg/dl)', y = 'Age', fill = 'Heart Disease')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = fbs, y = age, fill = sex))+ labs(title = "Distribution of Sugar level by Age and Sex", x = 'Sugar level(120 mg/dl)', y = 'Age', fill = 'Sex')

#restecg
ggplot(heartData_Modify,  aes(restecg, fill=target)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of heart Disease by Resting ECG results", x = 'Resting electrocardiogram', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify,  aes(restecg, fill=target)) +
  geom_bar(position = 'fill') + labs(title = "Bar chart of heart Disease by Resting ECG results", x = 'Resting electrocardiogram', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify,  aes(restecg, fill=sex)) +
  geom_bar(position = 'fill')+ labs(title = "Bar chart of Resting ECG results by Sex", x = 'Resting electrocardiogram', y = 'Number of patients', fill = 'Sex')

#thalach
ggplot(heartData_Modify, aes(thalach, fill=target)) +
  geom_histogram(binwidth=10) + labs(title = "Maximum heart rate during exercise with Heart Disease", x = 'Maximum heart rate', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify ,aes(x = target, y = thalach, fill = target)) + 
  geom_boxplot() + labs(title = "Distribution of Heart Disease by Maximum heart rate", x = 'Heart Disease', y = 'Maximum heart rate', fill = 'Heart Disease')
ggplot(heartData_Modify ,aes(x = target, y = thalach, fill = sex)) + 
  geom_boxplot() + labs(title = "Distribution of Heart Disease by Maximum heart rate and Sex", x = 'Heart Disease', y = 'Maximum heart rate', fill = 'Sex')
ggplot(heartData_Modify) + 
  geom_point(aes(age,thalach)) + 
  geom_smooth(aes(age,thalach),method = 'lm') + labs(title = "Relationship between Maximum heart rate by Age", x = 'Age', y = 'Maximum heart rate')

#exang
ggplot(heartData_Modify, aes(exang, fill=target)) +
  geom_bar(position = 'fill') + labs(title = "Bar chart of Heart Disease by Presence of angina during exercise", x = 'Presence of angina during exercise', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(exang, fill=target)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Heart Disease by Presence of angina during exercise", x = 'Presence of angina during exercise', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(exang, fill=sex)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Presence of angina during exercise by Sex", x = 'Presence of angina during exercise', y = 'Number of patients', fill = 'Sex')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = exang, y = age, fill = target)) + labs(title = "Distribution of Presence of angina during exercise by Age", x = 'Presence of angina during exercise', y = 'Age', fill = 'Heart Disease')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = exang, y = age, fill = sex)) + labs(title = "Distribution of Presence of angina during exercise by Age and Sex", x = 'Presence of angina during exercise', y = 'Age', fill = 'Sex')

#oldpeak
ggplot(heartData_Modify,  aes(oldpeak,  fill=target)) +
  geom_histogram(binwidth=0.25) + labs(title = "Depression of the ST segment with Heart Disease", x = 'Depression of the ST segment', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify,aes(x = target, y = oldpeak, fill = target)) + 
  geom_boxplot() + labs(title = "Distribution of Heart Disease by Depression of the ST segment", x = 'Heart Disease', y = 'Depression of the ST segment', fill = 'Heart Disease')
ggplot(heartData_Modify) + 
  geom_point(aes(age,oldpeak)) + 
  geom_smooth(aes(age,oldpeak),method = 'lm') + labs(title = "Relationship between Depression of the ST segment by Age", x = 'Age', y = 'Depression of the ST segment')

#slope
ggplot(heartData_Modify, aes(slope, fill=target)) +
  geom_bar(position = 'fill') + labs(title = "Bar chart of Heart Disease by Slope of the ST segment", x = 'Slope of the ST segment', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(slope, fill=target)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Heart Disease by Slope of the ST segment", x = 'Slope of the ST segment', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = slope, y = age, fill = target)) + labs(title = "Distribution of Heart Disease by Slope of the ST segment and Age", x = 'Slope of the ST segment', y = 'Age', fill = 'Heart Disease')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = slope, y = age, fill = sex)) + labs(title = "Distribution of Slope of the ST segment by Age and Sex", x = 'Slope of the ST segment', y = 'Age', fill = 'Sex')


# thal
ggplot(heartData_Modify, aes(thal, fill=target)) +
  geom_bar(position = 'fill') + labs(title = "Bar chart of Heart Disease by Results of the blood flow", x = 'Results of the blood flow', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(thal, fill=target)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Heart Disease by Results of the blood flow", x = 'Results of the blood flow', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = thal, y = age, fill = target)) + labs(title = "Distribution of Heart Disease by Results of the blood flow and Age", x = 'Results of the blood flow', y = 'Age', fill = 'Heart Disease')
# skip
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = thal, y = age, fill = sex)) + labs(title = "Distribution of Results of the blood flow by Age and Sex", x = 'Results of the blood flow', y = 'Age', fill = 'Sex')


#ca (type 0,1,2,3,4)
ggplot(heartData_Modify, aes(ca, fill=target)) +
  geom_bar(position = 'fill') + labs(title = "Bar chart of Heart Disease by main blood vessels coloured", x = 'Number of main blood vessels colored', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify, aes(ca, fill=target)) +
  geom_bar(position = 'dodge') + labs(title = "Bar chart of Heart Disease by main blood vessels coloured", x = 'Number of main blood vessels colored', y = 'Number of patients', fill = 'Heart Disease')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = ca, y = age, fill = target)) + labs(title = "Distribution of Heart Disease by main blood vessels colored and Age", x = 'Number of main blood vessels colored', y = 'Age', fill = 'Heart Disease')
ggplot(heartData_Modify) +
  geom_boxplot(aes(x = ca, y = age, fill = sex)) + labs(title = "Distribution of Main blood vessels colored by Age and Sex", x = 'Number of main blood vessels coloured', y = 'Age', fill = 'Sex')

summary(heartData_Modify)

heartData_Modify %>%
  ggplot(aes(x = thal, y = ca, fill = target)) + geom_boxplot() + theme_bw() +
  labs(title = "Distribution of Heart Disease by Results of the blood flow and main blood vessels colored", x = 'Results of the blood flow', y = 'Number of main blood vessels colored', fill = 'Heart Disease')

