summary(heartData_Modify)

# X-squared = 76.93, df = 1, p-value < 2.2e-16
table(heartData_Modify$sex,heartData_Modify$target)
chisq.test(table(heartData_Modify$sex,heartData_Modify$target))

# X-squared = 278.72, df = 3, p-value < 2.2e-16
table(heartData_Modify$cp,heartData_Modify$target)
chisq.test(table(heartData_Modify$cp,heartData_Modify$target))

# X-squared = 1.421, df = 1, p-value = 0.2332 
table(heartData_Modify$fbs,heartData_Modify$target)
chisq.test(table(heartData_Modify$fbs,heartData_Modify$target))

# X-squared = 36.842, df = 2, p-value = 9.997e-09
table(heartData_Modify$restecg,heartData_Modify$target)
chisq.test(table(heartData_Modify$restecg,heartData_Modify$target))

# X-squared = 193.26, df = 1, p-value < 2.2e-16
table(heartData_Modify$exang,heartData_Modify$target)
chisq.test(table(heartData_Modify$exang,heartData_Modify$target))

# X-squared = 158.46, df = 2, p-value < 2.2e-16
table(heartData_Modify$slope,heartData_Modify$target)
chisq.test(table(heartData_Modify$slope,heartData_Modify$target))

# X-squared = 255.36, df = 4, p-value < 2.2e-16
table(heartData_Modify$ca,heartData_Modify$target)
chisq.test(table(heartData_Modify$ca,heartData_Modify$target))

# X-squared = 283.53, df = 2, p-value < 2.2e-16
table(heartData_Modify$thal,heartData_Modify$target)
chisq.test(table(heartData_Modify$thal,heartData_Modify$target))


heartData_Modify %>%
  ggplot(aes(x = target, y = age, fill = target)) + geom_boxplot() + theme_bw() + labs(title = "Age") 

heartData_Modify %>%
  ggplot(aes(x = target, y = chol, fill = target)) + geom_boxplot() + theme_bw() + labs(title = "Cholesterol")

# Use
heartData_Modify %>%
  ggplot(aes(x = target, y = thalach, fill = target)) + geom_boxplot() + theme_bw() + labs(title = "Maxium heart rate")

# Use
heartData_Modify %>%
  ggplot(aes(x = target, y = oldpeak, fill = target)) + geom_boxplot() + theme_bw() + labs(title = "ST_depression")

heartData_Modify %>%
  ggplot(aes(x = target, y = trestbps, fill = target)) + geom_boxplot() + theme_bw() + labs(title = "Resting blood pressure")
