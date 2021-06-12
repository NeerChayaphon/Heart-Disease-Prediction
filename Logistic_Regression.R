install.packages('caret')
install.packages('e1071')
install.packages('rpart')
install.packages('rpart.plot')
library(caret)
library(tidyverse)
library(e1071)

# training and testing data
set.seed(555)
test_ind <- sample(nrow(heartData_Modify), 0.3*nrow(heartData_Modify)) 
heartData_training <- heartData_Modify[-test_ind,]
heartData_testing <- heartData_Modify[test_ind,]

summary(heartData_testing)
summary(heartData_training)

# Prior Probability = 0. 0.5117417 or 51 %
table(heartData_Modify$target)
523 / (523 + 499) # estimate 0.6

# logistic model (1)
# Model 1
model <- glm(target ~ ., data = heartData_training, family = binomial)
summary(model)

res <- predict(model,heartData_testing,type = "response")
res_c <- factor(ifelse(res > 0.5,"Yes","No"))
res_c

confusionMatrix(res_c,heartData_testing$target,mode="prec_recall", positive="Yes")


# Model 2 (best)
# remove fbs,restecg,age,chol and trestbps
model <- glm(target ~ oldpeak+thalach+sex+cp+exang+slope+ca+thal, data = heartData_training, family = binomial)
summary(model)

res <- predict(model,heartData_testing,type = "response")
res_c <- factor(ifelse(res > 0.6,"Yes","No"))
res_c

confusionMatrix(res_c,heartData_testing$target,mode="prec_recall", positive="Yes")

# lift
res_dataFrame <- as.data.frame(res)
res_dataFrame %>%
  mutate('actual' = heartData_testing$target) %>%
  rename('prob' = 'res') %>%
  arrange(desc(prob)) -> lift_result

head(lift_result)

lift_obj <- lift(actual ~ prob,data = lift_result,class="Yes") 
plot(lift_obj, values = 60)

pred <- prediction(res, heartData_testing$target)
perf_lift <- performance(pred, "lift", "rpp")
plot(perf_lift)


TopDecileLift(res,as.integer(heartData_testing$target)-1)


# cross-validation

train_control <- trainControl(method="cv", number=10)
model_cv <- train(target ~ oldpeak+thalach+sex+cp+exang+slope+ca+thal, data=heartData_testing,
                  trControl=train_control,
                  method="glm")
model_cv
model_cv$finalModel
model_cv$resample





