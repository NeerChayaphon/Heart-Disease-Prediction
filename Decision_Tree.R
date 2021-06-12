install.packages('rpart')
install.packages('rpart.plot')
install.packages('ROCR')
install.packages('lift')
library(rpart)
library(rpart.plot)
library(ROCR)
library(lift)

# Decision tree model
tree <- rpart(target ~ ., data =  heartData_training) 
rpart.plot(tree)
tree$variable.importance
summary(tree)

res2 = predict(tree,heartData_testing)
head(res2)
res_t = predict(tree,heartData_testing,type = "class")
res_t

confusionMatrix(res_t,heartData_testing$target,mode="prec_recall", positive="Yes")

# Lift calculation
res.p <- predict(tree,heartData_testing)[,"Yes"] 
lift_result <- data.frame(
  prob = res.p,
  y = heartData_testing$target) 
lift_obj <- lift(y ~ prob, 
                 data = lift_result,
                 class="Yes") 
plot(lift_obj, values = 60)

# Lift Chart
pred <- prediction(res.p, heartData_testing$target,
                   label.ordering = c("No","Yes"))
perf_lift <- performance(pred, "lift", "rpp")
plot(perf_lift)

# Lift at 10%
TopDecileLift(res.p,as.integer(heartData_testing$target)-1)


# cv
train_control <- trainControl(method="cv", number=10)
model_cv <- train(target ~ ., data=heartData_testing,
                  trControl=train_control,
                  method="rpart")
model_cv
model_cv$resample


