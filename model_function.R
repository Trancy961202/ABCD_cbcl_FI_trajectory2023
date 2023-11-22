model_performance <- function(data_predict_train1,data_predict_test1,data_predict_test2){
  library(ROCR)
  trctrl <- trainControl(method = "cv",number = 5,repeats = 5,summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
  performance1<-data.frame()
  performance2<-data.frame()
  #rf
  set.seed(1234)
  rfFit <- train(label ~ .,           
                 data = data_predict_train1[,-1],     
                 method = "rf", 
                 trControl = trctrl,
                 metric = "ROC")       
  
  predictions <- predict(rfFit, newdata = data_predict_test1,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test1$label)
  performance1[1,1]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit, newdata = data_predict_test1)
  conf_matrix <-confusionMatrix(predictions,data_predict_test1$label)
  performance1[1,2:4]<-conf_matrix$byClass[1:3]
  predictions <- predict(rfFit, newdata = data_predict_test2,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test2$label)
  performance2[1,1]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit, newdata = data_predict_test2)
  conf_matrix <-confusionMatrix(predictions,data_predict_test2$label)
  performance2[1,2:4]<-conf_matrix$byClass[1:3]
  
  
  set.seed(1234)
  rfFit_noFI <- train(label ~ .,           
                      data = data_predict_train1[,c(-1,-2)],     
                      method = "rf", 
                      trControl = trctrl,
                      metric = "ROC")       
  
  predictions <- predict(rfFit_noFI, newdata = data_predict_test1,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test1$label)
  performance1[1,5]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test1)
  conf_matrix <-confusionMatrix(predictions,data_predict_test1$label)
  performance1[1,6:8]<-conf_matrix$byClass[1:3]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test2,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test2$label)
  performance2[1,5]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test2)
  conf_matrix <-confusionMatrix(predictions,data_predict_test2$label)
  performance2[1,6:8]<-conf_matrix$byClass[1:3]
  
  #XGBoost
  set.seed(1234)
  rfFit <- train(label ~ .,           
                 data = data_predict_train1[,-1],     
                 method = "xgbTree", 
                 trControl = trctrl,
                 metric = "ROC")       
  
  predictions <- predict(rfFit, newdata = data_predict_test1,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test1$label)
  performance1[1,9]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit, newdata = data_predict_test1)
  conf_matrix <-confusionMatrix(predictions,data_predict_test1$label)
  performance1[1,10:12]<-conf_matrix$byClass[1:3]
  predictions <- predict(rfFit, newdata = data_predict_test2,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test2$label)
  performance2[1,9]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit, newdata = data_predict_test2)
  conf_matrix <-confusionMatrix(predictions,data_predict_test2$label)
  performance2[1,10:12]<-conf_matrix$byClass[1:3]
  
  
  set.seed(1234)
  rfFit_noFI <- train(label ~ .,           
                      data = data_predict_train1[,c(-1,-2)],     
                      method = "xgbTree", 
                      trControl = trctrl,
                      metric = "ROC")       
  
  predictions <- predict(rfFit_noFI, newdata = data_predict_test1,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test1$label)
  performance1[1,13]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test1)
  conf_matrix <-confusionMatrix(predictions,data_predict_test1$label)
  performance1[1,14:16]<-conf_matrix$byClass[1:3]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test2,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test2$label)
  performance2[1,13]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test2)
  conf_matrix <-confusionMatrix(predictions,data_predict_test2$label)
  performance2[1,14:16]<-conf_matrix$byClass[1:3]
  
  #gbm
  set.seed(1234)
  rfFit <- train(label ~ .,           
                 data = data_predict_train1[,-1],     
                 method = "gbm", 
                 trControl = trctrl,
                 metric = "ROC")       
  
  predictions <- predict(rfFit, newdata = data_predict_test1,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test1$label)
  performance1[1,17]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit, newdata = data_predict_test1)
  conf_matrix <-confusionMatrix(predictions,data_predict_test1$label)
  performance1[1,18:20]<-conf_matrix$byClass[1:3]
  predictions <- predict(rfFit, newdata = data_predict_test2,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test2$label)
  performance2[1,17]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit, newdata = data_predict_test2)
  conf_matrix <-confusionMatrix(predictions,data_predict_test2$label)
  performance2[1,18:20]<-conf_matrix$byClass[1:3]
  
  
  set.seed(1234)
  rfFit_noFI <- train(label ~ .,           
                      data = data_predict_train1[,c(-1,-2)],     
                      method = "gbm", 
                      trControl = trctrl,
                      metric = "ROC")       
  
  predictions <- predict(rfFit_noFI, newdata = data_predict_test1,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test1$label)
  performance1[1,21]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test1)
  conf_matrix <-confusionMatrix(predictions,data_predict_test1$label)
  performance1[1,22:24]<-conf_matrix$byClass[1:3]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test2,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test2$label)
  performance2[1,21]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test2)
  conf_matrix <-confusionMatrix(predictions,data_predict_test2$label)
  performance2[1,22:24]<-conf_matrix$byClass[1:3]
  
  #multinom
  set.seed(1234)
  rfFit <- train(label ~ .,           
                 data = data_predict_train1[,-1],     
                 method = "multinom", 
                 trControl = trctrl,
                 metric = "ROC")       
  
  predictions <- predict(rfFit, newdata = data_predict_test1,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test1$label)
  performance1[1,25]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit, newdata = data_predict_test1)
  conf_matrix <-confusionMatrix(predictions,data_predict_test1$label)
  performance1[1,26:28]<-conf_matrix$byClass[1:3]
  predictions <- predict(rfFit, newdata = data_predict_test2,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test2$label)
  performance2[1,25]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit, newdata = data_predict_test2)
  conf_matrix <-confusionMatrix(predictions,data_predict_test2$label)
  performance2[1,26:28]<-conf_matrix$byClass[1:3]
  
  
  set.seed(1234)
  rfFit_noFI <- train(label ~ .,           
                      data = data_predict_train1[,c(-1,-2)],     
                      method = "multinom", 
                      trControl = trctrl,
                      metric = "ROC")       
  
  predictions <- predict(rfFit_noFI, newdata = data_predict_test1,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test1$label)
  performance1[1,29]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test1)
  conf_matrix <-confusionMatrix(predictions,data_predict_test1$label)
  performance1[1,30:32]<-conf_matrix$byClass[1:3]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test2,type = "prob")
  pred<-prediction(predictions[,1],data_predict_test2$label)
  performance2[1,29]<-performance(pred,"auc")@y.values[[1]]
  predictions <- predict(rfFit_noFI, newdata = data_predict_test2)
  conf_matrix <-confusionMatrix(predictions,data_predict_test2$label)
  performance2[1,30:32]<-conf_matrix$byClass[1:3]
  
  colnames(performance1)<-c("auc_rf","sen_rf","spe_rf","ppv_rf","auc_rf_noFI","sen_rf_noFI","spe_rf_noFI","ppv_rf_noFI",
                            "auc_xgb","sen_xgb","spe_xgb","ppv_xgb","auc_xgb_noFI","sen_xgb_noFI","spe_xgb_noFI","ppv_xgb_noFI",
                            "auc_gbm","sen_gbm","spe_gbm","ppv_gbm","auc_gbm_noFI","sen_gbm_noFI","spe_gbm_noFI","ppv_gbm_noFI",
                            "auc_mul","sen_mul","spe_mul","ppv_mul","auc_mul_noFI","sen_mul_noFI","spe_mul_noFI","ppv_mul_noFI")
  colnames(performance2)<-c("auc_rf","sen_rf","spe_rf","ppv_rf","auc_rf_noFI","sen_rf_noFI","spe_rf_noFI","ppv_rf_noFI",
                            "auc_xgb","sen_xgb","spe_xgb","ppv_xgb","auc_xgb_noFI","sen_xgb_noFI","spe_xgb_noFI","ppv_xgb_noFI",
                            "auc_gbm","sen_gbm","spe_gbm","ppv_gbm","auc_gbm_noFI","sen_gbm_noFI","spe_gbm_noFI","ppv_gbm_noFI",
                            "auc_mul","sen_mul","spe_mul","ppv_mul","auc_mul_noFI","sen_mul_noFI","spe_mul_noFI","ppv_mul_noFI")
  performance<-list(performance1,performance2)
  performance
}