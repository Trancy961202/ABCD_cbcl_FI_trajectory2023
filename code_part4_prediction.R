#############################################################
#############################################################
#######################prediction############################
#############################################################
#############################################################
library(nnet)
library(caret)


##externalizing
load("data/r0727_2.RData")
smri_qc<-merge(smri,qc,by = c("src_subject_id","eventname"))
table(smri$eventname)
smri_qc<-smri_qc[smri_qc$imgincl_t1w_include==1,]
smri_base<-smri_qc[smri_qc$eventname=="baseline_year_1_arm_1",]
smri_2year<-smri_qc[smri_qc$eventname=="2_year_follow_up_y_arm_1",]

#baseline
index_train<-data0510_new_ex[data0510_new_ex$visit==0,]$src_subject_id#5254
index_test<-df1[!df1$src_subject_id%in%df$src_subject_id,]$src_subject_id  #1204
data0510_new_1114<-data0510[data0510$src_subject_id%in%union(index_train,index_test),] #6458
data_class_smri_tbss_1114<-merge(data0510_new_1114[data0510_new_1114$visit==2,-1],smri_base,by = "src_subject_id")
data_class_smri_tbss_1114<-merge(data0510_new_1114[data0510_new_1114$visit==0,c(1,6:10)],data_class_smri_tbss_1114,by = "src_subject_id")
colnames(data_class_smri_tbss_1114)[68]

index1<-data0510[data0510$visit==0&data0510$cbcl_scr_syn_external_t>=60,]$src_subject_id
index2<-data0510[data0510$visit==1&data0510$cbcl_scr_syn_external_t>=60,]$src_subject_id
index3<-data0510[data0510$visit==2&data0510$cbcl_scr_syn_external_t>=60,]$src_subject_id
index4<-data0510[data0510$visit==0&data0510$cbcl_scr_syn_external_t<60,]$src_subject_id
index5<-data0510[data0510$visit==1&data0510$cbcl_scr_syn_external_t<60,]$src_subject_id
index6<-data0510[data0510$visit==2&data0510$cbcl_scr_syn_external_t<60,]$src_subject_id
index7<-intersect(index1,intersect(index2,index3)) #persistent
index8<-intersect(index4,intersect(index5,index6)) #control
index9<-intersect(index1,index6) #remitting
index10<-intersect(index4,index3) #late-onset

data_class_smri_tbss_1114$class<-"fluctuation"
data_class_smri_tbss_1114[data_class_smri_tbss_1114$src_subject_id%in%index7,]$class<-"persistent"
data_class_smri_tbss_1114[data_class_smri_tbss_1114$src_subject_id%in%index8,]$class<-"control"
data_class_smri_tbss_1114[data_class_smri_tbss_1114$src_subject_id%in%index9,]$class<-"remitting"
data_class_smri_tbss_1114[data_class_smri_tbss_1114$src_subject_id%in%index10,]$class<-"late-onset"
table(data_class_smri_tbss_1114$class)


data_class_smri_tbss_1114_ex<-data_class_smri_tbss_1114[data_class_smri_tbss_1114$cbcl_scr_syn_external_t.x<60,]
table(data_class_smri_tbss_1114_ex$class)
data_y1<-data_class_smri_tbss_1114_ex[,ncol(data_class_smri_tbss_1114_ex)]
data_x1<-data_class_smri_tbss_1114_ex[,c(4:5,24,25,27,29,30,40:47,53,174,388,495,539,68:135,282:349,389:456)]

data_x1<-cbind(data_x1[,1:2],class.ind(data_x1$high.educ)[,-1],class.ind(data_x1$household.income)[,-1],class.ind(data_x1$sex)[,-1],
               class.ind(data_x1$site_id_l)[,-1],class.ind(data_x1$race_ethnicity)[,-1],data_x1[,8:ncol(data_x1)])
colnames(data_x1)[3:9]<-c("edu1","edu2","edu3","edu4","income1","income2","sex")
Process = preProcess(data_x1,method = c( "knnImpute"))
newdata = predict(Process, data_x1)
data_x1<-newdata

data_predict<-as.data.frame(cbind(data_class_smri_tbss_1114_ex$src_subject_id,data_x1$nihtbx_fluidcomp_uncorrected,data_x1[,-43],data_y1))
colnames(data_predict)[c(1,2,ncol(data_predict))]<-c("src_subject_id","nihtbx_fluidcomp_uncorrected","exts")
data_predict$exts <- ifelse(data_predict$exts == "late-onset", 1, 0)
data_predict$exts<-make.names(data_predict$exts,unique = F,allow_ = T)
data_predict$exts<-as.factor(data_predict$exts)
table(data_predict$exts) 
data_predict_train<-data_predict[data_predict$src_subject_id%in%index_train,] 
data_predict_test<-data_predict[data_predict$src_subject_id%in%index_test,]  

# feature selection
ctrl = rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "final",repeats = 3)
subsets = c(5,10,15,20,25,30,35,40)
set.seed(39)
Profile = rfe(data_predict_train[,c(-1,-ncol(data_predict_train))], data_predict_train[,ncol(data_predict_train)], sizes = subsets, rfeControl = ctrl)
print(Profile)   # variables = 10
fcol <- Profile$optVariables
fcol
paste(fcol,collapse = "','")

data_predict_train1<-as.data.frame(cbind(data_predict_train[,1],data_predict_train[,fcol],data_predict_train[,ncol(data_predict_train)]))
colnames(data_predict_train1)[ncol(data_predict_train1)]<-"label"
data_predict_train1$label<-ifelse(data_predict_train1$label=="X1",1,0)
table(data_predict_train1$label)
data_predict_test1<-as.data.frame(cbind(data_predict_test[,1],data_predict_test[,fcol],data_predict_test[,ncol(data_predict_test)]))
colnames(data_predict_test1)[ncol(data_predict_test1)]<-"label"
data_predict_test1$label<-ifelse(data_predict_test1$label=="X1",1,0)
table(data_predict_test1$label) 
write.csv(data_predict_train1,"predict/new1113/data/cbcltraj_exlow_train.csv",quote = F,row.names = F)
write.csv(data_predict_test1,"predict/new1113/data/cbcltraj_exlow_test.csv",quote = F,row.names = F)

visit36<-data_1_1_1[data_1_1_1$visit==36,]
ext<-visit36[match(data_predict_test1$src_subject_id,visit36$src_subject_id),]
data_predict_test2<-cbind(data_predict_test1,ext$cbcl_scr_syn_external_t)
data_predict_test2$label<-ifelse(data_predict_test2$`ext$cbcl_scr_syn_external_t`>=60,1,0)
data_predict_test2<-na.omit(data_predict_test2)
data_predict_test2<-data_predict_test2[,-ncol(data_predict_test2)]
write.csv(data_predict_train1,"predict/new1113/data/cbcltraj_exlow4_train.csv",quote = F,row.names = F)
write.csv(data_predict_test2,"predict/new1113/data/cbcltraj_exlow4_test.csv",quote = F,row.names = F)

save.image("result/r1115_exresult_low.RData")

data_predict_train1$label<-make.names(data_predict_train1$label,unique = F,allow_ = T)
data_predict_train1$label<-factor(data_predict_train1$label,levels = c("X1","X0"))
data_predict_test1$label<-make.names(data_predict_test1$label,unique = F,allow_ = T)
data_predict_test1$label<-factor(data_predict_test1$label,levels = c("X1","X0"))
data_predict_test2$label<-make.names(data_predict_test2$label,unique = F,allow_ = T)
data_predict_test2$label<-factor(data_predict_test2$label,levels = c("X1","X0"))
set.seed(1234)
datTrain <- downSample(data_predict_train1[,c(-1,-ncol(data_predict_train1))],data_predict_train1[,ncol(data_predict_train1)],yname = "label")
source(file.path("code/model_function.R"))
performance<-model_performance(datTrain,data_predict_test1,data_predict_test2)
write.csv(as.data.frame(rbind(performance[[1]],performance[[2]])),"tmp.csv",quote = F)
save.image("result/r1115_exresult_low_predict.RData")




data_class_smri_tbss_1114_ex<-data_class_smri_tbss_1114[data_class_smri_tbss_1114$cbcl_scr_syn_external_t.x>=60,]
table(data_class_smri_tbss_1114_ex$class) 

data_y1<-data_class_smri_tbss_1114_ex[,ncol(data_class_smri_tbss_1114_ex)]
data_x1<-data_class_smri_tbss_1114_ex[,c(4:5,24,25,27,29,30,40:47,53,174,388,495,539,68:135,282:349,389:456)]
data_x1<-cbind(data_x1[,1:2],class.ind(data_x1$high.educ)[,-1],class.ind(data_x1$household.income)[,-1],class.ind(data_x1$sex)[,-1],
               class.ind(data_x1$site_id_l)[,-1],class.ind(data_x1$race_ethnicity)[,-1],data_x1[,8:ncol(data_x1)])
colnames(data_x1)[3:9]<-c("edu1","edu2","edu3","edu4","income1","income2","sex")
Process = preProcess(data_x1,method = c( "knnImpute"))
newdata = predict(Process, data_x1)
data_x1<-newdata


data_predict<-as.data.frame(cbind(data_class_smri_tbss_1114_ex$src_subject_id,data_x1$nihtbx_fluidcomp_uncorrected,data_x1[,-43],data_y1))
colnames(data_predict)[c(1,2,ncol(data_predict))]<-c("src_subject_id","nihtbx_fluidcomp_uncorrected","exts")
data_predict$exts <- ifelse(data_predict$exts == "remitting", 1, 0)
data_predict$exts<-make.names(data_predict$exts,unique = F,allow_ = T)
data_predict$exts<-as.factor(data_predict$exts)
table(data_predict$exts) 
data_predict_train<-data_predict[data_predict$src_subject_id%in%index_train,]
data_predict_test<-data_predict[data_predict$src_subject_id%in%index_test,]  

# feature selection
ctrl = rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "final",repeats = 3)
subsets = c(5,10,15,20,25,30,35,40)
set.seed(3939)
Profile = rfe(data_predict_train[,c(-1,-ncol(data_predict_train))], data_predict_train[,ncol(data_predict_train)], sizes = subsets, rfeControl = ctrl)
print(Profile)   # variables = 25
fcol <- Profile$optVariables
fcol
paste(fcol,collapse = "','")

data_predict_train1<-as.data.frame(cbind(data_predict_train[,1],data_predict_train[,fcol],data_predict_train[,ncol(data_predict_train)]))
colnames(data_predict_train1)[ncol(data_predict_train1)]<-"label"
data_predict_train1$label<-ifelse(data_predict_train1$label=="X1",1,0)
table(data_predict_train1$label) 
data_predict_test1<-as.data.frame(cbind(data_predict_test[,1],data_predict_test[,fcol],data_predict_test[,ncol(data_predict_test)]))
colnames(data_predict_test1)[ncol(data_predict_test1)]<-"label"
data_predict_test1$label<-ifelse(data_predict_test1$label=="X1",1,0)
table(data_predict_test1$label)
write.csv(data_predict_train1,"predict/new1113/data/cbcltraj_exhigh_train.csv",quote = F,row.names = F)
write.csv(data_predict_test1,"predict/new1113/data/cbcltraj_exhigh_test.csv",quote = F,row.names = F)

visit36<-data_1_1_1[data_1_1_1$visit==36,]
ext<-visit36[match(data_predict_test1$src_subject_id,visit36$src_subject_id),]
data_predict_test2<-cbind(data_predict_test1,ext$cbcl_scr_syn_external_t)
data_predict_test2$label<-ifelse(data_predict_test2$`ext$cbcl_scr_syn_external_t`<60,1,0)
data_predict_test2<-na.omit(data_predict_test2)
data_predict_test2<-data_predict_test2[,-ncol(data_predict_test2)]
write.csv(data_predict_train1,"predict/new1113/data/cbcltraj_exhigh4_train.csv",quote = F,row.names = F)
write.csv(data_predict_test2,"predict/new1113/data/cbcltraj_exhigh4_test.csv",quote = F,row.names = F)

save.image("result/r1116_exresult_high.RData")


data_predict_train1$label<-make.names(data_predict_train1$label,unique = F,allow_ = T)
data_predict_train1$label<-factor(data_predict_train1$label,levels = c("X1","X0"))
data_predict_test1$label<-make.names(data_predict_test1$label,unique = F,allow_ = T)
data_predict_test1$label<-factor(data_predict_test1$label,levels = c("X1","X0"))
data_predict_test2$label<-make.names(data_predict_test2$label,unique = F,allow_ = T)
data_predict_test2$label<-factor(data_predict_test2$label,levels = c("X1","X0"))
set.seed(1234)
datTrain <- downSample(data_predict_train1[,c(-1,-ncol(data_predict_train1))],data_predict_train1[,ncol(data_predict_train1)],yname = "label")
source(file.path("code/model_function.R"))
performance<-model_performance(datTrain,data_predict_test1,data_predict_test2)
write.csv(as.data.frame(rbind(performance[[1]],performance[[2]])),"tmp.csv",quote = F)
save.image("result/r1116_exresult_high_predict.RData")



###internalizing
load("data/r0727_2.RData")
smri_qc<-merge(smri,qc,by = c("src_subject_id","eventname"))
table(smri$eventname)
smri_qc<-smri_qc[smri_qc$imgincl_t1w_include==1,]
smri_base<-smri_qc[smri_qc$eventname=="baseline_year_1_arm_1",]
smri_2year<-smri_qc[smri_qc$eventname=="2_year_follow_up_y_arm_1",]

#baseline
index_train<-data0510_new_in[data0510_new_in$visit==0,]$src_subject_id
index_test<-df1[!df1$src_subject_id%in%df$src_subject_id,]$src_subject_id 
data0510_new_1114<-data0510[data0510$src_subject_id%in%union(index_train,index_test),] 
data_class_smri_tbss_1114<-merge(data0510_new_1114[data0510_new_1114$visit==2,-1],smri_base,by = "src_subject_id")
data_class_smri_tbss_1114<-merge(data0510_new_1114[data0510_new_1114$visit==0,c(1,6:10)],data_class_smri_tbss_1114,by = "src_subject_id")
colnames(data_class_smri_tbss_1114)[68]

index1<-data0510[data0510$visit==0&data0510$cbcl_scr_syn_internal_t>=60,]$src_subject_id
index2<-data0510[data0510$visit==1&data0510$cbcl_scr_syn_internal_t>=60,]$src_subject_id
index3<-data0510[data0510$visit==2&data0510$cbcl_scr_syn_internal_t>=60,]$src_subject_id
index4<-data0510[data0510$visit==0&data0510$cbcl_scr_syn_internal_t<60,]$src_subject_id
index5<-data0510[data0510$visit==1&data0510$cbcl_scr_syn_internal_t<60,]$src_subject_id
index6<-data0510[data0510$visit==2&data0510$cbcl_scr_syn_internal_t<60,]$src_subject_id
index7<-intersect(index1,intersect(index2,index3)) #persistent
index8<-intersect(index4,intersect(index5,index6)) #control
index9<-intersect(index1,index6) #remitting
index10<-intersect(index4,index3) #late-onset

data_class_smri_tbss_1114$class<-"fluctuation"
data_class_smri_tbss_1114[data_class_smri_tbss_1114$src_subject_id%in%index7,]$class<-"persistent"
data_class_smri_tbss_1114[data_class_smri_tbss_1114$src_subject_id%in%index8,]$class<-"control"
data_class_smri_tbss_1114[data_class_smri_tbss_1114$src_subject_id%in%index9,]$class<-"remitting"
data_class_smri_tbss_1114[data_class_smri_tbss_1114$src_subject_id%in%index10,]$class<-"late-onset"
table(data_class_smri_tbss_1114$class)


data_class_smri_tbss_1114_in<-data_class_smri_tbss_1114[data_class_smri_tbss_1114$cbcl_scr_syn_internal_t.x<60,]
table(data_class_smri_tbss_1114_in$class)

data_y1<-data_class_smri_tbss_1114_in[,ncol(data_class_smri_tbss_1114_in)]
data_x1<-data_class_smri_tbss_1114_in[,c(4:5,24,25,27,29,30,40:47,53,174,388,495,539,68:135,282:349,389:456)]
data_x1<-cbind(data_x1[,1:2],class.ind(data_x1$high.educ)[,-1],class.ind(data_x1$household.income)[,-1],class.ind(data_x1$sex)[,-1],
               class.ind(data_x1$site_id_l)[,-1],class.ind(data_x1$race_ethnicity)[,-1],data_x1[,8:ncol(data_x1)])
colnames(data_x1)[3:9]<-c("edu1","edu2","edu3","edu4","income1","income2","sex")
Process = preProcess(data_x1,method = c( "knnImpute"))
newdata = predict(Process, data_x1)
data_x1<-newdata

data_predict<-as.data.frame(cbind(data_class_smri_tbss_1114_in$src_subject_id,data_x1$nihtbx_fluidcomp_uncorrected,data_x1[,-43],data_y1))
colnames(data_predict)[c(1,2,ncol(data_predict))]<-c("src_subject_id","nihtbx_fluidcomp_uncorrected","ints")
data_predict$ints <- ifelse(data_predict$ints == "late-onset", 1, 0)
data_predict$ints<-make.names(data_predict$ints,unique = F,allow_ = T)
data_predict$ints<-as.factor(data_predict$ints)
table(data_predict$ints) 
data_predict_train<-data_predict[data_predict$src_subject_id%in%index_train,]
data_predict_test<-data_predict[data_predict$src_subject_id%in%index_test,]  

# feature selection
ctrl = rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "final",repeats = 3)
subsets = c(5,10,15,20,25,30,35,40)
set.seed(39)
Profile = rfe(data_predict_train[,c(-1,-ncol(data_predict_train))], data_predict_train[,ncol(data_predict_train)], sizes = subsets, rfeControl = ctrl)
print(Profile)   # variables = 20
fcol <- Profile$optVariables
fcol
paste(fcol,collapse = "','")

data_predict_train1<-as.data.frame(cbind(data_predict_train[,1],data_predict_train[,fcol],data_predict_train[,ncol(data_predict_train)]))
colnames(data_predict_train1)[ncol(data_predict_train1)]<-"label"
data_predict_train1$label<-ifelse(data_predict_train1$label=="X1",1,0)
table(data_predict_train1$label)
data_predict_test1<-as.data.frame(cbind(data_predict_test[,1],data_predict_test[,fcol],data_predict_test[,ncol(data_predict_test)]))
colnames(data_predict_test1)[ncol(data_predict_test1)]<-"label"
data_predict_test1$label<-ifelse(data_predict_test1$label=="X1",1,0)
table(data_predict_test1$label) 
write.csv(data_predict_train1,"predict/new1113/data/cbcltraj_inlow_train.csv",quote = F,row.names = F)
write.csv(data_predict_test1,"predict/new1113/data/cbcltraj_inlow_test.csv",quote = F,row.names = F)

visit36<-data_1_1_1[data_1_1_1$visit==36,]
int<-visit36[match(data_predict_test1$src_subject_id,visit36$src_subject_id),]
data_predict_test2<-cbind(data_predict_test1,int$cbcl_scr_syn_internal_t)
data_predict_test2$label<-ifelse(data_predict_test2$`int$cbcl_scr_syn_internal_t`>=60,1,0)
data_predict_test2<-na.omit(data_predict_test2)
data_predict_test2<-data_predict_test2[,-ncol(data_predict_test2)]
write.csv(data_predict_train1,"predict/new1113/data/cbcltraj_inlow4_train.csv",quote = F,row.names = F)
write.csv(data_predict_test2,"predict/new1113/data/cbcltraj_inlow4_test.csv",quote = F,row.names = F)

save.image("result/r1116_inresult_low.RData")

data_predict_train1$label<-make.names(data_predict_train1$label,unique = F,allow_ = T)
data_predict_train1$label<-factor(data_predict_train1$label,levels = c("X1","X0"))
data_predict_test1$label<-make.names(data_predict_test1$label,unique = F,allow_ = T)
data_predict_test1$label<-factor(data_predict_test1$label,levels = c("X1","X0"))
data_predict_test2$label<-make.names(data_predict_test2$label,unique = F,allow_ = T)
data_predict_test2$label<-factor(data_predict_test2$label,levels = c("X1","X0"))
set.seed(1234)
datTrain <- downSample(data_predict_train1[,c(-1,-ncol(data_predict_train1))],data_predict_train1[,ncol(data_predict_train1)],yname = "label")
source(file.path("code/model_function.R"))
performance<-model_performance(datTrain,data_predict_test1,data_predict_test2)
write.csv(as.data.frame(rbind(performance[[1]],performance[[2]])),"tmp.csv",quote = F)
save.image("result/r1116_inresult_low_predict.RData")



data_class_smri_tbss_1114_in<-data_class_smri_tbss_1114[data_class_smri_tbss_1114$cbcl_scr_syn_internal_t.x>=60,]
table(data_class_smri_tbss_1114_in$class) 

data_y1<-data_class_smri_tbss_1114_in[,ncol(data_class_smri_tbss_1114_in)]
data_x1<-data_class_smri_tbss_1114_in[,c(4:5,24,25,27,29,30,40:47,53,174,388,495,539,68:135,282:349,389:456)]
data_x1<-cbind(data_x1[,1:2],class.ind(data_x1$high.educ)[,-1],class.ind(data_x1$household.income)[,-1],class.ind(data_x1$sex)[,-1],
               class.ind(data_x1$site_id_l)[,-1],class.ind(data_x1$race_ethnicity)[,-1],data_x1[,8:ncol(data_x1)])
colnames(data_x1)[3:9]<-c("edu1","edu2","edu3","edu4","income1","income2","sex")
Process = preProcess(data_x1,method = c( "knnImpute"))
newdata = predict(Process, data_x1)
data_x1<-newdata

data_predict<-as.data.frame(cbind(data_class_smri_tbss_1114_in$src_subject_id,data_x1$nihtbx_fluidcomp_uncorrected,data_x1[,-43],data_y1))
colnames(data_predict)[c(1,2,ncol(data_predict))]<-c("src_subject_id","nihtbx_fluidcomp_uncorrected","ints")
data_predict$ints <- ifelse(data_predict$ints == "remitting", 1, 0)
data_predict$ints<-make.names(data_predict$ints,unique = F,allow_ = T)
data_predict$ints<-as.factor(data_predict$ints)
table(data_predict$ints) 
data_predict_train<-data_predict[data_predict$src_subject_id%in%index_train,] 
data_predict_test<-data_predict[data_predict$src_subject_id%in%index_test,]

# feature selection
ctrl = rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "final",repeats = 3)
subsets = c(5,10,15,20,25,30,35,40)
set.seed(39)
Profile = rfe(data_predict_train[,c(-1,-ncol(data_predict_train))], data_predict_train[,ncol(data_predict_train)], sizes = subsets, rfeControl = ctrl)
print(Profile)   # variables = 25
fcol <- Profile$optVariables
fcol
paste(fcol,collapse = "','")

data_predict_train1<-as.data.frame(cbind(data_predict_train[,1],data_predict_train[,fcol],data_predict_train[,ncol(data_predict_train)]))
colnames(data_predict_train1)[ncol(data_predict_train1)]<-"label"
data_predict_train1$label<-ifelse(data_predict_train1$label=="X1",1,0)
table(data_predict_train1$label)
data_predict_test1<-as.data.frame(cbind(data_predict_test[,1],data_predict_test[,fcol],data_predict_test[,ncol(data_predict_test)]))
colnames(data_predict_test1)[ncol(data_predict_test1)]<-"label"
data_predict_test1$label<-ifelse(data_predict_test1$label=="X1",1,0)
table(data_predict_test1$label) 
write.csv(data_predict_train1,"predict/new1113/data/cbcltraj_inhigh_train.csv",quote = F,row.names = F)
write.csv(data_predict_test1,"predict/new1113/data/cbcltraj_inhigh_test.csv",quote = F,row.names = F)

visit36<-data_1_1_1[data_1_1_1$visit==36,]
int<-visit36[match(data_predict_test1$src_subject_id,visit36$src_subject_id),]
data_predict_test2<-cbind(data_predict_test1,int$cbcl_scr_syn_internal_t)
data_predict_test2$label<-ifelse(data_predict_test2$`int$cbcl_scr_syn_internal_t`<60,1,0)
data_predict_test2<-na.omit(data_predict_test2)
data_predict_test2<-data_predict_test2[,-ncol(data_predict_test2)]
write.csv(data_predict_train1,"predict/new1113/data/cbcltraj_inhigh4_train.csv",quote = F,row.names = F)
write.csv(data_predict_test2,"predict/new1113/data/cbcltraj_inhigh4_test.csv",quote = F,row.names = F)

save.image("result/r1116_inresult_high.RData")


data_predict_train1$label<-make.names(data_predict_train1$label,unique = F,allow_ = T)
data_predict_train1$label<-factor(data_predict_train1$label,levels = c("X1","X0"))
data_predict_test1$label<-make.names(data_predict_test1$label,unique = F,allow_ = T)
data_predict_test1$label<-factor(data_predict_test1$label,levels = c("X1","X0"))
data_predict_test2$label<-make.names(data_predict_test2$label,unique = F,allow_ = T)
data_predict_test2$label<-factor(data_predict_test2$label,levels = c("X1","X0"))
set.seed(1234)
datTrain <- downSample(data_predict_train1[,c(-1,-ncol(data_predict_train1))],data_predict_train1[,ncol(data_predict_train1)],yname = "label")
source(file.path("code/model_function.R"))
performance<-model_performance(datTrain,data_predict_test1,data_predict_test2)
write.csv(as.data.frame(rbind(performance[[1]],performance[[2]])),"tmp.csv",quote = F)
save.image("result/r1116_inresult_high_predict.RData")


