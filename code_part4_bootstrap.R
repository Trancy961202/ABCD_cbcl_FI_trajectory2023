#############################################################
#############################################################
#######################bootstrap#############################
#############################################################
#############################################################

#correlation
setwd("D:/lqy/ABCD_trajectory")
load("data/r0705_noimpute_1.RData")
beta_p<-data.frame()
for(kk in 1:100){
  df1<-data0510[data0510$visit==0,]
  a<-as.data.frame(table(df1$src_subject_id))
  a<-as.data.frame(table(df1$rel_family_id))
  length(a[a$Freq==1,1])
  family1<-df1[df1$rel_family_id%in%a[a$Freq==1,1],]
  bb<-as.data.frame(table(family1$rel_family_id))
  family2<-df1[df1$rel_family_id%in%a[a$Freq!=1,1],]
  cc<-as.numeric(as.matrix(a[a$Freq!=1,1]))
  ccc<-a[a$Freq!=1,]
  #One person from each family was randomly selected
  df<-family1
  for(i in 1:nrow(ccc)){
    set.seed(kk)
    n<-sample.int(ccc[i,2],1)
    temp<-df1[df1$rel_family_id==ccc[i,1],]
    temp1<-temp[n,]
    df<-rbind(df,temp1)
  }
  dd<-as.data.frame(table(df$rel_family_id))
  data0510_new<-data0510[data0510$src_subject_id%in%df$src_subject_id,]
  
  index1<-data0510_new[data0510_new$visit==0&data0510_new$cbcl_scr_syn_external_t<60,]$src_subject_id
  index2<-data0510_new[data0510_new$visit==1&data0510_new$cbcl_scr_syn_external_t<60,]$src_subject_id
  index3<-data0510_new[data0510_new$visit==2&data0510_new$cbcl_scr_syn_external_t<60,]$src_subject_id
  index4<-intersect(index1,intersect(index2,index3))
  
  data0510_new$class_ex<-"case"
  data0510_new[data0510_new$src_subject_id%in%index4,]$class_ex<-"control"
  table(data0510_new$class_ex)/3
  
  index1<-data0510_new[data0510_new$visit==0&data0510_new$cbcl_scr_syn_internal_t<60,]$src_subject_id
  index2<-data0510_new[data0510_new$visit==1&data0510_new$cbcl_scr_syn_internal_t<60,]$src_subject_id
  index3<-data0510_new[data0510_new$visit==2&data0510_new$cbcl_scr_syn_internal_t<60,]$src_subject_id
  index4<-intersect(index1,intersect(index2,index3))
  
  data0510_new$class_in<-"case"
  data0510_new[data0510_new$src_subject_id%in%index4,]$class_in<-"control"
  table(data0510_new$class_in)/3
  
  
  data0510_new_ex<-data0510_new[!(data0510_new$class_ex=="control"&data0510_new$class_in=="case"),]
  table(data0510_new_ex$class_ex,data0510_new_ex$class_in)/3
  table(data0510_new_ex$class_ex)/3
  data0510_new_in<-data0510_new[!(data0510_new$class_in=="control"&data0510_new$class_ex=="case"),]
  table(data0510_new_in$class_ex,data0510_new_in$class_in)/3
  table(data0510_new_in$class_in)/3
  
  library(lmerTest)
  library(effectsize)
  fit1<-lmer(cbcl_scr_syn_external_t~visit+
               sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
               (visit|src_subject_id),data = data0510_new_ex[data0510_new_ex$class_ex=="case",],
             control = lmerControl(optimizer= "optimx",
                                   optCtrl  = list(method="nlminb")))
  
  fit2<-lmer(cbcl_scr_syn_external_t~visit+
               sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
               (visit|src_subject_id),data = data0510_new_ex[data0510_new_ex$class_ex=="control",],
             control = lmerControl(optimizer= "optimx",
                                   optCtrl  = list(method="nlminb")))
  
  fit3<-lmer(cbcl_scr_syn_internal_t~visit+
               sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
               (visit|src_subject_id),data = data0510_new_in[data0510_new_in$class_in=="case",],
             control = lmerControl(optimizer= "optimx",
                                   optCtrl  = list(method="nlminb")))
  
  fit4<-lmer(cbcl_scr_syn_internal_t~visit+
               sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
               (visit|src_subject_id),data = data0510_new_in[data0510_new_in$class_in=="control",],
             control = lmerControl(optimizer= "optimx",
                                   optCtrl  = list(method="nlminb")))
  
  fit<-fit1
  exti<-ranef(fit)[["src_subject_id"]][["(Intercept)"]]
  exts<-ranef(fit)[["src_subject_id"]][["visit"]]
  cor.test(exti,exts) #0.4230677
  a<-ranef(fit)
  aa<-as.data.frame(rownames(a[["src_subject_id"]]))
  colnames(aa)<-"V1"
  colnames(aa)[1]<-"src_subject_id"
  com1<-merge(aa,data0510_new_ex[data0510_new_ex$visit==2,-1],by = "src_subject_id")
  com1<-merge(data0510_new_ex[data0510_new_ex$visit==0,c(1,2:18)],com1,by = "src_subject_id")
  com1<-cbind(com1[,1],exti,exts,com1[,-1])
  colnames(com1)[1:3]<-c("src_subject_id","exti","exts")
  
  fit<-fit2
  exti<-ranef(fit)[["src_subject_id"]][["(Intercept)"]]
  exts<-ranef(fit)[["src_subject_id"]][["visit"]]
  cor.test(exti,exts) #-0.3977716
  a<-ranef(fit)
  aa<-as.data.frame(rownames(a[["src_subject_id"]]))
  colnames(aa)<-"V1"
  colnames(aa)[1]<-"src_subject_id"
  com2<-merge(aa,data0510_new_ex[data0510_new_ex$visit==2,-1],by = "src_subject_id")
  com2<-merge(data0510_new_ex[data0510_new_ex$visit==0,c(1,2:18)],com2,by = "src_subject_id")
  com2<-cbind(com2[,1],exti,exts,com2[,-1])
  colnames(com2)[1:3]<-c("src_subject_id","exti","exts")
  com_ex<-rbind(com1,com2)
  table(com_ex$class_ex)
  
  
  fit<-fit3
  inti<-ranef(fit)[["src_subject_id"]][["(Intercept)"]]
  ints<-ranef(fit)[["src_subject_id"]][["visit"]]
  cor.test(inti,ints) #0.1945616
  a<-ranef(fit)
  aa<-as.data.frame(rownames(a[["src_subject_id"]]))
  colnames(aa)<-"V1"
  colnames(aa)[1]<-"src_subject_id"
  com3<-merge(aa,data0510_new_in[data0510_new_in$visit==2,-1],by = "src_subject_id")
  com3<-merge(data0510_new_in[data0510_new_in$visit==0,c(1,2:18)],com3,by = "src_subject_id")
  com3<-cbind(com3[,1],inti,ints,com3[,-1])
  colnames(com3)[1:3]<-c("src_subject_id","inti","ints")
  
  fit<-fit4
  inti<-ranef(fit)[["src_subject_id"]][["(Intercept)"]]
  ints<-ranef(fit)[["src_subject_id"]][["visit"]]
  cor.test(inti,ints) #0.2092207
  a<-ranef(fit)
  aa<-as.data.frame(rownames(a[["src_subject_id"]]))
  colnames(aa)<-"V1"
  colnames(aa)[1]<-"src_subject_id"
  com4<-merge(aa,data0510_new_in[data0510_new_in$visit==2,-1],by = "src_subject_id")
  com4<-merge(data0510_new_in[data0510_new_in$visit==0,c(1,2:18)],com4,by = "src_subject_id")
  com4<-cbind(com4[,1],inti,ints,com4[,-1])
  colnames(com4)[1:3]<-c("src_subject_id","inti","ints")
  com_in<-rbind(com3,com4)
  
  
  p<-rep(NA,2)
  fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),data = com1)
  p[1]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",4]
  fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),data = com3)
  p[2]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",4]
  p.adjust(p,method = "fdr") #0.01807046 0.02936947
  beta_p[kk,1:2]<-p
  
  p<-rep(NA,2)
  fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),data = com1)
  p[1]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",1]
  fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),data = com3)
  p[2]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",1]
  p.adjust(p,method = "fdr") #
  beta_p[kk,3:4]<-p
  
}

t.test(beta_p[,1])
t.test(beta_p[,2])
t.test(beta_p[,3])
t.test(beta_p[,4])

save.image("result/result1009.RData")



#prediction
setwd("D:/lqy/ABCD_trajectory")
load("data/r0705_noimpute_1.RData")
load("data/smri.RData")
qc<-read.table("data/abcd_imgincl01.txt",header = T,stringsAsFactors = F)
smri_qc<-merge(smri,qc,by = c("src_subject_id","eventname"))
table(smri$eventname)
smri_qc<-smri_qc[smri_qc$imgincl_t1w_include==1,]
smri_base<-smri_qc[smri_qc$eventname=="baseline_year_1_arm_1",]
smri_2year<-smri_qc[smri_qc$eventname=="2_year_follow_up_y_arm_1",]
dateset_exlow<-list()
dateset_exhigh<-list()
dateset_inlow<-list()
dateset_inhigh<-list()

library(nnet)
library(caret)
for(kk in 1:100){
  df1<-data0510[data0510$visit==0,]
  a<-as.data.frame(table(df1$src_subject_id))
  a<-as.data.frame(table(df1$rel_family_id))
  length(a[a$Freq==1,1])
  family1<-df1[df1$rel_family_id%in%a[a$Freq==1,1],]
  bb<-as.data.frame(table(family1$rel_family_id))
  family2<-df1[df1$rel_family_id%in%a[a$Freq!=1,1],]
  cc<-as.numeric(as.matrix(a[a$Freq!=1,1]))
  ccc<-a[a$Freq!=1,]
  #One person from each family was randomly selected
  df<-family1
  for(i in 1:nrow(ccc)){
    set.seed(kk)
    n<-sample.int(ccc[i,2],1)
    temp<-df1[df1$rel_family_id==ccc[i,1],]
    temp1<-temp[n,]
    df<-rbind(df,temp1)
  }
  dd<-as.data.frame(table(df$rel_family_id))
  data0510_new<-data0510[data0510$src_subject_id%in%df$src_subject_id,]
  
  index1<-data0510_new[data0510_new$visit==0&data0510_new$cbcl_scr_syn_external_t<60,]$src_subject_id
  index2<-data0510_new[data0510_new$visit==1&data0510_new$cbcl_scr_syn_external_t<60,]$src_subject_id
  index3<-data0510_new[data0510_new$visit==2&data0510_new$cbcl_scr_syn_external_t<60,]$src_subject_id
  index4<-intersect(index1,intersect(index2,index3))
  
  data0510_new$class_ex<-"case"
  data0510_new[data0510_new$src_subject_id%in%index4,]$class_ex<-"control"
  table(data0510_new$class_ex)/3
  
  index1<-data0510_new[data0510_new$visit==0&data0510_new$cbcl_scr_syn_internal_t<60,]$src_subject_id
  index2<-data0510_new[data0510_new$visit==1&data0510_new$cbcl_scr_syn_internal_t<60,]$src_subject_id
  index3<-data0510_new[data0510_new$visit==2&data0510_new$cbcl_scr_syn_internal_t<60,]$src_subject_id
  index4<-intersect(index1,intersect(index2,index3))
  
  data0510_new$class_in<-"case"
  data0510_new[data0510_new$src_subject_id%in%index4,]$class_in<-"control"
  table(data0510_new$class_in)/3
  
  
  data0510_new_ex<-data0510_new[!(data0510_new$class_ex=="control"&data0510_new$class_in=="case"),]
  table(data0510_new_ex$class_ex,data0510_new_ex$class_in)/3
  table(data0510_new_ex$class_ex)/3
  data0510_new_in<-data0510_new[!(data0510_new$class_in=="control"&data0510_new$class_ex=="case"),]
  table(data0510_new_in$class_ex,data0510_new_in$class_in)/3
  table(data0510_new_in$class_in)/3
  
  #exlow
  #baseline
  index_train<-data0510_new_ex[data0510_new_ex$visit==0,]$src_subject_id
  index_test<-df1[!df1$src_subject_id%in%df$src_subject_id,]$src_subject_id 
  data0510_new_1114<-data0510[data0510$src_subject_id%in%union(index_train,index_test),] 
  data_class_smri_tbss_1114<-merge(data0510_new_1114[data0510_new_1114$visit==2,-1],smri_base,by = "src_subject_id")
  data_class_smri_tbss_1114<-merge(data0510_new_1114[data0510_new_1114$visit==0,c(1,6:10)],data_class_smri_tbss_1114,by = "src_subject_id")
  
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
  data_class_smri_tbss_1114_ex<-data_class_smri_tbss_1114[data_class_smri_tbss_1114$cbcl_scr_syn_external_t.x<60,]
  
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
  data_predict_train<-data_predict[data_predict$src_subject_id%in%index_train,]
  data_predict_test<-data_predict[data_predict$src_subject_id%in%index_test,] 
  
  fcol<-c('nihtbx_fluidcomp_uncorrected','cbcl_scr_syn_internal_t.x','cbcl_scr_syn_external_t.x','smri_vol_cdk_paracnrh','smri_vol_cdk_parsopclh','asr_scr_totprob_r','smri_area_cdk_sutmlh','smri_vol_cdk_cuneuslh','smri_vol_cdk_precnlh','smri_area_cdk_locclh')
  data_predict_train1<-as.data.frame(cbind(data_predict_train[,1],data_predict_train[,fcol],data_predict_train[,ncol(data_predict_train)]))
  colnames(data_predict_train1)[ncol(data_predict_train1)]<-"label"
  data_predict_train1$label<-ifelse(data_predict_train1$label=="X1",1,0)
  data_predict_test1<-as.data.frame(cbind(data_predict_test[,1],data_predict_test[,fcol],data_predict_test[,ncol(data_predict_test)]))
  colnames(data_predict_test1)[ncol(data_predict_test1)]<-"label"
  data_predict_test1$label<-ifelse(data_predict_test1$label=="X1",1,0)
  file_path <- sprintf("predict/new1113/data/exlow/cbcltraj_exlow_train_%s.csv", kk)
  write.csv(data_predict_train1,sprintf("predict/new1113/data/exlow/cbcltraj_exlow_train_%s.csv", kk),quote = F,row.names = F)
  write.csv(data_predict_test1,sprintf("predict/new1113/data/exlow/cbcltraj_exlow_test_%s.csv", kk),quote = F,row.names = F)
  
  visit36<-data_1_1_1[data_1_1_1$visit==36,]
  ext<-visit36[match(data_predict_test1$src_subject_id,visit36$src_subject_id),]
  data_predict_test2<-cbind(data_predict_test1,ext$cbcl_scr_syn_external_t)
  data_predict_test2$label<-ifelse(data_predict_test2$`ext$cbcl_scr_syn_external_t`>=60,1,0)
  data_predict_test2<-na.omit(data_predict_test2)
  data_predict_test2<-data_predict_test2[,-ncol(data_predict_test2)]
  write.csv(data_predict_train1,sprintf("predict/new1113/data/exlow/cbcltraj_exlow4_train_%s.csv", kk),quote = F,row.names = F)
  write.csv(data_predict_test2,sprintf("predict/new1113/data/exlow/cbcltraj_exlow4_test_%s.csv", kk),quote = F,row.names = F)
  dateset_exlow[[kk]]<-list(data_predict_train1,data_predict_test1,data_predict_test2)
  
  #exhigh
  data_class_smri_tbss_1114_ex<-data_class_smri_tbss_1114[data_class_smri_tbss_1114$cbcl_scr_syn_external_t.x>=60,]
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
  data_predict_train<-data_predict[data_predict$src_subject_id%in%index_train,] 
  data_predict_test<-data_predict[data_predict$src_subject_id%in%index_test,] 
  
  fcol<-c('nihtbx_fluidcomp_uncorrected','cbcl_scr_syn_external_t.x','smri_thick_cdk_ptcaterh','smri_thick_cdk_smrh','cbcl_scr_syn_internal_t.x','smri_vol_cdk_lobfrlh','smri_thick_cdk_mobfrlh','smri_vol_cdk_parsobisrh','asr_scr_totprob_r','smri_vol_cdk_parahpallh','smri_vol_cdk_insularh','smri_thick_cdk_fusiformlh','smri_vol_cdk_insulalh','smri_thick_cdk_linguallh','smri_thick_cdk_supllh','smri_thick_cdk_periccrh','smri_area_cdk_trvtmrh','smri_thick_cdk_smlh','smri_thick_cdk_sufrlh','smri_thick_cdk_rrmdfrrh','smri_area_cdk_sutmlh','smri_vol_cdk_rracaterh','smri_area_cdk_suplrh','smri_area_cdk_postcnlh','smri_area_cdk_smlh')
  data_predict_train1<-as.data.frame(cbind(data_predict_train[,1],data_predict_train[,fcol],data_predict_train[,ncol(data_predict_train)]))
  colnames(data_predict_train1)[ncol(data_predict_train1)]<-"label"
  data_predict_train1$label<-ifelse(data_predict_train1$label=="X1",1,0)
  data_predict_test1<-as.data.frame(cbind(data_predict_test[,1],data_predict_test[,fcol],data_predict_test[,ncol(data_predict_test)]))
  colnames(data_predict_test1)[ncol(data_predict_test1)]<-"label"
  data_predict_test1$label<-ifelse(data_predict_test1$label=="X1",1,0)
  write.csv(data_predict_train1,sprintf("predict/new1113/data/exhigh/cbcltraj_exhigh_train_%s.csv", kk),quote = F,row.names = F)
  write.csv(data_predict_test1,sprintf("predict/new1113/data/exhigh/cbcltraj_exhigh_test_%s.csv", kk),quote = F,row.names = F)
  
  visit36<-data_1_1_1[data_1_1_1$visit==36,]
  ext<-visit36[match(data_predict_test1$src_subject_id,visit36$src_subject_id),]
  data_predict_test2<-cbind(data_predict_test1,ext$cbcl_scr_syn_external_t)
  data_predict_test2$label<-ifelse(data_predict_test2$`ext$cbcl_scr_syn_external_t`<60,1,0)
  data_predict_test2<-na.omit(data_predict_test2)
  data_predict_test2<-data_predict_test2[,-ncol(data_predict_test2)]
  write.csv(data_predict_train1,sprintf("predict/new1113/data/exhigh/cbcltraj_exhigh4_train_%s.csv", kk),quote = F,row.names = F)
  write.csv(data_predict_test2,sprintf("predict/new1113/data/exhigh/cbcltraj_exhigh4_test_%s.csv", kk),quote = F,row.names = F)
  dateset_exhigh[[kk]]<-list(data_predict_train1,data_predict_test1,data_predict_test2)
  
  #inlow
  index_train<-data0510_new_in[data0510_new_in$visit==0,]$src_subject_id
  index_test<-df1[!df1$src_subject_id%in%df$src_subject_id,]$src_subject_id 
  data0510_new_1114<-data0510[data0510$src_subject_id%in%union(index_train,index_test),] 
  data_class_smri_tbss_1114<-merge(data0510_new_1114[data0510_new_1114$visit==2,-1],smri_base,by = "src_subject_id")
  data_class_smri_tbss_1114<-merge(data0510_new_1114[data0510_new_1114$visit==0,c(1,6:10)],data_class_smri_tbss_1114,by = "src_subject_id")
  
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
  
  data_class_smri_tbss_1114_in<-data_class_smri_tbss_1114[data_class_smri_tbss_1114$cbcl_scr_syn_internal_t.x<60,]
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
  data_predict_train<-data_predict[data_predict$src_subject_id%in%index_train,] 
  data_predict_test<-data_predict[data_predict$src_subject_id%in%index_test,]
  
  fcol<-c('nihtbx_fluidcomp_uncorrected','cbcl_scr_syn_external_t.x','cbcl_scr_syn_internal_t.x','asr_scr_totprob_r','smri_area_cdk_locclh','smri_thick_cdk_banksstsrh','smri_vol_cdk_parsopclh','smri_vol_cdk_cdacatelh','smri_vol_cdk_fusiformrh','smri_vol_cdk_banksstsrh','smri_thick_cdk_sutmlh','smri_thick_cdk_suplrh','smri_thick_cdk_smlh','smri_area_cdk_suplrh','smri_vol_cdk_ihcaterh','smri_thick_cdk_lingualrh','smri_area_cdk_insularh','smri_vol_cdk_iftmlh','smri_vol_cdk_fusiformlh','smri_vol_cdk_ifplrh')
  data_predict_train1<-as.data.frame(cbind(data_predict_train[,1],data_predict_train[,fcol],data_predict_train[,ncol(data_predict_train)]))
  colnames(data_predict_train1)[ncol(data_predict_train1)]<-"label"
  data_predict_train1$label<-ifelse(data_predict_train1$label=="X1",1,0)
  data_predict_test1<-as.data.frame(cbind(data_predict_test[,1],data_predict_test[,fcol],data_predict_test[,ncol(data_predict_test)]))
  colnames(data_predict_test1)[ncol(data_predict_test1)]<-"label"
  data_predict_test1$label<-ifelse(data_predict_test1$label=="X1",1,0)
  write.csv(data_predict_train1,sprintf("predict/new1113/data/inlow/cbcltraj_inlow_train_%s.csv", kk),quote = F,row.names = F)
  write.csv(data_predict_test1,sprintf("predict/new1113/data/inlow/cbcltraj_inlow_test_%s.csv", kk),quote = F,row.names = F)
  
  visit36<-data_1_1_1[data_1_1_1$visit==36,]
  int<-visit36[match(data_predict_test1$src_subject_id,visit36$src_subject_id),]
  data_predict_test2<-cbind(data_predict_test1,int$cbcl_scr_syn_internal_t)
  data_predict_test2$label<-ifelse(data_predict_test2$`int$cbcl_scr_syn_internal_t`>=60,1,0)
  data_predict_test2<-na.omit(data_predict_test2)
  data_predict_test2<-data_predict_test2[,-ncol(data_predict_test2)]
  write.csv(data_predict_train1,sprintf("predict/new1113/data/inlow/cbcltraj_inlow4_train_%s.csv", kk),quote = F,row.names = F)
  write.csv(data_predict_test2,sprintf("predict/new1113/data/inlow/cbcltraj_inlow4_test_%s.csv", kk),quote = F,row.names = F)
  dateset_inlow[[kk]]<-list(data_predict_train1,data_predict_test1,data_predict_test2)
  
  #inhigh
  data_class_smri_tbss_1114_in<-data_class_smri_tbss_1114[data_class_smri_tbss_1114$cbcl_scr_syn_internal_t.x>=60,]
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
  data_predict_train<-data_predict[data_predict$src_subject_id%in%index_train,] 
  data_predict_test<-data_predict[data_predict$src_subject_id%in%index_test,]  
  
  fcol<-c('nihtbx_fluidcomp_uncorrected','cbcl_scr_syn_internal_t.x','asr_scr_totprob_r','cbcl_scr_syn_external_t.x','site03','smri_vol_cdk_cdacatelh','smri_thick_cdk_mobfrrh','smri_vol_cdk_parsopcrh','smri_area_cdk_tmpolelh','smri_area_cdk_parsobisrh','smri_area_cdk_periccrh','smri_vol_cdk_precnrh','smri_vol_cdk_paracnlh','smri_vol_cdk_mdtmlh','smri_thick_cdk_ihcatelh','smri_area_cdk_cdacatelh','Asian','smri_area_cdk_mdtmlh','smri_thick_cdk_sutmrh','smri_thick_cdk_periccrh','smri_vol_cdk_sufrrh','smri_vol_cdk_ihcaterh','smri_area_cdk_precnlh','sex','smri_vol_cdk_lobfrrh')
  data_predict_train1<-as.data.frame(cbind(data_predict_train[,1],data_predict_train[,fcol],data_predict_train[,ncol(data_predict_train)]))
  colnames(data_predict_train1)[ncol(data_predict_train1)]<-"label"
  data_predict_train1$label<-ifelse(data_predict_train1$label=="X1",1,0)
  data_predict_test1<-as.data.frame(cbind(data_predict_test[,1],data_predict_test[,fcol],data_predict_test[,ncol(data_predict_test)]))
  colnames(data_predict_test1)[ncol(data_predict_test1)]<-"label"
  data_predict_test1$label<-ifelse(data_predict_test1$label=="X1",1,0)
  write.csv(data_predict_train1,sprintf("predict/new1113/data/inhigh/cbcltraj_inhigh_train_%s.csv", kk),quote = F,row.names = F)
  write.csv(data_predict_test1,sprintf("predict/new1113/data/inhigh/cbcltraj_inhigh_test_%s.csv", kk),quote = F,row.names = F)
  
  visit36<-data_1_1_1[data_1_1_1$visit==36,]
  int<-visit36[match(data_predict_test1$src_subject_id,visit36$src_subject_id),]
  data_predict_test2<-cbind(data_predict_test1,int$cbcl_scr_syn_internal_t)
  data_predict_test2$label<-ifelse(data_predict_test2$`int$cbcl_scr_syn_internal_t`<60,1,0)
  data_predict_test2<-na.omit(data_predict_test2)
  data_predict_test2<-data_predict_test2[,-ncol(data_predict_test2)]
  write.csv(data_predict_train1,sprintf("predict/new1113/data/inhigh/cbcltraj_inhigh4_train_%s.csv", kk),quote = F,row.names = F)
  write.csv(data_predict_test2,sprintf("predict/new1113/data/inhigh/cbcltraj_inhigh4_test_%s.csv", kk),quote = F,row.names = F)
  dateset_inhigh[[kk]]<-list(data_predict_train1,data_predict_test1,data_predict_test2)
  
  print(kk)
}
save.image("result/r1116_sample100.RData")



performance_exlow<-list()
performance_exhigh<-list()
performance_inlow<-list()
performance_inhigh<-list()
for(i in 1:100){
  #exlow
  data_predict_train1<-dateset_exlow[[i]][[1]]
  data_predict_test1<-dateset_exlow[[i]][[2]]
  data_predict_test2<-dateset_exlow[[i]][[3]]
  
  data_predict_train1$label<-make.names(data_predict_train1$label,unique = F,allow_ = T)
  data_predict_train1$label<-factor(data_predict_train1$label,levels = c("X1","X0"))
  data_predict_test1$label<-make.names(data_predict_test1$label,unique = F,allow_ = T)
  data_predict_test1$label<-factor(data_predict_test1$label,levels = c("X1","X0"))
  data_predict_test2$label<-make.names(data_predict_test2$label,unique = F,allow_ = T)
  data_predict_test2$label<-factor(data_predict_test2$label,levels = c("X1","X0"))
  set.seed(1234)
  datTrain <- downSample(data_predict_train1[,c(-1,-ncol(data_predict_train1))],data_predict_train1[,ncol(data_predict_train1)],yname = "label")
  source(file.path("code/model_function.R"))
  performance_exlow[[i]]<-model_performance(datTrain,data_predict_test1,data_predict_test2)
  
  #exhigh
  data_predict_train1<-dateset_exhigh[[i]][[1]]
  data_predict_test1<-dateset_exhigh[[i]][[2]]
  data_predict_test2<-dateset_exhigh[[i]][[3]]
  
  data_predict_train1$label<-make.names(data_predict_train1$label,unique = F,allow_ = T)
  data_predict_train1$label<-factor(data_predict_train1$label,levels = c("X1","X0"))
  data_predict_test1$label<-make.names(data_predict_test1$label,unique = F,allow_ = T)
  data_predict_test1$label<-factor(data_predict_test1$label,levels = c("X1","X0"))
  data_predict_test2$label<-make.names(data_predict_test2$label,unique = F,allow_ = T)
  data_predict_test2$label<-factor(data_predict_test2$label,levels = c("X1","X0"))
  set.seed(1234)
  datTrain <- downSample(data_predict_train1[,c(-1,-ncol(data_predict_train1))],data_predict_train1[,ncol(data_predict_train1)],yname = "label")
  source(file.path("code/model_function.R"))
  performance_exhigh[[i]]<-model_performance(datTrain,data_predict_test1,data_predict_test2)
  
  #inlow
  data_predict_train1<-dateset_inlow[[i]][[1]]
  data_predict_test1<-dateset_inlow[[i]][[2]]
  data_predict_test2<-dateset_inlow[[i]][[3]]
  
  data_predict_train1$label<-make.names(data_predict_train1$label,unique = F,allow_ = T)
  data_predict_train1$label<-factor(data_predict_train1$label,levels = c("X1","X0"))
  data_predict_test1$label<-make.names(data_predict_test1$label,unique = F,allow_ = T)
  data_predict_test1$label<-factor(data_predict_test1$label,levels = c("X1","X0"))
  data_predict_test2$label<-make.names(data_predict_test2$label,unique = F,allow_ = T)
  data_predict_test2$label<-factor(data_predict_test2$label,levels = c("X1","X0"))
  set.seed(1234)
  datTrain <- downSample(data_predict_train1[,c(-1,-ncol(data_predict_train1))],data_predict_train1[,ncol(data_predict_train1)],yname = "label")
  source(file.path("code/model_function.R"))
  performance_inlow[[i]]<-model_performance(datTrain,data_predict_test1,data_predict_test2)
  
  #inhigh
  data_predict_train1<-dateset_inhigh[[i]][[1]]
  data_predict_test1<-dateset_inhigh[[i]][[2]]
  data_predict_test2<-dateset_inhigh[[i]][[3]]
  
  data_predict_train1$label<-make.names(data_predict_train1$label,unique = F,allow_ = T)
  data_predict_train1$label<-factor(data_predict_train1$label,levels = c("X1","X0"))
  data_predict_test1$label<-make.names(data_predict_test1$label,unique = F,allow_ = T)
  data_predict_test1$label<-factor(data_predict_test1$label,levels = c("X1","X0"))
  data_predict_test2$label<-make.names(data_predict_test2$label,unique = F,allow_ = T)
  data_predict_test2$label<-factor(data_predict_test2$label,levels = c("X1","X0"))
  set.seed(1234)
  datTrain <- downSample(data_predict_train1[,c(-1,-ncol(data_predict_train1))],data_predict_train1[,ncol(data_predict_train1)],yname = "label")
  source(file.path("code/model_function.R"))
  performance_inhigh[[i]]<-model_performance(datTrain,data_predict_test1,data_predict_test2)
  
}
save.image("result/r1118_predict100_result.RData")


load("result/r1118_predict100_result.RData")
performance_exlow_2year<-data.frame()
performance_exlow_3year<-data.frame()
performance_exhigh_2year<-data.frame()
performance_exhigh_3year<-data.frame()
performance_inlow_2year<-data.frame()
performance_inlow_3year<-data.frame()
performance_inhigh_2year<-data.frame()
performance_inhigh_3year<-data.frame()
for(i in 1:100){
  performance_exlow_2year<-rbind(performance_exlow_2year,performance_exlow[[i]][[1]])
  performance_exlow_3year<-rbind(performance_exlow_3year,performance_exlow[[i]][[2]])
  performance_exhigh_2year<-rbind(performance_exhigh_2year,performance_exhigh[[i]][[1]])
  performance_exhigh_3year<-rbind(performance_exhigh_3year,performance_exhigh[[i]][[2]])
  performance_inlow_2year<-rbind(performance_inlow_2year,performance_inlow[[i]][[1]])
  performance_inlow_3year<-rbind(performance_inlow_3year,performance_inlow[[i]][[2]])
  performance_inhigh_2year<-rbind(performance_inhigh_2year,performance_inhigh[[i]][[1]])
  performance_inhigh_3year<-rbind(performance_inhigh_3year,performance_inhigh[[i]][[2]])
}


perf<-list(performance_exlow_2year,performance_exhigh_2year,performance_inlow_2year,performance_inhigh_2year,
           performance_exlow_3year,performance_exhigh_3year,performance_inlow_3year,performance_inhigh_3year)
tmp<-data.frame()
for(j in 1:8){
  for(i in 1:4){
    aaaa<-t.test(perf[[j]][,(8*i-7)],perf[[j]][,(8*i-3)],paired = T)
    tmp[j,(12*i-11)]<-aaaa$estimate
    tmp[j,(12*i-10)]<-aaaa$statistic
    tmp[j,(12*i-9)]<-aaaa$p.value
    aaaa<-t.test(perf[[j]][,(8*i-6)],perf[[j]][,(8*i-2)],paired = T)
    tmp[j,(12*i-8)]<-aaaa$estimate
    tmp[j,(12*i-7)]<-aaaa$statistic
    tmp[j,(12*i-6)]<-aaaa$p.value
    aaaa<-t.test(perf[[j]][,(8*i-5)],perf[[j]][,(8*i-1)],paired = T)
    tmp[j,(12*i-5)]<-aaaa$estimate
    tmp[j,(12*i-4)]<-aaaa$statistic
    tmp[j,(12*i-3)]<-aaaa$p.value
    aaaa<-t.test(perf[[j]][,(8*i-4)],perf[[j]][,8*i],paired = T)
    tmp[j,(12*i-2)]<-aaaa$estimate
    tmp[j,(12*i-1)]<-aaaa$statistic
    tmp[j,12*i]<-aaaa$p.value
  }
}
write.csv(tmp,"tmp.csv",quote = F)


performance_exlow_2year<-read.csv("predict/new1113/result/new/ensemble_valid_exlow.csv",header = F)
performance_exlow_3year<-read.csv("predict/new1113/result/new/ensemble_valid_exlow4.csv",header = F)
performance_exhigh_2year<-read.csv("predict/new1113/result/new/ensemble_valid_exhigh.csv",header = F)
performance_exhigh_3year<-read.csv("predict/new1113/result/new/ensemble_valid_exhigh4.csv",header = F)
performance_inlow_2year<-read.csv("predict/new1113/result/new/ensemble_valid_inlow.csv",header = F)
performance_inlow_3year<-read.csv("predict/new1113/result/new/ensemble_valid_inlow4.csv",header = F)
performance_inhigh_2year<-read.csv("predict/new1113/result/new/ensemble_valid_inhigh.csv",header = F)
performance_inhigh_3year<-read.csv("predict/new1113/result/new/ensemble_valid_inhigh4.csv",header = F)
perf<-list(performance_exlow_2year,performance_exhigh_2year,performance_inlow_2year,performance_inhigh_2year,
           performance_exlow_3year,performance_exhigh_3year,performance_inlow_3year,performance_inhigh_3year)
tmp<-data.frame()
for(j in 1:8){
  for(i in 1){
    aaaa<-t.test(perf[[j]][,(8*i-7)],perf[[j]][,(8*i-3)],paired = T)
    tmp[j,(12*i-11)]<-aaaa$estimate
    tmp[j,(12*i-10)]<-aaaa$statistic
    tmp[j,(12*i-9)]<-aaaa$p.value
    aaaa<-t.test(perf[[j]][,(8*i-6)],perf[[j]][,(8*i-2)],paired = T)
    tmp[j,(12*i-8)]<-aaaa$estimate
    tmp[j,(12*i-7)]<-aaaa$statistic
    tmp[j,(12*i-6)]<-aaaa$p.value
    aaaa<-t.test(perf[[j]][,(8*i-5)],perf[[j]][,(8*i-1)],paired = T)
    tmp[j,(12*i-5)]<-aaaa$estimate
    tmp[j,(12*i-4)]<-aaaa$statistic
    tmp[j,(12*i-3)]<-aaaa$p.value
    aaaa<-t.test(perf[[j]][,(8*i-4)],perf[[j]][,8*i],paired = T)
    tmp[j,(12*i-2)]<-aaaa$estimate
    tmp[j,(12*i-1)]<-aaaa$statistic
    tmp[j,12*i]<-aaaa$p.value
  }
}
write.csv(tmp,"tmp.csv",quote = F)

library(boot)
bootstrap_ci <- boot(performance_inhigh_3year$ppv_rf, function(x, i) x[i], R = 100)
boot.ci(bootstrap_ci, type = c("norm","basic", "stud", "perc"))
t.test(performance_exlow_2year$V1,performance_exlow_2year$V5,paired = T)
