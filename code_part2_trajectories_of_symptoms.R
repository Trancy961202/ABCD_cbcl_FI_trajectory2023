#############################################################
#############################################################
################trajectories of symptoms#####################
#############################################################
#############################################################

load("data/r0727.RData")
library(lmerTest)
library(effectsize)
fit1<-lmer(cbcl_scr_syn_external_t~visit+
             sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
             (visit|src_subject_id),data = data0510_new_ex[data0510_new_ex$class_ex=="case",],
           control = lmerControl(optimizer= "optimx",
                                 optCtrl  = list(method="nlminb")))
summary(fit1)[["coefficients"]]
rand(fit1) 
eta_squared(fit1)

fit2<-lmer(cbcl_scr_syn_external_t~visit+
             sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
             (visit|src_subject_id),data = data0510_new_ex[data0510_new_ex$class_ex=="control",],
           control = lmerControl(optimizer= "optimx",
                                 optCtrl  = list(method="nlminb")))
summary(fit2)[["coefficients"]]
rand(fit2) 
eta_squared(fit2)

fit3<-lmer(cbcl_scr_syn_internal_t~visit+
             sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
             (visit|src_subject_id),data = data0510_new_in[data0510_new_in$class_in=="case",],
           control = lmerControl(optimizer= "optimx",
                                 optCtrl  = list(method="nlminb")))
summary(fit3)[["coefficients"]]
rand(fit3) 
eta_squared(fit3)

fit4<-lmer(cbcl_scr_syn_internal_t~visit+
             sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
             (visit|src_subject_id),data = data0510_new_in[data0510_new_in$class_in=="control",],
           control = lmerControl(optimizer= "optimx",
                                 optCtrl  = list(method="nlminb")))
summary(fit4)[["coefficients"]]
rand(fit4) 
eta_squared(fit4)

fit<-fit1
exti<-ranef(fit)[["src_subject_id"]][["(Intercept)"]]
exts<-ranef(fit)[["src_subject_id"]][["visit"]]
cor.test(exti,exts)
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
cor.test(exti,exts)
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
cor.test(inti,ints)
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
cor.test(inti,ints) 
a<-ranef(fit)
aa<-as.data.frame(rownames(a[["src_subject_id"]]))
colnames(aa)<-"V1"
colnames(aa)[1]<-"src_subject_id"
com4<-merge(aa,data0510_new_in[data0510_new_in$visit==2,-1],by = "src_subject_id")
com4<-merge(data0510_new_in[data0510_new_in$visit==0,c(1,2:18)],com4,by = "src_subject_id")
com4<-cbind(com4[,1],inti,ints,com4[,-1])
colnames(com4)[1:3]<-c("src_subject_id","inti","ints")
com_in<-rbind(com3,com4)
table(com_in$class_in)
save.image("data/r0727_2.RData")