#############################################################
#############################################################
###############Demographic characteristics###################
#############################################################
#############################################################
load("data/r0727.RData")
excase<-data0510_new_ex[data0510_new_ex$class_ex=="case",]
incase<-data0510_new_in[data0510_new_in$class_in=="case",]
control1<-data0510_new_ex[data0510_new_ex$class_ex=="control",]
control2<-data0510_new_in[data0510_new_in$class_in=="control",]
#control1=control2
control<-control1

tmp<-control[control$visit==0,]
sum(!is.na(tmp$interview_age))
mean(tmp$interview_age)
sd(tmp$interview_age)
sum(!is.na(tmp$sex))
table(tmp$sex)
sum(!is.na(tmp$household.income))
table(tmp$household.income)
sum(!is.na(tmp$race_ethnicity))
table(tmp$race_ethnicity)
sum(!is.na(tmp$famhx_ss_momdad_hspd_p))
table(tmp$famhx_ss_momdad_hspd_p)

func<-function(x){sum(!is.na(x))}
func1<-function(x){mean(x,na.rm=T)}
func2<-function(x){sd(x,na.rm=T)}
round(sapply(tmp[,45:54],func2),2)

tmp<-control[control$visit==2,]
sum(!is.na(tmp$cbcl_scr_syn_totprob_t))
round(mean(tmp$cbcl_scr_syn_totprob_t),2)
round(sd(tmp$cbcl_scr_syn_totprob_t),2)
sum(!is.na(tmp$cbcl_scr_syn_external_t))
round(mean(tmp$cbcl_scr_syn_external_t),2)
round(sd(tmp$cbcl_scr_syn_external_t),2)
sum(!is.na(tmp$cbcl_scr_syn_internal_t))
round(mean(tmp$cbcl_scr_syn_internal_t),2)
round(sd(tmp$cbcl_scr_syn_internal_t),2)

t.test(excase[excase$visit==0,]$interview_age,control[control$visit==0,]$interview_age)
t.test(incase[incase$visit==0,]$interview_age,control[control$visit==0,]$interview_age)
a<-matrix(c(1051,711,2098,2193),nrow = 2,ncol = 2)
chisq.test(a)
a<-matrix(c(488,531,611,927,1144,1916),nrow = 2,ncol = 3)
chisq.test(a)
a<-matrix(c(988,150,384,34,206,2431,484,854,126,396),nrow = 2,ncol = 5)
chisq.test(a)
a<-matrix(c(235,1423,255,3861),nrow = 2,ncol = 2)
chisq.test(a)

beta_p<-data.frame()
for(i in 45:54){
  beta_p[i-44,3]<-t.test(excase[excase$visit==0,i],control[control$visit==0,i])$statistic
  beta_p[i-44,4]<-t.test(excase[excase$visit==0,i],control[control$visit==0,i])$p.value
  beta_p[i-44,5]<-t.test(incase[incase$visit==0,i],control[control$visit==0,i])$statistic
  beta_p[i-44,6]<-t.test(incase[incase$visit==0,i],control[control$visit==0,i])$p.value
}
rownames(beta_p)<-colnames(excase)[45:54]
write.csv(beta_p,"p.csv",quote = F)

beta_p<-data.frame()
for(i in 8:10){
  beta_p[i-7,1]<-t.test(excase[excase$visit==0,i],control[control$visit==0,i])$statistic
  beta_p[i-7,2]<-t.test(excase[excase$visit==0,i],control[control$visit==0,i])$p.value
  beta_p[i-7,3]<-t.test(incase[incase$visit==0,i],control[control$visit==0,i])$statistic
  beta_p[i-7,4]<-t.test(incase[incase$visit==0,i],control[control$visit==0,i])$p.value
  beta_p[i-7,5]<-t.test(excase[excase$visit==1,i],control[control$visit==1,i])$statistic
  beta_p[i-7,6]<-t.test(excase[excase$visit==1,i],control[control$visit==1,i])$p.value
  beta_p[i-7,7]<-t.test(incase[incase$visit==1,i],control[control$visit==1,i])$statistic
  beta_p[i-7,8]<-t.test(incase[incase$visit==1,i],control[control$visit==1,i])$p.value
  beta_p[i-7,9]<-t.test(excase[excase$visit==2,i],control[control$visit==2,i])$statistic
  beta_p[i-7,10]<-t.test(excase[excase$visit==2,i],control[control$visit==2,i])$p.value
  beta_p[i-7,11]<-t.test(incase[incase$visit==2,i],control[control$visit==2,i])$statistic
  beta_p[i-7,12]<-t.test(incase[incase$visit==2,i],control[control$visit==2,i])$p.value
}
rownames(beta_p)<-colnames(excase)[8:10]
write.csv(beta_p,"p.csv",quote = F)

tmp<-excase[excase$visit==0,]
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          nihtbx_fluidcomp_uncorrected,data = tmp)
summary(fit)
tmp<-incase[incase$visit==0,]
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          nihtbx_fluidcomp_uncorrected,data = tmp)
summary(fit)
tmp<-control[control$visit==0,]
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          nihtbx_fluidcomp_uncorrected,data = tmp)
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          nihtbx_fluidcomp_uncorrected,data = tmp)
summary(fit)

tmp<-excase[excase$visit==0,]
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          nihtbx_cryst_uncorrected,data = tmp)
summary(fit)
tmp<-incase[incase$visit==0,]
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          nihtbx_cryst_uncorrected,data = tmp)
summary(fit)
tmp<-control[control$visit==0,]
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          nihtbx_cryst_uncorrected,data = tmp)
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          nihtbx_cryst_uncorrected,data = tmp)
summary(fit)