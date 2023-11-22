#############################################################
#############################################################
########################mediation############################
#############################################################
#############################################################

load("data/r0727_2.RData")
library(lm.beta)
library(effectsize)
library(boot)
##slope
#external
data_temp<-merge(com1[,1:3],data_class_smri_tbss_delta[,-29:-30],by = "src_subject_id")
#
fit01<-lm(smri_vol_cdk_locclh~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            smri_vol_scs_intracranialv,data = data_temp)
summary(fit01)
fit0<-lm(smri_vol_cdk_locclh~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,data = data_temp)
summary(fit0)
anova(fit0,fit01)
fit1<-lm(exts~smri_vol_cdk_locclh+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+
           smri_vol_scs_intracranialv,data = data_temp)
summary(fit1)
lm.beta(fit1)
fit2<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,data = data_temp)
summary(fit2)
fit21<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            smri_vol_scs_intracranialv,data = data_temp)
summary(fit21)
anova(fit2,fit21)


eta0<-eta_squared(fit0)
eta0[6,2]
eta1<-eta_squared(fit1)
eta1[1,2]
eta2<-eta_squared(fit2)
eta2[6,2]
delta_eta<-eta2[6,2]-eta1[7,2]
delta_eta/eta2[6,2]


etas<-function(formula1,formula2,data,indices){
  d<-data[indices,]
  fit1<-lm(formula1,data = d)
  fit2<-lm(formula2,data = d)
  return(eta_squared(fit2)[6,2]-eta_squared(fit1)[7,2])
}

set.seed(1234)
results<-boot(data = data_temp,statistic = etas,R = 1000,
              formula1=exts~poly(smri_vol_cdk_locclh,degree = 1,raw = T)+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,
              formula2=exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv)
a<-t.test(results[["t"]])
a[["conf.int"]]/eta2[6,2]*100


#internal
data_temp<-merge(com3[,1:3],data_class_smri_tbss_delta[,-29:-30],by = "src_subject_id")
#test association of FI and brain
fit0<-lm(smri_vol_cdk_locclh~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,data = data_temp)
summary(fit0)


fit01<-lm(smri_vol_cdk_loccrh~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            smri_vol_scs_intracranialv,data = data_temp)
summary(fit01)
fit0<-lm(smri_vol_cdk_loccrh~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,data = data_temp)
summary(fit0)
anova(fit0,fit01)
fit1<-lm(ints~smri_vol_cdk_loccrh+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+
           smri_vol_scs_intracranialv,data = data_temp)
summary(fit1)
lm.beta(fit1)
fit2<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,data = data_temp)
summary(fit2)
fit21<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            smri_vol_scs_intracranialv,data = data_temp)
summary(fit21)
anova(fit2,fit21)

eta0<-eta_squared(fit0)
eta0[6,2]
eta1<-eta_squared(fit1)
eta1[1,2]
eta2<-eta_squared(fit2)
eta2[6,2]
delta_eta<-eta2[6,2]-eta1[7,2]
delta_eta/eta2[6,2]


set.seed(1234)
results<-boot(data = data_temp,statistic = etas,R = 1000,
              formula1=ints~poly(smri_vol_cdk_loccrh,degree = 1,raw = T)+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,
              formula2=ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv)
a<-t.test(results[["t"]])
a[["conf.int"]]/eta2[6,2]*100

fit01<-lm(smri_vol_cdk_parstgrisrh~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            smri_vol_scs_intracranialv,data = data_temp)
summary(fit01)
fit0<-lm(smri_vol_cdk_parstgrisrh~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,data = data_temp)
summary(fit0)
anova(fit0,fit01)
fit1<-lm(ints~smri_vol_cdk_parstgrisrh+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+
           smri_vol_scs_intracranialv,data = data_temp)
summary(fit1)
lm.beta(fit1)
fit2<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
           poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,data = data_temp)
summary(fit2)
fit21<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
            smri_vol_scs_intracranialv,data = data_temp)
summary(fit2)
anova(fit2,fit21)


eta0<-eta_squared(fit0)
eta0[6,2]
eta1<-eta_squared(fit1)
eta1[1,2]
eta2<-eta_squared(fit2)
eta2[6,2]
delta_eta<-eta2[6,2]-eta1[7,2]
delta_eta/eta2[6,2]


set.seed(1234)
results<-boot(data = data_temp,statistic = etas,R = 1000,
              formula1=ints~poly(smri_vol_cdk_parstgrisrh,degree = 1,raw = T)+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv,
              formula2=ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+smri_vol_scs_intracranialv)
a<-t.test(results[["t"]])
a[["conf.int"]]/eta2[6,2]*100