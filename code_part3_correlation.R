#############################################################
#############################################################
######################correlation############################
#############################################################
#############################################################

load("data/r0727_2.RData")
###FI
#slope
p<-rep(NA,2)
fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),data = com1)
summary(fit)
p[1]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",4]
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),data = com3)
summary(fit)
p[2]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",4]
p.adjust(p,method = "fdr") 

fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+cbcl_scr_syn_external_t.x,data = com1)
summary(fit)
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+cbcl_scr_syn_internal_t.x,data = com3)
summary(fit)

fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)*cbcl_scr_syn_external_t.x,data = com1)
summary(fit)
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)*cbcl_scr_syn_internal_t.x,data = com3)
summary(fit)

p<-rep(NA,2)
fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 1,raw = T),data = com2)
summary(fit)
p[1]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 1, raw = T)",4]
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 1,raw = T),data = com4)
summary(fit)
p[2]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 1, raw = T)",4]
p.adjust(p,method = "fdr") 

#symptoms_2year
p<-rep(NA,4)
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="case",])
summary(fit)
p[1]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",4]
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="case",])
summary(fit)
p[2]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",4]
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="case",])
summary(fit)
p[3]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",4]
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="case",])
summary(fit)
p[4]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 2, raw = T)2",4]
p.adjust(p,method = "fdr")

p<-rep(NA,4)
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 1,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="control",])
summary(fit)
p[1]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 1, raw = T)",4]
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 1,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="control",])
summary(fit)
p[2]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 1, raw = T)",4]
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 1,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="control",])
summary(fit)
p[3]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 1, raw = T)",4]
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_fluidcomp_uncorrected,degree = 1,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="control",])
summary(fit)
p[4]<-summary(fit)[["coefficients"]]["poly(nihtbx_fluidcomp_uncorrected, degree = 1, raw = T)",4]
p.adjust(p,method = "fdr") 

###CI
#slope
fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 2,raw = T),data = com1)
summary(fit)
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 2,raw = T),data = com3)
summary(fit)

p<-rep(NA,2)
fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 1,raw = T),data = com2)
summary(fit)
p[1]<-summary(fit)[["coefficients"]]["poly(nihtbx_cryst_uncorrected, degree = 1, raw = T)",4]
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 1,raw = T),data = com4)
summary(fit)
p[2]<-summary(fit)[["coefficients"]]["poly(nihtbx_cryst_uncorrected, degree = 1, raw = T)",4]
p.adjust(p,method = "fdr")

#symptoms_2year
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 2,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 2,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 2,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 2,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="case",])
summary(fit)

fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 1,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="control",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 1,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="control",])
summary(fit)
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 1,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="control",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_cryst_uncorrected,degree = 1,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="control",])
summary(fit)


###total cognition
#slope
fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 2,raw = T),data = com1)
summary(fit)
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 2,raw = T),data = com3)
summary(fit)

p<-rep(NA,2)
fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 1,raw = T),data = com2)
summary(fit)
p[1]<-summary(fit)[["coefficients"]]["poly(nihtbx_totalcomp_uncorrected, degree = 1, raw = T)",4]
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 1,raw = T),data = com4)
summary(fit)
p[2]<-summary(fit)[["coefficients"]]["poly(nihtbx_totalcomp_uncorrected, degree = 1, raw = T)",4]
p.adjust(p,method = "fdr") 

#symptoms_2year
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="case",])
summary(fit)

fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 1,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="control",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 1,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="control",])
summary(fit)
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 1,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="control",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          poly(nihtbx_totalcomp_uncorrected,degree = 1,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="control",])
summary(fit)



##sensitivity analyses
fit<-lm(exts~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+high.educ+
          bmi+score+asr_scr_totprob_r+fes_y_ss_fc_pr+pmq_y_ss_mean+devhx_12a_p+birth_weight_lbs+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),data = com1)
summary(fit)
fit<-lm(ints~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+high.educ+
          bmi+score+asr_scr_totprob_r+fes_y_ss_fc_pr+pmq_y_ss_mean+devhx_12a_p+birth_weight_lbs+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),data = com3)
summary(fit)



#symptoms_2year
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          bmi+score+asr_scr_totprob_r+fes_y_ss_fc_pr+pmq_y_ss_mean+devhx_12a_p+birth_weight_lbs+high.educ+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          bmi+score+asr_scr_totprob_r+fes_y_ss_fc_pr+pmq_y_ss_mean+devhx_12a_p+birth_weight_lbs+high.educ+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_ex[data0510_new_ex$visit==2&data0510_new_ex$class_ex=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_external_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          bmi+score+asr_scr_totprob_r+fes_y_ss_fc_pr+pmq_y_ss_mean+devhx_12a_p+birth_weight_lbs+high.educ+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="case",])
summary(fit)
fit<-lm(cbcl_scr_syn_internal_t~sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+
          bmi+score+asr_scr_totprob_r+fes_y_ss_fc_pr+pmq_y_ss_mean+devhx_12a_p+birth_weight_lbs+high.educ+
          poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T),
        data = data0510_new_in[data0510_new_in$visit==2&data0510_new_in$class_in=="case",])
summary(fit)







#smri_baseline/2year/delta
data_class_smri_tbss<-merge(com1[,1:3],data_class_smri_tbss_delta[,-29:-30],by = "src_subject_id")
colnames(data_class_smri_tbss)[68]
for(i in 68:541){
  print(colnames(data_class_smri_tbss)[i])
  Q <- quantile(data_class_smri_tbss[,i], probs=c(.25, .75), na.rm = T)
  iqr <- IQR(data_class_smri_tbss[,i], na.rm = T)
  up <- Q[2]+1.5*iqr 
  low<- Q[1]-1.5*iqr
  for(j in 1:nrow(data_class_smri_tbss)){
    if((!is.na(data_class_smri_tbss[j,i])&data_class_smri_tbss[j,i]<low))
      data_class_smri_tbss[j,i]<-NA
    else if((!is.na(data_class_smri_tbss[j,i])&data_class_smri_tbss[j,i]>up))
      data_class_smri_tbss[j,i]<-NA
  }
}

for(i in 68:541){
  print(colnames(data_class_smri_tbss)[i])
  data_class_smri_tbss[,i]<-scale(data_class_smri_tbss[,i])
}


fit1<-lm(exts~smri_vol_scs_intracranialv+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(exts~smri_thick_cdk_mean+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(exts~smri_area_cdk_total+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(exts~smri_vol_cdk_total+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]

fit1<-lm(exts~smri_area_cdk_total+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+smri_vol_scs_intracranialv,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(exts~smri_vol_cdk_total+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+smri_vol_scs_intracranialv,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]

#thickness
betap_thick<-data.frame()
for(i in 68:135){
  fit1<-lm(exts~data_class_smri_tbss[,i]+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
  betap_thick[i-67,1:3]<-summary(fit1)[["coefficients"]][2,c(1,3,4)]
}
rownames(betap_thick)<-colnames(data_class_smri_tbss)[68:135]
colnames(betap_thick)<-c("exts_beta","exts_t","exts_p")
betap_thick_fdr<-betap_thick
for(i in 1){
  betap_thick_fdr[,3*i]<-p.adjust(betap_thick_fdr[,3*i],method = "fdr")
}

#area
betap_area<-data.frame()
for(i in 282:349){
  fit1<-lm(exts~data_class_smri_tbss[,i]+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
  betap_area[i-281,1:3]<-summary(fit1)[["coefficients"]][2,c(1,3,4)]
  
}
rownames(betap_area)<-colnames(data_class_smri_tbss)[282:349]
colnames(betap_area)<-c("exts_beta","exts_t","exts_p")
betap_area_fdr<-betap_area
for(i in 1){
  betap_area_fdr[,3*i]<-p.adjust(betap_area_fdr[,3*i],method = "fdr")
}

#vol
betap_vol<-data.frame()
for(i in 389:456){
  fit1<-lm(exts~data_class_smri_tbss[,i]+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
  betap_vol[i-388,1:3]<-summary(fit1)[["coefficients"]][2,c(1,3,4)]
  
}
rownames(betap_vol)<-colnames(data_class_smri_tbss)[389:456]
colnames(betap_vol)<-c("exts_beta","exts_t","exts_p")
betap_vol_fdr<-betap_vol
for(i in 1){
  betap_vol_fdr[,3*i]<-p.adjust(betap_vol_fdr[,3*i],method = "fdr")
}


#volb
#496:510,512:524,526:528,531:541
betap_volb<-data.frame()
for(i in c(496:510,512:524,526:528,531:541)){
  fit1<-lm(exts~data_class_smri_tbss[,i]+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
  betap_volb[i-495,1:3]<-summary(fit1)[["coefficients"]][2,c(1,3,4)]
}
rownames(betap_volb)<-colnames(data_class_smri_tbss)[496:541]
colnames(betap_volb)<-c("exts_beta","exts_t","exts_p")
betap_volb_fdr<-betap_volb
for(i in 1){
  betap_volb_fdr[,3*i]<-p.adjust(betap_volb_fdr[,3*i],method = "fdr")
}


fit1<-lm(exts~smri_vol_cdk_locclh+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+
           site_id_l+smri_vol_cdk_total,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(exts~poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+
           site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(smri_vol_cdk_locclh~poly(nihtbx_fluidcomp_uncorrected,degree = 2,raw = T)+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+
           site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]


data_class_smri_tbss<-merge(com3[,1:3],data_class_smri_tbss_delta[,-29:-30],by = "src_subject_id")
colnames(data_class_smri_tbss)[68]
for(i in 68:541){
  print(colnames(data_class_smri_tbss)[i])
  Q <- quantile(data_class_smri_tbss[,i], probs=c(.25, .75), na.rm = T)
  iqr <- IQR(data_class_smri_tbss[,i], na.rm = T)
  up <- Q[2]+1.5*iqr 
  low<- Q[1]-1.5*iqr
  for(j in 1:nrow(data_class_smri_tbss)){
    if((!is.na(data_class_smri_tbss[j,i])&data_class_smri_tbss[j,i]<low))
      data_class_smri_tbss[j,i]<-NA
    else if((!is.na(data_class_smri_tbss[j,i])&data_class_smri_tbss[j,i]>up))
      data_class_smri_tbss[j,i]<-NA
  }
}

for(i in 68:541){
  print(colnames(data_class_smri_tbss)[i])
  data_class_smri_tbss[,i]<-scale(data_class_smri_tbss[,i])
}


fit1<-lm(ints~smri_vol_scs_intracranialv+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(ints~smri_thick_cdk_mean+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(ints~smri_area_cdk_total+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(ints~smri_vol_cdk_total+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]

fit1<-lm(ints~smri_thick_cdk_mean+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+smri_vol_scs_intracranialv,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(ints~smri_area_cdk_total+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+smri_vol_scs_intracranialv,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]
fit1<-lm(ints~smri_vol_cdk_total+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l+smri_vol_scs_intracranialv,data = data_class_smri_tbss)
summary(fit1)[["coefficients"]]

#thickness
betap_thick<-data.frame()
for(i in 68:135){
  fit1<-lm(ints~data_class_smri_tbss[,i]+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+
             #smri_vol_scs_intracranialv+
             site_id_l,data = data_class_smri_tbss)
  betap_thick[i-67,1:3]<-summary(fit1)[["coefficients"]][2,c(1,3,4)]
}
rownames(betap_thick)<-colnames(data_class_smri_tbss)[68:135]
colnames(betap_thick)<-c("ints_beta","ints_t","ints_p")
betap_thick_fdr<-betap_thick
for(i in 1){
  betap_thick_fdr[,3*i]<-p.adjust(betap_thick_fdr[,3*i],method = "fdr")
}

#area
betap_area<-data.frame()
for(i in 282:349){
  fit1<-lm(ints~data_class_smri_tbss[,i]+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+site_id_l,data = data_class_smri_tbss)
  betap_area[i-281,1:3]<-summary(fit1)[["coefficients"]][2,c(1,3,4)]
  
}
rownames(betap_area)<-colnames(data_class_smri_tbss)[282:349]
colnames(betap_area)<-c("ints_beta","ints_t","ints_p")
betap_area_fdr<-betap_area
for(i in 1){
  betap_area_fdr[,3*i]<-p.adjust(betap_area_fdr[,3*i],method = "fdr")
}

#vol
betap_vol<-data.frame()
for(i in 389:456){
  fit1<-lm(ints~data_class_smri_tbss[,i]+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+
             #smri_vol_scs_intracranialv+
             site_id_l,data = data_class_smri_tbss)
  betap_vol[i-388,1:3]<-summary(fit1)[["coefficients"]][2,c(1,3,4)]
  
}
rownames(betap_vol)<-colnames(data_class_smri_tbss)[389:456]
colnames(betap_vol)<-c("ints_beta","ints_t","ints_p")
betap_vol_fdr<-betap_vol
for(i in 1){
  betap_vol_fdr[,3*i]<-p.adjust(betap_vol_fdr[,3*i],method = "fdr")
}


#volb
#496:510,512:524,526:528,531:541
betap_volb<-data.frame()
for(i in c(496:510,512:524,526:528,531:541)){
  fit1<-lm(ints~data_class_smri_tbss[,i]+sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+
             #smri_vol_scs_intracranialv+
             site_id_l,data = data_class_smri_tbss)
  betap_volb[i-495,1:3]<-summary(fit1)[["coefficients"]][2,c(1,3,4)]
}
rownames(betap_volb)<-colnames(data_class_smri_tbss)[496:541]
colnames(betap_volb)<-c("ints_beta","ints_t","ints_p")
betap_volb_fdr<-betap_volb
for(i in 1){
  betap_volb_fdr[,3*i]<-p.adjust(betap_volb_fdr[,3*i],method = "fdr")
}

write.csv(betap_vol_fdr,"p.csv",quote = F)