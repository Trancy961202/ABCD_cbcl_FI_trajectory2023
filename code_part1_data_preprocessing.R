rm(list = ls())
library(dplyr)
library(matrixStats)

#########################################
#########################################
###########data preprocessing############
#########################################
#########################################

###corvariables
nda = readRDS("nda3.0.Rds")
corvariables = nda[,c(which(names(nda)=="src_subject_id"),which(names(nda)=='high.educ'),
                      which(names(nda)=='household.income'),which(names(nda)=='interview_age'),
                      which(names(nda)=='sex'),which(names(nda)=='rel_family_id'),
                      which(names(nda)=='site_id_l'),which(names(nda)=="event_name"),
                      which(names(nda)=='race_ethnicity'),which(names(nda)=='anthro_1_height_in'),
                      which(names(nda)=='anthro_2_height_in'),which(names(nda)=='anthro_3_height_in'),
                      which(names(nda)=='anthro_weight1_lb'),which(names(nda)=='anthro_weight2_lb'),
                      which(names(nda)=='anthro_weight3_lb'))]
corvariables_total<-corvariables
corvariables<-corvariables[corvariables$event_name=="baseline_year_1_arm_1",]
corvariables$site_id_l<-factor(corvariables$site_id_l,levels = paste("site",c(paste(0,1:9,sep = ""),10:22),sep = ""))
table(corvariables$site_id_l)
summary(corvariables)
for(i in 10:12){
  for(j in 1:nrow(corvariables)){
    if(!is.na(corvariables[j,i])&corvariables[j,i]<30){
      corvariables[j,i]<-NA
    }
  }
}
for(i in 13:15){
  for(j in 1:nrow(corvariables)){
    if(!is.na(corvariables[j,i])&corvariables[j,i]<20){
      corvariables[j,i]<-NA
    }
  }
}

#BMI
corvariables$weightmean<-rowMeans(corvariables[,13:15],na.rm=T)
corvariables$weightsd<-rowSds(as.matrix(corvariables[,13:15]),na.rm=T)
for(j in 1:nrow(corvariables)){
  if(!is.na(corvariables[j,]$weightsd)&corvariables[j,]$weightsd>5){
    corvariables[j,]$weightmean<-NA
  }
}
corvariables$heightmean<-rowMeans(corvariables[,10:12],na.rm=T)
corvariables$bmi<-(corvariables$weightmean/2.20462)/(2.54*corvariables$heightmean/100)**2
hist(corvariables$bmi,breaks = 20)

#puberty
puberty<-read.table("abcd_ypdms01.txt",header = T,stringsAsFactors = F)
puberty<-puberty[puberty$eventname=="baseline_year_1_arm_1",]
for (i in 1:nrow(puberty)) {
  for(j in 1:ncol(puberty))
    if(!is.na(puberty[i,j])&puberty[i,j]==999|!is.na(puberty[i,j])&puberty[i,j]==777)
      puberty[i,j]<-NA
}
puberty$score<-NA
for(i in 1:nrow(puberty)){
  if(puberty[i,]$sex=="M"){
    puberty[i,]$score<-puberty[i,]$pds_bdyhair_y+puberty[i,]$pds_m4_y+puberty[i,]$pds_m5_y
  }
  else if(puberty[i,]$sex=="F"){
    puberty[i,]$score<-puberty[i,]$pds_bdyhair_y+puberty[i,]$pds_f4_2_y+puberty[i,]$pds_f5_y
  }
}

a<-corvariables
b<-puberty[,c(5,41)]
c<-merge(a,b,by="src_subject_id",all = T)

#mental_health
mental_health<-read.table("abcd_fhxssp01.txt",header = T,stringsAsFactors = F)
b<-as.data.frame(cbind(mental_health$src_subject_id,mental_health$famhx_ss_momdad_hspd_p))
names(b)<-c("src_subject_id","famhx_ss_momdad_hspd_p")
d<-merge(c,b,by="src_subject_id",all = T)

#asr/fes/premature
asrs<-read.table("abcd_asrs01.txt",header = T)
data_asrs_base<-asrs[asrs$eventname=="baseline_year_1_arm_1",c("src_subject_id","asr_scr_totprob_r")]
fes<-read.table("abcd_sscey01.txt",header = T)
data_fes_base<-fes[fes$eventname=="baseline_year_1_arm_1",c("src_subject_id","fes_y_ss_fc_pr","pmq_y_ss_mean")]
dhx<-read.table("dhx01.txt",header = T)
data_dhx_base<-dhx[,c("src_subject_id","devhx_12a_p","birth_weight_lbs")]
d<-cbind(d,data_asrs_base[match(d$src_subject_id,data_asrs_base$src_subject_id),],data_fes_base[match(d$src_subject_id,data_fes_base$src_subject_id),2:3],data_dhx_base[match(d$src_subject_id,data_dhx_base$src_subject_id),2:3])
d<-d[,-22]

#cbcl
cbcls<-read.table("abcd_cbcls01.txt",header = T)
data_cbcls_base<-cbcls[cbcls$eventname=="baseline_year_1_arm_1",c("src_subject_id","eventname","interview_age","sex",
                                                                  "cbcl_scr_dsm5_anxdisord_t",
                                                                  "cbcl_scr_dsm5_depress_t","cbcl_scr_dsm5_somaticpr_t",
                                                                  "cbcl_scr_dsm5_adhd_t","cbcl_scr_dsm5_opposit_t",
                                                                  "cbcl_scr_dsm5_conduct_t","cbcl_scr_syn_external_t","cbcl_scr_syn_internal_t","cbcl_scr_syn_totprob_t")]
data_cbcls_1year<-cbcls[cbcls$eventname=="1_year_follow_up_y_arm_1",c("src_subject_id","eventname","interview_age","sex",
                                                                      "cbcl_scr_dsm5_anxdisord_t",
                                                                      "cbcl_scr_dsm5_depress_t","cbcl_scr_dsm5_somaticpr_t",
                                                                      "cbcl_scr_dsm5_adhd_t","cbcl_scr_dsm5_opposit_t",
                                                                      "cbcl_scr_dsm5_conduct_t","cbcl_scr_syn_external_t","cbcl_scr_syn_internal_t","cbcl_scr_syn_totprob_t")]
data_cbcls_2year<-cbcls[cbcls$eventname=="2_year_follow_up_y_arm_1",c("src_subject_id","eventname","interview_age","sex",
                                                                      "cbcl_scr_dsm5_anxdisord_t",
                                                                      "cbcl_scr_dsm5_depress_t","cbcl_scr_dsm5_somaticpr_t",
                                                                      "cbcl_scr_dsm5_adhd_t","cbcl_scr_dsm5_opposit_t",
                                                                      "cbcl_scr_dsm5_conduct_t","cbcl_scr_syn_external_t","cbcl_scr_syn_internal_t","cbcl_scr_syn_totprob_t")]
data_cbcls_3year<-cbcls[cbcls$eventname=="3_year_follow_up_y_arm_1",c("src_subject_id","eventname","interview_age","sex",
                                                                      "cbcl_scr_dsm5_anxdisord_t",
                                                                      "cbcl_scr_dsm5_depress_t","cbcl_scr_dsm5_somaticpr_t",
                                                                      "cbcl_scr_dsm5_adhd_t","cbcl_scr_dsm5_opposit_t",
                                                                      "cbcl_scr_dsm5_conduct_t","cbcl_scr_syn_external_t","cbcl_scr_syn_internal_t","cbcl_scr_syn_totprob_t")]

index_cbcls<-intersect(intersect(data_cbcls_base$src_subject_id,data_cbcls_1year$src_subject_id),data_cbcls_2year$src_subject_id)
length(index_cbcls)
aa<-data_cbcls_3year[match(index_cbcls,data_cbcls_3year$src_subject_id),]
aa$src_subject_id<-index_cbcls
aa$eventname<-"3_year_follow_up_y_arm_1"
data_1_1<-cbind(rbind(data_cbcls_base[match(index_cbcls,data_cbcls_base$src_subject_id),],data_cbcls_1year[match(index_cbcls,data_cbcls_1year$src_subject_id),],
                      data_cbcls_2year[match(index_cbcls,data_cbcls_2year$src_subject_id),],aa),
                rbind(d[match(index_cbcls,d$src_subject_id),],d[match(index_cbcls,d$src_subject_id),],d[match(index_cbcls,d$src_subject_id),],
                      d[match(index_cbcls,d$src_subject_id),]))
data_1_1$visit<-0
data_1_1[data_1_1$eventname=="1_year_follow_up_y_arm_1",]$visit<-12
data_1_1[data_1_1$eventname=="2_year_follow_up_y_arm_1",]$visit<-24
data_1_1[data_1_1$eventname=="3_year_follow_up_y_arm_1",]$visit<-36
summary(data_1_1)


data_1_1_1<-data_1_1[,c(-2,-3,-4,-14,-21)]
str(data_1_1_1)
head(data_1_1_1$high.educ)
head(data_1_1_1$household.income)
data_1_1_1$sex<-factor(data_1_1_1$sex,levels = c("M","F"),labels = c("boy","girl"))
head(data_1_1_1$sex)
for(i in 29:32){
  data_1_1_1[,i]<-as.numeric(as.matrix(data_1_1_1[,i]))
}
str(data_1_1_1)
summary(data_1_1_1)

#tbss
tbss<-read.table("abcd_tbss01.txt",header = T)
data_tbss_base<-tbss[tbss$eventname=="baseline_year_1_arm_1",
                     c("src_subject_id","nihtbx_totalcomp_uncorrected","nihtbx_cryst_uncorrected",
                       "nihtbx_picvocab_uncorrected","nihtbx_reading_uncorrected","nihtbx_fluidcomp_uncorrected",
                       "nihtbx_picture_uncorrected","nihtbx_cardsort_uncorrected","nihtbx_flanker_uncorrected",
                       "nihtbx_list_uncorrected","nihtbx_pattern_uncorrected")]

#combination of cbcls and tbss
data_cbcls_tbss<-cbind(data_1_1_1,data_tbss_base[match(data_1_1_1$src_subject_id,data_tbss_base$src_subject_id),])

data0510<-data_cbcls_tbss[data_cbcls_tbss$visit!=36,]
a<-as.data.frame(table(data0510$src_subject_id))
b<-as.character(a$Var1)
data0510$visit<-data0510$visit/12
#delete no FI
index<-intersect(data_tbss_base[!is.na(data_tbss_base$nihtbx_fluidcomp_uncorrected),]$src_subject_id,b)
index<-unique(index)
data0510<-data0510[data0510$src_subject_id%in%index,]
mydata<-data0510

#missing value
data0510<-mydata
index_ex_missing<-data0510[is.na(data0510$cbcl_scr_syn_external_t),]$src_subject_id
length(unique(index_ex_missing))
index1<-index[!index%in%unique(index_ex_missing)]
data0510<-data0510[data0510$src_subject_id%in%index1,]  
data0510<-data0510[data0510$nihtbx_fluidcomp_uncorrected>70,]

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
  set.seed(9)
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
table(data0510_new_in$class_in,data0510_new_in$class_ex)/3
table(data0510_new_in$class_in)/3

index_caco<-unique(union(unique(data0510_new_ex$src_subject_id),unique(data0510_new_in$src_subject_id))) #6343

load("data/smri.RData")
qc<-read.table("data/abcd_imgincl01.txt",header = T,stringsAsFactors = F)
smri_qc<-merge(smri,qc,by = c("src_subject_id","eventname"))
table(smri$eventname)
smri_qc<-smri_qc[smri_qc$imgincl_t1w_include==1,]
smri_base<-smri_qc[smri_qc$eventname=="baseline_year_1_arm_1",]
smri_2year<-smri_qc[smri_qc$eventname=="2_year_follow_up_y_arm_1",]

#delta
index<-intersect(smri_base$src_subject_id,smri_2year$src_subject_id)
smri_base<-smri_base[smri_base$src_subject_id%in%index,]
smri_2year<-smri_2year[smri_2year$src_subject_id%in%index,]
for(i in 11:484){
  smri_base[,i]<-smri_2year[,i]-smri_base[,i]
}

index_qc<-intersect(smri_base$src_subject_id,data0510_new[data0510_new$visit==0,]$src_subject_id) #5076
data_class_smri_tbss_delta<-merge(data0510_new[data0510_new$visit==2,-1],smri_base,by = "src_subject_id")
data_class_smri_tbss_delta<-merge(data0510_new[data0510_new$visit==0,c(1,8:10)],data_class_smri_tbss_delta,by = "src_subject_id")
colnames(data_class_smri_tbss_delta)[68]

#baseline
smri_base<-smri_qc[smri_qc$eventname=="baseline_year_1_arm_1",]
smri_2year<-smri_qc[smri_qc$eventname=="2_year_follow_up_y_arm_1",]
data_class_smri_tbss_base<-merge(data0510_new[data0510_new$visit==2,-1],smri_base[smri_base$src_subject_id%in%index_qc,],by = "src_subject_id")
data_class_smri_tbss_base<-merge(data0510_new[data0510_new$visit==0,c(1,8:10)],data_class_smri_tbss_base,by = "src_subject_id")
colnames(data_class_smri_tbss_base)[68]

#2 year
smri_base<-smri_2year[smri_2year$imgincl_t1w_include==1,]
data_class_smri_tbss_2year<-merge(data0510_new[data0510_new$visit==2,-1],smri_base[smri_base$src_subject_id%in%index_qc,],by = "src_subject_id")
data_class_smri_tbss_2year<-merge(data0510_new[data0510_new$visit==0,c(1,8:10)],data_class_smri_tbss_2year,by = "src_subject_id")
colnames(data_class_smri_tbss_2year)[68]

data_total_tbss<-data0510_new[data0510_new$src_subject_id%in%index_qc,]
save.image("data/r0727.RData")