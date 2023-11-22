#############################################################
#############################################################
###########################figure############################
#############################################################
#############################################################
load("data/r0727_2.RData")
library(ggplot2)
library(ggsci)
library(ggpubr)
library(reshape2)
library(nnet)
library(psych)
library(rlang)
library(dplyr)
library(grid)
library(ggbreak)
library(interactions)

##figure 1
df<-data0510_new_ex[,c(1,43,8,22,55)]
colnames(df)[3]<-c("Externalizing")
df<-melt(df,c("src_subject_id","visit","sex","class_ex"))
colnames(df)[5]<-"symptom"
df1<-df
df1$visit<-factor(df1$visit,labels = c("10","11","12"))
p1<-ggplot(df1,aes(x=visit, y=value, group=src_subject_id)) + geom_line(color="grey",size=0.005,alpha = 0.25) +
  geom_point(color="grey",size=0.5,alpha = 0.25)+
  xlab(NULL)+ylab("Externalizing")+
  geom_smooth(mapping = aes(visit,value,group=sex,color=sex),method = "lm")+
  facet_wrap(.~class_ex) +
  theme_bw()  +
  scale_colour_brewer(palette = "Set1",direction = -1) +
  theme(legend.position = "top")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size=8, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))
p1

df<-data0510_new_in[,c(1,43,9,22,56)]
colnames(df)[3]<-c("Internalizing")
df<-melt(df,c("src_subject_id","visit","sex","class_in"))
colnames(df)[5]<-"symptom"
df1<-df
df1$visit<-factor(df1$visit,labels = c("10","11","12"))
p2<-ggplot(df1,aes(x=visit, y=value, group=src_subject_id)) + geom_line(color="grey",size=0.005,alpha = 0.25) +
  geom_point(color="grey",size=0.5,alpha = 0.25)+
  xlab(NULL)+ylab("Internalizing")+
  geom_smooth(mapping = aes(visit,value,group=sex,color=sex),method = "lm")+
  facet_wrap(.~class_in) +
  theme_bw()  +
  scale_colour_brewer(palette = "Set1",direction = -1) +
  theme(legend.position = "top")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size=8, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"))
p2
p3<-ggarrange(p1,p2,nrow = 1,ncol = 2,common.legend = T)


data_bnlearn<-com1[,c(3:37,54:61,63:72)]
data_bnlearn<-cbind(data_bnlearn,class.ind(com1$household.income)[,-1],class.ind(com1$sex)[,-1],class.ind(com1$race_ethnicity)[,-1],class.ind(com1$site_id_l)[,-1])
colnames(data_bnlearn)[c(1,54:56)]<-c("Externalizing_slope","income1","income2","sex")

set<-data_bnlearn[,c(1:53)]
data_bnlearn_resid<-apply(set,2,function(x){
  glm(x~income1+income2+sex+Black+Hispanic+Asian+Other+site02+site03+site04+site05+site06+site07+
        site08+site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
        site21+site22,data = data_bnlearn,na.action = na.exclude) %>% residuals.glm(type = "response")
})
data_bnlearn_resid<-as.data.frame(data_bnlearn_resid)

a<-corr.test(data_bnlearn_resid[,1],data_bnlearn_resid[,2:35])
a1<-a[["r"]]
a2<-a[["p"]]
a3<-a[["p.adj"]]

data_bnlearn<-com3[,c(3:37,54:61,63:72)]
data_bnlearn<-cbind(data_bnlearn,class.ind(com3$household.income)[,-1],class.ind(com3$sex)[,-1],class.ind(com3$race_ethnicity)[,-1],class.ind(com3$site_id_l)[,-1])
colnames(data_bnlearn)[c(1,54:56)]<-c("Internalizing_slope","income1","income2","sex")

set<-data_bnlearn[,c(1:53)]
data_bnlearn_resid<-apply(set,2,function(x){
  glm(x~income1+income2+sex+Black+Hispanic+Asian+Other+site02+site03+site04+site05+site06+site07+
        site08+site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
        site21+site22,data = data_bnlearn,na.action = na.exclude) %>% residuals.glm(type = "response")
})
data_bnlearn_resid<-as.data.frame(data_bnlearn_resid)

a<-corr.test(data_bnlearn_resid[,1],data_bnlearn_resid[,2:35])
a4<-a[["r"]]
a5<-a[["p"]]
a6<-a[["p.adj"]]


r<-rbind(a1,a4)
p<-rbind(a3,a6)
rownames(r)<-c("Externalizing_slope","Internalizing_slope")
rownames(p)<-c("Externalizing_slope","Internalizing_slope")


r <- 
  r %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'r'))

p <- 
  p %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'P_value')) %>% 
  mutate(P_value_sig = case_when(P_value > 0.05 ~ " ",
                                 P_value <= 0.05 & P_value > 0.01 ~ "*",
                                 P_value <= 0.01 & P_value > 0.001 ~ "**",
                                 P_value <= 0.001 ~ "***",
                                 TRUE ~ NA_character_))

data <- cbind(r,p) %>% select(-(4:5))
head(data)
data$group<-"External_case"
data[data$tax=="Internalizing_slope",]$group<-"Internal_case"
data$var<-"symptoms_baseline"
data[35:68,]$var<-"symptoms_2year"



data_bnlearn<-com2[,c(3:37,54:61,63:72)]
data_bnlearn<-cbind(data_bnlearn,class.ind(com2$household.income)[,-1],class.ind(com2$sex)[,-1],class.ind(com2$race_ethnicity)[,-1],class.ind(com2$site_id_l)[,-1])
colnames(data_bnlearn)[c(1,54:56)]<-c("Externalizing_slope","income1","income2","sex")

set<-data_bnlearn[,c(1:53)]
data_bnlearn_resid<-apply(set,2,function(x){
  glm(x~income1+income2+sex+Black+Hispanic+Asian+Other+site02+site03+site04+site05+site06+site07+
        site08+site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
        site21+site22,data = data_bnlearn,na.action = na.exclude) %>% residuals.glm(type = "response")
})
data_bnlearn_resid<-as.data.frame(data_bnlearn_resid)

a<-corr.test(data_bnlearn_resid[,1],data_bnlearn_resid[,2:35])
a1<-a[["r"]]
a2<-a[["p"]]
a3<-a[["p.adj"]]

data_bnlearn<-com4[,c(3:37,54:61,63:72)]
data_bnlearn<-cbind(data_bnlearn,class.ind(com4$household.income)[,-1],class.ind(com4$sex)[,-1],class.ind(com4$race_ethnicity)[,-1],class.ind(com4$site_id_l)[,-1])
colnames(data_bnlearn)[c(1,54:56)]<-c("Internalizing_slope","income1","income2","sex")

set<-data_bnlearn[,c(1:53)]
data_bnlearn_resid<-apply(set,2,function(x){
  glm(x~income1+income2+sex+Black+Hispanic+Asian+Other+site02+site03+site04+site05+site06+site07+
        site08+site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
        site21+site22,data = data_bnlearn,na.action = na.exclude) %>% residuals.glm(type = "response")
})
data_bnlearn_resid<-as.data.frame(data_bnlearn_resid)

a<-corr.test(data_bnlearn_resid[,1],data_bnlearn_resid[,2:35])
a4<-a[["r"]]
a5<-a[["p"]]
a6<-a[["p.adj"]]

r<-rbind(a1,a4)
p<-rbind(a3,a6)
rownames(r)<-c("Externalizing_slope","Internalizing_slope")
rownames(p)<-c("Externalizing_slope","Internalizing_slope")

r <- 
  r %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'r'))

p <- 
  p %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'P_value')) %>% 
  mutate(P_value_sig = case_when(P_value > 0.05 ~ " ",
                                 P_value <= 0.05 & P_value > 0.01 ~ "*",
                                 P_value <= 0.01 & P_value > 0.001 ~ "**",
                                 P_value <= 0.001 ~ "***",
                                 TRUE ~ NA_character_))

data1 <- cbind(r,p) %>% select(-(4:5))
head(data1)
data1$group<-"External_control"
data1[data1$tax=="Internalizing_slope",]$group<-"Internal_control"
data1$var<-"symptoms_baseline"
data1[35:68,]$var<-"symptoms_2year"

data2<-rbind(data,data1)
for(i in 1:nrow(data2)){
  if(data2[i,4]>=0.05){
    data2[i,3]<-NA
  }
}


theme <- theme_bw(base_size = 7) +
  theme(panel.grid.major = element_blank(),
        text = element_text(face='bold'),
        legend.key.size = unit(5, "pt"),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x=element_text(angle=45,vjust=1, hjust=1))
data2$group<-factor(data2$group,levels = c("External_case","Internal_case","External_control","Internal_control"))
data4<-data2

p6<-ggplot(data2,aes(x=Index, y=tax))+
  geom_tile(aes(fill=r), color = 'white',alpha = 0.6,na.rm = TRUE) +
  geom_text(aes(label = round(r,2)), color = 'black', size = 2) +
  ggsci::scale_fill_gsea(limits =c(-1,1),na.value="white") +
  theme +facet_grid(group ~ var, scales = 'free')+
  xlab('') +
  ylab('')


p6



data_bnlearn<-com1[,c(3:37,54:61,63:72)]
data_bnlearn<-cbind(data_bnlearn,class.ind(com1$household.income)[,-1],class.ind(com1$sex)[,-1],class.ind(com1$race_ethnicity)[,-1],class.ind(com1$site_id_l)[,-1])
colnames(data_bnlearn)[c(1,54:56)]<-c("Externalizing_slope","income1","income2","sex")

set<-data_bnlearn[,c(1:53)]
data_bnlearn_resid<-apply(set,2,function(x){
  glm(x~income1+income2+sex+Black+Hispanic+Asian+Other+site02+site03+site04+site05+site06+site07+
        site08+site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
        site21+site22,data = data_bnlearn,na.action = na.exclude) %>% residuals.glm(type = "response")
})
data_bnlearn_resid<-as.data.frame(data_bnlearn_resid)

a<-corr.test(data_bnlearn_resid[,1],data_bnlearn_resid[,44:53])
a1<-a[["r"]]
a2<-a[["p"]]
a3<-a[["p.adj"]]

data_bnlearn<-com3[,c(3:37,54:61,63:72)]
data_bnlearn<-cbind(data_bnlearn,class.ind(com3$household.income)[,-1],class.ind(com3$sex)[,-1],class.ind(com3$race_ethnicity)[,-1],class.ind(com3$site_id_l)[,-1])
colnames(data_bnlearn)[c(1,54:56)]<-c("Internalizing_slope","income1","income2","sex")

set<-data_bnlearn[,c(1:53)]
data_bnlearn_resid<-apply(set,2,function(x){
  glm(x~income1+income2+sex+Black+Hispanic+Asian+Other+site02+site03+site04+site05+site06+site07+
        site08+site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
        site21+site22,data = data_bnlearn,na.action = na.exclude) %>% residuals.glm(type = "response")
})
data_bnlearn_resid<-as.data.frame(data_bnlearn_resid)

a<-corr.test(data_bnlearn_resid[,1],data_bnlearn_resid[,44:53])
a4<-a[["r"]]
a5<-a[["p"]]
a6<-a[["p.adj"]]


r<-rbind(a1,a4)
p<-rbind(a3,a6)
rownames(r)<-c("Externalizing_slope","Internalizing_slope")
rownames(p)<-c("Externalizing_slope","Internalizing_slope")


r <- 
  r %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'r'))

p <- 
  p %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'P_value')) %>% 
  mutate(P_value_sig = case_when(P_value > 0.05 ~ " ",
                                 P_value <= 0.05 & P_value > 0.01 ~ "*",
                                 P_value <= 0.01 & P_value > 0.001 ~ "**",
                                 P_value <= 0.001 ~ "***",
                                 TRUE ~ NA_character_))

data <- cbind(r,p) %>% select(-(4:5))
head(data)
data$group<-"External_case"
data[data$tax=="Internalizing_slope",]$group<-"Internal_case"
data$var<-"cognition"



data_bnlearn<-com2[,c(3:37,54:61,63:72)]
data_bnlearn<-cbind(data_bnlearn,class.ind(com2$household.income)[,-1],class.ind(com2$sex)[,-1],class.ind(com2$race_ethnicity)[,-1],class.ind(com2$site_id_l)[,-1])
colnames(data_bnlearn)[c(1,54:56)]<-c("Externalizing_slope","income1","income2","sex")

set<-data_bnlearn[,c(1:53)]
data_bnlearn_resid<-apply(set,2,function(x){
  glm(x~income1+income2+sex+Black+Hispanic+Asian+Other+site02+site03+site04+site05+site06+site07+
        site08+site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
        site21+site22,data = data_bnlearn,na.action = na.exclude) %>% residuals.glm(type = "response")
})
data_bnlearn_resid<-as.data.frame(data_bnlearn_resid)

a<-corr.test(data_bnlearn_resid[,1],data_bnlearn_resid[,44:53])
a1<-a[["r"]]
a2<-a[["p"]]
a3<-a[["p.adj"]]

data_bnlearn<-com4[,c(3:37,54:61,63:72)]
data_bnlearn<-cbind(data_bnlearn,class.ind(com4$household.income)[,-1],class.ind(com4$sex)[,-1],class.ind(com4$race_ethnicity)[,-1],class.ind(com4$site_id_l)[,-1])
colnames(data_bnlearn)[c(1,54:56)]<-c("Internalizing_slope","income1","income2","sex")

set<-data_bnlearn[,c(1:53)]
data_bnlearn_resid<-apply(set,2,function(x){
  glm(x~income1+income2+sex+Black+Hispanic+Asian+Other+site02+site03+site04+site05+site06+site07+
        site08+site09+site10+site11+site12+site13+site14+site15+site16+site17+site18+site19+site20+
        site21+site22,data = data_bnlearn,na.action = na.exclude) %>% residuals.glm(type = "response")
})
data_bnlearn_resid<-as.data.frame(data_bnlearn_resid)

a<-corr.test(data_bnlearn_resid[,1],data_bnlearn_resid[,44:53])
a4<-a[["r"]]
a5<-a[["p"]]
a6<-a[["p.adj"]]

r<-rbind(a1,a4)
p<-rbind(a3,a6)
rownames(r)<-c("Externalizing_slope","Internalizing_slope")
rownames(p)<-c("Externalizing_slope","Internalizing_slope")

r <- 
  r %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'r'))

p <- 
  p %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'P_value')) %>% 
  mutate(P_value_sig = case_when(P_value > 0.05 ~ " ",
                                 P_value <= 0.05 & P_value > 0.01 ~ "*",
                                 P_value <= 0.01 & P_value > 0.001 ~ "**",
                                 P_value <= 0.001 ~ "***",
                                 TRUE ~ NA_character_))

data1 <- cbind(r,p) %>% select(-(4:5))
head(data1)
data1$group<-"External_control"
data1[data1$tax=="Internalizing_slope",]$group<-"Internal_control"
data1$var<-"cognition"

data2<-rbind(data,data1)
for(i in 1:nrow(data2)){
  if(data2[i,4]>=0.05){
    data2[i,3]<-NA
  }
}

theme <- theme_bw(base_size = 7) +
  theme(panel.grid.major = element_blank(),
        text = element_text(face='bold'),
        legend.key.size = unit(5, "pt"),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x=element_text(angle=45,vjust=1, hjust=1))
data2$group<-factor(data2$group,levels = c("External_case","Internal_case","External_control","Internal_control"))
data5<-rbind(data2,data4)

p7<-ggplot(data5,aes(x=Index, y=tax))+
  geom_tile(aes(fill=r), color = 'white',alpha = 0.6,na.rm = TRUE) +
  geom_text(aes(label = round(r,2)), color = 'black', size = 2) +
  ggsci::scale_fill_gsea(limits =c(-1,1),na.value="white") +
  theme +facet_grid(group ~ var, scales = 'free')+
  xlab('') +
  ylab('')


p7

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(p3, vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(p7, vp=viewport(layout.pos.row=2:3,layout.pos.col=1))


load("data/r0727_2.RData")
##figure 2
se <- function(x){
  sd(x,na.rm = T) / sqrt(length(x))
}


#slope
#externalizing
data_temp<-com_ex
num_groups <- 18 
continuous_var<-data_temp$nihtbx_fluidcomp_uncorrected

group_means <- tapply(continuous_var, cut(continuous_var, breaks = num_groups), mean)
grouped_var <- cut(continuous_var, breaks = num_groups, labels = round(group_means,1), ordered_result = TRUE)
df <- data.frame(continuous_var, grouped_var,data_temp)
data_plot<-merge(aggregate(df$exts,list(df$grouped_var,df$class_ex),mean),aggregate(df$exts,list(df$grouped_var,df$class_ex),se),by = c("Group.1","Group.2"))
colnames(data_plot)<-c("FI","class","mean","sd")
data_plot$FI<-as.numeric(as.character(data_plot$FI))

p1<-ggplot() + 
  geom_point(data=data_plot[data_plot$class=="control",], aes(FI, mean),size = 2, color = "gray") +
  geom_errorbar(data=data_plot[data_plot$class=="control",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "gray") +
  geom_smooth(data=data_plot[data_plot$class=="control",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 1, raw = TRUE), color = "gray") +
  geom_point(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(FI, mean),size = 2, color = "seagreen") +
  geom_errorbar(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "seagreen") +
  geom_smooth(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), color = "seagreen3") +
  labs(x="cognition fluid composite score", y="externalizing_slope") +
  #scale_y_break(c(45,54),scales = 3,space = 0.3)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


data_plot<-merge(aggregate(df$cbcl_scr_syn_external_t.y,list(df$grouped_var,df$class_ex),mean),aggregate(df$cbcl_scr_syn_external_t.y,list(df$grouped_var,df$class_ex),se),by = c("Group.1","Group.2"))
colnames(data_plot)<-c("FI","class","mean","sd")
data_plot$FI<-as.numeric(as.character(data_plot$FI))

p2<-ggplot() + 
  geom_point(data=data_plot[data_plot$class=="control",], aes(FI, mean),size = 2, color = "gray") +
  geom_errorbar(data=data_plot[data_plot$class=="control",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "gray") +
  geom_smooth(data=data_plot[data_plot$class=="control",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 1, raw = TRUE), color = "gray") +
  geom_point(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(FI, mean),size = 2, color = "seagreen") +
  geom_errorbar(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "seagreen") +
  geom_smooth(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), color = "seagreen3") +
  labs(x="cognition fluid composite score", y="externalizing_2year") +
  #scale_y_break(c(45,54),scales = 3,space = 0.3)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

data_plot<-merge(aggregate(df$cbcl_scr_syn_internal_t.y,list(df$grouped_var,df$class_ex),mean),aggregate(df$cbcl_scr_syn_internal_t.y,list(df$grouped_var,df$class_ex),se),by = c("Group.1","Group.2"))
colnames(data_plot)<-c("FI","class","mean","sd")
data_plot$FI<-as.numeric(as.character(data_plot$FI))

p3<-ggplot() + 
  geom_point(data=data_plot[data_plot$class=="control",], aes(FI, mean),size = 2, color = "gray") +
  geom_errorbar(data=data_plot[data_plot$class=="control",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "gray") +
  geom_smooth(data=data_plot[data_plot$class=="control",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 1, raw = TRUE), color = "gray") +
  geom_point(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(FI, mean),size = 2, color = "seagreen") +
  geom_errorbar(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "seagreen") +
  geom_smooth(data=data_plot[data_plot$class=="case"&data_plot$FI<=112,], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), color = "seagreen3") +
  labs(x="cognition fluid composite score", y="internalizing_2year") +
  #scale_y_break(c(45,54),scales = 3,space = 0.3)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#internalizing
data_temp<-com_in
num_groups <- 18
continuous_var<-data_temp$nihtbx_fluidcomp_uncorrected

group_means <- tapply(continuous_var, cut(continuous_var, breaks = num_groups), mean)
grouped_var <- cut(continuous_var, breaks = num_groups, labels = round(group_means,1), ordered_result = TRUE)
df <- data.frame(continuous_var, grouped_var,data_temp)
data_plot<-merge(aggregate(df$ints,list(df$grouped_var,df$class_in),mean),aggregate(df$ints,list(df$grouped_var,df$class_in),se),by = c("Group.1","Group.2"))
colnames(data_plot)<-c("FI","class","mean","sd")
data_plot$FI<-as.numeric(as.character(data_plot$FI))

p4<-ggplot() + 
  geom_point(data=data_plot[data_plot$class=="control",], aes(FI, mean),size = 2, color = "gray") +
  geom_errorbar(data=data_plot[data_plot$class=="control",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "gray") +
  geom_smooth(data=data_plot[data_plot$class=="control",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 1, raw = TRUE), color = "gray") +
  geom_point(data=data_plot[data_plot$class=="case",], aes(FI, mean),size = 2, color = "seagreen") +
  geom_errorbar(data=data_plot[data_plot$class=="case",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "seagreen") +
  geom_smooth(data=data_plot[data_plot$class=="case",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), color = "seagreen3") +
  labs(x="cognition fluid composite score", y="internalizing_slope") +
  #scale_y_break(c(45,54),scales = 3,space = 0.3)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

data_plot<-merge(aggregate(df$cbcl_scr_syn_external_t.y,list(df$grouped_var,df$class_in),mean),aggregate(df$cbcl_scr_syn_external_t.y,list(df$grouped_var,df$class_in),se),by = c("Group.1","Group.2"))
colnames(data_plot)<-c("FI","class","mean","sd")
data_plot$FI<-as.numeric(as.character(data_plot$FI))

p5<-ggplot() + 
  geom_point(data=data_plot[data_plot$class=="control",], aes(FI, mean),size = 2, color = "gray") +
  geom_errorbar(data=data_plot[data_plot$class=="control",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "gray") +
  geom_smooth(data=data_plot[data_plot$class=="control",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 1, raw = TRUE), color = "gray") +
  geom_point(data=data_plot[data_plot$class=="case",], aes(FI, mean),size = 2, color = "seagreen") +
  geom_errorbar(data=data_plot[data_plot$class=="case",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "seagreen") +
  geom_smooth(data=data_plot[data_plot$class=="case",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), color = "seagreen3") +
  labs(x="cognition fluid composite score", y="externalizing_2year") +
  #scale_y_break(c(45,54),scales = 3,space = 0.3)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


data_plot<-merge(aggregate(df$cbcl_scr_syn_internal_t.y,list(df$grouped_var,df$class_in),mean),aggregate(df$cbcl_scr_syn_internal_t.y,list(df$grouped_var,df$class_in),se),by = c("Group.1","Group.2"))
colnames(data_plot)<-c("FI","class","mean","sd")
data_plot$FI<-as.numeric(as.character(data_plot$FI))

p6<-ggplot() + 
  geom_point(data=data_plot[data_plot$class=="control",], aes(FI, mean),size = 2, color = "gray") +
  geom_errorbar(data=data_plot[data_plot$class=="control",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "gray") +
  geom_smooth(data=data_plot[data_plot$class=="control",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 1, raw = TRUE), color = "gray") +
  geom_point(data=data_plot[data_plot$class=="case",], aes(FI, mean),size = 2, color = "seagreen") +
  geom_errorbar(data=data_plot[data_plot$class=="case",], aes(x=FI, y=mean,ymin=mean-sd, ymax=mean+sd), width = 0.05, color = "seagreen") +
  geom_smooth(data=data_plot[data_plot$class=="case",], aes(FI, mean),method = "lm", formula = y ~ poly(x, degree = 2, raw = TRUE), color = "seagreen3") +
  labs(x="cognition fluid composite score", y="internalizing_2year") +
  #scale_y_break(c(45,54),scales = 3,space = 0.3)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p7<-ggarrange(p1,p4,nrow = 1,ncol = 2,labels = c("A","B"))





###figure 3
load("data/r0727_2.RData")
data_temp<-merge(data0510_new_ex[data0510_new_ex$src_subject_id%in%com1$src_subject_id,c(1,8,43)],data_class_smri_tbss_delta[,-29:-30],by = "src_subject_id")
colnames(data_temp)[68]
model_vol <- lmer(cbcl_scr_syn_external_t ~ visit +sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+ smri_vol_cdk_locclh*visit+ 
                    (1 + visit|site_id_l/src_subject_id), 
                  data = data_temp, 
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model_vol)

meansmri<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)
max<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)+sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)
min<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)-sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)

sim_slopes(model_vol, pred=visit, modx=smri_vol_cdk_locclh, modx.values=c(min,meansmri,max), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = data_temp)

p3<-interact_plot(model_vol, pred = visit, modx = smri_vol_cdk_locclh, 
                  modx.values=c(min,meansmri,max),
                  modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
                  legend.main = "Change of brain volume of \nleft lateral occipital cortex",
                  x.label = "Wave", y.label = "Externalizing Scores")

model_vol <- lmer(cbcl_scr_syn_external_t ~ visit +sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+ smri_vol_cdk_total*visit+ 
                    (1 + visit|site_id_l/src_subject_id), 
                  data = data_temp, 
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model_vol)

meansmri<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)
max<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)+sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)
min<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)-sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)

sim_slopes(model_vol, pred=visit, modx=smri_vol_cdk_total, modx.values=c(min,meansmri,max), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = data_temp)

p1<-interact_plot(model_vol, pred = visit, modx = smri_vol_cdk_total, 
                  modx.values=c(min,meansmri,max),
                  modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
                  legend.main = "Change of total \nbrain cortical volume",
                  x.label = "Wave", y.label = "Externalizing Scores")


data_temp<-merge(data0510_new_in[data0510_new_in$src_subject_id%in%com3$src_subject_id,c(1,9,43)],data_class_smri_tbss_delta[,-29:-30],by = "src_subject_id")
colnames(data_temp)[68]
model_vol <- lmer(cbcl_scr_syn_internal_t ~ visit +sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+ smri_vol_cdk_total*visit+ 
                    (1 + visit|site_id_l/src_subject_id), 
                  data = data_temp, 
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model_vol)

meansmri<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)
max<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)+sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)
min<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)-sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)

sim_slopes(model_vol, pred=visit, modx=smri_vol_cdk_total, modx.values=c(min,meansmri,max), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = data_temp)

p2<-interact_plot(model_vol, pred = visit, modx = smri_vol_cdk_total, 
                  modx.values=c(min,meansmri,max),
                  modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
                  legend.main = "Change of total \nbrain cortical volume",
                  x.label = "Wave", y.label = "Internalizing Scores")

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,3)))
print(p1, vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(p3, vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(p1, vp=viewport(layout.pos.row=2,layout.pos.col=2))
print(p2, vp=viewport(layout.pos.row=1,layout.pos.col=3))
print(p3, vp=viewport(layout.pos.row=2,layout.pos.col=3))

#figure s2
data_temp<-merge(data0510_new_ex[data0510_new_ex$src_subject_id%in%com2$src_subject_id,c(1,8,43)],data_class_smri_tbss_delta[,-29:-30],by = "src_subject_id")
colnames(data_temp)[68]
model_vol <- lmer(cbcl_scr_syn_external_t ~ visit +sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+ smri_vol_cdk_locclh*visit+ 
                    (1 + visit|site_id_l/src_subject_id), 
                  data = data_temp, 
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model_vol)

meansmri<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)
max<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)+sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)
min<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)-sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_locclh)

sim_slopes(model_vol, pred=visit, modx=smri_vol_cdk_locclh, modx.values=c(min,meansmri,max), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = data_temp)

p3<-interact_plot(model_vol, pred = visit, modx = smri_vol_cdk_locclh, 
                  modx.values=c(min,meansmri,max),
                  modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
                  legend.main = "Change of brain volume of \nleft lateral occipital cortex",
                  x.label = "Wave", y.label = "Externalizing Scores")

model_vol <- lmer(cbcl_scr_syn_external_t ~ visit +sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+ smri_vol_cdk_total*visit+ 
                    (1 + visit|site_id_l/src_subject_id), 
                  data = data_temp, 
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model_vol)

meansmri<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)
max<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)+sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)
min<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)-sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)

sim_slopes(model_vol, pred=visit, modx=smri_vol_cdk_total, modx.values=c(min,meansmri,max), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = data_temp)

p1<-interact_plot(model_vol, pred = visit, modx = smri_vol_cdk_total, 
                  modx.values=c(min,meansmri,max),
                  modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
                  legend.main = "Change of total \nbrain cortical volume",
                  x.label = "Wave", y.label = "Externalizing Scores")


data_temp<-merge(data0510_new_in[data0510_new_in$src_subject_id%in%com4$src_subject_id,c(1,9,43)],data_class_smri_tbss_delta[,-29:-30],by = "src_subject_id")
colnames(data_temp)[68]
model_vol <- lmer(cbcl_scr_syn_internal_t ~ visit +sex+household.income+race_ethnicity+famhx_ss_momdad_hspd_p+ smri_vol_cdk_total*visit+ 
                    (1 + visit|site_id_l/src_subject_id), 
                  data = data_temp, 
                  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model_vol)

meansmri<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)
max<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)+sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)
min<-mean(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)-sd(data_temp[data_temp$visit==0,]$smri_vol_cdk_total)

sim_slopes(model_vol, pred=visit, modx=smri_vol_cdk_total, modx.values=c(min,meansmri,max), 
           centered="none", cond.int=TRUE, johnson_neyman=FALSE, data = data_temp)

p2<-interact_plot(model_vol, pred = visit, modx = smri_vol_cdk_total, 
                  modx.values=c(min,meansmri,max),
                  modx.labels = c("1 SD Below Mean","Mean","1 SD Above Mean"),
                  legend.main = "Change of total \nbrain cortical volume",
                  x.label = "Wave", y.label = "Internalizing Scores")



grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(p1, vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(p3, vp=viewport(layout.pos.row=1,layout.pos.col=3))


