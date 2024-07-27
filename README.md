# Eye-sparing-Treatment-for-Sinonasal-Tumors7.28--
rm(list=ls())
---
# 1 packages for analysis
```{r}
library(ggplot2)
library(riskRegression)
library(dplyr)
library(survival)
library(survminer)
library(ggsci)
library(xlsx)
```
#1-2.reading data
```{r}
data_test<-openxlsx::read.xlsx ("~/Desktop", sheet = "SNC")
data2<-as.data.frame(data_test)
data2 %>% 
  select("tumor","age","age_group","sex","site","orbit_invasion","invasion_grade","skullbase_invasion","brain_invasion","skin_invasion","T","T_stage","N_stage","clinical_stage","radiation_technique","radiation_time","chemotherapy","surgery","surgery_mode","eyeball_enucleation","vision_loss_grade","OS","OS_status","LC","LC_status","PFS","PFS_status","Recurrence","RFS","RFS_status","DMFS","DMFS_status","Metastasis","vision_loss","dose","invsion_grade_origin","blindless")->data3
```
# 3.survival analysis-logrank
```{r}
data3$dose_group[data3$dose>=66]<-">=66"
data3$dose_group[data3$dose<66]<-"<66"
#分组T分期
data3$T_group[data3$T=="1"]<-"Te"
data3$T_group[data3$T=="2"]<-"Te"
data3$T_group[data3$T=="3"]<-"Te"
data3$T_group[data3$T=="4a"]<-"Te"
data3$T_group[data3$T=="4b"]<-"Ta"

data3<-data3%>%filter(orbit_invasion=="Yes") #filter(T_group=="Ta")%>%
data3$tumor[data3$tumor!="SCC"]<-"non-SCC"

str (data3$T_group)
table (data3$invasion_grade)#$radiation_time)


fit<-survfit(Surv(OS,OS_status)~radiation_time,data=data3)
ggsurvplot(fit,conf.int = F,pval = TRUE,pval.size=6,pval.round=3,surv.median.line = "hv",risk.table = TRUE,xlab="Time (months)",ylab="OS (%)",legend=c(0.80,0.9),legend.title="Surgery",legend.labs=c("No","h","c"),font.x=c (18,"bold"),font.y=c(18,"bold"),font.tickslabe=c(16),font.legend=c(16),break.x.by=24, palette = "lancet")
summary(fit)

```
#4. cox regresssion analysis
```{r}
data3$tumor[data3$tumor!="SCC"]<-"non-SCC"

data3$site[data3$site=="鼻腔"]<-"nasal"
data3$site[data3$site=="筛窦"]<-"nasal"
data3$site[data3$site=="上颌窦"]<-"maxillary_sinus"
data3$site[data3$site=="其他"]<-"nasal"

data3$new_stage[data3$clinical_stage="T3"]<-"Early_stage"
summary(data3$invasion_grade)
summary(data3$radiation_time)

fit.cox<-coxph(Surv(OS,OS_status)~sex+age_group+T_stage+N_stage+invasion_grade+surgery+tumor,data=data3)
fit.cox.sum<-summary(fit.cox)
fit.cox.sum$waldtest
#fit.cox.sum$logtest
fit.cox.sum$coefficients[2]
ggforest(fit.cox,data=data3,
         main="Hazard ratio",
         cpositions = c(0.01,0.15,0.35),#
         fontsize=1.0,
         refLabel="Reference",
         noDigits=2
         )

```
#5. Table of baseline characteristics
```{r}
library(tableone)  

table<-table1(~age_group+sex+tumor+site+T_stage+N_stage+clinical_stage+surgery+surgery_mode+chemotherapy+radiation_time+invasion_grade+radiation_technique+skullbase_invasion|radiation_time, data=data3)

table =CreateTableOne(vars = c("sex", "age_group","tumor","site","T_stage","N_stage","clinical_stage","surgery","surgery_mode","chemotherapy","radiation_time","radiation_technique","skullbase_invasion","invasion_grade"), strata = "radiation_time", data =data3)
table_mat = print(table, quote = FALSE, noSpaces = T, printToggle = FALSE)
write.xlsx(table_mat, file = "~/Desktop/table.xlsx")
```
#6.Histograme
```{r}
library(plyr)
library(tidyverse)

data2<-data2%>%
  group_by(Tumor)%>%
  mutate(label_y=cumsum(Number)-0.5*Number)
p<-ggplot(data2,aes(x=Tumor,y=Number,fill=Invasion))+geom_bar(stat = "identity",position = "fill",width=0.7)+
  xlab("Tumor type")+ylab("Percentage")+ #修改坐标轴
  theme(axis.title.x =element_text(size=16),axis.line = element_line(colour = "black"),axis.title.y =element_text(size=16),panel.border = element_blank(),legend.text = element_text(size=14))+#修改坐标轴刻度及字体颜色等
  scale_fill_lancet()
p

```
#7 cumulative risk curve
```{r}
ggsurvplot(fit,
           fun = "cumhaz",pval = T, pval.size=6, pval.coord=c(0.5,1.3),risk.table = FALSE,ncensor.plot = TRUE,
           xlab="Time (months)",font.x=c (18,"bold"),font.y=c(18,"bold"),font.tickslabe=c(16),font.legend=c(16),break.x.by=24,  conf.int = TRUE, # 可信区间
           palette = "lancet", # 支持ggsci配色，自定义颜色，brewer palettes中的配色，等
           ggtheme = theme_bw() # 支持ggplot2及其扩展包的主题
)

```
#8 cumulative incidence curve
```{r}
ggsurvplot(fit,
           fun = "event", pval = TRUE,pval.size=6,
           conf.int = TRUE, # 可信区间
           palette = "lancet",
           ggtheme = theme_pubclean() 
)
```

