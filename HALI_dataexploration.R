#############################################################################                                                                 #
#                           Load in needed packages                         #                                                                #
#############################################################################

library('readstata13')
library('data.table')
library('tableone')
library('xtable')
library('nlme')
library('gmodels')
library('multcomp')
library('gee')
library('ICC')
library('jomo')
library('mice')
library('lme4')
library('gtools')
library('ggplot2')
library('mitml')

############################################################################                                                        #
# Read in the whole dataset and chose the subset and variables that we want#                                                    #                                                               #
############################################################################

setwd('c://R//HALI')

dat=read.dta13('HALI_CLASS1_2539_MASTER_EDUC_FU1_FU2_LONG_accounts_withdrawals_BL_AS_COV_AND_OTHER_COV_17.11.2016.dta',
               nonint.factors = TRUE, generate.factors=TRUE)

## we only focus on the results at 9 month
data0=dat[dat$visit=='9-month FU',]
dim(data0)
## choose the variables
data00=data0[,c("school_id","child_id",'cluster','MAL_grp','LIT_grp',
                'sex','age_child','kcpemean_mal','BL_gll21_total',
                'gll21_total','BL_ses', 'lang_3cat','schlevel_comp','crowding',
                'people_hh','sfp','handwash' )]
## check the missing values
sapply(data00, function(x) sum(is.na(x)))

## Because we only focus on missing values in outcomes, 
## so I drop the missingness in covariates
dat0=data00[is.na(data00$BL_gll21_total)==0 
            & is.na(data00$BL_ses)==0
            &is.na(data00$lang_3cat)==0 
            & is.na(data00$schlevel_comp)==0
            & is.na(data00$crowding)==0,]
sapply(dat0, function(x) sum(is.na(x)))

## Add missingness indicator to the data
miss=ifelse(is.na(dat0$gll21_total)==1,'Miss','NonMiss')
dat0$miss=miss
#miss=ifelse(is.na(dat1$gll21_total)==1,'Miss','NonMiss')
#dat1$miss=miss

## subsets of the data
int_dat0=dat0[dat0$LIT_grp=='yes',]        ## intervention data
con_dat0=dat0[dat0$LIT_grp=='no',]         ## control data
na_dat=na.omit(dat0)                       ## data without missing values
mis_dat=dat0[is.na(dat0$gll21_total)==1,]  ## data with gll21_total missing

#lower baseline score was associated with a higher probability of missing 9-month
d10=dat0[dat0$BL_gll21_total<=10,]
d20=dat0[dat0$BL_gll21_total>10,]
sum(is.na(d10$gll21_total))/dim(d10)[1]
sum(is.na(d20$gll21_total))/dim(d20)[1]

## larger household size was associated with a higher probability of missing 9-month
d10=dat0[dat0$crowding=='8-9' | dat0$crowding=='10-32',]
d20=dat0[dat0$crowding=='1-5' | dat0$crowding=='6-7',]
sum(is.na(d10$gll21_total))/dim(d10)[1]
sum(is.na(d20$gll21_total))/dim(d20)[1]

## lower household head education was associated with a higher probability of missing 9-month
d10=dat0[dat0$schlevel_comp=='secondary' | dat0$schlevel_comp=='college/deree',]
d20=dat0[dat0$schlevel_comp=='no schooling' | dat0$schlevel_comp=='primary',]
sum(is.na(d10$gll21_total))/dim(d10)[1]
sum(is.na(d20$gll21_total))/dim(d20)[1]

############################################################################                                                        #
#                 Exploration of the data set analysis                     #                                                               #
############################################################################

## function that help calculate cluster size, (mean and standard deviation
## in each cluster)
cluster_size=function(dataset,Breaks=5){
  temp=table(dataset$school_id)
  histrv=hist(temp,breaks=Breaks)
  histrv$breaks
  a=histrv$breaks
  n=length(a)
  a1=a[-n]
  a2=a[-1]
  breaks=paste(a1,"-",a2,sep='')
  breaks=c(breaks,'total')
  clusize=as.data.frame(breaks)
  counts=histrv$counts
  counts=c(counts,sum(counts))
  clusize$counts=counts
  return(clusize)
}

## calculate the cluster size info for intervention group and control group
cluster_size(int_dat0)
cluster_size(con_dat0)

## function that help calculate cluster size distribution
size=function(dat0){
  clus=unique(dat0$school_id)
  mark=0
  size=c()
  n=length(unique(dat0$school_id))
  for(i in 1:n){
    m1=dat0[dat0$school_id==clus[i],]
    size=c(size,dim(m1)[1])
  }
  print(mean(size))
  print(sd(size))
  return(size)
}

## function that help calculate cluster size, (mean and standard deviation
## in each cluster)
cluster_size2=function(dat0){
  dat0_int=dat0[dat0$LIT_grp=='yes',]
  dat0_con=dat0[dat0$LIT_grp=='no',]
  a=length(table(dat0_int$school_id))
  b=length(table(dat0_con$school_id))
  print('int size')
  print(a)
  print('con size')
  print(b)
  interblsize<- data.table(dat0_int)
  controlblsize<- data.table(dat0_con)
  c=mean(interblsize[,list(data=length(unique(child_id))),by=school_id]$data)
  d=sd(interblsize[,list(data=length(unique(child_id))),by=school_id]$data)
  print('int mean std')
  t1=paste("(",round(c,2),", " ,round(d,2),")",sep='')
  print(t1)
  e=mean(controlblsize[,list(data=length(unique(child_id))),by=school_id]$data)
  f=sd(controlblsize[,list(data=length(unique(child_id))),by=school_id]$data)
  print('control mean std')
  t2=paste("(",round(e,2),", " ,round(f,2),")",sep='')
  print(t2)
}

## calculate the cluster size info for total/non-missing/missing subsets
cluster_size2(dat0)
cluster_size2(na_dat)
cluster_size2(mis_dat)
size(dat0)
size(na_dat)
size(mis_dat)

## calculate these subsets' ICC
school_id2=as.factor(dat0$school_id)
dat0$school_id2=school_id2

school_id2=as.factor(na_dat$school_id)
na_dat$school_id2=school_id2

#school_id2=as.factor(mis_dat$school_id)
#mis_dat$school_id2=school_id2

ICCest(dat0$school_id2, dat0$gll21_total, data =dat0, CI.type = "S")
ICCest(na_dat$school_id2, na_dat$gll21_total, data =na_dat, CI.type = "S")
#ICCest(school_id2, gll21_total, data =mis_dat, CI.type = "S")

### draw cluster size picture
temp=table(con_dat0$school_id)
con=c(sum(temp>=10&temp<15),sum(temp>=15&temp<20),sum(temp>=20&temp<25),
      sum(temp>=25&temp<30))
int=cluster_size(int_dat0)
int=int[-5,]
con=data.frame(int$breaks,con)
names(con)=c('breaks','counts')
size=rbind(int,con)
size$category=c(rep('intervention',4),rep('control',4))
## draw total number of cluster bar plot
ggplot(size, aes(factor(size$breaks), size$counts, fill = size$category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

### missing values distribtuion
count_int=table(int_dat0$school_id)
no_int_dat0=na.omit(int_dat0)
count_mis_int=table(no_int_dat0$school_id)
counts1=count_int-count_mis_int
count_con=table(con_dat0$school_id)
no_con_dat0=na.omit(con_dat0)
count_mis_con=table(no_con_dat0$school_id)
counts2=count_con-count_mis_con

h1=hist(counts1,breaks=5)
h2=hist(counts2,breaks=5)
bre=h1$breaks[-1]
bre2=h1$breaks[-6]
bre0=paste(bre2,'-',bre,sep='')
size_mis=data.frame(breaks=c(bre0,bre0),counts=c(h1$counts,h2$counts),
                    category=c(rep('intervention',5),rep('control',5)))
## draw missing number bar plot
ggplot(size_mis, aes(factor(size_mis$breaks), size_mis$counts, fill = size_mis$category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

### calculate missing value precentage
count_int=table(int_dat0$school_id)
no_int_dat0=na.omit(int_dat0)
count_mis_int=table(no_int_dat0$school_id)
counts1=1-count_mis_int/count_int
count_con=table(con_dat0$school_id)
no_con_dat0=na.omit(con_dat0)
count_mis_con=table(no_con_dat0$school_id)
counts2=1-count_mis_con/count_con

h1=hist(counts1,breaks=5)
h2=hist(counts2,breaks=seq(0,0.6,by=0.1))
bre=h1$breaks[-1]
bre2=h1$breaks[-7]
bre0=paste(bre2,'-',bre,sep='')
size_per=data.frame(breaks=c(bre0,bre0),counts=c(h1$counts,h2$counts),
                    category=c(rep('intervention',6),rep('control',6)))
## draw missing percentage bar plot
ggplot(size_per, aes(factor(size_per$breaks), size_per$counts, fill = size_per$category)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")


## calculate the P value missingness for the association between missingness and covariates
mis=dat0[is.na(dat0$gll21_total)==1,]
nonmis=dat0[is.na(dat0$gll21_total)==0,]
miss_num=ifelse(dat0$miss=='Miss',1,0)
dat0$miss_num=miss_num

## print contains cluster effects p values
car_vars=c('MAL_grp','LIT_grp','sex','BL_ses','lang_3cat','schlevel_comp',
           'crowding','sfp','handwash')
con_vars=c('age_child','BL_gll21_total','people_hh')

## function for categorical covariates p values
car_p_value=function(var,data=dat0){
  #print("**********")
  #print(var)
  formula1=paste('miss_num~(1|school_id)')
  formula2=paste('miss_num~(1|school_id)+',var)
  a=glmer(formula1,family = binomial,data=data)
  b=glmer(formula2,family = binomial,data=data)
  c=anova(a,b)
  cc=c$`Pr(>Chisq)`
  #print('contains cluster')
  #print(cc[2])
  
  formula3=paste('miss_num~1')
  formula4=paste('miss_num~',var)
  d=glm(formula3,family = binomial,data=data)
  e=glm(formula4,family = binomial,data=data)
  f=anova(d,e)
  #print('no cluster')
  #print(1-pchisq(f$Deviance[2],1))
  ttt=1-pchisq(f$Deviance[2],1)
  ppp=paste(var,round(cc[2],3),round(ttt,3),sep=',')
  print(ppp)
}

## function for continuous covartiates p valuse
con_p_value=function(var,data=dat0){
  #print('***********')
  #print(var)
  formula1=paste('miss_num~',var)
  formula2=paste('miss_num~(1|school_id)+',var)
  a=glm(formula1,data=data,family = binomial)
  aa=summary(a)
  #print('no cluster')
  #print(round(aa$coefficients[2,'Pr(>|z|)'],3))
  b=glmer(formula2,data=data,family=binomial)
  bb=summary(b)
  #print('cluster')
  #print(round(bb$coefficients[2,'Pr(>|z|)'],3))
  ttt=paste(var,round(bb$coefficients[2,'Pr(>|z|)'],3),round(aa$coefficients[2,'Pr(>|z|)'],3),sep=', ')
  print(ttt)
}

## print the p values for categorical covariates 
for(i in 1:length(car_vars)){
  car_p_value(car_vars[i],dat0)
}

## print the p values for continuous covariates 
for(i in 1:length(con_vars)){
  con_p_value(con_vars[i],dat0)
}

## Draw the demographic table for the data
## individual level
listvar=c('BL_gll21_total','gll21_total','age_child','people_hh')
catvar=c('MAL_grp','sex','schlevel_comp','BL_ses','crowding','lang_3cat','sfp','handwash')
st='LIT_grp'
table0=CreateTableOne(vars=c(listvar,catvar),strata = st,factorVars=catvar,data=dat0)
table0=print(table0,showAllLevels = TRUE)
xtable(table0)
## cluster level
clus=unique(dat0$school_id)
cluster_data=c()
for(i in 1:length(clus)){
  temp=dat0[dat0$school_id==clus[i],]
  temp=temp[1,]
  cluster_data=rbind(cluster_data,temp)
}
#cluster_data

catvar=c('sfp','handwash')
cluster_data=cluster_data[,c('LIT_grp','sfp','handwash')]
table00=CreateCatTable(strata = st,vars=catvar,data=cluster_data)
table00=print(table00,showAllLevels = TRUE)
xtable(table00)