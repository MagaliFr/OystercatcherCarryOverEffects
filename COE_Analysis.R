######################### Analyse carry-over effect #########################################
################# path analysis
load("YOUR_PATH/GlobalEnvFig4.Rdata")
library(brms)
library(bayesplot)

#carry over dataset read
#read table
R<-read.csv("YOUR_PATH/COE_data.csv", header=T, dec=",", sep=";", fill=T)
str(R)

### check that if success=1, chicksurvival in days is also at least 30 and not na

#### read condition index 
ID<-read.csv(file="YOUR_PATH/StateMatrix.csv")

## add year and state (catching loc) for each individual (based on obscatchNr)
ID$ObsCatchNr.x<-factor(ID$ObsCatchNr.x)
str(ID$ObsCatchNr.x)
summary(ID$ObsCatchNr.x)
ID$State<-0
ID$YearShort<-ifelse(ID$ObsCatchNr.x=="1", "2001", #van winter '00-'01 to summer '01
                     ifelse(ID$ObsCatchNr.x=="3", "2001", 
                            ifelse(ID$ObsCatchNr.x=="4", "2001", 
                                   ifelse(ID$ObsCatchNr.x=="5", "2001", 
                                          ifelse(ID$ObsCatchNr.x=="6", "2001",
                                                 ifelse(ID$ObsCatchNr.x=="7", "2001",
                                                        ifelse(ID$ObsCatchNr.x=="8", "2001",
                                                               ifelse(ID$ObsCatchNr.x=="9", "2001",
                                                                      ifelse(ID$ObsCatchNr.x=="10", "2001",
                                                                             ifelse(ID$ObsCatchNr.x=="92", "2002",
                                                                                    ifelse(ID$ObsCatchNr.x=="283", "2002",
                                                                                           ifelse(ID$ObsCatchNr.x=="364", "2003",
                                                                                                  ifelse(ID$ObsCatchNr.x=="387", "2003",
                                                                                                         ifelse(ID$ObsCatchNr.x=="389", "2003",
                                                                                                                ifelse(ID$ObsCatchNr.x=="392", "2003",
                                                                                                                       ifelse(ID$ObsCatchNr.x=="395", "2003",
                                                                                                                              ifelse(ID$ObsCatchNr.x=="396","2003",
                                                                                                                                     ifelse(ID$ObsCatchNr.x=="66900332", "2017", #van winter '16-'17 to summer '17
                                                                                                                                            ifelse(ID$ObsCatchNr.x=="66900333", "2017", 
                                                                                                                                                   ifelse(ID$ObsCatchNr.x=="66900334", "2017", 
                                                                                                                                                          ifelse(ID$ObsCatchNr.x=="66900335", "2017", 
                                                                                                                                                                 ifelse(ID$ObsCatchNr.x=="66900349", "2017",
                                                                                                                                                                        ifelse(ID$ObsCatchNr.x=="66900364", "2017",
                                                                                                                                                                               ifelse(ID$ObsCatchNr.x=="66900365", "2017","2018")))))))))))))))))))))))) #396



# 9 alive states : D, P, N, B, V, T, S, R, X
ID$State<-ifelse(ID$ObsCatchNr.x=="1", "8", #emma polder
                 ifelse(ID$ObsCatchNr.x=="3", "8", #emma polder
                        ifelse(ID$ObsCatchNr.x=="4", "6", #wierum
                               ifelse(ID$ObsCatchNr.x=="5", "4", #balgzand
                                      ifelse(ID$ObsCatchNr.x=="6", "4", #mokbaai texel
                                             ifelse(ID$ObsCatchNr.x=="7", "6", #zwarte haan
                                                    ifelse(ID$ObsCatchNr.x=="8", "6", #sexbierum
                                                           ifelse(ID$ObsCatchNr.x=="9", "5", #vlieland niewue kooi
                                                                  ifelse(ID$ObsCatchNr.x=="10", "7",#paesens
                                                                         ifelse(ID$ObsCatchNr.x=="92", "4", #mokbaai texel
                                                                                ifelse(ID$ObsCatchNr.x=="283", "4", #mokbaai texel
                                                                                       ifelse(ID$ObsCatchNr.x=="364", "5", #texel volharding
                                                                                              ifelse(ID$ObsCatchNr.x=="387", "5", #vlieland westerveld
                                                                                                     ifelse(ID$ObsCatchNr.x=="389", "4", #texel mokbaai kazerne
                                                                                                            ifelse(ID$ObsCatchNr.x=="392", "6", #wierum
                                                                                                                   ifelse(ID$ObsCatchNr.x=="395", "4", "4")))))))))))))))) #395=balgzand kuitje,396=nioz strandje texel

ID$State<-ifelse(ID$ObsCatchNr.x=="1", "8", #emma polder
                 ifelse(ID$ObsCatchNr.x=="3", "8", #emma polder
                        ifelse(ID$ObsCatchNr.x=="4", "6", #wierum
                               ifelse(ID$ObsCatchNr.x=="5", "4", #balgzand
                                      ifelse(ID$ObsCatchNr.x=="6", "4", #mokbaai texel
                                             ifelse(ID$ObsCatchNr.x=="7", "6", #zwarte haan
                                                    ifelse(ID$ObsCatchNr.x=="8", "6", #sexbierum
                                                           ifelse(ID$ObsCatchNr.x=="9", "5", #vlieland niewue kooi
                                                                  ifelse(ID$ObsCatchNr.x=="10", "7",#paesens
                                                                         ifelse(ID$ObsCatchNr.x=="92", "4", #mokbaai texel
                                                                                ifelse(ID$ObsCatchNr.x=="283", "4", #mokbaai texel
                                                                                       ifelse(ID$ObsCatchNr.x=="364", "5", #texel volharding
                                                                                              ifelse(ID$ObsCatchNr.x=="387", "5", #vlieland westerveld
                                                                                                     ifelse(ID$ObsCatchNr.x=="389", "4", #texel mokbaai kazerne
                                                                                                            ifelse(ID$ObsCatchNr.x=="392", "6", #wierum
                                                                                                                   ifelse(ID$ObsCatchNr.x=="395", "4", #395=balgzand kuitje,396=nioz strandje texel
                                                                                                                          ifelse(ID$ObsCatchNr.x=="396", "4",
                                                                                                                                 ifelse(ID$ObsCatchNr.x=="66900332", "5", #vlieland
                                                                                                                                        ifelse(ID$ObsCatchNr.x=="66900333", "5", #vlieland
                                                                                                                                               ifelse(ID$ObsCatchNr.x=="66900334", "5", #vlieland
                                                                                                                                                      ifelse(ID$ObsCatchNr.x=="66900335", "7", #schier
                                                                                                                                                             ifelse(ID$ObsCatchNr.x=="66900349", "4", #balgzand
                                                                                                                                                                    ifelse(ID$ObsCatchNr.x=="66900364", "8", #eemshaven
                                                                                                                                                                           ifelse(ID$ObsCatchNr.x=="66900365", "4", #texel zuid
                                                                                                                                                                                  ifelse(ID$ObsCatchNr.x=="66902231", "5", #vlieland
                                                                                                                                                                                         ifelse(ID$ObsCatchNr.x=="66902233", "5",#vlieland
                                                                                                                                                                                                ifelse(ID$ObsCatchNr.x=="66902236", "5", #vlieland
                                                                                                                                                                                                       ifelse(ID$ObsCatchNr.x=="66902246", "6", #terschelling
                                                                                                                                                                                                              ifelse(ID$ObsCatchNr.x=="66902247", "4", #balgzand
                                                                                                                                                                                                                     ifelse(ID$ObsCatchNr.x=="66902248", "1", #delta
                                                                                                                                                                                                                            ifelse(ID$ObsCatchNr.x=="66902249", "6", #ameland
                                                                                                                                                                                                                                   ifelse(ID$ObsCatchNr.x=="66902250", "6", #terschelling
                                                                                                                                                                                                                                          ifelse(ID$ObsCatchNr.x=="66902251", "6", #terschelling
                                                                                                                                                                                                                                                 ifelse(ID$ObsCatchNr.x=="66902257", "1", #delta, 59 schier, 60 is schier
                                                                                                                                                                                                                                                        ifelse(ID$ObsCatchNr.x=="66902259", "7","7")))))))))))))))))))))))))))))))))))

## import condition/eta 
eta<-read.csv(file="YOUR_PATH/Output20210805/Output_CR_SEM_nimble_Eta.csv")

######## link data frames cond and id
Cond<-cbind(eta,ID)
Cond$ObsCatchNr<-Cond$ObsCatchNr.x
Cond$Code<-Cond$Code.x

### link with repro data
library(plyr)
df<-join(R, Cond, by = "Code", type = "left", match = "all")


### add the manual calculated condition
d<-read.csv("YOUR_PATH/CondManual.csv", header=T, dec=",", sep=";", fill=T)
# BRP-YUC7 -0,002021724
#BRB-B9YU 0,250990583
#LG-W9RX 0,999224546
#BLG-WJGP 0,772130804
#RG-RPOK -0,480152116
#RY-GKWC 1,545319109
#BLR-W1R8 -0,386789467

df$Mean[which(df$Code == "BRP-YUC7")] <- -0.002021724
df$Mean[which(df$Code == "BRB-B9YU")] <- 0.250990583
df$Mean[which(df$Code == "LG-W9RX")] <- 0.999224546
df$Mean[which(df$Code == "BLG-WJGP")] <- 0.772130804
df$Mean[which(df$Code == "RG-RPOK")] <- -0.480152116
df$Mean[which(df$Code == "RY-GKWC")] <- 1.545319109
df$Mean[which(df$Code == "BLR-W1R8")] <- -0.386789467

##################################### relationship ic/habitat and lay date for standardization and supplements #################
### plot habitat/ic and lay date! for supplements
str(df$IC)
summary(df$IC)
summary(lm(LD_Num~Habitat, data=df))

df$IC_Name<-factor(df$IC, labels = c("Island", "Mainland"))
str(df$IC_Name)

df_LD_IC<-subset(df, clutchNr==1)

library(ggplot2)
p<-ggplot(df_LD_IC, aes(y=LD_Num, x=IC_Name))+
  geom_boxplot()+
  ylab("Lay date (1=1st April)")+xlab("Breeding area")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))
p


library(dplyr)
df_LD_IC$LD_Num<-as.numeric(df_LD_IC$LD_Num)
mean_LD <- df_LD_IC %>% 
  dplyr::group_by(IC_Name) %>% 
  dplyr::summarize(meanLD=mean(LD_Num, na.rm=TRUE))
mean_LD

mean_LD_Hab <- df %>% 
  dplyr::group_by(IC_Name, Habitat) %>% 
  #dplyr::count(IC_Name, Habitat, sort = TRUE) %>%
  #dplyr::add_count(IC_Name) %>%
  #dplyr::summarize(n=n(), .groups = "keep")
  dplyr::summarize(meanLD=mean(LD_Num, na.rm=TRUE), sdLD=sd(LD_Num,na.rm=T), n=n(), .groups = "keep")

  #dplyr::add_tally() %>%
  
  #dplyr::tally()
mean_LD_Hab


ggplot(df, aes(y=LD_Num, x=Habitat))+
  geom_boxplot()

ggplot(df, aes(y=ChickSurvival, x=Habitat))+
  geom_boxplot()

## combine habitat dune and nature
levels(df$Habitat)
df$HabitatNew<-ifelse(df$Habitat=="Dune", "Nature", 
                      ifelse(df$Habitat=="Cropfield", "Cropfield",
                             ifelse(df$Habitat=="Grassland", "Grassland",
                                    ifelse(df$Habitat=="Nature", "Nature",
                                           ifelse(df$Habitat=="Urban", "Urban", "Saltmarsh")))))
summary(df$Habitat)
df$HabitatNew<-factor(df$HabitatNew)
summary(df$HabitatNew)

##################################################################################################################
############################################# bayesian approach ##################################################
##################################################################################################################

##################################################################################################################
############################################ chick survival ######################################################
##################################################################################################################

d<-df
summary(d$HabitatNew)
### standardize LD per ic or habitat? coastal can also be cropfield!

levels(df$Habitat)
d_Mainland<-subset(d,IC=="Mainland")
d_Mainland$LD_NumN<-((d_Mainland$LD_Num)-mean(d_Mainland$LD_Num, na.rm=T))/(sd(d_Mainland$LD_Num, na.rm=T))

d_Coastal<-subset(d,IC=="Coastal")
d_Coastal$LD_NumN<-((d_Coastal$LD_Num)-mean(d_Coastal$LD_Num, na.rm=T))/(sd(d_Coastal$LD_Num, na.rm=T))

d<-rbind(d_Mainland,d_Coastal)

#########################################################################################
### daily chick survival
library(dplyr)
d$ChickSuccesTransform<-ifelse(d$ChickSucces==1,0,
                                ifelse(d$ChickSucces==0,1,NA))
d_Surv<-subset(d,d$NestSucces==1,
                select=c(Mean,LD_NumN, LD_Num,Habitat, IC,HabitatNew,ChickSucces,
                         ChickSurvival, ChickSuccesTransform, clutchNr))
d_Surv$ChickSurvival<-ifelse(d_Surv$ChickSurvival<=0.5, 1,d_Surv$ChickSurvival)

d_Surv<-na.omit(d_Surv)

#d_Surv<-subset(d_Surv, clutchNr==1)

d_Surv$DCS<-1-(d_Surv$ChickSuccesTransform/d_Surv$ChickSurvival)
d_Surv = d_Surv %>% mutate(DCS_1 = ifelse(DCS == 0, 0.001, DCS))
d_Surv = d_Surv %>% mutate(DCS_2 = ifelse(DCS == 1, 0.99, DCS_1))
d_Surv$logDCS<-log(d_Surv$DCS_2)

d_Surv$DCM<-(d_Surv$ChickSuccesTransform/d_Surv$ChickSurvival)
d_Surv = d_Surv %>% mutate(DCM_1 = ifelse(DCM == 0, 0.001, DCS))
d_Surv = d_Surv %>% mutate(DCM_2 = ifelse(DCM == 1, 0.99, DCS_1))
d_Surv$logDCM<-log(d_Surv$DCM_2)

plot(log(d_Surv$DCS)~d_Surv$Mean)

plot(d_Surv$DCS~d_Surv$Mean)

d_Surv$ChickSurvival_poisson<-round(d_Surv$ChickSurvival,0)

#also difference if only chick dataset:
ggplot(d_Surv, aes(y=LD_Num, x=IC))+
  geom_boxplot()+
  ylab("Lay date (1=1st April)")+xlab("Breeding area")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))

###############
summary(d_Surv$HabitatNew)
plot(d_Surv$HabitatNew, d_Surv$ChickSurvival)
plot(d_Surv$HabitatNew, d_Surv$Mean)
d_Surv$HabitatNew1<-ifelse(d_Surv$HabitatNew=="Urban", "Urban", "Non-Urban")

plot(df$HabitatNew, df$Mean)

d_Surv_nonUrb<-subset(d_Surv,HabitatNew!="Urban")
d_Surv_nonUrb$MeanStd<-((d_Surv_nonUrb$Mean)-mean(d_Surv_nonUrb$Mean, na.rm=T))/(sd(d_Surv_nonUrb$Mean, na.rm=T))

d_Surv_Urb<-subset(d_Surv,HabitatNew=="Urban")
d_Surv_Urb$MeanStd<-((d_Surv_Urb$Mean)-mean(d_Surv_Urb$Mean, na.rm=T))/(sd(d_Surv_Urb$Mean, na.rm=T))

d_Surv<-rbind(d_Surv_Urb, d_Surv_nonUrb)
plot(d_Surv$HabitatNew, d_Surv$Mean)
plot(d_Surv$HabitatNew, d_Surv$MeanStd)
plot(d_Surv$HabitatNew, d_Surv$ChickSurvival)

p1<-ggplot(d_Surv, aes(y=ChickSurvival, x=MeanStd, shape=HabitatNew))+
  geom_point(cex=3.5)+
  ylab("Chick survival (days)")+xlab("Standardized body condition")+
  labs(shape="Habitat", tag="c)")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))
p1

p2<-ggplot(d_Surv, aes(x=HabitatNew, y=MeanStd))+
  geom_boxplot()+
  ylab("Standardized body condition")+xlab("Habitat")+
  labs(tag="a)")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))
p2

p3<-ggplot(d_Surv, aes(y=ChickSurvival, x=HabitatNew))+
  geom_boxplot()+
  ylab("Chick survival (days)")+xlab("Habitat")+
  labs(tag="b)")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))
p3

library(patchwork)
png("YOUR_PATH/ChickSurv_Habitat.png", width = 12000, height = 4000,units = 'px', res = 800)
p2+p3+p1
dev.off() 

################
summary(d_Surv$HabitatNew)
d_Surv$Cr<-ifelse(d_Surv$HabitatNew=="Cropfield", 1,0) #cropfield y=0

mean(d_Surv$Mean) #0.09137241
sd(d_Surv$Mean) #0.9401101
sd(d_Surv$ChickSurvival_poisson)
mean(d_Surv$ChickSurvival_poisson) #18.24 days
mean(d_Surv$ChickSurvival) #18.2 days

str(d_Surv$clutchNr)

################
library(brms)
library(tidyverse)
summary(d_Surv)
chick_mod <- bf(ChickSurvival_poisson ~ MeanStd+LD_NumN+Cr, family = "poisson") #DCS_2
#chick_mod <- bf(DCS_2 ~ Mean+LD_NumN, family = "beta") #DCS_2
LD_mod <- bf(LD_NumN ~ MeanStd+Cr, family="gaussian")

head(d_Surv)

chick_fit_brms <- brm(chick_mod +
                    LD_mod + 
                    set_rescor(FALSE), 
                  data=d_Surv,
                  cores=4, chains = 2, iter=15000, warmup=4000)

plot(chick_fit_brms, N=6)

s_CS<-summary(chick_fit_brms, intercept=FALSE)
s_CS

#### plot results:
Family: MV(poisson, gaussian) 
Links: mu = log
mu = identity; sigma = identity 
Formula: ChickSurvival_poisson ~ MeanStd + LD_NumN + Cr 
LD_NumN ~ MeanStd + Cr 
Data: d_Surv (Number of observations: 25) 
Samples: 2 chains, each with iter = 15000; warmup = 4000; thin = 1;
total post-warmup samples = 22000

Population-Level Effects: 
                                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
ChickSurvivalpoisson_Intercept     3.07      0.07     2.93     3.20 1.00    29994    17469
LDNumN_Intercept                  -0.50      0.18    -0.85    -0.15 1.00    30988    15390
ChickSurvivalpoisson_MeanStd       0.13      0.05     0.02     0.23 1.00    34579    16613
ChickSurvivalpoisson_LD_NumN      -0.16      0.08    -0.32     0.00 1.00    27197    17823
ChickSurvivalpoisson_Cr           -1.13      0.16    -1.44    -0.82 1.00    22815    18784
LDNumN_MeanStd                     0.26      0.15    -0.05     0.56 1.00    32016    16648
LDNumN_Cr                          0.78      0.31     0.16     1.39 1.00    27518    16271

Family Specific Parameters: 
  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma_LDNumN     0.72      0.11     0.54     0.98 1.00    25635    16859

Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).

## R2
bayes_R2(chick_fit_brms)

Estimate  Est.Error       Q2.5     Q97.5
R2ChickSurvivalpoisson 0.4276231 0.04625859 0.33106623 0.5118687
R2LDNumN               0.3338052 0.11793642 0.08041352 0.5284017

##probability of direction:
## cond on surv
pd_Ch_CondSurv_1<-chick_fit_brms[["fit"]]@sim[["samples"]][[1]][["b_ChickSurvivalpoisson_MeanStd"]] #chain 1
pd_Ch_CondSurv_2<-chick_fit_brms[["fit"]]@sim[["samples"]][[2]][["b_ChickSurvivalpoisson_MeanStd"]] #chain 1
pd_Ch_CondSurv<-c(pd_Ch_CondSurv_1,pd_Ch_CondSurv_2)
round(mean(pd_Ch_CondSurv>0),2) #0.99

pd_Ch_LDSurv_1<-chick_fit_brms[["fit"]]@sim[["samples"]][[1]][["b_ChickSurvivalpoisson_LD_NumN"]] #chain 1
pd_Ch_LDSurv_2<-chick_fit_brms[["fit"]]@sim[["samples"]][[2]][["b_ChickSurvivalpoisson_LD_NumN"]] #chain 1
pd_Ch_LDSurv<-c(pd_Ch_LDSurv_1,pd_Ch_LDSurv_2)
round(mean(pd_Ch_LDSurv<0),2) #0.97

pd_Ch_CondLD_1<-chick_fit_brms[["fit"]]@sim[["samples"]][[1]][["b_LDNumN_MeanStd"]] #chain 1
pd_Ch_CondLD_2<-chick_fit_brms[["fit"]]@sim[["samples"]][[2]][["b_LDNumN_MeanStd"]] #chain 1
pd_Ch_CondLD<-c(pd_Ch_CondLD_1,pd_Ch_CondLD_2)
round(mean(pd_Ch_CondLD_1>0),2) #0.95


#####################################################################
########### plot convergence #########################################
######################################################################
library(bayesplot)
con_1<-mcmc_dens(chick_fit_brms, pars=c("b_ChickSurvivalpoisson_Intercept")) +#pars = c("wt", "sigma")
  labs(tag="a)", title="intercept chick survival", x="")
con_2<-mcmc_dens(chick_fit_brms, pars=c("b_LDNumN_Intercept")) +#pars = c("wt", "sigma")
  labs(tag="b)", title="intercept lay date", x="")
con_3<-mcmc_dens(chick_fit_brms, pars=c("b_ChickSurvivalpoisson_MeanStd")) +#pars = c("wt", "sigma")
  labs(tag="c)", title="condition -> chick survival", x="")
con_4<-mcmc_dens(chick_fit_brms, pars=c("b_ChickSurvivalpoisson_LD_NumN")) +#pars = c("wt", "sigma")
  labs(tag="d)", title="lay date -> chick survival", x="")
con_5<-mcmc_dens(chick_fit_brms, pars=c("b_LDNumN_MeanStd")) +#pars = c("wt", "sigma")
  labs(tag="e)", title="condition -> lay date", x="")
con_6<-mcmc_dens(chick_fit_brms, pars=c("sigma_LDNumN")) +#pars = c("wt", "sigma")
  labs(tag="f)", title="sigma lay date", x="")

con_8<-mcmc_trace(chick_fit_brms, pars=c("b_ChickSurvivalpoisson_Intercept")) +#pars = c("wt", "sigma")
  labs(tag="g)", title="intercept chick survival", y="")
con_9<-mcmc_trace(chick_fit_brms, pars=c("b_LDNumN_Intercept")) +#pars = c("wt", "sigma")
  labs(tag="h)", title="intercept lay date", y="")
con_10<-mcmc_trace(chick_fit_brms, pars=c("b_ChickSurvivalpoisson_MeanStd")) +#pars = c("wt", "sigma")
  labs(tag="i)", title="condition -> chick survival", y="")
con_11<-mcmc_trace(chick_fit_brms, pars=c("b_ChickSurvivalpoisson_LD_NumN")) +#pars = c("wt", "sigma")
  labs(tag="j)", title="lay date -> chick survival", y="")
con_12<-mcmc_trace(chick_fit_brms, pars=c("b_LDNumN_MeanStd")) +#pars = c("wt", "sigma")
  labs(tag="k)", title="condition -> lay date", y="")
con_13<-mcmc_trace(chick_fit_brms, pars=c("sigma_LDNumN")) +#pars = c("wt", "sigma")
  labs(tag="l)", title="sigma lay date", y="")

library(patchwork)
png("YOUR_PATH/ConvergenceChickSucces.png", width = 9000, height = 12000,units = 'px', res = 800)
(con_1|con_8)/(con_2|con_9)/(con_3|con_10)/(con_4|con_11)/(con_5|con_12)/(con_6|con_13)
dev.off() 

#Table with Rhat as well:
CS_table <- matrix(c(s_CS$fixed[1,1],s_CS$fixed[1,3],s_CS$fixed[1,4],s_CS$fixed[1,5], s_CS$fixed[1,6], s_CS$fixed[1,7], 
                     s_CS$fixed[2,1],s_CS$fixed[2,3],s_CS$fixed[2,4],s_CS$fixed[2,5], s_CS$fixed[2,6], s_CS$fixed[2,7],
                     s_CS$fixed[3,1],s_CS$fixed[3,3],s_CS$fixed[3,4],s_CS$fixed[3,5],s_CS$fixed[3,6],s_CS$fixed[3,7],
                     s_CS$fixed[4,1],s_CS$fixed[4,3],s_CS$fixed[4,4],s_CS$fixed[4,5],s_CS$fixed[4,6],s_CS$fixed[4,7],
                     s_CS$fixed[5,1],s_CS$fixed[5,3],s_CS$fixed[5,4],s_CS$fixed[5,5],s_CS$fixed[5,6],s_CS$fixed[5,7],
                     s_CS$fixed[6,1],s_CS$fixed[6,3],s_CS$fixed[6,4],s_CS$fixed[6,5],s_CS$fixed[5,6],s_CS$fixed[5,7],
                     s_CS$fixed[7,1],s_CS$fixed[7,3],s_CS$fixed[7,4],s_CS$fixed[7,5],s_CS$fixed[5,6],s_CS$fixed[5,7],
                     s_CS$fixed[1,1],s_CS$fixed[1,3],s_CS$fixed[1,4],s_CS$spec_pars[1,5],s_CS$spec_pars[1,6],s_CS$spec_pars[1,7]),
                     #round(s_CS$spec_pars[2,5],0),s_CS$spec_pars[2,6],s_CS$spec_pars[2,7]), 
                   ncol=6, byrow=TRUE)
colnames(CS_table) <- c("Mean", "lower 95% CI", "upper 95% CI", "R-hat", "Bulk-ESS", "Tail-ESS")
rownames(CS_table) <- c("intercept chick survival","intercept lay date", "condition -> chick survival", 
                        "lay date -> chick survival", "crop -> chick survival",
                        "condition -> lay date", "crop -> lay date",
                        "sigma lay date")
CS_table<-round(CS_table,3)
CS_table

knitr::kable(CS_table,digits=2, format="pandoc")
kable_out_CS_table <- knitr::kable(CS_table, "html", digits=4) %>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
kable_out_CS_table
readr::write_file(kable_out_CS_table, "YOUR_PATH/CS_table.doc")

##################################################################################################################
# -------------------------------------- plot chick survival ----------------------------------------------------
### plot effect of condition on laying date
# extract estimates per iteration
LDNumN_Intercept<-c(chick_fit_brms$fit@sim$samples[[1]]$b_LDNumN_Intercept,chick_fit_brms$fit@sim$samples[[2]]$b_LDNumN_Intercept)
LDNumN_Mean<-c(chick_fit_brms$fit@sim$samples[[1]]$b_LDNumN_MeanStd,chick_fit_brms$fit@sim$samples[[2]]$b_LDNumN_MeanStd)
LDNumN_Cr<-c(chick_fit_brms$fit@sim$samples[[1]]$b_LDNumN_Cr,chick_fit_brms$fit@sim$samples[[2]]$b_LDNumN_Cr)

mean.Cr = mean(d_Surv$Cr) # mean laying date
mean.mat.Cr = matrix(rep(mean.Cr,nrow(d_Surv)),ncol=1) # matrix of mean laying date

predicted_LD <- matrix(NA, nrow = length(LDNumN_Intercept), ncol = length(d_Surv[,1]))
for (i in 1:length(LDNumN_Intercept)){ #for each iteration
  for (j in 1:length((d_Surv[,1]))){ # for each individual
    predicted_LD[i,j] <- LDNumN_Intercept[i] +  LDNumN_Mean[i] * d_Surv[j,21] + LDNumN_Cr[i] * mean.mat.Cr[j]
  }
}

mean_LD <- apply(predicted_LD, 2, mean)
lciLD <- apply(predicted_LD, 2, quantile, prob = 2.5/100) #2.5, 25
uciLD <- apply(predicted_LD, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(d_Surv[,21]) #stvov for standardized
dfLD <- data.frame(cond = d_Surv[,21][ord], #stcov for standardized
                  LD = mean_LD[ord],
                  lciLD = lciLD[ord],
                  uciLD = uciLD[ord])

summary(d_Surv$MeanStd)
b <- seq(-2.4,2.4,0.6)
#test$EW_Bin<-cut(test$EW_Imp,breaks=b)
d_Surv$Cond_Bin<-cut(d_Surv$MeanStd,breaks=b)
summary(d_Surv$Cond_Bin)
df1<-d_Surv %>%
  dplyr::group_by(Cond_Bin) %>%
  dplyr::summarise(mean = mean(LD_NumN , na.rm=T), n = n())
df1
df1<-data.frame(df1)
df1$CondMean<-seq(-2.1,2.1,0.6)
df1$CondMean<-c(-2.1, -1.5, -0.9, -0.3,  0.3  ,0.9  ,1.5  ,2.0)

library(ggplot2)
p_LD<-ggplot(dfLD, aes(x = cond, y = LD)) + 
  geom_point(df1,mapping=aes(x=CondMean,y=mean, size=n),cex=6, alpha=0.5, col="#0072B2")+ #stcov for standardized
  geom_line(data=dfLD,mapping=aes(x = cond, y = LD), col="#0072B2",cex=4, lty="dashed") + 
  geom_ribbon(data=dfLD,aes(ymin = lciLD, ymax = uciLD), fill="#0072B2",alpha = 0.25)+
  labs(x = "body condition (standardized)", y = "laying date (standardized)")+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
        #axis.title.y=element_blank())
p_LD

# plot with residuals
head(dfLD)
head(d_Surv)

#d_Surv$residuals_chick_cond_laydate <- as.vector(d_Surv$LD_NumN - dfLD$LD)
res_chick <- as.data.frame(residuals(chick_fit_brms))
fit_chick <- as.data.frame(fitted(chick_fit_brms))
head(fit_chick)
d_Surv_chick <- cbind(d_Surv, fit_chick)
head(d_Surv_chick)

p_LD<-ggplot(dfLD, aes(x = cond, y = LD)) + 
  geom_point(d_Surv_chick,mapping=aes(x=MeanStd,y=Estimate.LDNumN), cex=6, alpha=0.5, col="#0072B2")+ #stcov for standardized
  #geom_point(mapping=aes(x=d_Surv[,21],y=d_Surv[,2]), alpha=0.5, col="#0072B2", cex=6)+ #stcov for standardized
  geom_line(data=dfLD,mapping=aes(x = cond, y = LD), col="#0072B2",cex=4, lty="dashed") + 
  geom_ribbon(data=dfLD,aes(ymin = lciLD, ymax = uciLD), fill="#0072B2",alpha = 0.25)+
  labs(x = "body condition (standardized)", y = "laying date (standardized)")+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_LD

## plot with residuals and bins
b <- seq(-2.4,2.4,0.6)
#test$EW_Bin<-cut(test$EW_Imp,breaks=b)
d_Surv_chick$Cond_Bin<-cut(d_Surv_chick$MeanStd,breaks=b)
summary(d_Surv_chick$Cond_Bin)
df1<-d_Surv_chick %>%
  dplyr::group_by(Cond_Bin) %>%
  dplyr::summarise(mean = mean(Estimate.LDNumN , na.rm=T), n = n())
df1
df1<-data.frame(df1)
df1$CondMean<-seq(-2.1,2.1,0.6)
df1$CondMean<-c(-2.1, -1.5, -0.9, -0.3,  0.3  ,0.9  ,1.5  ,2.0)
df1$n10 <- df1$n*10

p_LD<-ggplot(dfLD, aes(x = cond, y = LD)) + 
  geom_point(df1,mapping=aes(x=CondMean,y=mean, size=n10), alpha=0.5, col="#0072B2")+ #stcov for standardized
  #geom_point(mapping=aes(x=d_Surv[,21],y=d_Surv[,2]), alpha=0.5, col="#0072B2", cex=6)+ #stcov for standardized
  geom_line(data=dfLD,mapping=aes(x = cond, y = LD), col="#0072B2",cex=4, lty="dashed") + 
  geom_ribbon(data=dfLD,aes(ymin = lciLD, ymax = uciLD), fill="#0072B2",alpha = 0.25)+
  labs(x = "body condition (s.d.)", y = "laying date (s.d.)")+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_LD

################################################################################
### plot effect of condition on chick surv
# extract estimates per iteration
DCS_Intercept<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_Intercept,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_Intercept)
DCS_Mean<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_MeanStd,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_MeanStd)
DCS_LD<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_LD_NumN,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_LD_NumN)
DCS_Cr<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_Cr,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_Cr)

mean.LD = mean(d_Surv$LD_NumN) # mean laying date
mean.mat.LD = matrix(rep(mean.LD,nrow(d_Surv)),ncol=1) # matrix of mean laying date

mean.Cr = mean(d_Surv$Cr) # mean laying date
mean.mat.Cr = matrix(rep(mean.Cr,nrow(d_Surv)),ncol=1) # matrix of mean laying date

predicted_DCS <- matrix(NA, nrow = length(DCS_Intercept), ncol = length(d_Surv[,1]))
for (i in 1:length(DCS_Intercept)){ #for each iteration
  for (j in 1:length((d_Surv[,1]))){ # for each individual
    predicted_DCS[i,j] <- exp(DCS_Intercept[i] +  DCS_Mean[i] * d_Surv[j,21] + DCS_LD[i] * mean.mat.LD[j] + DCS_Cr[i] * mean.mat.Cr[j])
  }
}

mean_DCS <- apply(predicted_DCS, 2, mean)
lciDCS <- apply(predicted_DCS, 2, quantile, prob = 2.5/100) #2.5, 25
uciDCS <- apply(predicted_DCS, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(d_Surv[,21]) #stvov for standardized
dfDCS <- data.frame(cond = d_Surv[,21][ord], #stcov for standardized
                   DCS = mean_DCS[ord],
                   lciDCS = lciDCS[ord],
                   uciDCS = uciDCS[ord])
head(dfDCS)

library(radiant.data)
library(dplyr)
head(d_Surv)
summary(d_Surv$MeanStd)
b <- seq(-2.2,2.6,0.6)
#test$EW_Bin<-cut(test$EW_Imp,breaks=b)
d_Surv$Cond_Bin<-cut(d_Surv$MeanStd,breaks=b)
summary(d_Surv$Cond_Bin)
df2<-d_Surv %>%
  dplyr::group_by(Cond_Bin) %>%
  dplyr::summarise(mean = mean(ChickSurvival_poisson , na.rm=T), n = n())
df2
df2<-data.frame(df2)
df2$CondMean<-seq(-1.9,2.3,0.6)

p_DCS<-ggplot(dfDCS, aes(x = cond, y = DCS)) + 
  geom_point(mapping=aes(x=d_Surv[,21],y=d_Surv[,19]), alpha=0.5, cex=6, col="#0072B2")+ #stcov for standardized
  #geom_point(df2,mapping=aes(x=CondMean,y=mean, size=n), alpha=0.5, cex=6, col="#0072B2")+ #stcov for standardized
  geom_line(data=dfDCS,mapping=aes(x = cond, y = DCS), cex=4, col="#0072B2") + 
  geom_ribbon(data=dfDCS,aes(ymin = lciDCS, ymax = uciDCS), fill="#0072B2",alpha = 0.25)+
  labs(x = "body condition (standardized)", y = "chick survival (days)")+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_DCS

# plot with residuals
p_DCS<-ggplot(dfDCS, aes(x = cond, y = DCS)) + 
  geom_point(mapping=aes(x=d_Surv_chick$MeanStd,y=d_Surv_chick$Estimate.ChickSurvivalpoisson), alpha=0.5, cex=6, col="#0072B2")+ #stcov for standardized
  #geom_point(df2,mapping=aes(x=CondMean,y=mean, size=n), alpha=0.5, cex=6, col="#0072B2")+ #stcov for standardized
  geom_line(data=dfDCS,mapping=aes(x = cond, y = DCS), cex=4, col="#0072B2") + 
  geom_ribbon(data=dfDCS,aes(ymin = lciDCS, ymax = uciDCS), fill="#0072B2",alpha = 0.25)+
  labs(x = "body condition (standardized)", y = "chick survival (days)")+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_DCS


#residuals and bins
b <- seq(-2.3,2.3,0.3) #2.4, 0.6
d_Surv_chick$Cond_Bin<-cut(d_Surv_chick$MeanStd,breaks=b)
summary(d_Surv_chick$Cond_Bin)
df2<-d_Surv_chick %>%
  dplyr::group_by(Cond_Bin) %>%
  dplyr::summarise(mean = mean(Estimate.ChickSurvivalpoisson , na.rm=T), n = n())
df2
df2<-data.frame(df2)
df2$CondMean<-c(-2.15,-1.25, -0.95, -0.65, -0.35, -0.05,0.25, 0.55,0.85, 1.15,1.45,2.05)

p_DCS<-ggplot(dfDCS, aes(x = cond, y = DCS)) + 
  #geom_point(mapping=aes(x=d_Surv[,21],y=d_Surv[,19]), alpha=0.5, cex=6, col="#0072B2")+ #stcov for standardized
  geom_ribbon(data=dfDCS,aes(ymin = lciDCS, ymax = uciDCS), fill="#0072B2",alpha = 0.25)+
  geom_line(data=dfDCS,mapping=aes(x = cond, y = DCS), cex=4, col="#0072B2") + 
  geom_point(df2,mapping=aes(x=CondMean,y=mean, size=n), alpha=0.5, col="#0072B2")+ #stcov for standardized
  #geom_point(d_Surv_chick,mapping=aes(x=MeanStd,y=Estimate.ChickSurvivalpoisson), cex=6, alpha=0.5, col="#0072B2")+ #stcov for standardized
  labs(x = "body condition (standardized)", y = "chick survival (days)")+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_DCS

### plot effect of laying date on chick surv
# extract estimates per iteration
DCS_Intercept<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_Intercept,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_Intercept)
DCS_Mean<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_MeanStd,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_MeanStd)
DCS_LD<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_LD_NumN,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_LD_NumN)

mean.cond = mean(d_Surv$MeanStd) # mean condition
mean.mat.cond = matrix(rep(mean.cond,nrow(d_Surv)),ncol=1) # matrix of mean condition

predicted_DCS_LD <- matrix(NA, nrow = length(DCS_Intercept), ncol = length(d_Surv[,1]))
for (i in 1:length(DCS_Intercept)){ #for each iteration
  for (j in 1:length((d_Surv[,1]))){ # for each individual
    predicted_DCS_LD[i,j] <- exp(DCS_Intercept[i] +  DCS_Mean[i] * mean.mat.cond[j] + DCS_LD[i] * d_Surv[j,2] +DCS_Cr[i] * mean.mat.Cr[j])
  }
}

mean_DCS_LD <- apply(predicted_DCS_LD, 2, mean)
lciDCS_LD <- apply(predicted_DCS_LD, 2, quantile, prob = 2.5/100) #2.5, 25
uciDCS_LD <- apply(predicted_DCS_LD, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(d_Surv[,2]) #stvov for standardized, laying date
dfDCS_LD <- data.frame(LD = d_Surv[,2][ord], #stcov for standardized
                    DCS = mean_DCS_LD[ord],
                    lciDCS_LD = lciDCS_LD[ord],
                    uciDCS_LD = uciDCS_LD[ord])
head(dfDCS_LD)
summary(dfDCS_LD)

library(radiant.data)
library(dplyr)
head(d_Surv)
summary(d_Surv$LD_NumN)
b <- seq(-1.6,1.9,0.5)
d_Surv$LD_Bin<-cut(d_Surv$LD_NumN,breaks=b)
summary(d_Surv$LD_Bin)
df3<-d_Surv %>%
  dplyr::group_by(LD_Bin) %>%
  dplyr::summarise(mean = mean(ChickSurvival_poisson , na.rm=T), n = n())
df3
df3<-data.frame(df3)
df3$LDMean<-seq(-1.35,1.65,0.5)

p_DCS_LD<-ggplot(dfDCS_LD, aes(x = LD, y = DCS)) + 
  geom_point(df3,mapping=aes(x=LDMean,y=mean, size=n), alpha=0.5, cex=6, col="#0072B2")+ #stcov for standardized
  #geom_point(mapping=aes(x=d_Surv[,2],y=d_Surv[,19]), alpha=0.5, col="#0072B2", cex=6)+ #stcov for standardized
  geom_line(data=dfDCS_LD,mapping=aes(x = LD, y = DCS), cex=4, col="#0072B2") + 
  geom_ribbon(data=dfDCS_LD,aes(ymin = lciDCS_LD, ymax = uciDCS_LD), alpha = 0.25, fill="#0072B2")+
  labs(x = "laying date", y = "chick survival (days)")+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_DCS_LD

#RESIDUALS AND BINS
b <- seq(-1.6,1.9,0.5)
d_Surv_chick$LD_Bin<-cut(d_Surv_chick$LD_NumN,breaks=b)
summary(d_Surv_chick$LD_Bin)
df3<-d_Surv_chick%>%
  dplyr::group_by(LD_Bin) %>%
  dplyr::summarise(mean = mean(Estimate.ChickSurvivalpoisson , na.rm=T), n = n())
df3
df3<-data.frame(df3)
df3$LDMean<-seq(-1.35,1.65,0.5)

p_DCS_LD<-ggplot(dfDCS_LD, aes(x = LD, y = DCS)) + 
  geom_point(df3,mapping=aes(x=LDMean,y=mean, size=n), alpha=0.5, col="#0072B2")+ #stcov for standardized
  #geom_point(mapping=aes(x=d_Surv[,2],y=d_Surv[,19]), alpha=0.5, col="#0072B2", cex=6)+ #stcov for standardized
  geom_line(data=dfDCS_LD,mapping=aes(x = LD, y = DCS), cex=4, col="#0072B2") + 
  geom_ribbon(data=dfDCS_LD,aes(ymin = lciDCS_LD, ymax = uciDCS_LD), alpha = 0.25, fill="#0072B2")+
  labs(x = "laying date", y = "chick survival (days)")+
  theme_bw()+
  scale_y_continuous(position = "right")+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_DCS_LD

####################################################################################################################
################################################### nest survival ##################################################
####################################################################################################################
df_Nest<-df
df_Nest$DNS<-1-(df_Nest$Fail/df_Nest$ObsDay)
levels(df_Nest$Habitat)
summary(df_Nest$Habitat)

df_Nest_Mainland<-subset(df_Nest,IC=="Mainland")
df_Nest_Mainland$LD_NumN_1<-((df_Nest_Mainland$LD_Num)-mean(df_Nest_Mainland$LD_Num, na.rm=T))/(sd(df_Nest_Mainland$LD_Num, na.rm=T))

df_Nest_Coastal<-subset(df_Nest,IC=="Coastal")
df_Nest_Coastal$LD_NumN_1<-((df_Nest_Coastal$LD_Num)-mean(df_Nest_Coastal$LD_Num, na.rm=T))/(sd(df_Nest_Coastal$LD_Num, na.rm=T))

df_Nest<-rbind(df_Nest_Mainland,df_Nest_Coastal)

#df_Nest_Inland<-subset(df_Nest, Habitat=="Cropfield"|Habitat=="Grassland"|Habitat=="Urban"|Habitat=="Dune"|Habitat=="Nature")
#df_Nest_Island<-subset(df_Nest, Habitat=="Saltmarsh")
#df_Nest_Inland$LD_NumN_1<-((df_Nest_Inland$LD_Num)-mean(df_Nest_Inland$LD_Num, na.rm=T))/(sd(df_Nest_Inland$LD_Num, na.rm=T))
#df_Nest_Island$LD_NumN_1<-((df_Nest_Island$LD_Num)-mean(df_Nest_Island$LD_Num, na.rm=T))/(sd(df_Nest_Island$LD_Num, na.rm=T))
#df_Nest<-rbind(df_Nest_Island,df_Nest_Inland)

NS<-subset(df_Nest,
           select=c(DNS,Mean,ObsDay,FailTransform, Fail, HabitatNew, LD_NumN_1, LD_Num, IC))
summary(NS)

library(dplyr)
NS = NS %>% mutate(DNS_1 = ifelse(DNS == 0, 0.001, DNS))
NS = NS %>% mutate(DNS_2 = ifelse(DNS == 1, 0.99, DNS_1))

NS<-na.omit(NS)

mean(NS$DNS_2) #0.9640455
sd(NS$DNS_2) #0.0711608
mean(NS$Mean) #-0.1045528
sd(NS$Mean) #1.043292


ggplot(NS, aes(y=LD_Num, x=IC))+
  geom_boxplot()+
  ylab("Lay date")+xlab("Habitat")+
  labs(tag="b)")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))

### standardize condition (low cond in urban hab)
NS_nonUrb<-subset(NS,HabitatNew!="Urban")
NS_nonUrb$MeanStd<-((NS_nonUrb$Mean)-mean(NS_nonUrb$Mean, na.rm=T))/(sd(NS_nonUrb$Mean, na.rm=T))

NS_Urb<-subset(NS,HabitatNew=="Urban")
NS_Urb$MeanStd<-((NS_Urb$Mean)-mean(NS_Urb$Mean, na.rm=T))/(sd(NS_Urb$Mean, na.rm=T))

NS<-rbind(NS_Urb, NS_nonUrb)

### check effect habitat
p4<-ggplot(NS, aes(y=DNS_2, x=MeanStd, shape=HabitatNew))+
  geom_point(cex=3.5)+
  ylab("Daily nest survival probability")+xlab("Standardized body condition")+
  labs(shape="Habitat", tag="c)")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))
p4

p5<-ggplot(NS, aes(x=HabitatNew, y=MeanStd))+
  geom_boxplot()+
  ylab("Standardized body condition")+xlab("Habitat")+
  labs(tag="a)")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))
p5

p6<-ggplot(NS, aes(y=DNS_2, x=HabitatNew))+
  geom_boxplot()+
  ylab("daily nest survival probability")+xlab("Habitat")+
  labs(tag="b)")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))
p6

library(patchwork)
png("YOUR_PATH/NestSurv_Habitat.png", width = 12000, height = 4000,units = 'px', res = 800)
p5+p6+p4
dev.off() 
#############

library(brms)
DNS_mod1 <- bf(DNS_2 ~ MeanStd+LD_NumN_1, family = "beta")
LD_mod1 <- bf(LD_NumN_1 ~ MeanStd, family="gaussian")

nest_fit_brms <- brm(DNS_mod1 +
                    LD_mod1 + 
                    set_rescor(FALSE), 
                  data=NS,
                  cores=4, chains = 2, iter=15000, warmup=4000)

#plot(nest_fit_brms)
s_NS<-summary(nest_fit_brms, intercept=TRUE)
s_NS

#output
Family: MV(beta, gaussian) 
Links: mu = logit; phi = identity
mu = identity; sigma = identity 
Formula: DNS_2 ~ MeanStd + LD_NumN_1 
LD_NumN_1 ~ MeanStd 
Data: NS (Number of observations: 32) 
Samples: 2 chains, each with iter = 15000; warmup = 4000; thin = 1;
total post-warmup samples = 22000

Population-Level Effects: 
  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
DNS2_Intercept        3.32      0.21     2.89     3.72 1.00    18900    15159
LDNumN1_Intercept     0.08      0.20    -0.33     0.47 1.00    29637    16069
DNS2_MeanStd          0.34      0.17    -0.00     0.68 1.00    29084    17234
DNS2_LD_NumN_1       -0.29      0.13    -0.53    -0.01 1.00    28529    16692
LDNumN1_MeanStd       0.14      0.21    -0.28     0.56 1.00    29716    16133

Family Specific Parameters: 
  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
phi_DNS2         22.34      6.58    11.36    36.95 1.00    19233    15678
sigma_LDNumN1     1.15      0.16     0.89     1.50 1.00    28795    15978

Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).

#r2
bayes_R2(nest_fit_brms)

Estimate  Est.Error         Q2.5     Q97.5
R2DNS2    0.08869987 0.05997597 8.164721e-03 0.2361690
R2LDNumN1 0.04306444 0.05080465 6.463582e-05 0.1828242

#######
##probability of direction:
## cond on surv
pd_NS_CondSurv_1<-nest_fit_brms[["fit"]]@sim[["samples"]][[1]][["b_DNS2_MeanStd"]] #chain 1
pd_NS_CondSurv_2<-nest_fit_brms[["fit"]]@sim[["samples"]][[2]][["b_DNS2_MeanStd"]] #chain 1
pd_NS_CondSurv<-c(pd_NS_CondSurv_1,pd_NS_CondSurv_2)
round(mean(pd_NS_CondSurv>0),2) #0.97

pd_NS_LDSurv_1<-nest_fit_brms[["fit"]]@sim[["samples"]][[1]][["b_DNS2_LD_NumN_1"]] #chain 1
pd_NS_LDSurv_2<-nest_fit_brms[["fit"]]@sim[["samples"]][[2]][["b_DNS2_LD_NumN_1"]] #chain 1
pd_NS_LDSurv<-c(pd_NS_LDSurv_1,pd_NS_LDSurv_2)
round(mean(pd_NS_LDSurv<0),2) #0.98

pd_NS_CondLD_1<-nest_fit_brms[["fit"]]@sim[["samples"]][[1]][["b_LDNumN1_MeanStd"]] #chain 1
pd_NS_CondLD_2<-nest_fit_brms[["fit"]]@sim[["samples"]][[2]][["b_LDNumN1_MeanStd"]] #chain 1
pd_NS_CondLD<-c(pd_NS_CondLD_1,pd_NS_CondLD_2)
round(mean(pd_NS_CondLD>0),2) #0.75

library(MuMIn)
r.squaredGLMM(nest_fit_brms)
#R2m       R2c
#theoretical 0.21582417 0.5708809
#delta       0.06817415 0.1803288

#####################################################################
########### plot convergence #########################################
######################################################################
library(bayesplot)
con_1a<-mcmc_dens(nest_fit_brms, pars=c("b_DNS2_Intercept")) +#pars = c("wt", "sigma")
  labs(tag="a)", title="intercept nest survival", x="")
con_2a<-mcmc_dens(nest_fit_brms, pars=c("b_LDNumN1_Intercept")) +#pars = c("wt", "sigma")
  labs(tag="b)", title="intercept lay date", x="")
con_3a<-mcmc_dens(nest_fit_brms, pars=c("b_DNS2_MeanStd")) +#pars = c("wt", "sigma")
  labs(tag="c)", title="condition -> nest survival", x="")
con_4a<-mcmc_dens(nest_fit_brms, pars=c("b_DNS2_LD_NumN_1")) +#pars = c("wt", "sigma")
  labs(tag="d)", title="lay date -> nest survival", x="")
con_5a<-mcmc_dens(nest_fit_brms, pars=c("b_LDNumN1_MeanStd")) +#pars = c("wt", "sigma")
  labs(tag="e)", title="condition -> lay date", x="")
con_6a<-mcmc_dens(nest_fit_brms, pars=c("sigma_LDNumN1")) +#pars = c("wt", "sigma")
  labs(tag="f)", title="sigma lay date", x="")
con_7a<-mcmc_dens(nest_fit_brms, pars=c("phi_DNS2")) +#pars = c("wt", "sigma")
  labs(tag="g)", title="sigma nest survival", x="")

con_8a<-mcmc_trace(nest_fit_brms, pars=c("b_DNS2_Intercept")) +#pars = c("wt", "sigma")
  labs(tag="h)", title="intercept nest survival", y="")
con_9a<-mcmc_trace(nest_fit_brms, pars=c("b_LDNumN1_Intercept")) +#pars = c("wt", "sigma")
  labs(tag="i)", title="intercept lay date", y="")
con_10a<-mcmc_trace(nest_fit_brms, pars=c("b_DNS2_MeanStd")) +#pars = c("wt", "sigma")
  labs(tag="j)", title="condition -> nest survival", y="")
con_11a<-mcmc_trace(nest_fit_brms, pars=c("b_DNS2_LD_NumN_1")) +#pars = c("wt", "sigma")
  labs(tag="k)", title="lay date -> nest survival", y="")
con_12a<-mcmc_trace(nest_fit_brms, pars=c("b_LDNumN1_MeanStd")) +#pars = c("wt", "sigma")
  labs(tag="l)", title="condition -> lay date", y="")
con_13a<-mcmc_trace(nest_fit_brms, pars=c("sigma_LDNumN1")) +#pars = c("wt", "sigma")
  labs(tag="m)", title="sigma lay date", y="")
con_14a<-mcmc_trace(nest_fit_brms, pars=c("phi_DNS2")) +#pars = c("wt", "sigma")
  labs(tag="n)", title="sigma nest survival", y="")

library(patchwork)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/PathAnalysisConvergence/ConvergenceNestSucces.png", width = 9000, height = 12000,units = 'px', res = 800)
(con_1a|con_8a)/(con_2a|con_9a)/(con_3a|con_10a)/(con_4a|con_11a)/(con_5a|con_12a)/(con_6a|con_13a)/(con_7a|con_14a)
dev.off() 

#Table with Rhat as well:
NS_table <- matrix(c(s_NS$fixed[1,1],s_NS$fixed[1,3],s_NS$fixed[1,4],s_NS$fixed[1,5], s_NS$fixed[1,6], s_NS$fixed[1,7], 
                     s_NS$fixed[2,1],s_NS$fixed[2,3],s_NS$fixed[2,4],s_NS$fixed[2,5], s_NS$fixed[2,6], s_NS$fixed[2,7],
                     s_NS$fixed[3,1],s_NS$fixed[3,3],s_NS$fixed[3,4],s_NS$fixed[3,5],s_NS$fixed[3,6],s_NS$fixed[3,7],
                     s_NS$fixed[4,1],s_NS$fixed[4,3],s_NS$fixed[4,4],s_NS$fixed[4,5],s_NS$fixed[4,6],s_NS$fixed[4,7],
                     s_NS$fixed[5,1],s_NS$fixed[5,3],s_NS$fixed[5,4],s_NS$fixed[5,5],s_NS$fixed[5,6],s_NS$fixed[5,7],
                     s_NS$spec_pars[1,1],s_NS$spec_pars[1,3],s_NS$spec_pars[1,4],s_NS$spec_pars[1,5],s_NS$spec_pars[1,6],s_NS$spec_pars[1,7],
                     s_NS$spec_pars[2,1],s_NS$spec_pars[2,3],s_NS$spec_pars[2,4],s_NS$spec_pars[2,5],s_NS$spec_pars[2,6],s_NS$spec_pars[2,7]), 
                   ncol=6, byrow=TRUE)
colnames(NS_table) <- c("Mean", "lower 95% CI", "upper 95% CI", "R-hat", "Bulk-ESS", "Tail-ESS")
rownames(NS_table) <- c("intercept nest survival","intercept lay date", "condition -> nest survival", 
                        "lay date -> nest survival", "condition -> lay date", "sigma nest survival","sigma lay date")
NS_table<-round(NS_table,4)
NS_table

knitr::kable(NS_table,digits=2, format="pandoc")
kable_out_NS_table <- knitr::kable(NS_table, "html", digits=4) %>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
kable_out_NS_table
readr::write_file(kable_out_NS_table, "P:/CHIRP/Carry-over/Analysis/Figures/Supplements/PathAnalysisConvergence/NS_table.doc")


### dataframe with fitted values (points)
#create dataframe with fitted values from model
fit_nest <- as.data.frame(fitted(nest_fit_brms))
head(fit_nest)
d_Surv_nest <- cbind(NS, fit_nest)
head(d_Surv_nest)

##################################################################
#DNS_mod1 <- bf(DNS_2 ~ MeanStd+LD_NumN_1, family = "beta")
#LD_mod1 <- bf(LD_NumN_1 ~ MeanStd, family="gaussian")

### plot effect of condition on laying date
# extract estimates per iteration
LDNumN_Intercept_NS<-c(nest_fit_brms$fit@sim$samples[[1]]$b_LDNumN1_Intercept,nest_fit_brms$fit@sim$samples[[2]]$b_LDNumN1_Intercept)
LDNumN_Mean_NS<-c(nest_fit_brms$fit@sim$samples[[1]]$b_LDNumN1_MeanStd,nest_fit_brms$fit@sim$samples[[2]]$b_LDNumN1_MeanStd)


predicted_LD_NS <- matrix(NA, nrow = length(LDNumN_Intercept_NS), ncol = length(NS[,1]))
for (i in 1:length(LDNumN_Intercept_NS)){ #for each iteration
  for (j in 1:length((NS[,1]))){ # for each individual
    predicted_LD_NS[i,j] <- LDNumN_Intercept_NS[i] +  LDNumN_Mean_NS[i] * NS[j,12]
  }
}

mean_LD_NS <- apply(predicted_LD_NS, 2, mean)
lciLD_NS <- apply(predicted_LD_NS, 2, quantile, prob = 2.5/100) #2.5, 25
uciLD_NS <- apply(predicted_LD_NS, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(NS[,12]) #stvov for standardized
dfLD_NS <- data.frame(cond = NS[,12][ord], #stcov for standardized
                   LD = mean_LD_NS[ord],
                   lciLD_NS = lciLD_NS[ord],
                   uciLD_NS = uciLD_NS[ord])
head(dfLD_NS)

library(radiant.data)
library(dplyr)
head(NS)
summary(NS$MeanStd)
b <- seq(-2.5,2.3,0.4)
#test$EW_Bin<-cut(test$EW_Imp,breaks=b)
d_Surv_nest$Cond_Bin<-cut(d_Surv_nest$MeanStd,breaks=b)
summary(d_Surv_nest$Cond_Bin)
df1<-d_Surv_nest %>%
  dplyr::group_by(Cond_Bin) %>%
  dplyr::summarise(mean = mean(LD_NumN_1, na.rm=T), n = n())
df1
df1<-data.frame(df1)
df1$CondMean<-seq(-2.35,2.15,0.3)
df1$CondMean<-c(-2.2,-1.6, -1, -0.4, 0.2, 0.8, 1.4, 2)
df1$CondMean<-c(-2.35, -1.75, -1.45, -1.15, -0.85,-0.55, -0.25, 0.05, 0.35, 0.65, 0.95, 1.25, 1.55, 2.15)
df1$CondMean<-seq(-2.1,1.9,0.8)
df1$CondMean<-seq(-2.3,2.1,0.4)
df1$CondMean<-c(-2.3, -1.5, -1.1, -0.7, -0.3,0.1,0.5,0.9,1.3,2.1) 


library(ggplot2)
p_LD_NS<-ggplot(dfLD_NS, aes(x = cond, y = LD)) + #dfLD_NS, aes(x = cond, y = LD)
  #geom_point(df1,mapping=aes(x=CondMean,y=mean,size=n),color="#E69F00", alpha=0.5, shape=15)+ #stcov for standardized
  geom_point(df1,mapping=aes(x=CondMean,y= mean, size=n),color="#E69F00", alpha=0.5, shape=15)+ #stcov for standardized
  #geom_point(d_Surv_nest,mapping=aes(x=MeanStd,y=Estimate.DNS2),cex=6,color="#E69F00", alpha=0.5, shape=15)+ #stcov for standardized
  #geom_point(df3,mapping=aes(x=LDMean,y=mean, size=n), alpha=0.5, col="#0072B2")+ #stcov for standardized
  geom_line(data=dfLD_NS,mapping=aes(x = cond, y = LD),col="#E69F00",cex=4, lty="dashed") + 
  #geom_point(mapping=aes(x=NS[,12],y=NS[,7]),color="#E69F00", alpha=0.5, cex=6, shape=15)+ #stcov for standardized
  geom_ribbon(data=dfLD_NS,aes(ymin = lciLD_NS, ymax = uciLD_NS), fill="#E69F00", alpha = 0.25)+
  labs(x = "body condition \n(standardized)", y = "laying date \n(standardized)")+
  theme_bw()+
  #ylim(-1.32,1.05)+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_LD_NS

### plot effect of condition on clutch surv
# extract estimates per iteration

##DNS_mod1 <- bf(DNS_2 ~ MeanStd+LD_NumN_1, family = "beta")
#LD_mod1 <- bf(LD_NumN_1 ~ MeanStd, family="gaussian")

DNS_Intercept<-c(nest_fit_brms$fit@sim$samples[[1]]$b_DNS2_Intercept,nest_fit_brms$fit@sim$samples[[2]]$b_DNS2_Intercept)
DNS_Mean<-c(nest_fit_brms$fit@sim$samples[[1]]$b_DNS2_MeanStd, nest_fit_brms$fit@sim$samples[[2]]$b_DNS2_MeanStd)
DNS_LD<-c(nest_fit_brms$fit@sim$samples[[1]]$b_DNS2_LD_NumN_1,nest_fit_brms$fit@sim$samples[[2]]$b_DNS2_LD_NumN_1)

mean.LD_NS = mean(NS$LD_NumN_1) # mean laying date
mean.mat.LD_NS = matrix(rep(mean.LD_NS,nrow(NS)),ncol=1) # matrix of mean laying date

predicted_DNS <- matrix(NA, nrow = length(DNS_Intercept), ncol = length(NS[,1]))
for (i in 1:length(DNS_Intercept)){ #for each iteration
  for (j in 1:length((NS[,1]))){ # for each individual
    predicted_DNS[i,j] <- plogis(DNS_Intercept[i] +  DNS_Mean[i] * NS[j,12] + DNS_LD[i] * mean.mat.LD_NS[j])
  }
}

mean_DNS <- apply(predicted_DNS, 2, mean)
lciDNS <- apply(predicted_DNS, 2, quantile, prob = 2.5/100) #2.5, 25
uciDNS <- apply(predicted_DNS, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(NS[,12]) #stvov for standardized, order cond
dfDNS <- data.frame(cond = NS[,12][ord], #stcov for standardized
                    DNS = mean_DNS[ord],
                    lciDNS = lciDNS[ord],
                    uciDNS = uciDNS[ord])
head(dfDNS)

library(radiant.data)
library(dplyr)
head(NS)
summary(NS$MeanStd)
b <- seq(-2.5,2.3,0.6)
#test$EW_Bin<-cut(test$EW_Imp,breaks=b)
d_Surv_nest$Cond_Bin<-cut(d_Surv_nest$MeanStd,breaks=b)
summary(d_Surv_nest$Cond_Bin)
df1a<-d_Surv_nest %>%
  dplyr::group_by(Cond_Bin) %>%
  dplyr::summarise(mean = weighted.mean(DNS_2,ObsDay, na.rm=T), n = n())
df1a
df1a<-data.frame(df1a)
df1a$CondMean<-seq(-2.2,2,0.6)
df1a$mean_adj<-df1a$mean-0.02

p_DNS<-ggplot(dfDNS, aes(x = cond, y = DNS)) + 
  geom_point(df1a,mapping=aes(x=CondMean,y=mean_adj,size=n),color="#E69F00", alpha=0.5, shape=15)+ #stcov for standardized
  #geom_point(d_Surv_nest,mapping=aes(x=MeanStd,y=DNS_2),cex=6,color="#E69F00", alpha=0.5, shape=15)+ #stcov for standardized
  #geom_point(mapping=aes(x=NS[,12],y=NS[,1]), alpha=0.5, shape=15,cex=6, col="#E69F00")+ #stcov for standardized
  geom_line(data=dfDNS,mapping=aes(x = cond, y = DNS), cex=4, col="#E69F00", lty="solid") + 
  geom_ribbon(data=dfDNS,aes(ymin = lciDNS, ymax = uciDNS), fill="#E69F00",alpha = 0.25)+
  labs(x = "body condition (standardized)", y = "daily nest survival probability")+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_DNS

### plot effect of laying date on chick surv
# extract estimates per iteration
mean.cond_NS = mean(NS$MeanStd) # mean condition
mean.mat.cond_NS = matrix(rep(mean.cond_NS,nrow(NS)),ncol=1) # matrix of mean condition

predicted_DNS_LD <- matrix(NA, nrow = length(DNS_Intercept), ncol = length(NS[,1]))
for (i in 1:length(DNS_Intercept)){ #for each iteration
  for (j in 1:length((NS[,1]))){ # for each individual
    predicted_DNS_LD[i,j] <- plogis(DNS_Intercept[i] +  DNS_Mean[i] * mean.mat.cond_NS[j] + DNS_LD[i] * NS[j,7])
  }
}

mean_DNS_LD <- apply(predicted_DNS_LD, 2, mean)
lciDNS_LD <- apply(predicted_DNS_LD, 2, quantile, prob = 2.5/100) #2.5, 25
uciDNS_LD <- apply(predicted_DNS_LD, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(NS[,7]) #stvov for standardized, laying date
dfDNS_LD <- data.frame(LD = NS[,7][ord], #stcov for standardized
                       DNS = mean_DNS_LD[ord],
                       lciDNS_LD = lciDNS_LD[ord],
                       uciDNS_LD = uciDNS_LD[ord])
head(dfDNS_LD)

library(radiant.data)
library(dplyr)
head(NS)
summary(NS$LD_NumN_1)
b <- seq(-1.4,3.4,0.5)
#test$EW_Bin<-cut(test$EW_Imp,breaks=b)
d_Surv_nest$LD_Bin<-cut(d_Surv_nest$LD_NumN_1,breaks=b)
summary(d_Surv_nest$LD_Bin)
df1b<-d_Surv_nest %>%
  dplyr::group_by(LD_Bin) %>%
  dplyr::summarise(mean = weighted.mean(DNS_2,ObsDay, na.rm=T), n = n())
df1b
df1b<-data.frame(df1b)
df1b$LDMean<-seq(-1.15,2.85,0.5)
df1b$mean_adj<-df1b$mean-0.02

p_DNS_LD<-ggplot(dfDNS_LD, aes(x = LD, y = DNS)) + 
  geom_point(df1b,mapping=aes(x=LDMean,y=mean_adj,size=n),color="#E69F00", alpha=0.5, shape=15)+ #stcov for standardized
  #geom_point(mapping=aes(x=NS[,7],y=NS[,1]), alpha=0.5, shape=15,cex=6,col="#E69F00" )+ #stcov for standardized
  #geom_point(d_Surv_nest,mapping=aes(x=LD_NumN_1,y=DNS_2),cex=6,color="#E69F00", alpha=0.5, shape=15)+ #stcov for standardized
  geom_line(data=dfDNS_LD,mapping=aes(x = LD, y = DNS), cex=4, col="#E69F00") + 
  geom_ribbon(data=dfDNS_LD,aes(ymin = lciDNS_LD, ymax = uciDNS_LD),fill="#E69F00", alpha = 0.25)+
  labs(x = "laying date", y = "daily nest survival")+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_DNS_LD

##############################################################################################
############# combine plots ##################################################################
##############################################################################################
colors <- c("nest" = "#E69F00", "chick" = "#0072B2")
shapes <-c("nest" = 15, "chick" = 16)

#### daily nest and chick survival and laying date
# chick surv days, nest surv and laydate
p_DNCS_LD<-ggplot(dfDNS_LD, aes(x = LD, y = DNS)) + 
  geom_point(NS,mapping=aes(x=NS[,7],y=NS[,1],color="nest", shape="nest"),alpha=0.5, cex=2.5)+ #stcov for standardized
  geom_point(d_Surv,mapping=aes(x=d_Surv[,2],y=d_Surv[,16], color="chick", shape="chick"), alpha=0.5, cex=2.5)+ #stcov for standardized
  geom_line(data=dfDNS_LD,mapping=aes(x = LD, y = DNS,  color="nest"), cex=1.2, linetype="dashed") + 
  geom_line(data=dfDCS_LD,mapping=aes(x = LD, y = DCS, color="chick"), cex=1.2) + 
  geom_ribbon(data=dfDNS_LD,aes(ymin = lciDNS_LD, ymax = uciDNS_LD, x=LD, y=DNS), alpha = 0.25, fill="#E69F00")+
  geom_ribbon(data=dfDCS_LD,aes(ymin = lciDCS_LD, ymax = uciDCS_LD, x=LD, y=DCS), alpha = 0.25, fill="#0072B2")+
  labs(x = "lay date (standardized)", y = "daily survival")+
  theme_bw()+
  #ylim(-1.32,1.05)+
  theme(legend.position="right", 
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        text = element_text(size=18),
        legend.key.size = unit(1,"cm"),
        axis.title=element_text(size=16))+
  scale_color_manual(values = colors, name="daily surivival of")+
  scale_shape_manual(name = "daily surivival of", values = shapes)
p_DNCS_LD

# Extract the legend. Returns a gtable
library(ggpubr)
leg <- get_legend(p_DNCS_LD)

# Convert to a ggplot and print
p_leg<-as_ggplot(leg)
#plot_layout(tag_level = 'new')

#without legend
#p_DNCS_LD<-ggplot(dfDNS_LD, aes(x = LD, y = DNS)) + 
#  geom_point(NS,mapping=aes(x=NS[,7],y=NS[,1],color="nest", shape="nest"),alpha=0.5, cex=6)+ #stcov for standardized
#  geom_point(d_Surv,mapping=aes(x=d_Surv[,2],y=d_Surv[,8], color="chick", shape="chick"), alpha=0.5, cex=6)+ #stcov for standardized
##  geom_line(data=dfDNS_LD,mapping=aes(x = LD, y = DNS,  color="nest"), cex=4, linetype="dashed") + 
##  geom_line(data=dfDCS_LD,mapping=aes(x = LD, y = DCS, color="chick"), cex=4) + 
#  geom_ribbon(data=dfDNS_LD,aes(ymin = lciDNS_LD, ymax = uciDNS_LD, x=LD, y=DNS), alpha = 0.25, fill="#E69F00")+
#  geom_ribbon(data=dfDCS_LD,aes(ymin = lciDCS_LD, ymax = uciDCS_LD, x=LD, y=DCS), alpha = 0.25, fill="#0072B2")+
#  labs(x = "lay date (standardized)", y = "daily survival probability", tag="a)")+
#  theme_bw()+
#  #ylim(-1.32,1.05)+
#  theme(legend.position="none", 
#        legend.title = element_text(size = 19),
#        legend.text = element_text(size = 18),
#        text = element_text(size=35),
#        legend.key.size = unit(1,"cm"),
#        axis.title=element_text(size=35))+
#  scale_color_manual(values = colors, name="daily surivival of")+
#  scale_shape_manual(name = "daily surivival of", values = shapes)
#p_DNCS_LD

p_DNCS_LD<-ggplot(dfDNS_LD, aes(x = LD, y = DNS)) + 
  geom_point(NS,mapping=aes(x=NS[,7],y=NS[,1],color="nest", shape="nest"),alpha=0.5, cex=6)+ #stcov for standardized
  geom_point(d_Surv,mapping=aes(x=d_Surv[,2],y=d_Surv[,18]/40, color="chick", shape="chick"), alpha=0.5, cex=6)+ #stcov for standardized
  geom_line(data=dfDNS_LD,mapping=aes(x = LD, y = DNS,  color="nest"), cex=4, linetype="solid") + 
  geom_line(data=dfDCS_LD,mapping=aes(x = LD, y = DCS/40, color="chick"), cex=4) + 
  geom_ribbon(data=dfDNS_LD,aes(ymin = lciDNS_LD, ymax = uciDNS_LD, x=LD, y=DNS), alpha = 0.25, fill="#E69F00")+
  geom_ribbon(data=dfDCS_LD,aes(ymin = lciDCS_LD/40, ymax = uciDCS_LD/40, x=LD, y=DCS/40), alpha = 0.25, fill="#0072B2")+
  labs(x = "lay date (standardized)", y = "daily nest survival probability", tag="a)")+
  scale_y_continuous(name = "daily nest survival probability", 
                     sec.axis = sec_axis(~.*40, name = "chick survival (days)"), limits=c(0,1.05))+ 
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 19),
        legend.text = element_text(size = 18),
        text = element_text(size=35),
        legend.key.size = unit(1,"cm"),
        axis.title.y.right=element_text(size=35, colour = "#0072B2"),
        axis.title.y.left=element_text(size=35, colour = "#E69F00"))+
  scale_color_manual(values = colors, name="daily surivival of")+
  scale_shape_manual(name = "daily surivival of", values = shapes)
p_DNCS_LD

p_DNCS<-ggplot(dfDNS, aes(x = cond, y = DNS)) + 
  geom_point(dfDNS,mapping=aes(x=NS[,12],y=NS[,1], color="nest", shape="nest"), cex=6,alpha=0.5)+ #stcov for standardized
  geom_point(dfDCS,mapping=aes(x=d_Surv[,20],y=d_Surv[,18]/40, colour="chick", shape="chick"),cex=6, alpha=0.5)+ #stcov for standardized
  
  geom_line(data=dfDNS,mapping=aes(x = cond, y = DNS, color="nest"), cex=4, linetype="dashed") + 
  geom_line(data=dfDCS,mapping=aes(x = cond, y = DCS/40, color="chick"), cex=4) + 
  
  geom_ribbon(data=dfDNS,aes(ymin = lciDNS, ymax = uciDNS, x=cond, y=DNS), fill="#E69F00",alpha = 0.25)+
  geom_ribbon(data=dfDCS,aes(ymin = lciDCS/40, ymax = uciDCS/40, x=cond, y=DCS/40), fill="#0072B2", alpha = 0.25)+
  
  labs(x = "body condition", y = "daily nest survival probability", tag="c)")+
  scale_y_continuous(name = "daily nest survival probability", #ylim=c(0.7,1),
                     sec.axis = sec_axis(~.*40, name = "chick survival (days)"))+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=35),
        axis.title.y.right=element_text(size=35, colour = "#0072B2"),
        axis.title.y.left=element_text(size=35, colour = "#E69F00"))+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = shapes)
p_DNCS
p_DNS

p_LD_cond<-ggplot(dfLD_NS, aes(x = cond, y = LD)) + 
  geom_point(dfLD_NS,mapping=aes(x=NS[,12],y=NS[,7], color="nest", shape="nest"), cex=6, alpha=0.5)+ #stcov for standardized
  geom_point(dfLD, mapping=aes(x=d_Surv[,20],y=d_Surv[,2], color="chick", shape="chick"),cex=6, alpha=0.5)+ #stcov for standardized
  
  geom_line(data=dfLD_NS,mapping=aes(x = cond, y = LD, color="nest"), cex=4, linetype="dashed") + 
  geom_line(data=dfLD,mapping=aes(x = cond, y = LD, color="chick"), cex=4, linetype="dashed") + 
  
  geom_ribbon(data=dfLD_NS,aes(ymin = lciLD_NS, ymax = uciLD_NS, x = cond, y = LD), fill="#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfLD,aes(ymin = lciLD, ymax = uciLD, x = cond, y = LD), fill="#0072B2", alpha = 0.25)+
  
  labs(x = "body condition (standardized)", y = "lay date (standardized)", tag="b)")+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=35),
        axis.title=element_text(size=35))+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = shapes)
p_LD_cond

#########################################  combine plots ###########
#libraries
library(gridExtra)
library(grid)
library(gridtext)


packageurl <- "https://cran.r-project.org/src/contrib/Archive/purrr/purrr_1.0.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
library(purrr)

##### direct effect condition on survival
p_DNS
p_DCS

# Remove axis titles from all plots
p = list(p_DNS,p_DCS) %>% map(~.x + labs(x=NULL, y=NULL))

# gridtext
yright = richtext_grob("hatchling survival (days)", rot=90, gp=gpar(fontsize=30))

yleft = richtext_grob("daily clutch survival probability", rot=90, gp=gpar(fontsize=30))

bottom = richtext_grob(
  text = 'body condition (s.d.)', gp=gpar(fontsize=30))


png("YOUR_PATH/DNCS_cond.png", width = 7500, height = 5000,units = 'px', res = 600)

p_a <- grid.arrange(grobs=p, ncol = 2, nrow = 1, 
                    right = yright, left = yleft, bottom = bottom)
grid.text("a)", x=0.02,y=0.96,
          gp=gpar(fontsize=35, col="black"))
dev.off() 


### condition and laying date
# Remove axis titles from all plots
ppp = list(p_LD_NS,p_LD) %>% map(~.x + labs(x=NULL, y=NULL))

# gridtext
yright = richtext_grob("lay date (standardized)", rot=90, gp=gpar(fontsize=30))

yleft = richtext_grob("lay date (standardized)", rot=90, gp=gpar(fontsize=30))

bottom = richtext_grob(
  text = 'body condition (s.d.)', gp=gpar(fontsize=30))

png("YOUR_PATH/LD_cond.png", width = 7500, height = 5000,units = 'px', res = 600)

p_b <- grid.arrange(grobs=ppp, ncol = 2, nrow = 1, 
                    right = yright, left = yleft, bottom = bottom)
grid.text("b)", x=0.02,y=0.96,
          gp=gpar(fontsize=35, col="black"))
dev.off() 


############## survival and laydate
p_DCS_LD
p_DNS_LD

# Remove axis titles from all plots
pp = list(p_DNS_LD,p_DCS_LD) %>% map(~.x + labs(x=NULL, y=NULL))

# gridtext
yright = richtext_grob("hatchling survival (days)", rot=90, gp=gpar(fontsize=30))

yleft = richtext_grob("daily clutch survival probability", rot=90, gp=gpar(fontsize=30))

bottom = richtext_grob(
  text = 'lay date (standardized)', gp=gpar(fontsize=30))

png("YOUR_PATH/DNCS_LD.png", width = 7500, height = 5000,units = 'px', res = 600)

p_c <- grid.arrange(grobs=pp, ncol = 2, nrow = 1, 
                    right = yright, left = yleft, bottom = bottom)
grid.text("c)", x=0.02,y=0.96,
          gp=gpar(fontsize=35, col="black"))
dev.off() 


png("YOUR_PATH/Legend.png", width = 8500, height = 5000,units = 'px', res = 600)
p_leg
dev.off() 

############################################################################################################
#################################### FOR SUPPLEMENTS ######################################################
#############################################################################################################
#### model results:
Family: MV(poisson, gaussian) 
Links: mu = log
mu = identity; sigma = identity 
Formula: ChickSurvival_poisson ~ MeanStd + LD_NumN + Cr 
LD_NumN ~ MeanStd + Cr 
Data: d_Surv (Number of observations: 25) 
Samples: 2 chains, each with iter = 15000; warmup = 4000; thin = 1;
total post-warmup samples = 22000

Population-Level Effects: 
                                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
ChickSurvivalpoisson_Intercept     3.07      0.07     2.93     3.20 1.00    29994    17469
LDNumN_Intercept                  -0.50      0.18    -0.85    -0.15 1.00    30988    15390
ChickSurvivalpoisson_MeanStd       0.13      0.05     0.02     0.23 1.00    34579    16613
ChickSurvivalpoisson_LD_NumN      -0.16      0.08    -0.32     0.00 1.00    27197    17823
ChickSurvivalpoisson_Cr           -1.13      0.16    -1.44    -0.82 1.00    22815    18784
LDNumN_MeanStd                     0.26      0.15    -0.05     0.56 1.00    32016    16648
LDNumN_Cr                          0.78      0.31     0.16     1.39 1.00    27518    16271

Family Specific Parameters: 
  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma_LDNumN     0.72      0.11     0.54     0.98 1.00    25635    16859

Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).

## R2
bayes_R2(chick_fit_brms)

Estimate  Est.Error       Q2.5     Q97.5
R2ChickSurvivalpoisson 0.4276231 0.04625859 0.33106623 0.5118687
R2LDNumN               0.3338052 0.11793642 0.08041352 0.5284017

# crop on survival
# extract estimates per iteration
DCS_Intercept<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_Intercept,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_Intercept)
DCS_Mean<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_MeanStd,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_MeanStd)
DCS_LD<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_LD_NumN,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_LD_NumN)
DCS_Cr<-c(chick_fit_brms$fit@sim$samples[[1]]$b_ChickSurvivalpoisson_Cr,chick_fit_brms$fit@sim$samples[[2]]$b_ChickSurvivalpoisson_Cr)

mean.LD = mean(d_Surv$LD_NumN) # mean laying date
mean.mat.LD = matrix(rep(mean.LD,nrow(d_Surv)),ncol=1) # matrix of mean laying date

mean.Cond = mean(d_Surv$MeanStd) # mean laying date
mean.mat.Cond = matrix(rep(mean.Cond,nrow(d_Surv)),ncol=1) # matrix of mean laying date

#mean.Cr = mean(d_Surv$Cr) # mean laying date
#mean.mat.Cr = matrix(rep(mean.Cr,nrow(d_Surv)),ncol=1) # matrix of mean laying date

predicted_DCS_Cr <- matrix(NA, nrow = length(DCS_Intercept), ncol = length(d_Surv[,1]))
for (i in 1:length(DCS_Intercept)){ #for each iteration
  for (j in 1:length((d_Surv[,1]))){ # for each individual
    predicted_DCS_Cr[i,j] <- exp(DCS_Intercept[i] +  DCS_Mean[i] * mean.mat.Cond[j] + DCS_LD[i] * mean.mat.LD[j] + DCS_Cr[i] * d_Surv[j,22])
  }
}

mean_DCS_Cr <- apply(predicted_DCS_Cr, 2, mean)
lciDCS_Cr <- apply(predicted_DCS_Cr, 2, quantile, prob = 2.5/100) #2.5, 25
uciDCS_Cr <- apply(predicted_DCS_Cr, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(d_Surv[,22]) #stvov for standardized
dfDCS_Cr <- data.frame(crop = d_Surv[,22][ord], #stcov for standardized
                    DCS = mean_DCS_Cr[ord],
                    lciDCS = lciDCS_Cr[ord],
                    uciDCS = uciDCS_Cr[ord])
head(dfDCS_Cr)
dfDCS_Cr

crop <- c('No crop','Crop')
mean <- c(22.49891 , 7.354333  )
lci <- c(20.07059 ,5.477983  )
uci <- c(25.071765 , 9.530898)
dfDCS_Cr_new <- data.frame(crop, mean, lci, uci)

d_Surv$Cr <- factor(d_Surv$Cr, levels=c('0', '1'),
                    labels=c('No crop', 'Crop'))

#boxplot
#mean with confidence interval 95%
library(ggplot2)
p_CS_Cr <- ggplot() + 
  geom_errorbar(dfDCS_Cr_new, mapping=aes(x=crop, y=mean,ymin=lci, ymax=uci), width=.1, linewidth=1) +
  geom_point(dfDCS_Cr_new, mapping=aes(x=crop, y=mean),cex=3) +
  #geom_errorbar(dfLD_Cr_new,mapping=aes(y=mean, x=crop, ymin=lci, ymax=uci), width=.1, linewidth=1) +
  geom_point(d_Surv, mapping=aes(x=Cr, y=ChickSurvival_poisson),col='red', cex=2)+
  labs(x = "Born on", y = "hatchling survival (days)")+
  theme_bw() +
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_CS_Cr


png("YOUR_PATH/CropHatchling.png", width = 7500, height = 5000,units = 'px', res = 600)
p_CS_Cr
dev.off() 

#### relationship of laydate and crop (crop on laydate)
LDNumN_Intercept<-c(chick_fit_brms$fit@sim$samples[[1]]$b_LDNumN_Intercept,chick_fit_brms$fit@sim$samples[[2]]$b_LDNumN_Intercept)
LDNumN_Mean<-c(chick_fit_brms$fit@sim$samples[[1]]$b_LDNumN_MeanStd,chick_fit_brms$fit@sim$samples[[2]]$b_LDNumN_MeanStd)
LDNumN_Cr<-c(chick_fit_brms$fit@sim$samples[[1]]$b_LDNumN_Cr,chick_fit_brms$fit@sim$samples[[2]]$b_LDNumN_Cr)

mean.Cond = mean(d_Surv$MeanStd) # mean laying date
mean.mat.Cond = matrix(rep(mean.Cond,nrow(d_Surv)),ncol=1) 

predicted_LD_Cr <- matrix(NA, nrow = length(LDNumN_Intercept), ncol = length(d_Surv[,1]))
for (i in 1:length(LDNumN_Intercept)){ #for each iteration
  for (j in 1:length((d_Surv[,1]))){ # for each individual
    predicted_LD_Cr[i,j] <- LDNumN_Intercept[i] +  LDNumN_Mean[i] * mean.mat.Cond[j] + LDNumN_Cr[i] * d_Surv[j,22]
  }
}

mean_LD_Cr <- apply(predicted_LD_Cr, 2, mean)
lciLD_Cr <- apply(predicted_LD_Cr, 2, quantile, prob = 2.5/100) #2.5, 25
uciLD_Cr <- apply(predicted_LD_Cr, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(d_Surv[,22]) #stvov for standardized
dfLD_Cr <- data.frame(cond = d_Surv[,22][ord], #stcov for standardized
                   LD = mean_LD_Cr[ord],
                   lciLD = lciLD_Cr[ord],
                   uciLD = uciLD_Cr[ord])

head(dfLD_Cr)
dfLD_Cr

crop <- c('No crop','Crop')
mean <- c(-0.5027494  , 0.2764166   )
lci <- c(-0.8500183  ,-0.1540290  )
uci <- c(-0.2253674   , 0.7802732)
dfLD_Cr_new <- data.frame(crop, mean, lci, uci)

str(d_Surv$Cr)

d_Surv$Cr <- factor(d_Surv$Cr, levels=c('0', '1'),
                    labels=c('No crop', 'Crop'))

#boxplot
#mean with confidence interval 95%
library(ggplot2)
p_LD_Cr <- ggplot() + 
  geom_errorbar(dfLD_Cr_new,mapping=aes(y=mean, x=crop, ymin=lci, ymax=uci), width=.1, linewidth=1) +
  #geom_line() +
  geom_point(dfLD_Cr_new, mapping=aes(x=crop, y=mean),cex=3) +
  geom_point(d_Surv, mapping=aes(x=Cr, y=LD_NumN),col='red', cex=2)+
  #geom_point(df1,mapping=aes(x=CondMean,y=mean, size=n),cex=6, alpha=0.5, col="#0072B2")+ #stcov for standardized
  labs(x = "Born on", y = "Lay date (standardized)")+
  theme_bw() +
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=40),
        axis.title=element_text(size=40))
p_LD_Cr


png("YOUR_PATH/CropLD.png", width = 7500, height = 5000,units = 'px', res = 600)
p_LD_Cr
dev.off() 



