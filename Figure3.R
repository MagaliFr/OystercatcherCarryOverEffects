#libraries necessary
library(ggplot2)
library(patchwork)
load("G:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/MainDoc/Files for Fig3/GlobalEnv.Rdata")

setwd("G:/NIOO drive/N drive/_Magali/Nimble_M1_P12/Output20210805")

gamma2<-read.csv(file="gamma2.csv", header = T)
head(gamma2[,2])
gamma2<-gamma2[,2]

gamma3<-read.csv(file="gamma3.csv", header = T)
head(gamma3[,2])
gamma3<-gamma3[,2]
head(gamma3)

mean.mat.stcov<-read.csv(file="mean.mat.stcov.csv", header = T)
head(mean.mat.stcov[,2:10])
mean.mat.stcov<-mean.mat.stcov[,2:10]

stcov<-read.csv(file="stcov.csv", header = T)
head(stcov[,2:10])
stcov<-stcov[,2:10]

samplesSummaryEta<-read.csv(file="Output_CR_SEM_nimble_Eta.csv", header = T)
samplesSummaryEta<-samplesSummaryEta[,2:6]

beta1<-read.csv(file="beta1.csv", header = T)
head(beta1)
beta1<-beta1[,2]

beta2<-read.csv(file="beta2.csv", header = T)
head(beta2)
beta2<-beta2[,2]

int_phi_p<-read.csv(file="Output_CR_SEM_nimble_Surv_int.phi_int.p.csv", header = T)
head(int_phi_p)
int_phi<-int_phi_p[28:54,]

epst_phi<-read.csv(file="Output_CR_SEM_nimble_Surv_epst.phi.csv", header = T)
head(epst_phi)

setwd("F:/NIOO drive/N drive/_Magali/Nimble_M1_P12/Output20210808_hemaRef")
gamma1<-read.csv(file="gamma1.csv", header = T)
head(gamma1[,2])
gamma1<-gamma1[,2]

gamma3_HR<-read.csv(file="gamma3.csv", header = T)
head(gamma3_HR[,2])
gamma3_HR<-gamma3_HR[,2]
head(gamma3_HR)

#write.csv(gamma22,"Output/gamma22.csv", row.names =TRUE)
#write.csv(gamma3,"Output/gamma3.csv", row.names =TRUE)
#write.csv(gamma32,"Output/gamma32.csv", row.names =TRUE)
#write.csv(mean.mat.stcov,"Output/mean.mat.stcov.csv", row.names =TRUE)
#write.csv(stcov,"Output/stcov.csv", row.names =TRUE)
#write.csv(beta1,"Output/beta1.csv", row.names =TRUE)
#write.csv(beta2,"Output/beta2.csv", row.names =TRUE)

######################## Figure 3 #####################################################################
#setwd("N:/Dep.AnE/AnE-share/_Magali/Nimble_M1_P12/Output20210805")
setwd("G:/NIOO drive/N drive/_Magali/Nimble_M1_P12/Output20210805")


# predict linear effect of mass on condition
predicted_conditionMS <- matrix(NA, nrow = length(gamma2), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(gamma2)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionMS[i,j] <- gamma1[i] * stcov[j,1] + #gamma12[i] * pow(mean.mat.stcov[j,1],2) + 
      1 * mean.mat.stcov[j,2] + #gamma22[i] * pow(mean.mat.stcov[j,2],2) + 
      gamma3_HR[i] * mean.mat.stcov[j,3] #+ gamma32[i] * pow(mean.mat.stcov[j,3],2)
  }
}

# Now we calculate posterior mean and the credible interval. Note the ordering.
mean_conditionMS <- apply(predicted_conditionMS, 2, mean)
lciMS <- apply(predicted_conditionMS, 2, quantile, prob = 2.5/100) #2.5, 25
uciMS <- apply(predicted_conditionMS, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(stcov[,1])
dfMS <- data.frame(mass = stcov[,1][ord],
                   cond = mean_conditionMS[ord],
                   lciMS = lciMS[ord],
                   uciMS = uciMS[ord])

head(dfMS)

# Now it's time to visualize. 
#library(ggplot2)
p_1<-ggplot(dfMS, aes(x=mass, y=cond)) + 
  geom_point(mapping=aes(x=stcov[,1],y=samplesSummaryEta[,1]), col="#56B4E9",alpha=0.3, cex=1.5)+ #stcov[,1] samplesSummaryMu1[,1]
  #geom_line(data=dfMS2,mapping=aes(x = mass, y = cond, col="#E69F00"),cex=1.2) + 
  geom_line(data=dfMS,mapping=aes(x = mass, y = cond), linewidth=1.3) + 
  geom_ribbon(data=dfMS,aes(ymin = lciMS, ymax = uciMS), alpha = 0.25)+
  labs(x = "size-corrected body mass", y = "body condition")+
  ggtitle("Effect of condition \nvariables on \nbody condition")+
  theme_bw()+
  theme(legend.position="bottom", 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        text = element_text(size=15),
        axis.title=element_text(size=14),
        title = element_text(size=11))
p_1

############## hameatocrit and condition ##########
#hema linear effect
predicted_conditionH <- matrix(NA, nrow = length(gamma2), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(gamma2)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionH[i,j] <- -1 * mean.mat.stcov[j,1] + #gamma12[i] * pow(mean.mat.stcov[j,1],2) + 
      gamma2[i] * stcov[j,2] + #gamma22[i] * pow(mean.mat.stcov[j,2],2) + 
      gamma3[i] * mean.mat.stcov[j,3] #+ gamma32[i] * pow(mean.mat.stcov[j,3],2)
  }
}

#' Now we calculate posterior mean and the credible interval. Note the ordering.
mean_conditionH <- apply(predicted_conditionH, 2, mean)
lciH <- apply(predicted_conditionH, 2, quantile, prob = 2.5/100) #2.5, 25
uciH <- apply(predicted_conditionH, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(stcov[,2])
dfH <- data.frame(hema = stcov[,2][ord],
                  cond = mean_conditionH[ord],
                  lciH = lciH[ord],
                  uciH = uciH[ord])

head(dfH)

# Now time to visualize. 
p_2<-ggplot(dfH, aes(x=hema, y=cond)) + 
  geom_point(mapping=aes(x=stcov[,2],y=samplesSummaryEta[,1]), col="#56B4E9",alpha=0.3, cex=1.5)+
  #geom_line(data=dfH2,mapping=aes(x = hema, y = cond,col="#E69F00"), cex=1.2) + 
  geom_line(data=dfH,mapping=aes(x = hema, y = cond), cex=1.3) + 
  #geom_ribbon(data=dfH2,aes(ymin = lciH2, ymax = uciH2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfH,aes(ymin = lciH, ymax = uciH), alpha = 0.25)+
  labs(x = "haematocrit", y = "body condition")+
  theme_bw()+
  theme(legend.position="bottom", 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        text = element_text(size=15),
        axis.title=element_text(size=14))
        #axis.title.y=element_blank())
p_2

############## buffy coat and condition ##########
#buffy coat linear effect
predicted_conditionB <- matrix(NA, nrow = length(gamma2), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(gamma2)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionB[i,j] <- -1 * mean.mat.stcov[j,1] + #gamma12[i] * pow(mean.mat.stcov[j,1],2) + 
      gamma2[i] * mean.mat.stcov[j,2] + #gamma22[i] * pow(mean.mat.stcov[j,2],2) + 
      gamma3[i] * stcov[j,3] #+ gamma32[i] * pow(mean.mat.stcov[j,3],2)
  }
}

# Now we calculate posterior mean and the credible interval. Note the ordering.
mean_conditionB <- apply(predicted_conditionB, 2, mean)
lciB <- apply(predicted_conditionB, 2, quantile, prob = 2.5/100) #2.5, 25
uciB <- apply(predicted_conditionB, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(stcov[,3])
dfB <- data.frame(buffy = stcov[,3][ord],
                  cond = mean_conditionB[ord],
                  lciB = lciB[ord],
                  uciB = uciB[ord])

head(dfB)

# Now time to visualize. 
p_3<-ggplot(dfB, aes(x=buffy, y=cond)) + 
  geom_point(mapping=aes(x=stcov[,3],y=samplesSummaryEta[,1]), col="#56B4E9",alpha=0.3, cex=1.3)+
  #geom_line(data=dfB2,mapping=aes(x = buffy, y = cond, col="#E69F00"), cex=1.2) + 
  geom_line(data=dfB,mapping=aes(x = buffy, y = cond), cex=1.5) + 
  #geom_ribbon(data=dfB2,aes(ymin = lciB2, ymax = uciB2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfB,aes(ymin = lciB, ymax = uciB), alpha = 0.25)+
  labs(x = "buffy coat", y = "body condition")+
  theme_bw()+
  theme(legend.position="bottom", 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        text = element_text(size=15),
        axis.title=element_text(size=14))
        #axis.title.y=element_blank())
p_3

###############################################################################################################
###############################################################################################################
###############################################################################################################
### effect of condition on survival
## extract only cond ind:
Phi<-read.csv(file="Output_CR_SEM_nimble_Surv_Phi.csv")

## add state as variable
Phi$State<-factor(substr(Phi$X, 5,5))
str(Phi$State)
levels(Phi$State)
# 9 alive states : D, P, N, B, V, T, S, R, X
Phi$StateText<-factor(Phi$State, levels=c( "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                      labels=c("Delta", "Inland South", "Inland North", "Texel-Balgzand", 
                               "Texel-Vlieland","Terschelling-Ameland", "Schiermonnikoog","Rottum",  
                               "Abroad"))
## add individual
Phi$Ind<-factor(substr(Phi$X, 8,11))

# add time
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
str(Phi$X)
Phi$X<-as.character(Phi$X)

Phi$YearDigit<-factor(substrRight(Phi$X, 3)) #1 is summer 2000, 2 is winter 2000-2001
str(Phi$YearDigit)
levels(Phi$YearDigit)

Phi$Season<-ifelse(Phi$YearDigit==" 1]", "WS",
                   ifelse(Phi$YearDigit==" 3]", "WS", 
                          ifelse(Phi$YearDigit==" 5]", "WS", 
                                 ifelse(Phi$YearDigit==" 7]", "WS", 
                                        ifelse(Phi$YearDigit==" 9]", "WS", 
                                               ifelse(Phi$YearDigit=="11]", "WS", 
                                                      ifelse(Phi$YearDigit=="13]", "WS", 
                                                             ifelse(Phi$YearDigit=="15]", "WS", 
                                                                    ifelse(Phi$YearDigit=="17]", "WS", "SW")))))))))

Phi$Year<-ifelse(Phi$YearDigit==" 1]", "W 1999-2000 to S 2000", #means winter 1999-2000 to summer 2001
                 ifelse(Phi$YearDigit==" 3]", "W 2000-2001 to S 2001", #winter 2000-2001 to summer 2001
                        ifelse(Phi$YearDigit==" 5]", "W 2001-2002 to S 2002", 
                               ifelse(Phi$YearDigit==" 7]", "W 2002-2003 to S 2003", 
                                      ifelse(Phi$YearDigit==" 9]", "W 2003-2004 to S 2004", 
                                             ifelse(Phi$YearDigit==" 2]", "S 2000 to W 2000-2001",
                                                    ifelse(Phi$YearDigit==" 4]", "S 2001 to W 2001-2002",
                                                           ifelse(Phi$YearDigit==" 6]", "S 2002 to W 2002-2003",
                                                                  ifelse(Phi$YearDigit==" 8]", "S 2003 to W 2003-2004",
                                                                         ifelse(Phi$YearDigit=="10]",  "S 2004 to W 2004-2005",
                                                                                ifelse(Phi$YearDigit=="11]", "W 2015-2016 to S 2016", #means winter 1999-2000 to summer 2001
                                                                                       ifelse(Phi$YearDigit=="13]", "W 2016-2017 to S 2017", #winter 2000-2001 to summer 2001
                                                                                              ifelse(Phi$YearDigit=="15]", "W 2017-2018 to S 2018", 
                                                                                                     ifelse(Phi$YearDigit=="17]", "W 2018-2019 to S 2019", 
                                                                                                            ifelse(Phi$YearDigit=="12]", "S 2016 to W 2016-2017",
                                                                                                                   ifelse(Phi$YearDigit=="14]", "S 2017 to W 2017-2018",
                                                                                                                          ifelse(Phi$YearDigit=="16]", "S 2018 to W 2018-2019","S 2019 to W 2019-2020")))))))))))))))))



cov<-read.csv(file="F:/NIOO drive/N drive/_Magali/Nimble_M1_P12/StateMatrix2000_2019Cond_3class9Years.csv")
ID<-read.csv(file="F:/NIOO drive/N drive/_Magali/Nimble_M1_P12/StateMatrix2000_2019Cond_3class_9Years_ID.csv")


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



Cov<-cbind(ID,cov)

#add ind number
Cov$Ind<-seq(1,1574)

# create link variable
Cov$Link<-paste(Cov$State, Cov$YearShort,Cov$Ind, sep = "_")

Phi$YearShort<-ifelse(Phi$YearDigit==" 1]", "2000", #t0-t1, niet te schatten, from winter 1999-2000 to summer 2000
                      ifelse(Phi$YearDigit==" 3]", "2001",  # means surv from t2 to t3 --> from winter 2016-2017 bis 2017 summer
                             ifelse(Phi$YearDigit==" 5]", "2002", # # means surv from t4 to t5 --> from winter 2017-2018 bis 2018 summer
                                    ifelse(Phi$YearDigit==" 7]", "2003", 
                                           ifelse(Phi$YearDigit==" 9]", "2004", 
                                                  ifelse(Phi$YearDigit==" 2]", "2000-2001",
                                                         ifelse(Phi$YearDigit==" 4]", "2001-2002",
                                                                ifelse(Phi$YearDigit==" 6]", "2002-2003",
                                                                       ifelse(Phi$YearDigit==" 8]", "2003-2004", 
                                                                              ifelse(Phi$YearDigit=="10]", "2004-2005",
                                                                                     ifelse(Phi$YearDigit=="11]", "2016", #t0-t1, niet te schatten, from winter 2015-2016 to summer 2016
                                                                                            ifelse(Phi$YearDigit=="13]", "2017",  # means surv from t2 to t3 --> from winter 2016-2017 bis 2017 summer
                                                                                                   ifelse(Phi$YearDigit=="15]", "2018", # # means surv from t4 to t5 --> from winter 2017-2018 bis 2018 summer
                                                                                                          ifelse(Phi$YearDigit=="17]", "2019", 
                                                                                                                 ifelse(Phi$YearDigit=="12]", "2016-2017",
                                                                                                                        ifelse(Phi$YearDigit=="14]", "2017-2018",
                                                                                                                               ifelse(Phi$YearDigit=="16]", "2018-2019","2019-2020")))))))))))))))))

# interprete, number/year is always survival to that year!

# remove comma after individual number
Phi$Ind<-sub("\\,.*", "", Phi$Ind)
str(Phi$Ind)
Phi$Ind<-factor(Phi$Ind)

#check that number of individuals is correct
library(dplyr)
sum_data <- Phi %>% group_by(Ind) %>% count(Ind)
str(Phi$Ind)

# create link variable
Phi$Link<-paste(Phi$State, Phi$YearShort,Phi$Ind, sep = "_")

######## link data frames
library(plyr)
Surv_DS<-join(Cov, Phi, by = "Link", type = "left", match = "all")

eta<-samplesSummaryEta
eta$MeanCond<-eta$Mean
Surv_DS$MeanSurv<-Surv_DS$Mean
Surv_DS$se<-Surv_DS$St.Dev.
str(Surv_DS)

Cond_Surv<-cbind(eta,Surv_DS)
str(Cond_Surv)
Cond_Surv<-subset(Cond_Surv,
                  select=c(MeanSurv,MeanCond, se, StateText, AgeN, Year))
str(Cond_Surv)
Cond_Surv$Year<-factor(Cond_Surv$Year)

p_CondSurv<-ggplot(Cond_Surv, aes(y=MeanSurv, x=MeanCond))+
  geom_point()+
  xlab("Condition")+ylab("Survival proability")
p_CondSurv

# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
mean_epst_phi<-mean(epst_phi[,2], na.rm=T)
mean_int_phi<-mean(int_phi[,2], na.rm=T)

mean.mat.epst_phi = matrix(rep(mean_epst_phi,nrow(stcov)),byrow=T) #for length of nr ind
mean.mat.int_phi = matrix(rep(mean_int_phi,nrow(stcov)),byrow=T) #for length of nr ind

##model equation
# logit(phi[k,i,j]) <- mu.phi[k,i,j]  + epst.phi[k,j-1]   # age and state specific survival probability,  with site-specific time random effect
# mu.phi[k,i,j] <- logit(int.phi[k,age[i,j-1]]) + (j == TCatch[i]+1) * beta1 * eta1[i] + (j == TCatch[i]+1) * beta2 * cov[i,9]

#predict
library(gtools)
predicted_Surv <- matrix(NA, nrow = length(beta1), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_Surv[i,j] <- plogis(
      logit(mean.mat.int_phi[j]) + #average surv across age/stat  
      beta1[i] * samplesSummaryEta[j,1] + #condition effect
      beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
      mean.mat.epst_phi[j] # time  effect
    )
  }
}

# Now we calculate posterior mean and the credible interval. Note the ordering.
mean_Surv <- apply(predicted_Surv, 2, mean)
lciSurv <- apply(predicted_Surv, 2, quantile, prob = 2.5/100) #2.5, 25
uciSurv <- apply(predicted_Surv, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(samplesSummaryEta[,1])
dfSurv <- data.frame(cond = samplesSummaryEta[,1][ord],
                  Surv = mean_Surv[ord],
                  lciSurv = lciSurv[ord],
                  uciSurv = uciSurv[ord])

head(dfSurv)

# Now time to visualize. 
p_4<-ggplot(dfSurv, aes(x=cond, y=Surv)) + 
  geom_point(data=Cond_Surv,mapping=aes(x=MeanCond,y=MeanSurv, color=StateText,shape=Year), alpha=0.2, cex=3)+ #Cond_Surv

  #geom_line(data=dfB2,mapping=aes(x = buffy, y = cond, col="#E69F00"), cex=1.2) + 
  geom_line(data=dfSurv,mapping=aes(x = cond, y = Surv), cex=1.5) + 
  #geom_ribbon(data=dfB2,aes(ymin = lciB2, ymax = uciB2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfSurv,aes(ymin = lciSurv, ymax = uciSurv), alpha = 0.25)+
  labs(x = "body condition", y = "survival probability")+
  theme_bw()+
  theme(#legend.position="right", 
        #legend.direction = "vertical",
        legend.position = c(.98, .40),
        legend.justification = c("right", "top"),
        legend.box.just = "left",
        legend.margin = margin(7, 0, 5, 3),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        #legend.spacing = unit(-0.6, "cm"),
        #legend.margin=margin(0,0,0,0),
        text = element_text(size=15),
        axis.title=element_text(size=14),
        title=element_text(size=11))+
  guides(fill=guide_legend(ncol=2, byrow=TRUE))+
  ggtitle("Effect of body condition on survival")+
  scale_color_manual(name = "Site (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))
#theme(legend.position ="bottom")
p_4


#### only adults:
summary(Cond_Surv)
### plot mean cond and surv per age, state, year
Cond_Surv$AgeN[Cond_Surv$AgeN==1] <- 1 #1st year=1
Cond_Surv$AgeN[Cond_Surv$AgeN==2] <- 2 #2nd year=2
Cond_Surv$AgeN[Cond_Surv$AgeN==3] <- 2 #3rd year and adult=3
Cond_Surv$AgeN[Cond_Surv$AgeN==4] <- 3 #3rd year and adult=3, 3rd year and adult combined because only few indivivudals from 3rd year. 
summary(Cond_Surv$AgeN)
Cond_Surv$AgeF<-factor(Cond_Surv$AgeN)

library(tidyverse)
p_Cond_df <- Cond_Surv %>% 
  dplyr::group_by(StateText, Year, AgeF) %>% 
  dplyr::summarize(meanCond=median(MeanCond),meanSurv=median(MeanSurv), sdCond=sd(MeanCond),sdSurv=sd(MeanSurv), .groups = "keep")

summary(Cond_Surv)
summary(p_Cond_df)

ptest<-ggplot(p_Cond_df, aes(y=meanSurv, x=meanCond, colour=Year, shape=StateText))+
  geom_point()+
  ylim(0,1) + 
  xlab("Condition")+ylab("Predicted survival proability")+
  #geom_errorbarh(aes(xmin = meanCond-sdCond, xmax = meanCond+sdCond), alpha=0.5)+
  #geom_errorbar(aes(ymin = meanSurv-sdSurv, ymax = meanSurv+sdSurv), alpha=0.5)+
  
  labs(col = "Year", shape="State")+
  #facet_wrap(~StateText)+
  ggtitle("all age classes")+
  #geom_smooth(method = "glm", formula=y~x, 
  #            se = FALSE) +
  theme(legend.position="right", 
        #legend.title = element_text(size = 17),
        #legend.text = element_text(size = 16),
        #text = element_text(size=18),
        #axis.title=element_text(size=16),
        axis.title.y=element_blank())
ptest

testAd<-subset(p_Cond_df, AgeF==3)
ptestAd<-ggplot(testAd, aes(y=meanSurv, x=meanCond, colour=Year, shape=StateText))+
  geom_point()+
  ylim(0,1) + 
  xlab("Condition")+ylab("Predicted survival proability")+
  #geom_errorbarh(aes(xmin = meanCond-sdCond, xmax = meanCond+sdCond), alpha=0.5)+
  #geom_errorbar(aes(ymin = meanSurv-sdSurv, ymax = meanSurv+sdSurv), alpha=0.5)+
  
  labs(col = "Year", shape="State")+
  #facet_wrap(~StateText)+
  ggtitle("Adults")+
  #geom_smooth(method = "glm", formula=y~x, 
  #            se = FALSE) +
  theme(legend.position="right", 
        #legend.title = element_text(size = 17),
        #legend.text = element_text(size = 16),
        #text = element_text(size=18),
        #axis.title=element_text(size=16),
        axis.title.y=element_blank())
ptestAd

testSAd<-subset(p_Cond_df, AgeF==2)
ptestSAd<-ggplot(testSAd, aes(y=meanSurv, x=meanCond, colour=Year, shape=StateText))+
  geom_point()+
  ylim(0,1) + 
  xlab("Condition")+ylab("Predicted survival proability")+
  #geom_errorbarh(aes(xmin = meanCond-sdCond, xmax = meanCond+sdCond), alpha=0.5)+
  #geom_errorbar(aes(ymin = meanSurv-sdSurv, ymax = meanSurv+sdSurv), alpha=0.5)+
  
  labs(col = "Year", shape="State")+
  #facet_wrap(~StateText)+
  ggtitle("Sub-Adults")+
  #geom_smooth(method = "glm", formula=y~x, 
  #            se = FALSE) +
  theme(legend.position="none", 
        #legend.title = element_text(size = 17),
        #legend.text = element_text(size = 16),
        #text = element_text(size=18),
        #axis.title=element_text(size=16),
        axis.title.y=element_blank())
ptestSAd

testJuv<-subset(p_Cond_df, AgeF==1)
ptestJuv<-ggplot(testJuv, aes(y=meanSurv, x=meanCond, colour=Year, shape=StateText))+
  geom_point()+
  ylim(0,1) + 
  xlab("Condition")+ylab("Predicted survival proability")+
  #geom_errorbarh(aes(xmin = meanCond-sdCond, xmax = meanCond+sdCond), alpha=0.5)+
  #geom_errorbar(aes(ymin = meanSurv-sdSurv, ymax = meanSurv+sdSurv), alpha=0.5)+
  
  labs(col = "Year")+
  #facet_wrap(~StateText)+
  ggtitle("Juveniles")+
  #geom_smooth(method = "glm", formula=y~x, 
  #            se = FALSE) +
  theme(legend.position="none", 
        #legend.title = element_text(size = 17),
        #legend.text = element_text(size = 16),
        #text = element_text(size=18),
        #axis.title=element_text(size=16),
        axis.title.y=element_blank())
ptestJuv


summary(Cond_Surv)
Cond_Surv_Ad<-subset(Cond_Surv, AgeF==3)
p_4_Ad<-ggplot(dfSurv, aes(x=cond, y=Surv)) + 
  geom_point(data=Cond_Surv_Ad,mapping=aes(x=MeanCond,y=MeanSurv, color=StateText,shape=Year), alpha=0.2, cex=3)+ #Cond_Surv
  #geom_point(data=p_Cond_df,mapping=aes(x=meanCond,y=meanSurv, color=StateText,shape=Year), cex=3)+ #Cond_Surv
  
  #geom_line(data=dfB2,mapping=aes(x = buffy, y = cond, col="#E69F00"), cex=1.2) + 
  #geom_line(data=dfSurv,mapping=aes(x = cond, y = Surv), cex=1.5) + 
  #geom_ribbon(data=dfB2,aes(ymin = lciB2, ymax = uciB2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfSurv,aes(ymin = lciSurv, ymax = uciSurv), alpha = 0.25)+
  labs(x = "", y = "survival probability", tag="a)")+
  theme_bw()+
  ylim(0,1)+
  xlim(-4.7,5.6)+
  theme(legend.position="none", 
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        #legend.spacing = unit(-0.6, "cm"),
        #legend.margin=margin(0,0,0,0),
        text = element_text(size=15),
        axis.title=element_text(size=14),
        title=element_text(size=11))+
  guides(fill=guide_legend(ncol=2, byrow=TRUE))+
  ggtitle("Effect of body condition on survival \nin adults (>3 year old)")+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))
#theme(legend.position ="bottom")
p_4_Ad

Cond_Surv_SAd<-subset(Cond_Surv, AgeF==2)
p_4_SAd<-ggplot(dfSurv, aes(x=cond, y=Surv)) + 
  geom_point(data=Cond_Surv_SAd,mapping=aes(x=MeanCond,y=MeanSurv, color=StateText,shape=Year), alpha=0.2, cex=3)+ #Cond_Surv
  #geom_point(data=p_Cond_df,mapping=aes(x=meanCond,y=meanSurv, color=StateText,shape=Year), cex=3)+ #Cond_Surv
  
  #geom_line(data=dfB2,mapping=aes(x = buffy, y = cond, col="#E69F00"), cex=1.2) + 
  #geom_line(data=dfSurv,mapping=aes(x = cond, y = Surv), cex=1.5) + 
  #geom_ribbon(data=dfB2,aes(ymin = lciB2, ymax = uciB2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfSurv,aes(ymin = lciSurv, ymax = uciSurv), alpha = 0.25)+
  labs(x = "", y = "", tag="b)")+
  theme_bw()+
  ylim(0,1)+
  xlim(-4.7,5.6)+
  theme(legend.position="none", 
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        #legend.spacing = unit(-0.6, "cm"),
        #legend.margin=margin(0,0,0,0),
        text = element_text(size=15),
        axis.title=element_text(size=14),
        title=element_text(size=11))+
  guides(fill=guide_legend(ncol=2, byrow=TRUE))+
  ggtitle("Effect of body condition on survival \nin 2-& 3-year-old birds")+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))
#theme(legend.position ="bottom")
p_4_SAd

Cond_Surv_Juv<-subset(Cond_Surv, AgeF==1)
p_4_Juv<-ggplot(dfSurv, aes(x=cond, y=Surv)) + 
  geom_point(data=Cond_Surv_Juv,mapping=aes(x=MeanCond,y=MeanSurv, color=StateText,shape=Year), alpha=0.2, cex=3)+ #Cond_Surv
  #geom_point(data=p_Cond_df,mapping=aes(x=meanCond,y=meanSurv, color=StateText,shape=Year), cex=3)+ #Cond_Surv
  
  #geom_line(data=dfB2,mapping=aes(x = buffy, y = cond, col="#E69F00"), cex=1.2) + 
  #geom_line(data=dfSurv,mapping=aes(x = cond, y = Surv), cex=1.5) + 
  #geom_ribbon(data=dfB2,aes(ymin = lciB2, ymax = uciB2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfSurv,aes(ymin = lciSurv, ymax = uciSurv), alpha = 0.25)+
  labs(x = "", y = "", tag="c)")+
  theme_bw()+
  ylim(0,1)+
  xlim(-4.7,5.6)+
  theme(legend.position="none", 
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        #legend.spacing = unit(-0.6, "cm"),
        #legend.margin=margin(0,0,0,0),
        text = element_text(size=15),
        axis.title=element_text(size=14),
        title=element_text(size=11))+
  guides(fill=guide_legend(ncol=2, byrow=TRUE))+
  ggtitle("Effect of body condition on survival \nin 1-year-old birds")+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))
#theme(legend.position ="bottom")
p_4_Juv

summary(Cond_Surv_Juv$MeanCond)
summary(Cond_Surv_SAd$MeanCond)
summary(Cond_Surv_Ad$MeanCond)

#### combine plots for supplements
library(patchwork)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Survival_Condition_Fig3/Surv_Cond_perAgeClass.png", width = 13000, height = 6000,units = 'px', res = 800)
patchwork <- p_4_Ad|p_4_SAd|p_4_Juv+
  xlab(label = "body condition")+
  theme(axis.title.x = element_text(vjust=1.9, hjust=-1.2))
patchwork
dev.off() 


### p_4 with mean cond per state and time
p_4_test<-ggplot(dfSurv, aes(x=cond, y=Surv)) + 
  geom_point(data=Cond_Surv,mapping=aes(x=MeanCond,y=MeanSurv, color=StateText,shape=Year), alpha=0.2, cex=3)+ #Cond_Surv
  geom_point(data=p_Cond_df,mapping=aes(x=meanCond,y=meanSurv, color=StateText,shape=Year), cex=6)+ #Cond_Surv
  
  #geom_line(data=dfB2,mapping=aes(x = buffy, y = cond, col="#E69F00"), cex=1.2) + 
  geom_line(data=dfSurv,mapping=aes(x = cond, y = Surv), cex=1.5) + 
  #geom_ribbon(data=dfB2,aes(ymin = lciB2, ymax = uciB2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfSurv,aes(ymin = lciSurv, ymax = uciSurv), alpha = 0.25)+
  labs(x = "body condition", y = "survival probability")+
  theme_bw()+
  theme(legend.position="right", 
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        #legend.spacing = unit(-0.6, "cm"),
        #legend.margin=margin(0,0,0,0),
        text = element_text(size=15),
        axis.title=element_text(size=14),
        title=element_text(size=11))+
  guides(fill=guide_legend(ncol=2, byrow=TRUE))+
  ggtitle("Effect of body condition on survival")+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))
#theme(legend.position ="bottom")
p_4_test

################################################################################################################
############################################# caterpillar plots for confounding variables ######################
################################################################################################################
## alternativly manually like this; see l=plottingputput p12
PathCoef<-read.csv(file="F:/NIOO drive/N drive/_Magali/Nimble_M1_P12/Output20210805/Output_CR_SEM_nimble_PathCoef.csv")

### plot sem parameters
#confounding variables on mass
library(ggplot2)
d_alpha1<-PathCoef[2:8,]
d_alpha2<-PathCoef[57:59,]
d_alpha<-rbind(d_alpha1,d_alpha2)
d_alpha$X <- factor(d_alpha$X, 
                    levels=c("alpha1", "alpha2", "alpha3", "alpha4", "alpha5", "alpha52", "alpha6", "Juv_MS", "sAd_MS", "Ad_MS"),
                    labels=c("tarsus length", "head length", "wing length", "bill tip height", "handling time", "handling time^2", "catch day", "1-yr-olds", "2-yr-olds", ">3-yr-olds"))
d_alpha_ord <- d_alpha[order(d_alpha$X), ]

library(dplyr)
CM<-
  ggplot(d_alpha_ord, aes(y=X,x=Median)) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "grey", size=1.5)+
  geom_point(cex=2)+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))+
  geom_errorbar(data=d_alpha_ord,mapping=aes(xmin=X95.CI_low, xmax=X95.CI_upp, y=X, x=Median), width=0.5)+
  labs(x="posterior median (??95% CI)", y="")+
  ggtitle("Effect of (confunding) variables \non condition variable \n'body mass'")+
  scale_y_discrete(limits = rev(levels(d_alpha_ord$X)), 
                   labels=c("tarsus length"="tarsus length", "head length"="head length", "wing length"="wing length",
    "bill tip height"="bill tip height", "handling time"="handling time", "handling time^2"=expression(paste("handling time"^"2")), 
    "catch day"="catch day","1-yr-olds"="1-yr-olds", "2-yr-olds"="2-yr-olds", ">3-yr-olds"=">3-yr-olds")
  )
CM

#confounding variables on hema
library(ggplot2)
d_delta1<-PathCoef[13:16,]
d_delta2<-PathCoef[60:62,]
d_delta3<-PathCoef[68:69,]

d_delta<-rbind(d_delta1,d_delta2, d_delta3)
d_delta$X <- factor(d_delta$X, 
                    levels=c("delta1", "delta2", "delta22", "delta3", "Juv_H", "sAd_H", "Ad_H", "F_H", "M_H"),
                    labels=c("bill tip height", "handling time", "handling time^2", "catch day", "1-yr-olds", "2-yr-olds", ">3-yr-olds", "females", "males"))
d_delta_ord <- d_delta[order(d_delta$X), ]

levels(d_delta$X)

library(dplyr)
CH<-
  ggplot(d_delta_ord, aes(y=X,x=Median)) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "grey", size=1.5)+
  geom_point(cex=2)+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        title = element_text(size=11))+
  geom_errorbar(data=d_delta_ord,mapping=aes(xmin=X95.CI_low, xmax=X95.CI_upp, y=X, x=Median), width=0.5)+
  labs(x="posterior median (??95% CI)", y="")+
  ggtitle("'haematocrit'")+
  scale_y_discrete(limits = rev(levels(d_delta_ord$X)), labels=c("bill tip height"="bill tip height", "handling time"="handling time", "handling time^2"=expression(paste("handling time"^"2")), 
                                                                 "catch day"="catch day","1-yr-olds"="1-yr-olds", "2-yr-olds"="2-yr-olds", ">3-yr-olds"=">3-yr-olds", "females"="females", "males"="males")
  )
CH

#confounding variables on buffy coat
library(ggplot2)
d_theta1<-PathCoef[23:26,]
d_theta2<-PathCoef[63:67,]

d_theta<-rbind(d_theta1,d_theta2)
d_theta$X <- factor(d_theta$X, 
                    levels=c("theta1", "theta2", "theta22", "theta3", "Juv_B", "sAd_B", "Ad_B", "F_B", "M_B"),
                    labels=c("bill tip height", "handling time", 'handling time^2', "catch day", "1-yr-olds", "2-yr-olds", ">3-yr-olds", "females", "males"))
d_theta_ord <- d_theta[order(d_theta$X), ]

library(dplyr)
CB<-
  ggplot(d_theta_ord, aes(y=X,x=Median)) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "grey", size=1.5)+
  geom_point(cex=2)+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size=11))+
  geom_errorbar(data=d_theta_ord,mapping=aes(xmin=X95.CI_low, xmax=X95.CI_upp, y=X, x=Median), width=0.5)+
  labs(x="posterior median (??95% CI)", y="")+
  ggtitle("'buffy coat'")+
  scale_y_discrete(limits = rev(levels(d_theta_ord$X)), labels=c("bill tip height"="bill tip height", "handling time"="handling time", "handling time^2"=expression(paste("handling time"^"2")), 
                                                                 "catch day"="catch day","1-yr-olds"="1-yr-olds", "2-yr-olds"="2-yr-olds", ">3-yr-olds"=">3-yr-olds", "females"="females", "males"="males")
  )
CB



######## combine plots ###########
library(ggplot2)
library(patchwork)
#install.packages("scales")
library(scales)
#png("P:/CHIRP/Carry-over/Analysis/Figures/MainDoc/Fig3_ConditionVariables.png", width = 14000, height = 9000,units = 'px', res = 800)
#png("P:/CHIRP/Presentaties/AnE seminar/Fig3_ConditionVariables.png", width = 14000, height = 9000,units = 'px', res = 800)
png("G:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/MainDoc/Fig3_ConditionVariables.png", width = 14000, height = 9000,units = 'px', res = 800)

layout <- "
AADDGGGG
BBEEGGGG
CCFFGGGG
"
patchwork <- CM+CH+CB+p_1+p_2+p_3+p_4 + 
  plot_layout(design = layout)
patchwork+plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 18))
dev.off() 

save.image("G:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/MainDoc/Files for Fig3/GlobalEnv.Rdata")

############################################################################################################################
####################################### distribution of mass across periods ##################################################
############################################################################################################################
Cond_Surv_mass<-cbind(eta,Surv_DS)
str(Cond_Surv_mass)
summary(Cond_Surv_mass$MS)

Cond_Surv_mass$YearShort<-factor(Cond_Surv_mass$YearShort)
Cond_Surv_mass<-subset(Cond_Surv_mass, 
                       select=c(MS,YearShort))
Cond_Surv_mass$Period<-ifelse(Cond_Surv_mass$YearShort=="2001"|Cond_Surv_mass$YearShort=="2002"|
                                Cond_Surv_mass$YearShort=="2003",1,2)

str(Cond_Surv_mass)
summary(Cond_Surv_mass$Period)
Cond_Surv_mass$Period<-factor(Cond_Surv_mass$Period)

#### histogram voor supplements masss ###
Cond_Surv_mass_P1<-subset(Cond_Surv_mass,Cond_Surv_mass$Period==1)
Cond_Surv_mass_P2<-subset(Cond_Surv_mass,Cond_Surv_mass$Period==2)

mean(Cond_Surv_mass_P1$MS) #575.9
sd(Cond_Surv_mass_P1$MS) #50.3

mean(Cond_Surv_mass_P2$MS) #560.4
sd(Cond_Surv_mass_P2$MS) #50.1

p_mass_Period<-ggplot(Cond_Surv_mass, aes(x=MS, color=Period)) +
  geom_histogram(fill="white", alpha=0.5, position="dodge", bins=50)+
  #geom_vline(data=mu, aes(xintercept=mean, color=Year),
  #           linetype="dashed")+
  xlab("Body mass (g)") + ylab("Frequency")
p_mass_Period
##############################################################################################################################################################################
######################################################## calculate bayesian R2 ###############################################################################################################################
########################################################### Gelman et al 2019 ############################################################
### bayesian R2 ####

### bayesian R2 for Fig.3g (average year,state)
library(gtools)
#predicted_Surv <- matrix(NA, nrow = length(beta1), ncol = length(mean.mat.stcov[,1]))
#for (i in 1:length(beta1)){ #for each iteration
#  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
#    predicted_Surv[i,j] <- plogis(
#      logit(mean.mat.int_phi[j]) + #average surv across age/stat  
#        beta1[i] * samplesSummaryEta[j,1] + #condition effect
#        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
#        mean.mat.epst_phi[j] # time  effect average
#    )
#  }
#}

## take variance of prediction
var_fit <- apply(predicted_Surv, 1, var)
head(var_fit)
mean(var_fit)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res <- Cond_Surv$MeanSurv-predicted_Surv
#calculate variance of resiudlas
var_res <- apply(res, 1, var)

#calculate r2
r2<-var_fit / (var_fit + var_res)
round(mean(r2),2) #0.25

#############################################################################################################################
#################################### for each state ########################################################################
################################################################################################################################
#Phi$StateText<-factor(Phi$State, levels=c( "1", "2", "3", "4", "5", "6", "7", "8", "9"),
#                      labels=c("Delta (D)", "Inland South (P)", "Inland North (N)", "Texel-Balgzand (B)", 
#                               "Texel-Vlieland (V)","Terschelling-Ameland (T)", "Schiermonnikoog (S)","Rottum (R)",  
#                               "Abroad (X)"))

##################################### for state 4 example, average year and age ########################
# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
epst_phi$State<-factor(substr(epst_phi$X, 10,10)) #1 is summer 2000, 2 is winter 2000-2001
levels(epst_phi$State)
epst_phi_S4<-subset(epst_phi, State=="4")
mean_epst_phi_S4<-mean(epst_phi_S4[,2], na.rm=T)

int_phi$State<-factor(substr(int_phi$X, 9,9)) #1 is summer 2000, 2 is winter 2000-2001
levels(int_phi$State)
int_phi_S4<-subset(int_phi, State=="4")
mean_int_phi_S4<-mean(int_phi_S4[,2], na.rm=T)

mean.mat.epst_phi_S4 = matrix(rep(mean_epst_phi_S4,nrow(stcov)),byrow=T) #for length of nr ind
mean.mat.int_phi_S4 = matrix(rep(mean_int_phi_S4,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_S4<-subset(Cond_Surv, StateText=="Texel-Balgzand (B)")

library(gtools)
predicted_Surv_S4 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_S4[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_S4[,1]))){ # for each individual
    predicted_Surv_S4[i,j] <- plogis(
      logit(mean.mat.int_phi_S4[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_S4[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_S4 <- apply(predicted_Surv_S4, 1, var)
head(var_fit_S4)
mean(var_fit_S4)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_S4 <- Cond_Surv_S4$MeanSurv-predicted_Surv_S4
#calculate variance of resiudlas
var_res_S4 <- apply(res_S4, 1, var)

#calculate r2
r2_S4<-var_fit_S4 / (var_fit_S4 + var_res_S4)
r2_S4_Mean<-round(mean(r2_S4),2) #0.37
r2_S4_Mean

##################################### for state 5 example, average year and age #######################
# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#epst_phi$State<-factor(substr(epst_phi$X, 10,10)) #1 is summer 2000, 2 is winter 2000-2001
#levels(epst_phi$State)
epst_phi_S5<-subset(epst_phi, State=="5")
mean_epst_phi_S5<-mean(epst_phi_S5[,2], na.rm=T)

#int_phi$State<-factor(substr(int_phi$X, 9,9)) #1 is summer 2000, 2 is winter 2000-2001
#levels(int_phi$State)
int_phi_S5<-subset(int_phi, State=="5")
mean_int_phi_S5<-mean(int_phi_S5[,2], na.rm=T)

mean.mat.epst_phi_S5 = matrix(rep(mean_epst_phi_S5,nrow(stcov)),byrow=T) #for length of nr ind
mean.mat.int_phi_S5 = matrix(rep(mean_int_phi_S5,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_S5<-subset(Cond_Surv, StateText=="Texel-Vlieland (V)")

library(gtools)
predicted_Surv_S5 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_S5[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_S5[,1]))){ # for each individual
    predicted_Surv_S5[i,j] <- plogis(
      logit(mean.mat.int_phi_S5[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_S5[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_S5 <- apply(predicted_Surv_S5, 1, var)
head(var_fit_S5)
mean(var_fit_S5)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_S5 <- Cond_Surv_S5$MeanSurv-predicted_Surv_S5
#calculate variance of resiudlas
var_res_S5 <- apply(res_S5, 1, var)

#calculate r2
r2_S5<-var_fit_S5 / (var_fit_S5 + var_res_S5)
r2_S5_Mean<-round(mean(r2_S5),2) #0.37
r2_S5_Mean

##################################### for state 6 example, average year and age #######################
# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#epst_phi$State<-factor(substr(epst_phi$X, 10,10)) #1 is summer 2000, 2 is winter 2000-2001
#levels(epst_phi$State)
epst_phi_S6<-subset(epst_phi, State=="6")
mean_epst_phi_S6<-mean(epst_phi_S6[,2], na.rm=T)

#int_phi$State<-factor(substr(int_phi$X, 9,9)) #1 is summer 2000, 2 is winter 2000-2001
#levels(int_phi$State)
int_phi_S6<-subset(int_phi, State=="6")
mean_int_phi_S6<-mean(int_phi_S6[,2], na.rm=T)

mean.mat.epst_phi_S6 = matrix(rep(mean_epst_phi_S6,nrow(stcov)),byrow=T) #for length of nr ind
mean.mat.int_phi_S6 = matrix(rep(mean_int_phi_S6,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_S6<-subset(Cond_Surv, StateText=="Terschelling-Ameland (T)")

library(gtools)
predicted_Surv_S6 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_S6[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_S6[,1]))){ # for each individual
    predicted_Surv_S6[i,j] <- plogis(
      logit(mean.mat.int_phi_S6[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_S6[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_S6 <- apply(predicted_Surv_S6, 1, var)
head(var_fit_S6)
mean(var_fit_S6)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_S6 <- Cond_Surv_S6$MeanSurv-predicted_Surv_S6
#calculate variance of resiudlas
var_res_S6 <- apply(res_S6, 1, var)

#calculate r2
r2_S6<-var_fit_S6 / (var_fit_S6 + var_res_S6)
r2_S6_Mean<-round(mean(r2_S6),2) #0.12
r2_S6_Mean

##################################### for state 7 example, average year and age #######################
# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#epst_phi$State<-factor(substr(epst_phi$X, 10,10)) #1 is summer 2000, 2 is winter 2000-2001
#levels(epst_phi$State)
epst_phi_S7<-subset(epst_phi, State=="7")
mean_epst_phi_S7<-mean(epst_phi_S7[,2], na.rm=T)

#int_phi$State<-factor(substr(int_phi$X, 9,9)) #1 is summer 2000, 2 is winter 2000-2001
#levels(int_phi$State)
int_phi_S7<-subset(int_phi, State=="7")
mean_int_phi_S7<-mean(int_phi_S7[,2], na.rm=T)

mean.mat.epst_phi_S7 = matrix(rep(mean_epst_phi_S7,nrow(stcov)),byrow=T) #for length of nr ind
mean.mat.int_phi_S7 = matrix(rep(mean_int_phi_S7,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_S7<-subset(Cond_Surv, StateText=="Schiermonnikoog (S)")

library(gtools)
predicted_Surv_S7 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_S7[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_S7[,1]))){ # for each individual
    predicted_Surv_S7[i,j] <- plogis(
      logit(mean.mat.int_phi_S7[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_S7[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_S7 <- apply(predicted_Surv_S7, 1, var)
head(var_fit_S7)
mean(var_fit_S7)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_S7 <- Cond_Surv_S7$MeanSurv-predicted_Surv_S7
#calculate variance of resiudlas
var_res_S7 <- apply(res_S7, 1, var)

#calculate r2
r2_S7<-var_fit_S7 / (var_fit_S7 + var_res_S7)
r2_S7_Mean<-round(mean(r2_S7),2) #0.21
r2_S7_Mean


##################################### for state 8 example, average year and age #######################
# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#epst_phi$State<-factor(substr(epst_phi$X, 10,10)) #1 is summer 2000, 2 is winter 2000-2001
#levels(epst_phi$State)
epst_phi_S8<-subset(epst_phi, State=="8")
mean_epst_phi_S8<-mean(epst_phi_S8[,2], na.rm=T)

#int_phi$State<-factor(substr(int_phi$X, 9,9)) #1 is summer 2000, 2 is winter 2000-2001
#levels(int_phi$State)
int_phi_S8<-subset(int_phi, State=="8")
mean_int_phi_S8<-mean(int_phi_S8[,2], na.rm=T)

mean.mat.epst_phi_S8 = matrix(rep(mean_epst_phi_S8,nrow(stcov)),byrow=T) #for length of nr ind
mean.mat.int_phi_S8 = matrix(rep(mean_int_phi_S8,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_S8<-subset(Cond_Surv, StateText=="Rottum (R)")

library(gtools)
predicted_Surv_S8 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_S8[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_S8[,1]))){ # for each individual
    predicted_Surv_S8[i,j] <- plogis(
      logit(mean.mat.int_phi_S8[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_S8[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_S8 <- apply(predicted_Surv_S8, 1, var)
head(var_fit_S8)
mean(var_fit_S8)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_S8 <- Cond_Surv_S8$MeanSurv-predicted_Surv_S8
#calculate variance of resiudlas
var_res_S8 <- apply(res_S8, 1, var)

#calculate r2
r2_S8<-var_fit_S8 / (var_fit_S8 + var_res_S8)
r2_S8_Mean<-round(mean(r2_S8),2) #0.5
r2_S8_Mean

##################################### for state 1 example, average year and age #######################
# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#epst_phi$State<-factor(substr(epst_phi$X, 10,10)) #1 is summer 2000, 2 is winter 2000-2001
#levels(epst_phi$State)
epst_phi_S1<-subset(epst_phi, State=="1")
mean_epst_phi_S1<-mean(epst_phi_S1[,2], na.rm=T)

#int_phi$State<-factor(substr(int_phi$X, 9,9)) #1 is summer 2000, 2 is winter 2000-2001
#levels(int_phi$State)
int_phi_S1<-subset(int_phi, State=="1")
mean_int_phi_S1<-mean(int_phi_S1[,2], na.rm=T)

mean.mat.epst_phi_S1 = matrix(rep(mean_epst_phi_S1,nrow(stcov)),byrow=T) #for length of nr ind
mean.mat.int_phi_S1 = matrix(rep(mean_int_phi_S1,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_S1<-subset(Cond_Surv, StateText=="Delta (D)")

library(gtools)
predicted_Surv_S1 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_S1[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_S1[,1]))){ # for each individual
    predicted_Surv_S1[i,j] <- plogis(
      logit(mean.mat.int_phi_S1[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_S1[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_S1 <- apply(predicted_Surv_S1, 1, var)
head(var_fit_S1)
mean(var_fit_S1)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_S1 <- Cond_Surv_S1$MeanSurv-predicted_Surv_S1
#calculate variance of resiudlas
var_res_S1 <- apply(res_S1, 1, var)

#calculate r2
r2_S1<-var_fit_S1 / (var_fit_S1 + var_res_S1)
r2_S1_Mean<-round(mean(r2_S1),2) #0.49
r2_S1_Mean

####### average across all states
StateMean<-mean(r2_S1_Mean,r2_S4_Mean,r2_S5_Mean, r2_S6_Mean, r2_S7_Mean, r2_S8_Mean) #0.49

#Table:
R2_table <- matrix(c(r2_S1_Mean,r2_S4_Mean,r2_S5_Mean,r2_S6_Mean, r2_S7_Mean, r2_S8_Mean, StateMean),
                   #round(s_CS$spec_pars[2,5],0),s_CS$spec_pars[2,6],s_CS$spec_pars[2,7]), 
                   ncol=7, byrow=TRUE)
colnames(R2_table) <- c("Delta (D)", "Texel-Balgzand (B)", "Texel-Vlieland (V)", "Terschelling-Ameland (T)", "Schiermonnikoog (S)", 
                        "Rottum (R", "Average")
rownames(R2_table) <- c("Explained variation within the state (with an average year and age)")
R2_table

#Phi$StateText<-factor(Phi$State, levels=c( "1", "2", "3", "4", "5", "6", "7", "8", "9"),
#                      labels=c("Delta (D)", "Inland South (P)", "Inland North (N)", "Texel-Balgzand (B)", 
#                               "Texel-Vlieland (V)","Terschelling-Ameland (T)", "Schiermonnikoog (S)","Rottum (R)",  
#                               "Abroad (X)"))

knitr::kable(R2_table,digits=2, format="pandoc")
kable_out_R2_table <- knitr::kable(R2_table, "html", digits=4) %>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
readr::write_file(kable_out_R2_table, "P:/CHIRP/Carry-over/Analysis/Figures/Supplements/R2/R2_table.doc")

######################################### bayesian r2 per year ##############################################
# W 2000-2001 to S 2001 " 3]"
# W 2001-2002 to S 2002  " 5]",
#"W 2002-2003 to S 2003  " 7]"
#"W 2016-2017 to S 2017  "13]"
#"W 2017-2018 to S 2018  "15]"

##################################################### year 1 ######################################################
# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
epst_phi$X<-as.character(epst_phi$X)
epst_phi$Year<-factor(substrRight(epst_phi$X, 3)) #1 is summer 2000, 2 is winter 2000-2001

levels(epst_phi$Year)
epst_phi_Y1<-subset(epst_phi, Year==" 3]")  #W 2000-2001 to S 2001
mean_epst_phi_Y1<-mean(epst_phi_Y1[,2], na.rm=T)

mean.mat.epst_phi_Y1 = matrix(rep(mean_epst_phi_Y1,nrow(stcov)),byrow=T) #for length of nr ind

levels(Cond_Surv$YearShort)
Cond_Surv_Y1<-subset(Cond_Surv, Year=="W 2000-2001 to S 2001")

library(gtools)
predicted_Surv_Y1 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_Y1[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_Y1[,1]))){ # for each individual
    predicted_Surv_Y1[i,j] <- plogis(
      logit(mean.mat.int_phi[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_Y1[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_Y1 <- apply(predicted_Surv_Y1, 1, var)
head(var_fit_Y1)
mean(var_fit_Y1)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_Y1 <- Cond_Surv_Y1$MeanSurv-predicted_Surv_Y1
#calculate variance of resiudlas
var_res_Y1 <- apply(res_Y1, 1, var)

#calculate r2
r2_Y1<-var_fit_Y1 / (var_fit_Y1 + var_res_Y1)
r2_Y1_Mean<-round(mean(r2_Y1),2) 
r2_Y1_Mean #0.1

################################################################# year 2 ##########################################

# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#substrRight <- function(x, n){
#  substr(x, nchar(x)-n+1, nchar(x))
#}
#epst_phi$X<-as.character(epst_phi$X)
#epst_phi$Year<-factor(substrRight(epst_phi$X, 3)) #1 is summer 2000, 2 is winter 2000-2001

#levels(epst_phi$Year)
epst_phi_Y2<-subset(epst_phi, Year==" 5]")  #W 2000-2001 to S 2001
mean_epst_phi_Y2<-mean(epst_phi_Y2[,2], na.rm=T)

mean.mat.epst_phi_Y2 = matrix(rep(mean_epst_phi_Y2,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_Y2<-subset(Cond_Surv, Year=="W 2001-2002 to S 2002")

library(gtools)
predicted_Surv_Y2 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_Y2[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_Y2[,1]))){ # for each individual
    predicted_Surv_Y2[i,j] <- plogis(
      logit(mean.mat.int_phi[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_Y2[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_Y2 <- apply(predicted_Surv_Y2, 1, var)
head(var_fit_Y2)
mean(var_fit_Y2)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_Y2 <- Cond_Surv_Y2$MeanSurv-predicted_Surv_Y2
#calculate variance of resiudlas
var_res_Y2 <- apply(res_Y2, 1, var)

#calculate r2
r2_Y2<-var_fit_Y2 / (var_fit_Y2 + var_res_Y2)
r2_Y2_Mean<-round(mean(r2_Y2),2) 
r2_Y2_Mean #0.35

################################################################# year 3 ##########################################

# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#substrRight <- function(x, n){
#  substr(x, nchar(x)-n+1, nchar(x))
#}
#epst_phi$X<-as.character(epst_phi$X)
#epst_phi$Year<-factor(substrRight(epst_phi$X, 3)) #1 is summer 2000, 2 is winter 2000-2001

#levels(epst_phi$Year)
epst_phi_Y3<-subset(epst_phi, Year==" 7]")  #W 2000-2001 to S 2001
mean_epst_phi_Y3<-mean(epst_phi_Y3[,2], na.rm=T)

mean.mat.epst_phi_Y3 = matrix(rep(mean_epst_phi_Y3,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_Y3<-subset(Cond_Surv, Year=="W 2002-2003 to S 2003")

library(gtools)
predicted_Surv_Y3 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_Y3[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_Y3[,1]))){ # for each individual
    predicted_Surv_Y3[i,j] <- plogis(
      logit(mean.mat.int_phi[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_Y3[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_Y3 <- apply(predicted_Surv_Y3, 1, var)
head(var_fit_Y3)
mean(var_fit_Y3)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_Y3 <- Cond_Surv_Y3$MeanSurv-predicted_Surv_Y3
#calculate variance of resiudlas
var_res_Y3 <- apply(res_Y3, 1, var)

#calculate r2
r2_Y3<-var_fit_Y3 / (var_fit_Y3 + var_res_Y3)
r2_Y3_Mean<-round(mean(r2_Y3),2) 
r2_Y3_Mean #0.0.28

################################################################# year 4 ##########################################

# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#substrRight <- function(x, n){
#  substr(x, nchar(x)-n+1, nchar(x))
#}
#epst_phi$X<-as.character(epst_phi$X)
#epst_phi$Year<-factor(substrRight(epst_phi$X, 3)) #1 is summer 2000, 2 is winter 2000-2001

#levels(epst_phi$Year)
epst_phi_Y4<-subset(epst_phi, Year=="13]")  #W 2000-2001 to S 2001
mean_epst_phi_Y4<-mean(epst_phi_Y4[,2], na.rm=T)

mean.mat.epst_phi_Y4 = matrix(rep(mean_epst_phi_Y4,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_Y4<-subset(Cond_Surv, Year=="W 2016-2017 to S 2017")

library(gtools)
predicted_Surv_Y4 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_Y4[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_Y4[,1]))){ # for each individual
    predicted_Surv_Y4[i,j] <- plogis(
      logit(mean.mat.int_phi[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_Y4[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_Y4 <- apply(predicted_Surv_Y4, 1, var)
head(var_fit_Y4)
mean(var_fit_Y4)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_Y4 <- Cond_Surv_Y4$MeanSurv-predicted_Surv_Y4
#calculate variance of resiudlas
var_res_Y4 <- apply(res_Y4, 1, var)

#calculate r2
r2_Y4<-var_fit_Y4 / (var_fit_Y4 + var_res_Y4)
r2_Y4_Mean<-round(mean(r2_Y4),2) 
r2_Y4_Mean #0.16

################################################################# year 5 ##########################################

# average survival across years (epst.phi) and age/state (int.phi) (for prediction)
#substrRight <- function(x, n){
#  substr(x, nchar(x)-n+1, nchar(x))
#}
#epst_phi$X<-as.character(epst_phi$X)
#epst_phi$Year<-factor(substrRight(epst_phi$X, 3)) #1 is summer 2000, 2 is winter 2000-2001

#levels(epst_phi$Year)
epst_phi_Y5<-subset(epst_phi, Year=="15]")  #W 2000-2001 to S 2001
mean_epst_phi_Y5<-mean(epst_phi_Y5[,2], na.rm=T)

mean.mat.epst_phi_Y5 = matrix(rep(mean_epst_phi_Y5,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_Y5<-subset(Cond_Surv, Year=="W 2017-2018 to S 2018")

library(gtools)
predicted_Surv_Y5 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_Y5[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_Y5[,1]))){ # for each individual
    predicted_Surv_Y5[i,j] <- plogis(
      logit(mean.mat.int_phi[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi_Y5[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_Y5 <- apply(predicted_Surv_Y5, 1, var)
head(var_fit_Y5)
mean(var_fit_Y5)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_Y5 <- Cond_Surv_Y5$MeanSurv-predicted_Surv_Y5
#calculate variance of resiudlas
var_res_Y5 <- apply(res_Y5, 1, var)

#calculate r2
r2_Y5<-var_fit_Y5 / (var_fit_Y5 + var_res_Y5)
r2_Y5_Mean<-round(mean(r2_Y5),2) 
r2_Y5_Mean #0.13

###############################################################################################################
################################################################# ages  ##########################################

######################################## age class 1 ######################################################
int_phi$Age<-factor(substrRight(int_phi$X, 2)) #1 is summer 2000, 2 is winter 2000-2001
levels(int_phi$Age)
int_phi_A1<-subset(int_phi, Age=="1]")
mean_int_phi_A1<-mean(int_phi_A1[,2], na.rm=T)

mean.mat.int_phi_A1 = matrix(rep(mean_int_phi_A1,nrow(stcov)),byrow=T) #for length of nr ind

levels(Cond_Surv$AgeF)

Cond_Surv_A1<-subset(Cond_Surv, AgeF=="1")

library(gtools)
predicted_Surv_A1 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_A1[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_A1[,1]))){ # for each individual
    predicted_Surv_A1[i,j] <- plogis(
      logit(mean.mat.int_phi_A1[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_A1 <- apply(predicted_Surv_A1, 1, var)
head(var_fit_A1)
mean(var_fit_A1)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_A1 <- Cond_Surv_A1$MeanSurv-predicted_Surv_A1
#calculate variance of resiudlas
var_res_A1 <- apply(res_A1, 1, var)

#calculate r2
r2_A1<-var_fit_A1 / (var_fit_A1 + var_res_A1)
r2_A1_Mean<-round(mean(r2_A1),2) 
r2_A1_Mean #0.28

######################################## age class 2 ######################################################
#int_phi$Age<-factor(substrRight(int_phi$X, 2)) #1 is summer 2000, 2 is winter 2000-2001
#levels(int_phi$Age)
int_phi_A2<-subset(int_phi, Age=="2]")
mean_int_phi_A2<-mean(int_phi_A2[,2], na.rm=T)

mean.mat.int_phi_A2 = matrix(rep(mean_int_phi_A2,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_A2<-subset(Cond_Surv, AgeF=="2")

library(gtools)
predicted_Surv_A2 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_A2[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_A2[,1]))){ # for each individual
    predicted_Surv_A2[i,j] <- plogis(
      logit(mean.mat.int_phi_A2[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_A2 <- apply(predicted_Surv_A2, 1, var)
head(var_fit_A2)
mean(var_fit_A2)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_A2 <- Cond_Surv_A2$MeanSurv-predicted_Surv_A2
#calculate variance of resiudlas
var_res_A2 <- apply(res_A2, 1, var)

#calculate r2
r2_A2<-var_fit_A2 / (var_fit_A2 + var_res_A2)
r2_A2_Mean<-round(mean(r2_A2),2) 
r2_A2_Mean #0.2

######################################## age class 3 ######################################################
#int_phi$Age<-factor(substrRight(int_phi$X, 2)) #1 is summer 2000, 2 is winter 2000-2001
#levels(int_phi$Age)
int_phi_A3<-subset(int_phi, Age=="3]")
mean_int_phi_A3<-mean(int_phi_A3[,2], na.rm=T)

mean.mat.int_phi_A3 = matrix(rep(mean_int_phi_A3,nrow(stcov)),byrow=T) #for length of nr ind

Cond_Surv_A3<-subset(Cond_Surv, AgeF=="3")

library(gtools)
predicted_Surv_A3 <- matrix(NA, nrow = length(beta1), ncol = length(Cond_Surv_A3[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((Cond_Surv_A3[,1]))){ # for each individual
    predicted_Surv_A3[i,j] <- plogis(
      logit(mean.mat.int_phi_A3[j]) + #average surv across age/stat  
        beta1[i] * samplesSummaryEta[j,1] + #condition effect
        beta2[i] * mean.mat.stcov[j,9] +  # average effect of catch day
        mean.mat.epst_phi[j] # time  effect average
    )
  }
}

## take variance of prediction
var_fit_A3 <- apply(predicted_Surv_A3, 1, var)
head(var_fit_A3)
mean(var_fit_A3)

# calculate residuals (true-prediction), true=Cond_Surv$MeanSurv
res_A3 <- Cond_Surv_A3$MeanSurv-predicted_Surv_A3
#calculate variance of resiudlas
var_res_A3 <- apply(res_A3, 1, var)

#calculate r2
r2_A3<-var_fit_A3 / (var_fit_A3 + var_res_A3)
r2_A3_Mean<-round(mean(r2_A3),2) 
r2_A3_Mean #0.13



#############################################################################################################
####### average across all states
StateMean<-mean(r2_S1_Mean,r2_S4_Mean,r2_S5_Mean, r2_S6_Mean, r2_S7_Mean, r2_S8_Mean) #0.49
YearMean<-mean(r2_Y1_Mean,r2_Y2_Mean,r2_Y3_Mean, r2_Y4_Mean, r2_Y5_Mean) #0.1
AgeMean<-mean(r2_A1_Mean,r2_A2_Mean,r2_A3_Mean) #0.28

#Table:
R2_table <- matrix(c(r2_S1_Mean,r2_S4_Mean,r2_S5_Mean,r2_S6_Mean, r2_S7_Mean, r2_S8_Mean, StateMean,
                     "Winter 2000-2001", "Winter 2001-2002", "Winter 2002-2003", "Winter 2016-2017", "Winter 2017-2018", " "," ",
                     r2_Y1_Mean,r2_Y2_Mean,r2_Y3_Mean, r2_Y4_Mean, r2_Y5_Mean, "", YearMean,
                     "1 year-old birds", "2 & 3-year old birds", ">3 year-old birds", "", "", "", "",
                     r2_A1_Mean,r2_A2_Mean,r2_A3_Mean, "", "", "",AgeMean),ncol=7, byrow=TRUE)
colnames(R2_table) <- c("Delta (D)", "Texel-Balgzand (B)", "Texel-Vlieland (V)", "Terschelling-Ameland (T)", "Schiermonnikoog (S)", 
                        "Rottum (R", "Average")
rownames(R2_table) <- c("Explained variation within the state (with an average year and age)",
                        "Year",
                        "Explained variation within the year (with an average state and age)",
                        "Age class",
                        "Explained variation within the age class (with an average state and year)")
R2_table

#Phi$StateText<-factor(Phi$State, levels=c( "1", "2", "3", "4", "5", "6", "7", "8", "9"),
#                      labels=c("Delta (D)", "Inland South (P)", "Inland North (N)", "Texel-Balgzand (B)", 
#                               "Texel-Vlieland (V)","Terschelling-Ameland (T)", "Schiermonnikoog (S)","Rottum (R)",  
#                               "Abroad (X)"))

knitr::kable(R2_table,digits=2, format="pandoc")
kable_out_R2_table <- knitr::kable(R2_table, "html", digits=4) %>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
readr::write_file(kable_out_R2_table, "P:/CHIRP/Carry-over/Analysis/Figures/Supplements/R2/R2_table.doc")

#####################################################################################################
######### calculate bayesian r2 EXAMPLE #############################################################
####################################################################################################
bayes_R2 <- function(fit) {
  y_pred <- rstanarm::posterior_linpred(fit)
  var_fit <- apply(y_pred, 1, var)
  var_res <- as.matrix(fit, pars = c("sigma"))^2
  var_fit / (var_fit + var_re)
}
## Example from Figure 1 of the paper
x <- 1:5 - 3
y <- c(1.7, 2.6, 2.5, 4.4, 3.8) - 3
xy <- data.frame(x,y)
## Bayes fit with strong priors
library("rstanarm")
fit_bayes <- stan_glm(y ~ x, data = xy,
                      prior_intercept = normal(0, 0.2, autoscale = FALSE),
                      prior = normal(1, 0.2, autoscale = FALSE),
                      prior_aux = NULL)
## Compute Bayesian R2
rsq_bayes <- bayes_R2(fit_bayes)
hist(rsq_bayes)
print(c(median(rsq_bayes), mean(rsq_bayes), sd(rsq_bayes)))


y_pred <- rstanarm::posterior_linpred(fit_bayes) #predicted value
var_fit <- apply(y_pred, 1, var)
var_res <- as.matrix(fit_bayes, pars = c("sigma"))^2
var_fit / (var_fit + var_res)
