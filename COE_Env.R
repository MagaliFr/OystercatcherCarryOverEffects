detach("package:ggplot2", unload = TRUE)
detach("package:patchwork", unload = TRUE)

packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.3.5.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

packageurl <- "http://cran.r-project.org/src/contrib/Archive/patchwork/patchwork_1.1.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

packageurl <- "http://cran.r-project.org/src/contrib/Archive/cowplot/cowplot_1.1.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggpubr/ggpubr_0.4.0.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

library(grid)
library(png)
library(ggimage)
library(ggpubr)
library(patchwork)
library(ggplot2)
library(cowplot)

################################# ENV ~ COND ###########################################################
############################## env dataset ################################################
memory.limit(size = 350000)    
env<- read.table("YOUR_PATH/EnvironmentalVariables.csv", header=T,dec=".", sep=",", fill=T)#food data
str(env)
summary(env)

plot(env$NrSamplesFood_C~env$Kg_C_avail_Ind) # onder 20 eruit?, of onder 5 om die met 400 eruit te halen
plot(env$NrSamplesFood_M~env$Kg_M_avail_Ind)


env$ObsCatchNr<-factor(env$ObsCatchNr)
env$NrSamplesFood_C<-as.numeric(env$NrSamplesFood_C)
env$NrSamplesFood_M<-as.numeric(env$NrSamplesFood_M)
env$sumWS_2M<-as.numeric(env$sumWS_2M)
str(env)

########### cond/eta ###########################
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

################################ join/link food to cond dataset ########################################
library(plyr)
str(Cond)
str(env)
df<-join(Cond, env, by = "ObsCatchNr", type = "left", match = "all")

################### take competition, food (including exposure time) seperately ########
str(df)

#response variable, = eta in cr-sem
r<-df[,2]

## food including exposure time
df$AFDMgrm2_C_CondLoss_exp<-df$AFDMgrm2_C_CondLoss*df$Avg_exp_NovFeb
df$AFDMgrm2_M_CondLoss_exp<-df$AFDMgrm2_M_CondLoss*df$Avg_exp_NovFeb
str(df)

# Other covariates are continuous : condition index
food2 <- subset(df,
                select=c(PropGrassland))
food1<-df[,34:35]
comp <- df[,18]
weather<-df[,24:28] # 1 month
contcov<-cbind(food1,food2,comp,weather)
head(contcov)
# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov = apply(contcov,2,mean) # mean of each covariate
mean.mat = matrix(rep(mean.cov,nrow(contcov)),byrow=T,ncol=ncol(contcov))
sd.cov = apply(contcov,2,sd)
sd.mat = matrix(rep(sd.cov,nrow(contcov)),byrow=T,ncol=ncol(contcov))

stcov = (contcov-mean.mat)/sd.mat # stcov = standardized covariates values of the continuous individual covariates
dim(stcov) # 8 continuous covariates
names(stcov)
#[1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"           "comp"                   
#[5] "avgTemp_1M"              "avgWC_1M"                "sumRH_month_1M"          "PrepAnom_month_1M"      
#[9] "sumWS_1M"  
cor(stcov)

n <- nrow(contcov)

df$ObsNum<-as.numeric(df$ObsCatchNr)
str(df)
Site<-df[,36]
nsites <- max(Site)

mydata<-cbind(r,stcov, Site)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
    
    # composite variable food
    #mu6[i] <- gamma4 * cov[i,1] + gamma5 * cov[i,2] + 1 * cov[i,3] + gamma42 * pow(cov[i,1],2) + gamma52 * pow(cov[i,2],2) + gamma62 * pow(cov[i,3],2)
    #food[i] ~ dnorm(mu6[i], sd=0)
    
    # composite variable weather
    #mu7[i] <- 1 * temp[i] + gamma72 * pow(temp[i],2) +  gamma8 * prec[i] + gamma82 * pow(prec[i],2)
  }
  
  for (i in 1:n){
    #predicted.r[i] <- beta1 * temp[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta12 * pow(temp[i],2) + beta32 * pow(cov[i,1],2) + beta42 * pow(cov[i,2],2) + beta52 * pow(cov[i,3],2) + eps[Site[i]]
    predicted.r[i] <- beta0 + beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * mu6[i] + beta12 * pow(temp[i],2) + beta22 * pow(prec[i],2)
    #predicted.r[i] <- beta1 * mu6[i] + beta2 * mu7[i] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + eps[Site[i]]
    
    
    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0)
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[1] <- 0 # site 1
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta22  ~ dnorm(0,1)
  #beta32  ~ dnorm(0,1)
  #beta42  ~ dnorm(0,1)
  #beta52  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  #sig8x ~ dunif(0,10)
  #sig6x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
  
})

#constants
constants <- list(n=n, Site=Site, nsites=nsites, cov=stcov)

#data
mydatax <- list(r=r)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  #sig8x=runif(1,0,10), 
  #sig6x=runif(1,0,10), 
  sig7x=runif(1,0,10), 
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  beta6=rnorm(1,0,1),
  #beta12=rnorm(1,0,1), beta22=rnorm(1,0,1),
  #beta32=rnorm(1,0,1),beta42=rnorm(1,0,1),beta52=rnorm(1,0,1),
  temp=rnorm(n,0,1), prec=rnorm(n,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)


params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          #"sig8x", "sig6x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax,
  constants=constants,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 15000, #10000
                       nburnin = 1000, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=TRUE, summary=T,
                       setSeed = mySeed)

library(MCMCvis)
MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6'),
         labels = c("Temperature", #expression('Temperature'^2), 
                    "Precipitation", #expression('Precipitation'^2), 
                    "Cockle abundance", #expression('Cockle availability'^2), 
                    "Mussel abundance", #expression('Mussel availability'^2), 
                    "Grassland proportion", #expression('Grassland proportion'^2)
                    "Conspecific density"
         ),
         main="Effect of environmental variables (standardized) \non condition")

summary(samplesList[["samples"]][,c("beta0")]) #intercept 1.4
summary(samplesList[["samples"]][,c("beta1")]) #0.13[-0.15,0.42], sd 0.14, temp
summary(samplesList[["samples"]][,c("beta2")]) #0.04[-0.26,0.32], sd 0.14, precipitation
summary(samplesList[["samples"]][,c("beta3")]) #0.09[-0.11,0.28], sd 0.10, cockle
summary(samplesList[["samples"]][,c("beta4")]) #0.02[-0.17,0.21], sd 0.10, mussel
summary(samplesList[["samples"]][,c("beta5")]) #0.24[0.02,0.45], sd 0.11, grasland
summary(samplesList[["samples"]][,c("beta6")]) #0.14[-0.06,0.35], sd 0.10, competition

samplesSummary <- round(samplesList$summary$all.chains[c("gamma1", "gamma2", "gamma3",
                                                         "beta0", 
                                                         "beta1", "beta2", 
                                                         "beta3", 
                                                         "beta4", "beta5",
                                                         "beta6",
                                                         "sig4x", "sig5x",
                                                         "sig7x","sig9x",
                                                         "sig1x","sig2x","sig3x","mu.eps","sig.eps",
                                                         "sig.r", "sig.temp" , "sig.prec"
),1:5],2) #can also extract predicted.r for plotting

samplesSummaryEps<-round(samplesList$summary$all.chains[grep("eps", rownames(samplesList$summary$all.chains)), ],2)
samplesSummaryEps<-samplesSummaryEps[1:36,]
samplesSummaryTemp<-round(samplesList$summary$all.chains[grep("temp", rownames(samplesList$summary$all.chains)), ],2)
samplesSummaryTemp<-samplesSummaryTemp[2:1575,]
samplesSummaryPrec<-round(samplesList$summary$all.chains[grep("prec", rownames(samplesList$summary$all.chains)), ],2)
samplesSummaryPrec<-samplesSummaryPrec[1:1574,]
samplesSummaryPredictedCond<-round(samplesList$summary$all.chains[grep("predicted.r", rownames(samplesList$summary$all.chains)), ],2)

cor(samplesSummaryTemp[,1],samplesSummaryPrec[,1]) #0.72
cor(stcov[,1],samplesSummaryPrec[,1]) #-0.08
cor(stcov[,1],samplesSummaryTemp[,1]) #0.04
cor(stcov[,2],samplesSummaryPrec[,1]) #0.26
cor(stcov[,2],samplesSummaryTemp[,1]) #0.29
cor(stcov[,3],samplesSummaryTemp[,1]) #-0.28
cor(stcov[,3],samplesSummaryPrec[,1]) #-0.31

Mean Median St.Dev. 95%CI_low 95%CI_upp
gamma1    1.00   1.00    0.00      0.99      1.01
gamma2   -0.88  -0.88    0.01     -0.90     -0.85
gamma3    1.00   1.00    0.01      0.99      1.02
beta0     1.40   1.41    0.27      0.89      1.91
beta1     0.07   0.07    0.13     -0.18      0.33
beta2     0.10   0.10    0.13     -0.15      0.38
beta3     0.11   0.11    0.11     -0.10      0.32
beta4     0.01   0.02    0.10     -0.18      0.20
beta5     0.27   0.27    0.11      0.06      0.49
beta6     0.13   0.13    0.10     -0.05      0.34
sig4x     1.00   1.00    0.02      0.97      1.04
sig5x     0.12   0.12    0.00      0.11      0.12
sig7x     0.20   0.20    0.00      0.19      0.21
sig9x     0.49   0.49    0.01      0.47      0.50
sig1x     1.00   1.00    0.02      0.97      1.04
sig2x     1.00   1.00    0.02      0.97      1.04
sig3x     1.00   1.00    0.02      0.97      1.04
mu.eps   -1.32  -1.33    0.29     -1.87     -0.77
sig.eps   0.49   0.48    0.09      0.35      0.69
sig.r     1.18   1.18    0.02      1.14      1.23
sig.temp  0.99   0.99    0.02      0.96      1.03
sig.prec  0.98   0.98    0.02      0.95      1.02

#############################################################
#### interaction bill shape (continuous bill tip height and age class) 
Cov<-read.csv(file="P:/CHIRP/Carry-over/Analysis/Data/DataAnalysis/StateMatrix2000_2019Cond_3class_9Years_ID_ForEnv.csv")
str(Cov)
hist(Cov$BH)
table(Cov$BillShape)
BS<-Cov[,6]
BS<-factor(BS)
BH<-Cov[,7]

Cov1<-read.csv(file="N:/Dep.AnE/AnE-share/_Magali/Nimble_M1_P12/StateMatrix2000_2019Cond_3class9Years.csv")
str(Cov1)
Age<-Cov1[,2]

Cov<-cbind(ID,BS, Age, BH)
head(Cov)

df$WaderID_ObsOcc<-paste(df$WaderBirdID,df$ObsCatchNr,sep="_")
Cov$WaderID_ObsOcc<-paste(Cov$WaderBirdID,Cov$ObsCatchNr,sep="_")

library(plyr)
df<-join(Cov, df, by = "WaderID_ObsOcc", type = "left", match = "all")
str(df)
summary(df$BS)
#B   H  HB   P  PB  PH 
#361  67  23 373 713  37 

df$BillShape<-ifelse(df$BS=="B"|df$BS=="H"|df$BS=="HB","B",
                      ifelse(df$BS=="P","P","I"
                               ))
df$BillShape<-factor(df$BillShape)
summary(df$BillShape1)
df$BillShape<-df$BillShape
summary(df$BillShape)
#B   I   P 
#451 750 373

################### take competition, food (including exposure time) seperately ########
str(df)
#response variable, = eta in cr-sem
r<-df[,13]

## food including exposure time
df$AFDMgrm2_C_CondLoss_exp<-df$AFDMgrm2_C_CondLoss*df$Avg_exp_NovFeb
df$AFDMgrm2_M_CondLoss_exp<-df$AFDMgrm2_M_CondLoss*df$Avg_exp_NovFeb
str(df)

# Other covariates are continuous : condition index
food2 <- subset(df,
                select=c(PropGrassland))
food1<-df[,46:47]
comp <- subset(df,
               select=c(Density_km2))
weather<-df[,35:39] # 1 month
head(weather)
BH<-subset(df,
           select=c(BH))
Age<-subset(df,
           select=c(Age))
contcov<-cbind(food1,food2,comp,weather, BH, Age)
head(contcov)
# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov = apply(contcov,2,mean) # mean of each covariate
mean.mat = matrix(rep(mean.cov,nrow(contcov)),byrow=T,ncol=ncol(contcov))
sd.cov = apply(contcov,2,sd)
sd.mat = matrix(rep(sd.cov,nrow(contcov)),byrow=T,ncol=ncol(contcov))

stcov = (contcov-mean.mat)/sd.mat # stcov = standardized covariates values of the continuous individual covariates
dim(stcov) # 8 continuous covariates
names(stcov)
#[1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"           "comp"                   
#[5] "avgTemp_1M"              "avgWC_1M"                "sumRH_month_1M"          "PrepAnom_month_1M"      
#[9] "sumWS_1M"  "BH"                      "Age"  
cor(stcov)

n <- nrow(contcov)

df$ObsNum<-as.numeric(df$ObsCatchNr)
str(df)
Site<-df[,48]
nsites <- max(Site)

#BS<-as.numeric(df$BillShape)

mydata<-cbind(r,stcov, Site)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
  }
  
  for (i in 1:n){
    predicted.r[i] <- beta0 + 
      beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + 
      beta7 * cov[i,10] +
      beta8 * cov[i,10] * temp[i] + beta9 * cov[i,10] * prec[i] + beta10 * cov[i,10] * cov[i,1] + beta11 * cov[i,10] * cov[i,2] + beta12 * cov[i,10] * cov[i,3] + beta13 * cov[i,10] * cov[i,4] + 
      beta14 * cov[i,11] * temp[i] + beta15 * cov[i,11] * prec[i] + beta16 * cov[i,11] * cov[i,1] + beta17 * cov[i,11] * cov[i,2] + beta18 * cov[i,11] * cov[i,3] + beta19 * cov[i,11] * cov[i,4] + 
      beta20 * pow(cov[i,11],2) * temp[i] + beta21 * pow(cov[i,11],2) * prec[i] + beta22 * pow(cov[i,11],2) * cov[i,1] + beta23 * pow(cov[i,11],2) * cov[i,2] + beta24 * pow(cov[i,11],2) * cov[i,3] + beta25 *pow(cov[i,11],2) * cov[i,4] + 
      eps[Site[i]]

    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0), nsites-1
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[14] <- 0 # site 14, highest nr obs
  
  #for BS
  #beta7[2] ~ dnorm(0,1)
  #beta7[3] ~ dnorm(0,1)
  #beta8[2] ~ dnorm(0,1)
  #beta8[3] ~ dnorm(0,1)
  #beta9[2] ~ dnorm(0,1)
  #beta9[3] ~ dnorm(0,1)
  #beta10[2] ~ dnorm(0,1)
  #beta10[3] ~ dnorm(0,1)
  #beta11[2] ~ dnorm(0,1)
  #beta11[3] ~ dnorm(0,1)
  #beta12[2] ~ dnorm(0,1)
  #beta12[3] ~ dnorm(0,1)
  #beta13[2] ~ dnorm(0,1)
  #beta13[3] ~ dnorm(0,1)
  
  # fix the effect of the first level (reference groups: B) to 0 because we use intercepts beta0
  #beta7[1] <- 0 # B billshape
  #beta8[1] <- 0 # B billshape
  #beta9[1] <- 0 # B billshape
  #beta10[1] <- 0 # B billshape
  #beta11[1] <- 0 # B billshape
  #beta12[1] <- 0 # B billshape
  #beta13[1] <- 0 # B billshape
  
  #beta71<-mean(c(eps[1], eps[2], eps[3], eps[4], eps[5], eps[6], eps[7],
  #               eps[8],eps[9],eps[10],eps[11],eps[12],eps[13],eps[14],
  #               eps[15],eps[16],eps[17],eps[18],eps[19],eps[20],eps[21],eps[22],
  #               eps[23],eps[24],eps[25],eps[26],eps[27],eps[28],eps[29],eps[30],
  #               eps[31],eps[32],eps[33],eps[34],eps[35])) + beta0
  
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    cov[i,10] ~ dnorm(0, sd=sig10x)
    cov[i,11] ~ dnorm(0, sd=sig11x)
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  beta7  ~ dnorm(0,1)
  beta8  ~ dnorm(0,1)
  beta9  ~ dnorm(0,1)
  beta10  ~ dnorm(0,1)
  beta11  ~ dnorm(0,1)
  beta12  ~ dnorm(0,1)
  beta13  ~ dnorm(0,1)
  beta14  ~ dnorm(0,1)
  beta15  ~ dnorm(0,1)
  beta16  ~ dnorm(0,1)
  beta17  ~ dnorm(0,1)
  beta18  ~ dnorm(0,1)
  beta19  ~ dnorm(0,1)
  beta20  ~ dnorm(0,1)
  beta21  ~ dnorm(0,1)
  beta22  ~ dnorm(0,1)
  beta23  ~ dnorm(0,1)
  beta24  ~ dnorm(0,1)
  beta25  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  sig10x ~ dunif(0,10)
  sig11x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
})

#constants
constants <- list(n=n, Site=Site, nsites=nsites, cov=stcov)

#data
mydatax <- list(r=r)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  sig10x=runif(1,0,10), 
  sig11x=runif(1,0,10), 
  sig7x=runif(1,0,10), 
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  beta6=rnorm(1,0,1),
  beta7=rnorm(1,0,1),
  beta8=rnorm(1,0,1),
  beta9=rnorm(1,0,1),
  beta10=rnorm(1,0,1),
  beta11=rnorm(1,0,1),
  beta12=rnorm(1,0,1),
  beta13=rnorm(1,0,1),
  beta14=rnorm(1,0,1), beta15=rnorm(1,0,1),
  beta16=rnorm(1,0,1),beta17=rnorm(1,0,1),beta18=rnorm(1,0,1),
  beta19=rnorm(1,0,1),beta20=rnorm(1,0,1),beta21=rnorm(1,0,1),beta22=rnorm(1,0,1),
  beta23=rnorm(1,0,1),beta24=rnorm(1,0,1),beta25=rnorm(1,0,1),
  temp=rnorm(n,0,1), prec=rnorm(n,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)

params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          "beta7","beta8","beta9","beta10",
          "beta11","beta12","beta13","beta14","beta15","beta16","beta17","beta18",
          "beta19","beta20","beta21","beta22","beta23","beta24","beta25",
          #"beta71",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          "sig10x", "sig11x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax,
  constants=constants,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 1, #15000
                       nburnin = 1, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=TRUE, summary=T,
                       setSeed = mySeed)

summary(samplesList[["samples"]][,c("beta0")]) #0.11[-0.14,0.35] reference site, blunt bill
summary(samplesList[["samples"]][,c("beta1")]) #0.11[-0.14,0.35]
summary(samplesList[["samples"]][,c("beta2")]) #0.08[-0.16,0.33]
summary(samplesList[["samples"]][,c("beta3")]) #-0.24[-0.06,0.10]
summary(samplesList[["samples"]][,c("beta4")]) #-0.07[-0.23,0.12]
summary(samplesList[["samples"]][,c("beta5")]) #1.17[-0.03,0.36]
summary(samplesList[["samples"]][,c("beta6")]) #1.17[-0.03,0.36]
summary(samplesList[["samples"]][,c("beta7[1]")]) #1.17[-0.03,0.36]
summary(samplesList[["samples"]][,c("beta7[2]")]) #1.17[-0.03,0.36]
summary(samplesList[["samples"]][,c("beta7[3]")]) #1.17[-0.03,0.36]
summary(samplesList[["samples"]][,c("beta8[2]")]) #1.17[-0.03,0.36] #temp ns
summary(samplesList[["samples"]][,c("beta8[3]")]) #1.17[-0.03,0.36] #temp sig
summary(samplesList[["samples"]][,c("beta9[2]")]) #1.17[-0.03,0.36] #prec ns
summary(samplesList[["samples"]][,c("beta9[3]")]) #1.17[-0.03,0.36] #prec ns
summary(samplesList[["samples"]][,c("beta10[3]")]) #1.17[-0.03,0.36] #cockle
summary(samplesList[["samples"]][,c("beta11[3]")]) #1.17[-0.03,0.36] #mussel
summary(samplesList[["samples"]][,c("beta12[3]")]) #1.17[-0.03,0.36] #grass
summary(samplesList[["samples"]][,c("beta13[3]")]) #1.17[-0.03,0.36] #comp
summary(samplesList[["samples"]][,c("beta71")]) #1.17[-0.03,0.36] #comp

library(MCMCvis)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalEffectContBH_Age.png", width = 5000, height = 10000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6',
                                        'beta7', 'beta8', 'beta9',
                                        'beta10',
                                        'beta11', 'beta12',
                                        'beta13','beta14','beta15','beta16','beta17','beta18','beta19',
                                        'beta20','beta21','beta22','beta23','beta24', 'beta25' 
                                        ),
         #labels = c("Intercept",
        #   "Temperature", #expression('Temperature'^2), 
          #          "Cockle abundance", #expression('Cockle availability'^2), 
           #         "Mussel abundance", #expression('Mussel availability'^2), 
            #        "Grassland proportion", #expression('Grassland proportion'^2)
             #       "Conspecific density", "Intermediate feeder", "Worm specialist", "Temp int",
          # "Temp worm", "Prec int", "Prec worm", "Cockle int", "Cockle worm", "Mussel int",
          # "Mussel worm", "Grassland int", "Grassland worm", "Dens int", "Dens worm"),
         main="Effect of environmental variables (standardized) \non condition")
dev.off()

samplesSummary <- round(samplesList$summary$all.chains[c("gamma1", "gamma2", "gamma3",
                                                         "beta0", 
                                                         "beta1", "beta2", 
                                                         "beta3", 
                                                         "beta4", "beta5",
                                                         "beta6",
                                                         "beta71",
                                                         "beta7[2]", "beta7[3]",
                                                         "beta8[2]", "beta8[3]",
                                                         "beta9[2]", "beta9[3]",
                                                         "beta10[2]", "beta10[3]",
                                                         "beta11[2]", "beta11[3]",
                                                         "beta12[2]", "beta12[3]",
                                                         "beta13[2]", "beta13[3]",
                                                         "sig4x", "sig5x",
                                                         "sig7x","sig9x",
                                                         "sig1x","sig2x","sig3x","mu.eps","sig.eps",
                                                         "sig.r", "sig.temp" , "sig.prec"
),1:5],2) #can also extract predicted.r for plotting

### plot parameter estimates
row.names(samplesSummary) = c("gamma1", "gamma2", "gamma3",
                   "beta0", 
                   "beta1", "beta2", 
                   "beta3", 
                   "beta4", "beta5",
                   "beta6","beta71",
                   "beta7[2]", "beta7[3]",
                   "beta8[2]", "beta8[3]",
                   "beta9[2]", "beta9[3]",
                   "beta10[2]", "beta10[3]",
                   "beta11[2]", "beta11[3]",
                   "beta12[2]", "beta12[3]",
                   "beta13[2]", "beta13[3]",
                   "sig4x", "sig5x",
                   "sig7x","sig9x",
                   "sig1x","sig2x","sig3x","mu.eps","sig.eps",
                   "sig.r", "sig.temp" , "sig.prec")

d_plot<-data.frame(X = row.names(samplesSummary),samplesSummary)
d_plot<-subset(d_plot,grepl("beta", d_plot$X))
d_plot<-d_plot[2:22,] #mydata[1:5,]

### plot difference between feeding specialists and condition
d_plot1<-d_plot[7:9,] #mydata[1:5,]
# order data
d_plot1$X <- factor(d_plot1$X, levels = c("beta71", "beta7[2]", "beta7[3]"),
                    labels=c("Shellfish specialist", "Intermediate feeder",
                             "Worm specialist"))
d_p1<-ggplot(data=d_plot1) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "grey", size=1.5)+
  geom_point(aes(y=reorder(X, desc(X)),x=Median))+ #reorder(position, desc(position)
  geom_errorbarh(data=d_plot1,mapping=aes(xmin=X95.CI_low, xmax=X95.CI_upp, y=X, x=Median),size=.8, height=.2)+
  labs(x="Posterior median (+-95% CI)", y="")+
  ggtitle("Effect of feeding specialization on \nconditon (standardized)")
d_p1

d_plot2a<-d_plot[1:6,] #mydata[1:5,]
d_plot2b<-d_plot[10:21,] #mydata[1:5,]
d_plot2<-rbind(d_plot2a,d_plot2b)

# order data
d_plot2$X <- factor(d_plot2$X, levels = c(
                                        "beta1", "beta8[2]", "beta8[3]",
                                        "beta2", "beta9[2]", "beta9[3]",
                                        "beta3", "beta10[2]", "beta10[3]",
                                        "beta4", "beta11[2]", "beta11[3]",
                                        "beta5", "beta12[2]", "beta12[3]",
                                        "beta6", "beta13[2]", "beta13[3]"),
                    labels=c("Temperature Shellfish spec","Temperature Intermediate feeder","Temperature Worm spec", 
                             "Precipitation Shellfish spec","Precipitation Intermediate feeder","Precipitation Worm spec",
                             "Cockle avail Shellfish spec","Cockle avail Intermediate feeder","Cockle avail Worm spec",
                             "Mussel avail Shellfish spec","Mussel avail Intermediate feeder","Mussel avail Worm spec",
                             "Grassland Shellfish spec","Grassland Intermediate feeder","Grassland Worm spec",
                             "Density Shellfish spec","Density Intermediate feeder","Density Worm spec"
                             ))
d_plot2$Specialization<-c("Shellfish", "Shellfish", "Shellfish",
                          "Shellfish", "Shellfish", "Shellfish",
                          "Intermediate", "Worm", 
                          "Intermediate", "Worm",
                          "Intermediate", "Worm",
                          "Intermediate", "Worm",
                          "Intermediate", "Worm",
                          "Intermediate", "Worm"
                          )
d_plot2$Specialization<-factor(d_plot2$Specialization,
                               levels=c("Worm","Intermediate","Shellfish"),
                               labels =c("Worm","Intermediate","Shellfish"))
str(d_plot2$Specialization)
levels(d_plot2$Specialization)

d_plot2$EnvVariable<-c("Temperature", "Precipitation", "Cockle availability",
                          "Mussel availability", "Grassland proportion", "Conspecific density",
                       "Temperature", "Temperature", 
                          "Precipitation", "Precipitation",
                          "Cockle availability", "Cockle availability",
                          "Mussel availability", "Mussel availability",
                          "Grassland proportion", "Grassland proportion",
                          "Conspecific density", "Conspecific density")

str(d_plot2$EnvVariable)
d_plot2$EnvVariable<-factor(d_plot2$EnvVariable,
                            levels=c("Temperature", "Precipitation", "Cockle availability",
                                     "Mussel availability", "Grassland proportion","Conspecific density"),
                            labels=c("Temperature", "Precipitation", "Cockle availability",
                                     "Mussel availability", "Grassland proportion","Conspecific density"))

library(ggplot2)
d_p2<-ggplot(data=d_plot2) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "grey", size=1.5)+
  geom_point(aes(y=reorder(Specialization, desc(Specialization)),x=Median))+ #reorder(position, desc(position)
  geom_errorbarh(data=d_plot2,aes(xmin=X95.CI_low, xmax=X95.CI_upp, y=Specialization, x=Median), size=.8, height=.2)+
  labs(x="Posterior median (+-95% CI)", y="")+
  ggtitle("Effect of environmental variables (standardized) on condition")+
  facet_wrap(~EnvVariable)
d_p2

###################################################################################################
###################################################################################################
#############################################################
#### interaction bill shape (factor) 
Cov<-read.csv(file="F:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Data/DataAnalysis/StateMatrix2000_2019Cond_3class_9Years_ID_ForEnv.csv")
str(Cov)
hist(Cov$BH)
table(Cov$BillShape)
Cov<-Cov[,6]

Cov<-cbind(ID,Cov)
Cov$BillShape<-Cov$Cov

df$WaderID_ObsOcc<-paste(df$WaderBirdID,df$ObsCatchNr,sep="_")
Cov$WaderID_ObsOcc<-paste(Cov$WaderBirdID,Cov$ObsCatchNr,sep="_")

library(plyr)
df<-join(Cov, df, by = "WaderID_ObsOcc", type = "left", match = "all")
df$BillShape<-factor(df$BillShape)
summary(df$BillShape)
#B   H  HB   P  PB  PH 
#361  67  23 373 713  37 

df$BillShape1<-ifelse(df$BillShape=="B"|df$BillShape=="H"|df$BillShape=="HB","B",
                      ifelse(df$BillShape=="P","P","I"
                      ))
df$BillShape1<-factor(df$BillShape1)
summary(df$BillShape1)
df$BillShape<-df$BillShape1
summary(df$BillShape)
#B   I   P 
#451 750 373

################### take competition, food (including exposure time) seperately ########
str(df)
#response variable, = eta in cr-sem
r<-df[,12]

## food including exposure time
df$AFDMgrm2_C_CondLoss_exp<-df$AFDMgrm2_C_CondLoss*df$Avg_exp_NovFeb
df$AFDMgrm2_M_CondLoss_exp<-df$AFDMgrm2_M_CondLoss*df$Avg_exp_NovFeb
str(df)

# Other covariates are continuous : condition index
food2 <- subset(df,
                select=c(PropGrassland))
head(food2)
head(df)
food1<-df[,44:45]
head(food1)
comp <- subset(df,
               select=c(Density_km2))
weather<-df[,34:38] # 1 month
head(weather)
contcov<-cbind(food1,food2,comp,weather)
head(contcov)
# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov = apply(contcov,2,mean) # mean of each covariate
mean.mat = matrix(rep(mean.cov,nrow(contcov)),byrow=T,ncol=ncol(contcov))
sd.cov = apply(contcov,2,sd)
sd.mat = matrix(rep(sd.cov,nrow(contcov)),byrow=T,ncol=ncol(contcov))

stcov = (contcov-mean.mat)/sd.mat # stcov = standardized covariates values of the continuous individual covariates
dim(stcov) # 8 continuous covariates
names(stcov)
#[1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"           "comp"                   
#[5] "avgTemp_1M"              "avgWC_1M"                "sumRH_month_1M"          "PrepAnom_month_1M"      
#[9] "sumWS_1M"  
cor(stcov)

n <- nrow(contcov)

df$ObsNum<-as.numeric(df$ObsCatchNr)
str(df)
Site<-df[,46] #ObsNum
head(Site)
nsites <- max(Site)
nsites

BS<-as.numeric(df$BillShape)

mydata<-cbind(r,stcov, Site, BS)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
  }
  
  for (i in 1:n){
    predicted.r[i] <- beta0 + 
      beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + 
      beta7[BS[i]] +
      beta8[BS[i]] * temp[i] + beta9[BS[i]] * prec[i] + beta10[BS[i]] * cov[i,1] + beta11[BS[i]] * cov[i,2] + beta12[BS[i]] * cov[i,3] + beta13[BS[i]] * cov[i,4] + 
      eps[Site[i]]
    
    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0), nsites-1
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[14] <- 0 # site 14, highest nr obs
  
  #for BS
  beta7[2] ~ dnorm(0,1)
  beta7[3] ~ dnorm(0,1)
  beta8[2] ~ dnorm(0,1)
  beta8[3] ~ dnorm(0,1)
  beta9[2] ~ dnorm(0,1)
  beta9[3] ~ dnorm(0,1)
  beta10[2] ~ dnorm(0,1)
  beta10[3] ~ dnorm(0,1)
  beta11[2] ~ dnorm(0,1)
  beta11[3] ~ dnorm(0,1)
  beta12[2] ~ dnorm(0,1)
  beta12[3] ~ dnorm(0,1)
  beta13[2] ~ dnorm(0,1)
  beta13[3] ~ dnorm(0,1)
  
  # fix the effect of the first level (reference groups: B) to 0 because we use intercepts beta0
  beta7[1] <- 0 # B billshape
  beta8[1] <- 0 # B billshape
  beta9[1] <- 0 # B billshape
  beta10[1] <- 0 # B billshape
  beta11[1] <- 0 # B billshape
  beta12[1] <- 0 # B billshape
  beta13[1] <- 0 # B billshape
  
  beta71<-mean(c(eps[1], eps[2], eps[3], eps[4], eps[5], eps[6], eps[7],
                 eps[8],eps[9],eps[10],eps[11],eps[12],eps[13],eps[14],
                 eps[15],eps[16],eps[17],eps[18],eps[19],eps[20],eps[21],eps[22],
                 eps[23],eps[24],eps[25],eps[26],eps[27],eps[28],eps[29],eps[30],
                 eps[31],eps[32],eps[33],eps[34],eps[35])) + beta0
  
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  #beta7  ~ dnorm(0,1)
  #beta8  ~ dnorm(0,1)
  #beta9  ~ dnorm(0,1)
  #beta10  ~ dnorm(0,1)
  #beta11  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta13  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta22  ~ dnorm(0,1)
  #beta32  ~ dnorm(0,1)
  #beta42  ~ dnorm(0,1)
  #beta52  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  #sig8x ~ dunif(0,10)
  #sig6x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
  
})

#constants
constants <- list(n=n, Site=Site, nsites=nsites, cov=stcov, BS=BS)

#data
mydatax <- list(r=r)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  #sig8x=runif(1,0,10), 
  #sig6x=runif(1,0,10), 
  sig7x=runif(1,0,10), 
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  beta6=rnorm(1,0,1),
  beta7=rnorm(3,0,1),
  beta8=rnorm(3,0,1),
  beta9=rnorm(3,0,1),
  beta10=rnorm(3,0,1),
  beta11=rnorm(3,0,1),
  beta12=rnorm(3,0,1),
  beta13=rnorm(3,0,1),
  #beta12=rnorm(1,0,1), beta22=rnorm(1,0,1),
  #beta32=rnorm(1,0,1),beta42=rnorm(1,0,1),beta52=rnorm(1,0,1),
  temp=rnorm(n,0,1), prec=rnorm(n,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)

params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          "beta7","beta8","beta9","beta10",
          "beta11","beta12","beta13",
          "beta71",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          #"sig8x", "sig6x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax,
  constants=constants,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf)#, enableWAIC = F)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 15000, #10000
                       nburnin = 1000, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, summary=T,
                       setSeed = mySeed) #WAIC=TRUE

#beta1-6= effect for reference (blunt bill)=intercept
#beta8-13[3]= slope=effect for pointed bill
# effect difference=intercept+slope

summary(samplesList[["samples"]][,c("beta0")]) #0.11[-0.14,0.35] reference site, blunt bill
summary(samplesList[["samples"]][,c("beta1")]) #0.11[-0.14,0.35]
summary(samplesList[["samples"]][,c("beta2")]) #0.08[-0.16,0.33]
summary(samplesList[["samples"]][,c("beta3")]) #-0.24[-0.06,0.10]
summary(samplesList[["samples"]][,c("beta4")]) #-0.07[-0.23,0.12]
summary(samplesList[["samples"]][,c("beta5")]) #1.17[-0.03,0.36]
summary(samplesList[["samples"]][,c("beta6")]) #1.17[-0.03,0.36]
summary(samplesList[["samples"]][,c("beta71")]) #0.12[-0.02,0.27] shellfish specialist
summary(samplesList[["samples"]][,c("beta7[2]")]) #0.01[-0.14,0.16] intermediate
summary(samplesList[["samples"]][,c("beta7[3]")]) #-0.36[-0.54,-0.17] worm specialist
summary(samplesList[["samples"]][,c("beta8[2]")]) #1.17[-0.03,0.36] #temp ns
summary(samplesList[["samples"]][,c("beta8[3]")]) #1.17[-0.03,0.36] #temp sig
summary(samplesList[["samples"]][,c("beta9[2]")]) #1.17[-0.03,0.36] #prec ns
summary(samplesList[["samples"]][,c("beta9[3]")]) #1.17[-0.03,0.36] #prec ns
summary(samplesList[["samples"]][,c("beta10[3]")]) #1.17[-0.03,0.36] #cockle
summary(samplesList[["samples"]][,c("beta11[3]")]) #1.17[-0.03,0.36] #mussel
summary(samplesList[["samples"]][,c("beta12[3]")]) #1.17[-0.03,0.36] #grass
summary(samplesList[["samples"]][,c("beta13[3]")]) #1.17[-0.03,0.36] #comp
summary(samplesList[["samples"]][,c("beta71")]) #1.17[-0.03,0.36] #comp

library(MCMCvis)
MCMCplot(samplesList$samples, params =c('beta0', 'beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6',
                                        'beta71', 'beta7', 'beta8',
                                        'beta9', 'beta10',
                                        'beta11', 'beta12',
                                        'beta13'),
         #labels = c("Intercept",
         #   "Temperature", #expression('Temperature'^2), 
         #           "Precipitation", #expression('Precipitation'^2), 
         #          "Cockle abundance", #expression('Cockle availability'^2), 
         #         "Mussel abundance", #expression('Mussel availability'^2), 
         #        "Grassland proportion", #expression('Grassland proportion'^2)
         #       "Conspecific density", "Intermediate feeder", "Worm specialist", "Temp int",
         # "Temp worm", "Prec int", "Prec worm", "Cockle int", "Cockle worm", "Mussel int",
         # "Mussel worm", "Grassland int", "Grassland worm", "Dens int", "Dens worm"),
         main="Effect of environmental variables (standardized) \non condition")

samplesSummary <- round(samplesList$summary$all.chains[c("gamma1", "gamma2", "gamma3",
                                                         "beta0", 
                                                         "beta1", "beta2", 
                                                         "beta3", 
                                                         "beta4", "beta5",
                                                         "beta6",
                                                         "beta71",
                                                         "beta7[2]", "beta7[3]",
                                                         "beta8[2]", "beta8[3]",
                                                         "beta9[2]", "beta9[3]",
                                                         "beta10[2]", "beta10[3]",
                                                         "beta11[2]", "beta11[3]",
                                                         "beta12[2]", "beta12[3]",
                                                         "beta13[2]", "beta13[3]",
                                                         "sig4x", "sig5x",
                                                         "sig7x","sig9x",
                                                         "sig1x","sig2x","sig3x","mu.eps","sig.eps",
                                                         "sig.r", "sig.temp" , "sig.prec"
),1:5],2) #can also extract predicted.r for plotting

### plot parameter estimates
row.names(samplesSummary) = c("gamma1", "gamma2", "gamma3",
                              "beta0", 
                              "beta1", "beta2", 
                              "beta3", 
                              "beta4", "beta5",
                              "beta6","beta71",
                              "beta7[2]", "beta7[3]",
                              "beta8[2]", "beta8[3]",
                              "beta9[2]", "beta9[3]",
                              "beta10[2]", "beta10[3]",
                              "beta11[2]", "beta11[3]",
                              "beta12[2]", "beta12[3]",
                              "beta13[2]", "beta13[3]",
                              "sig4x", "sig5x",
                              "sig7x","sig9x",
                              "sig1x","sig2x","sig3x","mu.eps","sig.eps",
                              "sig.r", "sig.temp" , "sig.prec")

d_plot<-data.frame(X = row.names(samplesSummary),samplesSummary)
d_plot<-subset(d_plot,grepl("beta", d_plot$X))
d_plot<-d_plot[2:22,] #mydata[1:5,]


beta71 <- c(samplesList$samples$chain1[,'beta71'], samplesList$samples$chain2[,'beta71'])
beta72 <- c(samplesList$samples$chain1[,'beta7[2]'], samplesList$samples$chain2[,'beta7[2]'])
beta73 <- c(samplesList$samples$chain1[,'beta7[3]'], samplesList$samples$chain2[,'beta7[3]'])

#### calculate probability of being strictly pos/neg
round(mean(beta71>0),2) #shellfishspecialist, 0.95
round(mean(beta72>0),2) #intermediate, 0.56
round(mean(beta73<0),2) #worm specialist, 1

### plot difference between feeding specialists and condition
d_plot1<-d_plot[7:9,] #mydata[1:5,]
# order data
d_plot1$X <- factor(d_plot1$X, levels = c("beta71", "beta7[2]", "beta7[3]"),
                    labels=c("Shellfish", "Generalist",
                             "Worm"))
d_p1<-ggplot(data=d_plot1) +
  theme_classic() +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "grey", size=1.5)+
  geom_point(aes(y=X,x=Median), size=3)+ #reorder(position, desc(position)
  #geom_point(aes(y=reorder(X, desc(X)),x=Median), size=3)+ #reorder(position, desc(position)
  geom_errorbarh(data=d_plot1,mapping=aes(xmin=X95.CI_low, xmax=X95.CI_upp, y=X, x=Median),size=1.2, height=.2)+
  labs(x="", y="")+
  ggtitle("(a) Effect of diet specialization on conditon (standardized)") +
  theme(text=element_text(size=18),
        plot.title = element_text(hjust = 0.9, vjust = 3),
        axis.text = element_text(#face="bold", color="#993333", 
          size=14, #angle=45
          
        ))
d_p1

png("F:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/FeedingSpec.png", width = 3500, height = 3000,units = 'px', res = 600)
d_p1
dev.off()


d_plot2a<-d_plot[1:6,] #mydata[1:5,]
d_plot2b<-d_plot[10:21,] #mydata[1:5,]
d_plot2<-rbind(d_plot2a,d_plot2b)

# order data
d_plot2$X <- factor(d_plot2$X, levels = c(
  "beta1", "beta8[2]", "beta8[3]",
  "beta2", "beta9[2]", "beta9[3]",
  "beta3", "beta10[2]", "beta10[3]",
  "beta4", "beta11[2]", "beta11[3]",
  "beta5", "beta12[2]", "beta12[3]",
  "beta6", "beta13[2]", "beta13[3]"),
  labels=c("Temperature Shellfish spec","Temperature Intermediate feeder","Temperature Worm spec", 
           "Precipitation Shellfish spec","Precipitation Intermediate feeder","Precipitation Worm spec",
           "Cockle avail Shellfish spec","Cockle avail Intermediate feeder","Cockle avail Worm spec",
           "Mussel avail Shellfish spec","Mussel avail Intermediate feeder","Mussel avail Worm spec",
           "Grassland Shellfish spec","Grassland Intermediate feeder","Grassland Worm spec",
           "Density Shellfish spec","Density Intermediate feeder","Density Worm spec"
  ))
d_plot2$Specialization<-c("Shellfish", "Shellfish", "Shellfish",
                          "Shellfish", "Shellfish", "Shellfish",
                          "Generalist", "Worm", 
                          "Generalist", "Worm",
                          "Generalist", "Worm",
                          "Generalist", "Worm",
                          "Generalist", "Worm",
                          "Generalist", "Worm"
)
d_plot2$Specialization<-factor(d_plot2$Specialization,
                               levels=c("Worm","Generalist","Shellfish"),
                               labels =c("Worm","Generalist","Shellfish"))
str(d_plot2$Specialization)
levels(d_plot2$Specialization)

d_plot2$EnvVariable<-c("Temperature", "Precipitation", "Cockle availability",
                       "Mussel availability", "Grassland proportion", "Conspecific density",
                       "Temperature", "Temperature", 
                       "Precipitation", "Precipitation",
                       "Cockle availability", "Cockle availability",
                       "Mussel availability", "Mussel availability",
                       "Grassland proportion", "Grassland proportion",
                       "Conspecific density", "Conspecific density")

str(d_plot2$EnvVariable)
d_plot2$EnvVariable<-factor(d_plot2$EnvVariable,
                            levels=c("Temperature", "Precipitation", "Cockle availability",
                                     "Mussel availability", "Grassland proportion","Conspecific density"),
                            labels=c("Temperature", "Precipitation", "Cockle availability",
                                     "Mussel availability", "Grassland proportion","Conspecific density"))

library(ggplot2)
library(dplyr)
d_p2<-ggplot(data=d_plot2) +
  theme_classic() +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "grey", size=1.5)+
  geom_point(aes(y=reorder(Specialization, desc(Specialization)),x=Median), size=3)+ #reorder(position, desc(position)
  geom_errorbarh(data=d_plot2,aes(xmin=X95.CI_low, xmax=X95.CI_upp, y=Specialization, x=Median), size=1.2, height=.2)+
  labs(x="", y="")+
  ggtitle("(b) Effect of environmental variables (standardized) on condition")+
  facet_wrap(~EnvVariable) +
  theme(text=element_text(size=18),
        plot.title = element_text(hjust = -0.3, vjust = 3),
        axis.text = element_text(#face="bold", color="#993333", 
                                   size=14, #angle=45
                                   ),
        panel.spacing = unit(1, "lines")
              )
d_p2

png("F:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentFeedingSpec.png", width = 6000, height = 3500,units = 'px', res = 600)
d_p2
dev.off()

### combine both plots
d_p2
d_p1
library(patchwork)

png("G:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/MainDoc/Fig6_FeedingSpecOnCondition.png", width = 8500, height = 7500,units = 'px', res = 600)
patchwork1<- (d_p1+plot_spacer())/d_p2 +
  #plot_annotation(tag_levels = 'a') +
  plot_layout(widths = c(1,3), heights =  c(1, 2)) 

patchwork1
dev.off() 

#### calculate probability of being strictly pos/neg ALL VALUES CORRECT IN TEXT
d_plot2
beta1 <- c(samplesList$samples$chain1[,'beta1'], samplesList$samples$chain2[,'beta1']) #Temperature Shellfish spec
beta2 <- c(samplesList$samples$chain1[,'beta2'], samplesList$samples$chain2[,'beta2']) #Precipitation Shellfish spec
beta3 <- c(samplesList$samples$chain1[,'beta3'], samplesList$samples$chain2[,'beta3']) # Cockle avail Shellfish spec
beta4 <- c(samplesList$samples$chain1[,'beta4'], samplesList$samples$chain2[,'beta4']) # Mussel avail Shellfish spec
beta5 <- c(samplesList$samples$chain1[,'beta5'], samplesList$samples$chain2[,'beta5']) #Grassland Shellfish spec
beta6 <- c(samplesList$samples$chain1[,'beta6'], samplesList$samples$chain2[,'beta6']) #Density Shellfish spec

beta82 <- c(samplesList$samples$chain1[,'beta8[2]'], samplesList$samples$chain2[,'beta8[2]']) #Temperature Intermediate feeder
beta92 <- c(samplesList$samples$chain1[,'beta9[2]'], samplesList$samples$chain2[,'beta9[2]']) #Precipitation Intermediate feeder
beta102 <- c(samplesList$samples$chain1[,'beta10[2]'], samplesList$samples$chain2[,'beta10[2]']) #Cockle avail Intermediate feeder
beta112 <- c(samplesList$samples$chain1[,'beta11[2]'], samplesList$samples$chain2[,'beta11[2]']) #Mussel avail Intermediate feeder
beta122 <- c(samplesList$samples$chain1[,'beta12[2]'], samplesList$samples$chain2[,'beta12[2]']) #Grassland Intermediate feeder
beta132 <- c(samplesList$samples$chain1[,'beta13[2]'], samplesList$samples$chain2[,'beta13[2]']) #Density Intermediate feeder

beta83 <- c(samplesList$samples$chain1[,'beta8[3]'], samplesList$samples$chain2[,'beta8[3]']) #Temperature worm feeder
beta93 <- c(samplesList$samples$chain1[,'beta9[3]'], samplesList$samples$chain2[,'beta9[3]']) #Precipitation worm feeder
beta103 <- c(samplesList$samples$chain1[,'beta10[3]'], samplesList$samples$chain2[,'beta10[3]']) #Cockle avail worm feeder
beta113 <- c(samplesList$samples$chain1[,'beta11[3]'], samplesList$samples$chain2[,'beta11[3]']) #Mussel avail worm feeder
beta123 <- c(samplesList$samples$chain1[,'beta12[3]'], samplesList$samples$chain2[,'beta12[3]']) #Grassland worm feeder
beta133 <- c(samplesList$samples$chain1[,'beta13[3]'], samplesList$samples$chain2[,'beta13[3]']) #Density worm feeder


round(mean(beta1<0),2) #Temperature Shellfish spec, 0.54
round(mean(beta2>0),2) #Precipitation Shellfish spec, 0.9
round(mean(beta3>0),2) #Cockle avail Shellfish spec, 0.75
round(mean(beta4<0),2) #Mussel avail Shellfish spec, 0.53
round(mean(beta5>0),2) #Grassland Shellfish spec, 0.96
round(mean(beta6>0),2) #Density Shellfish spec, 0.76

round(mean(beta82>0),2) #Temperature Intermediate feeder, 0.92
round(mean(beta92<0),2) #Precipitation Intermediate feeder, 0.92
round(mean(beta102>0),2) #Cockle avail Intermediate feeder, 0.51
round(mean(beta112>0),2) #Mussel avail Intermediate feeder, 0.8
round(mean(beta122<0),2) #Grassland Intermediate feeder, 0.62
round(mean(beta132>0),2) #Density Intermediate feeder, 0.67

round(mean(beta83>0),2) #Temperature worm feeder, 0.98
round(mean(beta93<0),2) #Precipitation worm feeder, 0.89
round(mean(beta103<0),2) #Cockle avail worm feeder, 0.88
round(mean(beta113<0),2) #Mussel avail worm feeder, 0.86
round(mean(beta123>0),2) #Grassland worm feeder, 0.71
round(mean(beta133>0),2) #Density worm feeder, 0.94

save.image("F:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/MainDoc/Files for Fig5/GlobalEnvInteractionBillShape.Rdata")


############################################################################################################
############################################################################################################
############################################################################################################
# convergence
library(basicMCMCplots)
chainsPlot(samplesList$samples, #width=4000, height=6000,
           var = c("gamma1", "gamma2", "gamma3"),
           file="P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot1.pdf")
chainsPlot(samplesList$samples, #width=4000, height=6000,
           var = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6"),
           file="P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot2.pdf")
chainsPlot(samplesList$samples, #width=4000, height=6000,
           var = c("beta7[2]", "beta7[3]", "beta8[2]", "beta8[3]", "beta9[2]", "beta9[3]"),
           file="P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot2a.pdf")
chainsPlot(samplesList$samples, #width=4000, height=6000,
           var = c("beta10[2]", "beta10[3]", "beta11[2]", "beta11[3]", "beta12[2]", "beta12[3]", "beta13[2]", "beta13[3]"),
           file="P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot2b.pdf")
#chainsPlot(samplesList$samples, #width=4000, height=6000,
#           var = c("beta12", "beta22", "beta32", "beta42", "beta52"),
#           file="P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/2Months/TracePlot3.pdf")
chainsPlot(samplesList$samples, #width=4000, height=6000,
           var = c("sig1x","sig2x","sig3x","sig4x", "sig5x","sig7x", "sig9x"),
           file="P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot4.pdf")
chainsPlot(samplesList$samples, #width=4000, height=6000,
           var = c("sig.r", "sig.temp", "sig.prec", "sig.eps", "mu.eps"),
           file="P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot6.pdf")
chainsPlot(samplesList$samples, #width=4000, height=6000,
           var = c("eps[1]", "eps[2]", "eps[35]", "eps[36]"),
           file="P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot7.pdf")

## combine pdfs
library(pdftools)
pdf_combine(c("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot1.pdf","P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot2.pdf",
              "P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot4.pdf",
              "P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot5.pdf", "P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot6.pdf",
              "P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot7.pdf"), 
            output = "P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/TracePlot_joined.pdf")

#R-hat
library(coda)
prhat = gelman.diag(samplesList$samples
                    [,c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5","beta6",
                        #"beta12", "beta22", "beta32", "beta42", "beta52",
                        "gamma1", "gamma2", "gamma3",
                        "sig1x","sig2x","sig3x","sig4x","sig5x", "sig7x","sig9x",
                        "mu.eps","sig.eps",
                        "sig.r", "sig.temp" , "sig.prec"
                    )],multivariate = FALSE)
prhat$psrf[,1]
GelRub<-as.data.frame(prhat$psrf[,1])
names(GelRub)[1] <- "PSRF" #potential scale reduction factor 
write.csv2(GelRub,"P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/GelRub.csv", 
           row.names =TRUE)

###### coefficient plots without chain
library(MCMCvis)

# main effects as caterpillar plot
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalEffectPlot1Months.png", width = 5000, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6'),
         labels = c("Temperature", #expression('Temperature'^2), 
                    "Precipitation", #expression('Precipitation'^2), 
                    "Cockle availability", #expression('Cockle availability'^2), 
                    "Mussel availability", #expression('Mussel availability'^2), 
                    "Grassland proportion", #expression('Grassland proportion'^2)
                    "Conspecific density"
         ),
         main="Effect of environmental variables \n(standardized) on condition")
dev.off() 

# appendix sigmas
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalSigmas.png", width = 8500, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c( 'sig5x', 
                                        'sig7x', "sig9x", 
                                        'sig1x', 'sig2x','sig3x','sig4x',
                                        "sig.eps","sig.temp", 
                                        "sig.prec"),
         labels=c(expression(sigma[X12]), expression(sigma[X14]), expression(sigma[X15]), 
                  expression(sigma[X17]), 
                  expression(sigma[X18]), expression(sigma[X19]), expression(sigma[X20]), expression(sigma[X21]),
                  expression(sigma[X22]), expression(sigma[X23])
                  ))
dev.off()

# appendix 
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalLatent.png", width = 8500, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('gamma1', 'gamma2', 'gamma3'),
         labels = c(expression(zeta[1]),expression(zeta[2]),expression(zeta[3])))
dev.off()

# appendix random effects
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalRandomEffect.png", width = 8500, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('eps'),
         #  , 'eps[2]', 'eps[3]', 'eps[4]', 'eps[5]','eps[6]',"eps[7]","eps[8]", "eps[9]","eps[10]",
         #  'eps[11]', 'eps[12]', 'eps[13]', 'eps[14]', 'eps[15]','eps[16]',"eps[17]","eps[18]", "eps[19]","eps[20]",
         #  'eps[21]', 'eps[22]', 'eps[23]', 'eps[24]', 'eps[25]','eps[26]',"eps[27]","eps[28]", "eps[29]","eps[30]",
         #  'eps[31]', 'eps[32]', 'eps[33]', 'eps[34]', 'eps[35]','eps[36]'))
         labels=c(expression(epsilon[1]), expression(epsilon[2]), expression(epsilon[3]), expression(epsilon[4]), expression(epsilon[5]),expression(epsilon[6]),expression(epsilon[7]),expression(epsilon[8]),expression(epsilon[9]),expression(epsilon[10]),
                  expression(epsilon[11]), expression(epsilon[12]), expression(epsilon[13]), expression(epsilon[14]), expression(epsilon[15]),expression(epsilon[16]),expression(epsilon[17]),expression(epsilon[18]),expression(epsilon[19]),expression(epsilon[20]),
                  expression(epsilon[21]), expression(epsilon[22]), expression(epsilon[23]), expression(epsilon[24]), expression(epsilon[25]),expression(epsilon[26]),expression(epsilon[27]),expression(epsilon[28]),expression(epsilon[29]),expression(epsilon[30]),
                  expression(epsilon[31]), expression(epsilon[32]), expression(epsilon[33]), expression(epsilon[34]), expression(epsilon[35]),expression(epsilon[36])
         ))

dev.off()

#samplesList$samples$chain1[grep("eps"),]
########## plotting results ########################
####### plot relationships cond variables and composite condition

#' Let's plot the relationship. First, we gather the values generated from the posterior distribution of the regression parameters in the two chains. 
## ---------------------------------------------------------------------------------------------------------
beta0 <- c(samplesList$samples$chain1[,'beta0'], samplesList$samples$chain2[,'beta0'])
beta1 <- c(samplesList$samples$chain1[,'beta1'], samplesList$samples$chain2[,'beta1'])
#beta12 <- c(samplesList$samples$chain1[,'beta12'], samplesList$samples$chain2[,'beta12'])
beta2 <- c(samplesList$samples$chain1[,'beta2'], samplesList$samples$chain2[,'beta2'])
#beta22 <- c(samplesList$samples$chain1[,'beta22'], samplesList$samples$chain2[,'beta22'])
beta3 <- c(samplesList$samples$chain1[,'beta3'], samplesList$samples$chain2[,'beta3'])
#beta32 <- c(samplesList$samples$chain1[,'beta32'], samplesList$samples$chain2[,'beta32'])
beta4 <- c(samplesList$samples$chain1[,'beta4'], samplesList$samples$chain2[,'beta4'])
#beta42 <- c(samplesList$samples$chain1[,'beta42'], samplesList$samples$chain2[,'beta42'])
beta5 <- round(c(samplesList$samples$chain1[,'beta5'], samplesList$samples$chain2[,'beta5']),2)
#beta52 <- c(samplesList$samples$chain1[,'beta52'], samplesList$samples$chain2[,'beta52'])
#mu.eps <- c(samplesList$samples$chain1[,'mu.eps'], samplesList$samples$chain2[,'mu.eps'])
beta6 <- round(c(samplesList$samples$chain1[,'beta6'], samplesList$samples$chain2[,'beta6']),2)

#### calculate probability of being strictly pos/neg
round(mean(beta1>0),2) #0.82, temp
round(mean(beta2>0),2) #0.63, prec
round(mean(beta3>0),2) #0.81, cockle
round(mean(beta4>0),2) #0.58, mussel
round(mean(beta5>0),2) #0.98, grassland
round(mean(beta6>0),2) #0.90, density

temp<-c(expression("0.13 [-0.15;0.42]"),
        expression(paste('p'["d"],"=0.82")))#=0.82

prec<-c(expression("0.04 [-0.26;0.32]"),
          expression(paste("p"["d"],"=0.63")))

cock<-c(expression("0.09 [-0.11;0.28]"),
        expression(paste("p"["d"],"=0.81")))

mus<-c(expression("0.02 [-0.17;0.21]"),
       expression(paste("p"["d"],"=0.58")))

gras<-c(expression("0.24 [0.02;0.45]"),
        expression(paste("p"["d"],"=0.98")))

dens<-c(expression("0.14 [-0.06;0.35]"),
        expression(paste("p"["d"],"=0.90")))

## average random effect 
eps1 <- c(samplesList$samples$chain1[,'eps[1]'], samplesList$samples$chain2[,'eps[1]'])
eps2 <- c(samplesList$samples$chain1[,'eps[2]'], samplesList$samples$chain2[,'eps[2]'])
eps3 <- c(samplesList$samples$chain1[,'eps[3]'], samplesList$samples$chain2[,'eps[3]'])
eps4 <- c(samplesList$samples$chain1[,'eps[4]'], samplesList$samples$chain2[,'eps[4]'])
eps5 <- c(samplesList$samples$chain1[,'eps[5]'], samplesList$samples$chain2[,'eps[5]'])
eps6 <- c(samplesList$samples$chain1[,'eps[6]'], samplesList$samples$chain2[,'eps[6]'])
eps7 <- c(samplesList$samples$chain1[,'eps[7]'], samplesList$samples$chain2[,'eps[7]'])
eps8 <- c(samplesList$samples$chain1[,'eps[8]'], samplesList$samples$chain2[,'eps[8]'])
eps9 <- c(samplesList$samples$chain1[,'eps[9]'], samplesList$samples$chain2[,'eps[9]'])
eps10 <- c(samplesList$samples$chain1[,'eps[10]'], samplesList$samples$chain2[,'eps[10]'])
eps11 <- c(samplesList$samples$chain1[,'eps[11]'], samplesList$samples$chain2[,'eps[11]'])
eps12 <- c(samplesList$samples$chain1[,'eps[12]'], samplesList$samples$chain2[,'eps[12]'])
eps13 <- c(samplesList$samples$chain1[,'eps[13]'], samplesList$samples$chain2[,'eps[13]'])
eps14 <- c(samplesList$samples$chain1[,'eps[14]'], samplesList$samples$chain2[,'eps[14]'])
eps15 <- c(samplesList$samples$chain1[,'eps[15]'], samplesList$samples$chain2[,'eps[15]'])
eps16 <- c(samplesList$samples$chain1[,'eps[16]'], samplesList$samples$chain2[,'eps[16]'])
eps17 <- c(samplesList$samples$chain1[,'eps[17]'], samplesList$samples$chain2[,'eps[17]'])
eps18 <- c(samplesList$samples$chain1[,'eps[18]'], samplesList$samples$chain2[,'eps[18]'])
eps19 <- c(samplesList$samples$chain1[,'eps[19]'], samplesList$samples$chain2[,'eps[19]'])
eps20 <- c(samplesList$samples$chain1[,'eps[20]'], samplesList$samples$chain2[,'eps[20]'])
eps21 <- c(samplesList$samples$chain1[,'eps[21]'], samplesList$samples$chain2[,'eps[21]'])
eps22 <- c(samplesList$samples$chain1[,'eps[22]'], samplesList$samples$chain2[,'eps[22]'])
eps23 <- c(samplesList$samples$chain1[,'eps[23]'], samplesList$samples$chain2[,'eps[23]'])
eps24 <- c(samplesList$samples$chain1[,'eps[24]'], samplesList$samples$chain2[,'eps[24]'])
eps25 <- c(samplesList$samples$chain1[,'eps[25]'], samplesList$samples$chain2[,'eps[25]'])
eps26 <- c(samplesList$samples$chain1[,'eps[26]'], samplesList$samples$chain2[,'eps[26]'])
eps27 <- c(samplesList$samples$chain1[,'eps[27]'], samplesList$samples$chain2[,'eps[27]'])
eps28 <- c(samplesList$samples$chain1[,'eps[28]'], samplesList$samples$chain2[,'eps[28]'])
eps29 <- c(samplesList$samples$chain1[,'eps[29]'], samplesList$samples$chain2[,'eps[29]'])
eps30 <- c(samplesList$samples$chain1[,'eps[30]'], samplesList$samples$chain2[,'eps[30]'])
eps31 <- c(samplesList$samples$chain1[,'eps[31]'], samplesList$samples$chain2[,'eps[31]'])
eps32 <- c(samplesList$samples$chain1[,'eps[32]'], samplesList$samples$chain2[,'eps[32]'])
eps33 <- c(samplesList$samples$chain1[,'eps[33]'], samplesList$samples$chain2[,'eps[33]'])
eps34 <- c(samplesList$samples$chain1[,'eps[34]'], samplesList$samples$chain2[,'eps[34]'])
eps35 <- c(samplesList$samples$chain1[,'eps[35]'], samplesList$samples$chain2[,'eps[35]'])
eps36 <- c(samplesList$samples$chain1[,'eps[36]'], samplesList$samples$chain2[,'eps[36]'])

eps_f<-matrix(c(eps2, eps3, eps4, eps5,eps6,eps7,eps8,eps9,eps10,
                eps11,eps12,eps13,eps14,eps15,eps16,eps17,eps18,eps19,eps20,
                eps21,eps22,eps23,eps24,eps25,eps26,eps27,eps28,eps29,eps30,
                eps31,eps32,eps33,eps34,eps35,eps36
), ncol=35) #not first eps because it is intercept
eps<-apply(eps_f,2,mean)

mean.eps = mean(eps) # mean of each covariate (eps2-36)
mean.mat.eps = matrix(rep(mean.eps,nrow(samplesSummaryTemp)),ncol=1)

#mean.eps1 = mean(eps[1]) #intercept
#mean.eps<-mean.eps1+mean.eps #intercept + mean slope random effect
#mean.mat.eps = matrix(rep(mean.eps,nrow(samplesSummaryTemp)),ncol=1)

### mean for latent temp and prec
mean.temp = apply(samplesSummaryTemp,2,mean) # mean of each covariate
mean.mat.temp = matrix(rep(mean.temp,nrow(samplesSummaryTemp)),byrow=T,ncol=ncol(samplesSummaryTemp))

mean.prec = apply(samplesSummaryPrec,2,mean) # mean of each covariate
mean.mat.prec = matrix(rep(mean.prec,nrow(samplesSummaryPrec)),byrow=T,ncol=ncol(samplesSummaryPrec))

## mean of covariates
mean.stcov = apply(stcov,2,mean) # mean of each covariate
mean.mat.stcov = matrix(rep(mean.stcov,nrow(stcov)),byrow=T,ncol=ncol(stcov))

###
df$StateText<-factor(df$State, levels=c( "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                     labels=c("Delta", "Inland South", "Inland North", "Texel-Balgzand", 
                              "Texel-Vlieland","Terschelling-Ameland", "Schiermonnikoog","Rottum",  
                              "Abroad"))

####### temperature ########
## linear
predicted_conditionT <- matrix(NA, nrow = length(beta1), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionT[i,j] <- beta0[i] + beta1[i] * samplesSummaryTemp[j,1] + 
      beta2[i] * mean.mat.prec[j,1] + 
      beta3[i] * mean.mat.stcov[j,1] + 
      beta4[i] * mean.mat.stcov[j,2] + 
      beta5[i] * mean.mat.stcov[j,3] +
      beta6[i] * mean.mat.stcov[j,4] +
      #beta12[i] * pow(mean.mat.temp[j,1],2) + beta22[i] * pow(mean.mat.prec[j,1],2) + beta32[i] * pow(mean.mat.stcov[j,1],2) + beta42[i] * pow(mean.mat.stcov[j,2],2) + beta52[i] * pow(mean.mat.stcov[j,3],2) +
      mean.mat.eps[j,1]
  }
}


#' Now we calculate posterior mean and the credible interval. Note the ordering.
## ---------------------------------------------------------------------------------------------------------
mean_conditionT <- apply(predicted_conditionT, 2, mean)
lciT <- apply(predicted_conditionT, 2, quantile, prob = 2.5/100) #2.5, 25
uciT <- apply(predicted_conditionT, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(samplesSummaryTemp[,1])
dfT <- data.frame(t = samplesSummaryTemp[,1][ord],
                  cond = mean_conditionT[ord],
                  lciT = lciT[ord],
                  uciT = uciT[ord]
)
head(dfT)

library(png)
library(grid)
library(ggimage)
imgTemp<-readPNG("P:/CHIRP/Carry-over/Analysis/Figures/Symbols for Figures/Temp.png")

head(samplesSummaryTemp)

#' Now time to visualize. 
## ---- fig.width = 7.5, fig.asp = 0.618, dev = "svg"-------------------------------------------------------
library(ggplot2)
p_T<-ggplot(dfT, aes(x = t, y = cond)) + 
  geom_point(mapping=aes(x=samplesSummaryTemp[,1],y=samplesSummaryPredictedCond[,1], color=df$StateText,shape=df$YearShort), cex=2.5,alpha=0.5)+
  #geom_line(data=dfT,mapping=aes(x = t, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  geom_line(data=dfT,mapping=aes(x = t, y = cond), cex=1.2) + 
  #geom_ribbon(data=dfT,aes(ymin = lciT2, ymax = uciT2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfT,aes(ymin = lciT, ymax = uciT), alpha = 0.25)+
  labs(x = "temperature (s.d.)", y = "body condition")+
  theme_bw()+
  ylim(-1.32,1.25)+
  scale_color_manual(name = "Site (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  theme(legend.position="right", 
        legend.direction = "vertical",
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16))+
  guides(fill=guide_legend(ncol=2, byrow=TRUE))+
  annotation_custom(rasterGrob(imgTemp, 
                               width = unit(1.5,"npc"),
                               height = unit(1.5,"npc")), 
                    1.93,2.3, .82, 1.1)+
  annotate("text", x=-1, y=c(1.2,1.05), label=temp, size=6)
  #annotate("text",x=-1, y=1.1, label=expression("0.13[-0.14;0.39], "p(|kappa|>0))???, size=8 )
p_T


# Extract the legend. Returns a gtable
library(ggpubr)
leg <- get_legend(p_T)

# Convert to a ggplot and print
p_leg<-as_ggplot(leg)
#plot_layout(tag_level = 'new')

###now without legend
p_T<-ggplot(dfT, aes(x = t, y = cond)) + 
  geom_point(mapping=aes(x=samplesSummaryTemp[,1],y=samplesSummaryPredictedCond[,1], color=df$StateText,shape=df$YearShort), cex=2.5,alpha=0.5)+
  #geom_line(data=dfT,mapping=aes(x = t, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  geom_line(data=dfT,mapping=aes(x = t, y = cond), cex=1.2, linetype="dashed") + 
  #geom_ribbon(data=dfT,aes(ymin = lciT2, ymax = uciT2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfT,aes(ymin = lciT, ymax = uciT), alpha = 0.25)+
  labs(x = "temperature (s.d.)", y = "body condition", tag="a")+
  theme_bw()+
  ylim(-1.32,1.25)+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  theme(legend.position="none", 
        legend.direction = "vertical",
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16))+
  guides(fill=guide_legend(ncol=2, byrow=TRUE))+
  annotation_custom(rasterGrob(imgTemp, 
                               width = unit(1.5,"npc"),
                               height = unit(1.5,"npc")), 
                    1.93,2.3, .95, 1.3)+
  annotate("text", x=-0.88, y=c(1.23,1.06), label=temp, size=4)
p_T

###### precipitation
## linear
predicted_conditionP <- matrix(NA, nrow = length(beta1), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionP[i,j] <- beta0[i] + beta1[i] * mean.mat.temp[j,1] + 
      beta2[i] * samplesSummaryPrec[j,1] + 
      beta3[i] * mean.mat.stcov[j,1] + 
      beta4[i] * mean.mat.stcov[j,2] + 
      beta5[i] * mean.mat.stcov[j,3] +
      beta6[i] * mean.mat.stcov[j,4] +
      #beta12[i] * pow(mean.mat.temp[j,1],2) + beta22[i] * pow(mean.mat.prec[j,1],2) + beta32[i] * pow(mean.mat.stcov[j,1],2) + beta42[i] * pow(mean.mat.stcov[j,2],2) + beta52[i] * pow(mean.mat.stcov[j,3],2) +
      mean.mat.eps[j,1]
  }
}

mean_conditionP <- apply(predicted_conditionP, 2, mean)
lciP <- apply(predicted_conditionP, 2, quantile, prob = 2.5/100) #2.5, 25
uciP <- apply(predicted_conditionP, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(samplesSummaryPrec[,1])
dfP <- data.frame(p = samplesSummaryPrec[,1][ord],
                  cond = mean_conditionP[ord],
                  lciP = lciP[ord],
                  uciP = uciP[ord])
head(dfP)

imgPrec<-readPNG("P:/CHIRP/Carry-over/Analysis/Figures/Symbols for Figures/Prec.png")

p_P<-ggplot(dfP, aes(x = p, y = cond)) + 
  geom_point(mapping=aes(x=samplesSummaryPrec[,1],y=samplesSummaryPredictedCond[,1],color=df$StateText,shape=df$YearShort),cex=2.5, alpha=0.5)+
  #geom_line(data=dfP2,mapping=aes(x = p, y = cond, col="#E69F00"),cex=1.2) + 
  geom_line(data=dfP,mapping=aes(x = p, y = cond), linetype="dashed", cex=1.2) + 
  #geom_ribbon(data=dfP2,aes(ymin = lciP2, ymax = uciP2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfP,aes(ymin = lciP, ymax = uciP), alpha = 0.25)+
  labs(x = "precipitation (s.d.)", y = "body condition", tag="b")+
  theme_bw()+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  ylim(-1.32,1.25)+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  annotation_custom(rasterGrob(imgPrec, 
                               width = unit(1.5,"npc"),
                               height = unit(1.5,"npc")), 
                    1.48,1.95, .92, 1.25)+
  annotate("text", x=-0.97, y=c(1.23,1.06), label=prec, size=4)
p_P

## linear cockle
predicted_conditionC <- matrix(NA, nrow = length(beta1), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionC[i,j] <- beta0[i] + beta1[i] * mean.mat.temp[j,1] + 
      beta2[i] * mean.mat.prec[j,1] + 
      beta3[i] * stcov[j,1] + 
      beta4[i] * mean.mat.stcov[j,2] + 
      beta5[i] * mean.mat.stcov[j,3] +
      beta6[i] * mean.mat.stcov[j,4] +
      #beta12[i] * pow(mean.mat.temp[j,1],2) + beta22[i] * pow(mean.mat.prec[j,1],2) + beta32[i] * pow(mean.mat.stcov[j,1],2) + beta42[i] * pow(mean.mat.stcov[j,2],2) + beta52[i] * pow(mean.mat.stcov[j,3],2) +
      mean.mat.eps[j,1]
  }
}

mean_conditionC <- apply(predicted_conditionC, 2, mean)
lciC <- apply(predicted_conditionC, 2, quantile, prob = 2.5/100) #2.5, 25
uciC <- apply(predicted_conditionC, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(contcov[,1]) #stcov for standardized
dfC <- data.frame(c = contcov[,1][ord], #stcov for standardized
                  cond = mean_conditionC[ord],
                  lciC = lciC[ord],
                  uciC = uciC[ord])
head(dfC)

imgCock<-readPNG("P:/CHIRP/Carry-over/Analysis/Figures/Symbols for Figures/Cockle.png")

p_C<-ggplot(dfC, aes(x = c, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,1],y=samplesSummaryPredictedCond[,1],color=df$StateText,shape=df$YearShort),cex=2.5, alpha=0.5)+ #stcov for standardized plot
  #geom_line(data=dfC2,mapping=aes(x = c, y = cond),cex=1.2, linetype="dotted") + 
  geom_line(data=dfC,mapping=aes(x = c, y = cond),linetype="dashed", cex=1.2) + 
  #geom_ribbon(data=dfC2,aes(ymin = lciC2, ymax = uciC2), alpha = 0.25)+
  geom_ribbon(data=dfC,aes(ymin = lciC, ymax = uciC), alpha = 0.25)+
  labs(x = expression(paste("cockle availability (g AFDM/m"^"2", ")")), y = "body condition", tag="d")+
  theme_bw()+
  ylim(-1.32,1.25)+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16))+
  annotation_custom(rasterGrob(imgCock, 
                               width = unit(1.5,"npc"),
                               height = unit(1.5,"npc")), 
                    19,23, .93, 1.3)+
  annotate("text", x=4, y=c(1.23,1.06), label=cock, size=4)
p_C

##  mussel
predicted_conditionM <- matrix(NA, nrow = length(beta1), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionM[i,j] <- beta0[i] + beta1[i] * mean.mat.temp[j,1] + 
      beta2[i] * mean.mat.prec[j,1] + 
      beta3[i] * mean.mat.stcov[j,1] + 
      beta4[i] * stcov[j,2] + 
      beta5[i] * mean.mat.stcov[j,3] +
      beta6[i] * mean.mat.stcov[j,4] +
      #beta12[i] * pow(mean.mat.temp[j,1],2) + beta22[i] * pow(mean.mat.prec[j,1],2) + beta32[i] * pow(mean.mat.stcov[j,1],2) + beta42[i] * pow(mean.mat.stcov[j,2],2) + beta52[i] * pow(mean.mat.stcov[j,3],2) +
      mean.mat.eps[j,1]
  }
}

mean_conditionM <- apply(predicted_conditionM, 2, mean)
lciM <- apply(predicted_conditionM, 2, quantile, prob = 2.5/100) #2.5, 25
uciM <- apply(predicted_conditionM, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(contcov[,2]) #stcov for standardized
dfM <- data.frame(m = contcov[,2][ord], #stcov for standardized
                  cond = mean_conditionM[ord],
                  lciM = lciM[ord],
                  uciM = uciM[ord])
head(dfM)

imgMussel<-readPNG("P:/CHIRP/Carry-over/Analysis/Figures/Symbols for Figures/Mussel.png")

p_M<-ggplot(dfM, aes(x = m, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,2],y=samplesSummaryPredictedCond[,1], color=df$StateText,shape=df$YearShort),cex=2.5, alpha=0.5)+
  #geom_line(data=dfM2,mapping=aes(x = m, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  geom_line(data=dfM,mapping=aes(x = m, y = cond), linetype="dashed",cex=1.2) + 
  #geom_ribbon(data=dfM2,aes(ymin = lciM2, ymax = uciM2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfM,aes(ymin = lciM, ymax = uciM), alpha = 0.25)+
  labs(x = expression(paste("mussel availability (g AFDM/m"^"2", ")")), y = "body condition", tag="e")+
  theme_bw()+
  ylim(-1.32,1.25)+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  annotation_custom(rasterGrob(imgMussel, 
                               width = unit(1.5,"npc"),
                               height = unit(1.5,"npc")), 
                    105,120, .99, 1.25)+
  annotate("text", x=20, y=c(1.23,1.06), label=mus, size=4)
p_M

##### grassland
predicted_conditionG <- matrix(NA, nrow = length(beta1), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionG[i,j] <- beta0[i] + 
      beta1[i] * mean.mat.temp[j,1] + 
      beta2[i] * mean.mat.prec[j,1] + 
      beta3[i] * mean.mat.stcov[j,1] + 
      beta4[i] * mean.mat.stcov[j,2] + 
      beta5[i] * stcov[j,3] +
      beta6[i] * mean.mat.stcov[j,4] +
      #beta12[i] * pow(mean.mat.temp[j,1],2) + beta22[i] * pow(mean.mat.prec[j,1],2) + beta32[i] * pow(mean.mat.stcov[j,1],2) + beta42[i] * pow(mean.mat.stcov[j,2],2) + beta52[i] * pow(mean.mat.stcov[j,3],2) +
      mean.mat.eps[j,1]
  }
}

mean_conditionG <- apply(predicted_conditionG, 2, mean)
lciG <- apply(predicted_conditionG, 2, quantile, prob = 2.5/100) #2.5, 25
uciG <- apply(predicted_conditionG, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(contcov[,3]) #stvov for standardized
dfG <- data.frame(g = contcov[,3][ord], #stcov for standardized
                  cond = mean_conditionG[ord],
                  lciG = lciG[ord],
                  uciG = uciG[ord])
head(dfG)

imgGras<-readPNG("P:/CHIRP/Carry-over/Analysis/Figures/Symbols for Figures/Earthworm.png")

p_G<-ggplot(dfG, aes(x = g, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,3],y=samplesSummaryPredictedCond[,1],color=df$StateText,shape=df$YearShort),cex=2.5, alpha=0.5)+ #stcov for standardized
  #geom_line(data=dfG2,mapping=aes(x = g, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  geom_line(data=dfG,mapping=aes(x = g, y = cond), cex=1.2) + 
  #geom_ribbon(data=dfG2,aes(ymin = lciG2, ymax = uciG2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfG,aes(ymin = lciG, ymax = uciG), alpha = 0.25)+
  labs(x = "grassland proportion", y = "body condition", tag="f")+
  theme_bw()+
  ylim(-1.32,1.25)+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  annotation_custom(rasterGrob(imgGras, 
                               width = unit(1.5,"npc"),
                               height = unit(1.5,"npc")), 
                    0.149,0.172, .93, 1.26)+
  annotate("text", x=0.026, y=c(1.23,1.06), label=gras, size=4)
p_G

##### density
predicted_conditionD <- matrix(NA, nrow = length(beta1), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(beta1)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionD[i,j] <- beta0[i] + 
      beta1[i] * mean.mat.temp[j,1] + 
      beta2[i] * mean.mat.prec[j,1] + 
      beta3[i] * mean.mat.stcov[j,1] + 
      beta4[i] * mean.mat.stcov[j,2] + 
      beta5[i] * mean.mat.stcov[j,3] +
      beta6[i] * stcov[j,4] +
      #beta12[i] * pow(mean.mat.temp[j,1],2) + beta22[i] * pow(mean.mat.prec[j,1],2) + beta32[i] * pow(mean.mat.stcov[j,1],2) + beta42[i] * pow(mean.mat.stcov[j,2],2) + beta52[i] * pow(mean.mat.stcov[j,3],2) +
      mean.mat.eps[j,1]
  }
}

mean_conditionD <- apply(predicted_conditionD, 2, mean)
lciD <- apply(predicted_conditionD, 2, quantile, prob = 2.5/100) #2.5, 25
uciD <- apply(predicted_conditionD, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(contcov[,4]) #stvov for standardized
dfD <- data.frame(d = contcov[,4][ord], #stcov for standardized
                  cond = mean_conditionD[ord],
                  lciD = lciD[ord],
                  uciD = uciD[ord])
head(dfD)

imgDens<-readPNG("P:/CHIRP/Carry-over/Analysis/Figures/Symbols for Figures/OYC_Density.png")

p_D<-ggplot(dfD, aes(x = d, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,4],y=samplesSummaryPredictedCond[,1],color=df$StateText,shape=df$YearShort),cex=2.5, alpha=0.5)+ #stcov for standardized
  #geom_line(data=dfG2,mapping=aes(x = g, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  geom_line(data=dfD,mapping=aes(x = d, y = cond), linetype="dashed",cex=1.2) + 
  #geom_ribbon(data=dfG2,aes(ymin = lciG2, ymax = uciG2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfD,aes(ymin = lciD, ymax = uciD), alpha = 0.25)+
  labs(x = expression(paste("oystercatcher density (ind/km"^"2", ")")), y = "body condition", tag="c")+
  #expression('Temperature'^2)
  theme_bw()+
  ylim(-1.32,1.25)+
  scale_color_manual(name = "Site (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  annotation_custom(rasterGrob(imgDens, 
                               width = unit(1.5,"npc"),
                               height = unit(1.5,"npc")), 
                    226,265, .93, 1.29)+
  annotate("text", x=54, y=c(1.23,1.06), label=dens, size=4)

p_D

############# combine plots #############
library(patchwork)
png("G:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/MainDoc/Fig5_EnvironmentalVariables.png", width = 9500, height = 4500,units = 'px', res = 600)
layout = "
ABCG
DEFG
"
patchwork <- p_T+p_P+p_D+p_C+p_M+p_G+p_leg+ #(p_T|p_P|p_leg)/(p_C|p_M|p_G)+
  plot_layout(design = layout)
patchwork
dev.off() 

save.image("P:/CHIRP/Carry-over/Analysis/Figures/MainDoc/Files for Fig5/GlobalEnvFig5.Rdata")

#############################################################################################################################
########################################### only adults (suppl) #################################################################################
############################################################################################################################
Cov<-read.csv(file="N:/Dep.AnE/AnE-share/_Magali/Nimble_M1_P12/StateMatrix2000_2019Cond_3class9Years.csv")
str(Cov)
Cov<-Cov[,2]

Cov<-cbind(ID,Cov)
Cov$Age<-Cov$Cov

df$WaderID_ObsOcc<-paste(df$WaderBirdID,df$ObsCatchNr,sep="_")
Cov$WaderID_ObsOcc<-paste(Cov$WaderBirdID,Cov$ObsCatchNr,sep="_")

library(plyr)
dfAd<-join(Cov, df, by = "WaderID_ObsOcc", type = "left", match = "all")

dfAd<-subset(dfAd, Age==4|Age==3) #n=1197


#dfAd<-data.frame(dfAd[,1])
#str(dfAd)
#test <- dfAd %>% 
#  dplyr::group_by(dfAd...1.) %>% 
#  dplyr::count(dfAd...1.) ### 25 ind were caught twice!!! at different catch occasions (in first period)

################################################## NIMBLE CODE #################################
#### try nimble approach with latent variable ###

#response variable, = eta in cr-sem
r_Ad<-dfAd[,12]

## food including exposure time
dfAd$AFDMgrm2_C_CondLoss_exp<-dfAd$AFDMgrm2_C_CondLoss*dfAd$Avg_exp_NovFeb
dfAd$AFDMgrm2_M_CondLoss_exp<-dfAd$AFDMgrm2_M_CondLoss*dfAd$Avg_exp_NovFeb
str(dfAd)

# Other covariates are continuous : condition index
food2_Ad <- subset(dfAd,
                select=c(PropGrassland))
food1_Ad<-dfAd[,44:45]
comp_Ad <- subset(dfAd,
               select=c(Density_km2))
weather_Ad<-dfAd[,34:38] # 1 month
contcov_Ad<-cbind(food1_Ad,food2_Ad,comp_Ad,weather_Ad)
head(contcov_Ad)

# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov_Ad = apply(contcov_Ad,2,mean) # mean of each covariate
mean.mat_Ad = matrix(rep(mean.cov_Ad,nrow(contcov_Ad)),byrow=T,ncol=ncol(contcov_Ad))
sd.cov_Ad = apply(contcov_Ad,2,sd)
sd.mat_Ad = matrix(rep(sd.cov_Ad,nrow(contcov_Ad)),byrow=T,ncol=ncol(contcov_Ad))

stcov_Ad = (contcov_Ad-mean.mat_Ad)/sd.mat_Ad # stcov = standardized covariates values of the continuous individual covariates
dim(stcov_Ad) # 8 continuous covariates
names(stcov_Ad)
#[1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"          
#[4] "Density_km2"             "avgTemp_1M"              "avgWC_1M"               
#[7] "sumRH_month_1M"          "PrepAnom_month_1M"       "sumWS_1M" 

n_Ad <- nrow(contcov_Ad)

dfAd$ObsNum<-as.numeric(dfAd$ObsCatchNr)
str(dfAd)
Site_Ad<-dfAd[,46]
nsites_Ad <- max(Site_Ad)

mydata_Ad<-cbind(r_Ad,stcov_Ad, Site_Ad)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
    
    # composite variable food
    #mu6[i] <- gamma4 * cov[i,1] + gamma5 * cov[i,2] + 1 * cov[i,3] + gamma42 * pow(cov[i,1],2) + gamma52 * pow(cov[i,2],2) + gamma62 * pow(cov[i,3],2)
    #food[i] ~ dnorm(mu6[i], sd=0)
    
    # composite variable weather
    #mu7[i] <- 1 * temp[i] + gamma72 * pow(temp[i],2) +  gamma8 * prec[i] + gamma82 * pow(prec[i],2)
  }
  
  for (i in 1:n){
    #predicted.r[i] <- beta1 * temp[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta12 * pow(temp[i],2) + beta32 * pow(cov[i,1],2) + beta42 * pow(cov[i,2],2) + beta52 * pow(cov[i,3],2) + eps[Site[i]]
    predicted.r[i] <- beta0 + beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * mu6[i] + beta12 * pow(temp[i],2) + beta22 * pow(prec[i],2)
    #predicted.r[i] <- beta1 * mu6[i] + beta2 * mu7[i] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + eps[Site[i]]
    
    
    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0)
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[1] <- 0 # site 1
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta22  ~ dnorm(0,1)
  #beta32  ~ dnorm(0,1)
  #beta42  ~ dnorm(0,1)
  #beta52  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  #sig8x ~ dunif(0,10)
  #sig6x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
  
})

#constants
constants <- list(n=n_Ad, Site=Site_Ad, nsites=nsites_Ad, cov=stcov_Ad)

#data
mydatax_Ad <- list(r=r_Ad)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  #sig8x=runif(1,0,10), 
  #sig6x=runif(1,0,10), 
  sig7x=runif(1,0,10),
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  #beta12=rnorm(1,0,1), beta22=rnorm(1,0,1),
  #beta32=rnorm(1,0,1),beta42=rnorm(1,0,1),beta52=rnorm(1,0,1),
  temp=rnorm(n_Ad,0,1), prec=rnorm(n_Ad,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites_Ad,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)

params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          #"sig8x", "sig6x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax_Ad,
  constants=constants,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 15000, #10000
                       nburnin = 1000, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=TRUE, 
                       summary=F, setSeed=mySeed)

summary(samplesList[["samples"]][,c("beta1")]) #0.11[-0.11,0.35]
summary(samplesList[["samples"]][,c("beta2")]) #0.08[-0.14,0.33]
summary(samplesList[["samples"]][,c("beta3")]) #-0.012[-0.19,0.17]
summary(samplesList[["samples"]][,c("beta4")]) #-0.001[-0.16,0.16]
summary(samplesList[["samples"]][,c("beta5")]) #0.19[0.002,0.37]
summary(samplesList[["samples"]][,c("beta6")]) #0.09[-0.07,0.27]

library(MCMCvis)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalEffectAdults.png", width = 5000, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6'),
         labels = c("Temperature", #expression('Temperature'^2), 
                    "Precipitation", #expression('Precipitation'^2), 
                    "Cockle availability", #expression('Cockle availability'^2), 
                    "Mussel availability", #expression('Mussel availability'^2), 
                    "Grassland proportion", #expression('Grassland proportion'^2)
                    "Conspecific density"
         ),
         main="Effect of environmental variables (standardized) on adult \n(3 & >3 year old birds) condition")
dev.off()

#############################################################################################################################
########################################### only juv and 2 year olds #################################################################################
############################################################################################################################
library(plyr)
dfJuv<-join(Cov, df, by = "WaderID_ObsOcc", type = "left", match = "all")

dfJuv<-subset(dfJuv, Age==1|Age==2) #n=377

################################################## NIMBLE CODE #################################
#response variable, = eta in cr-sem
r_Juv<-dfJuv[,12]

# Other covariates are continuous : condition index
food2_Juv <- subset(dfJuv,
                   select=c(PropGrassland))
food1_Juv<-dfJuv[,44:45]
comp_Juv <- subset(dfJuv,
                  select=c(Density_km2))
weather_Juv<-dfJuv[,34:38] # 1 month
contcov_Juv<-cbind(food1_Juv,food2_Juv,comp_Juv,weather_Juv)
head(contcov_Juv)

# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov_Juv = apply(contcov_Juv,2,mean) # mean of each covariate
mean.mat_Juv = matrix(rep(mean.cov_Juv,nrow(contcov_Juv)),byrow=T,ncol=ncol(contcov_Juv))
sd.cov_Juv = apply(contcov_Juv,2,sd)
sd.mat_Juv = matrix(rep(sd.cov_Juv,nrow(contcov_Juv)),byrow=T,ncol=ncol(contcov_Juv))

stcov_Juv = (contcov_Juv-mean.mat_Juv)/sd.mat_Juv # stcov = standardized covariates values of the continuous individual covariates
dim(stcov_Juv) # 8 continuous covariates
names(stcov_Juv)
#[1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"           "Density_km2"            
#[5] "avgTemp_1M"              "avgWC_1M"                "sumRH_month_1M"          "PrepAnom_month_1M"      
#[9] "sumWS_1M"  

n_Juv <- nrow(contcov_Juv)

dfJuv$ObsNum<-as.numeric(dfJuv$ObsCatchNr)
str(dfJuv)
Site_Juv<-dfJuv[,46]
nsites_Juv <- max(Site_Juv)

mydata_Juv<-cbind(r_Juv,stcov_Juv, Site_Juv)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
    
    # composite variable food
    #mu6[i] <- gamma4 * cov[i,1] + gamma5 * cov[i,2] + 1 * cov[i,3] + gamma42 * pow(cov[i,1],2) + gamma52 * pow(cov[i,2],2) + gamma62 * pow(cov[i,3],2)
    #food[i] ~ dnorm(mu6[i], sd=0)
    
    # composite variable weather
    #mu7[i] <- 1 * temp[i] + gamma72 * pow(temp[i],2) +  gamma8 * prec[i] + gamma82 * pow(prec[i],2)
  }
  
  for (i in 1:n){
    #predicted.r[i] <- beta1 * temp[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta12 * pow(temp[i],2) + beta32 * pow(cov[i,1],2) + beta42 * pow(cov[i,2],2) + beta52 * pow(cov[i,3],2) + eps[Site[i]]
    predicted.r[i] <- beta0 + beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * mu6[i] + beta12 * pow(temp[i],2) + beta22 * pow(prec[i],2)
    #predicted.r[i] <- beta1 * mu6[i] + beta2 * mu7[i] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + eps[Site[i]]
    
    
    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0)
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[1] <- 0 # site 1
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta22  ~ dnorm(0,1)
  #beta32  ~ dnorm(0,1)
  #beta42  ~ dnorm(0,1)
  #beta52  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  #sig8x ~ dunif(0,10)
  #sig6x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
  
})

#constants
constants <- list(n=n_Juv, Site=Site_Juv, nsites=nsites_Juv, cov=stcov_Juv)

#data
mydatax_Juv <- list(r=r_Juv)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  #sig8x=runif(1,0,10), 
  #sig6x=runif(1,0,10), 
  sig7x=runif(1,0,10), 
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  #beta12=rnorm(1,0,1), beta22=rnorm(1,0,1),
  #beta32=rnorm(1,0,1),beta42=rnorm(1,0,1),beta52=rnorm(1,0,1),
  temp=rnorm(n_Juv,0,1), prec=rnorm(n_Juv,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites_Juv,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)

params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          #"sig8x", "sig6x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax_Juv,
  constants=constants,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 15000, #10000
                       nburnin = 1000, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=TRUE, 
                       summary=F, setSeed=mySeed)

summary(samplesList[["samples"]][,c("beta1")]) #-0.009[-0.46,0.43]
summary(samplesList[["samples"]][,c("beta2")]) #0.24[-0.25,0.7]
summary(samplesList[["samples"]][,c("beta3")]) #0.27[-0.1,0.65]
summary(samplesList[["samples"]][,c("beta4")]) #-0.07[-0.47,0.3]
summary(samplesList[["samples"]][,c("beta5")]) #0.43[0.03,0.81]
summary(samplesList[["samples"]][,c("beta6")]) #0.21[-0.12,0.58]

library(MCMCvis)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalEffectJuv.png", width = 5000, height = 5000,units = 'px', res = 600)

MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6'),
         labels = c("Temperature", #expression('Temperature'^2), 
                    "Precipitation", #expression('Precipitation'^2), 
                    "Cockle availability", #expression('Cockle availability'^2), 
                    "Mussel availability", #expression('Mussel availability'^2), 
                    "Grassland proportion", #expression('Grassland proportion'^2)
                    "Conspecific density"
         ),
         main="Effect of environmental variables (standardized) on condition \n(of 1&2 year old birds)")
dev.off()

#############################################################################################################################
########################################### different bill shapes ###########################################################
############################################################################################################################
Cov<-read.csv(file="P:/CHIRP/Carry-over/Analysis/Data/DataAnalysis/StateMatrix2000_2019Cond_3class_9Years_ID_ForEnv.csv")
str(Cov)
hist(Cov$BH)
table(Cov$BillShape)
Cov<-Cov[,6]

Cov<-cbind(ID,Cov)
Cov$BillShape<-Cov$Cov

df$WaderID_ObsOcc<-paste(df$WaderBirdID,df$ObsCatchNr,sep="_")
Cov$WaderID_ObsOcc<-paste(Cov$WaderBirdID,Cov$ObsCatchNr,sep="_")

library(plyr)
df_BillShape<-join(Cov, df, by = "WaderID_ObsOcc", type = "left", match = "all")
df_BillShape$BillShape<-factor(df_BillShape$BillShape)
summary(df_BillShape$BillShape)
#B   H  HB   P  PB  PH 
#361  67  23 373 713  37 


###verdeling leeftijdsklassen, snavelvorm ##
#CovAge<-read.csv(file="N:/Dep.AnE/AnE-share/_Magali/Nimble_M1_P12/StateMatrix2000_2019Cond_3class9Years.csv")
#str(CovAge)
#CovAge$AgeF<-factor(CovAge$AgeN)

#CovAgeF<-CovAge[,30]

#summary(CovAgeF)
#df_BillShape<-cbind(df_BillShape,CovAgeF)

#summary(df_BillShape$BillShape)

#table(df_BillShape$BillShape,df_BillShape$CovAgeF)

#df_BillShapeT<-subset(df_BillShape,
#                      select=c(BillShape,CovAgeF))
#library(dplyr)
#tableBillAge<-df_BillShapeT %>%
#  group_by(BillShape,CovAgeF) %>%
#  summarise(n = n())
#tableBillAge
#    1   2   3   4
#B   56  41   1 263
#H    3   1   1  62
#HB   3   5   0  15
#P   32  34   4 303
#PB 126  68   9 510
#PH   4   4   1  28

#################################################################
############# worm specialists ##################################
dfP<-subset(df_BillShape, BillShape=="P") #n=373

################################################## NIMBLE CODE #################################
#response variable, = eta in cr-sem
r_P<-dfP[,12]

# Other covariates are continuous : condition index
food2_P <- subset(dfP,
                    select=c(PropGrassland))
food1_P<-dfP[,44:45]
comp_P <- subset(dfP,
                   select=c(Density_km2))
weather_P<-dfP[,34:38] # 1 month
contcov_P<-cbind(food1_P,food2_P,comp_P,weather_P)
head(contcov_P)

# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov_P = apply(contcov_P,2,mean) # mean of each covariate
mean.mat_P = matrix(rep(mean.cov_P,nrow(contcov_P)),byrow=T,ncol=ncol(contcov_P))
sd.cov_P = apply(contcov_P,2,sd)
sd.mat_P = matrix(rep(sd.cov_P,nrow(contcov_P)),byrow=T,ncol=ncol(contcov_P))

stcov_P = (contcov_P-mean.mat_P)/sd.mat_P # stcov = standardized covariates values of the continuous individual covariates
dim(stcov_P) # 8 continuous covariates
names(stcov_P)
#1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"          
#[4] "Density_km2"             "avgTemp_1M"              "avgWC_1M"               
#[7] "sumRH_month_1M"          "PrepAnom_month_1M"       "sumWS_1M" 

n_P <- nrow(contcov_P)

dfP$ObsNum<-as.numeric(dfP$ObsCatchNr)
str(dfP)
Site_P<-dfP[,46]
nsites_P <- max(Site_P)

mydata_P<-cbind(r_P,stcov_P, Site_P)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
    
    # composite variable food
    #mu6[i] <- gamma4 * cov[i,1] + gamma5 * cov[i,2] + 1 * cov[i,3] + gamma42 * pow(cov[i,1],2) + gamma52 * pow(cov[i,2],2) + gamma62 * pow(cov[i,3],2)
    #food[i] ~ dnorm(mu6[i], sd=0)
    
    # composite variable weather
    #mu7[i] <- 1 * temp[i] + gamma72 * pow(temp[i],2) +  gamma8 * prec[i] + gamma82 * pow(prec[i],2)
  }
  
  for (i in 1:n){
    #predicted.r[i] <- beta1 * temp[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta12 * pow(temp[i],2) + beta32 * pow(cov[i,1],2) + beta42 * pow(cov[i,2],2) + beta52 * pow(cov[i,3],2) + eps[Site[i]]
    predicted.r[i] <- beta0 + beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * mu6[i] + beta12 * pow(temp[i],2) + beta22 * pow(prec[i],2)
    #predicted.r[i] <- beta1 * mu6[i] + beta2 * mu7[i] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + eps[Site[i]]
    
    
    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0)
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[1] <- 0 # site 1
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta22  ~ dnorm(0,1)
  #beta32  ~ dnorm(0,1)
  #beta42  ~ dnorm(0,1)
  #beta52  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  #sig8x ~ dunif(0,10)
  #sig6x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
})

#constants
constants <- list(n=n_P, Site=Site_P, nsites=nsites_P, cov=stcov_P)

#data
mydatax_P <- list(r=r_P)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  #sig8x=runif(1,0,10), 
  #sig6x=runif(1,0,10), 
  sig7x=runif(1,0,10),
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  #beta12=rnorm(1,0,1), beta22=rnorm(1,0,1),
  #beta32=rnorm(1,0,1),beta42=rnorm(1,0,1),beta52=rnorm(1,0,1),
  temp=rnorm(n_P,0,1), prec=rnorm(n_P,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites_P,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)

params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          #"sig8x", "sig6x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax_P,
  constants=constants,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 15000, #10000
                       nburnin = 1000, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=TRUE, 
                       summary=F, setSeed = mySeed)

summary(samplesList[["samples"]][,c("beta1")]) #0.22[-0.12,0.58]
summary(samplesList[["samples"]][,c("beta2")]) #0.09[-0.24,0.44]
summary(samplesList[["samples"]][,c("beta3")]) #-0.09[-0.36,0.16]
summary(samplesList[["samples"]][,c("beta4")]) #-0.08[-0.34,0.19]
summary(samplesList[["samples"]][,c("beta5")]) #0.29[0.03,0.57]
summary(samplesList[["samples"]][,c("beta6")]) #0.13[-0.13,0.37]

#### calculate probability of being strictly pos/neg
round(mean(beta1>0),2) #0.82, temp
round(mean(beta2>0),2) #0.63, prec
round(mean(beta3>0),2) #0.81, cockle
round(mean(beta4>0),2) #0.58, mussel
round(mean(beta5>0),2) #0.98, grassland
round(mean(beta6>0),2) #0.90, density

library(MCMCvis)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalEffectWormSpec.png", width = 5000, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6'),
         labels = c("Temperature", #expression('Temperature'^2), 
                    "Precipitation", #expression('Precipitation'^2), 
                    "Cockle availability", #expression('Cockle availability'^2), 
                    "Mussel availability", #expression('Mussel availability'^2), 
                    "Grassland proportion", #expression('Grassland proportion'^2)
                    "Conspecific density"
         ),
         main="Effect of environmental variables (standardized) on \ncondition (for worm specialists)")
dev.off()

#######################################################
#### shellfish specialists ############################

summary(df_BillShape$BillShape)

dfB<-subset(df_BillShape, BillShape=="B"|BillShape=="H"|BillShape=="HB") #n=451

################################################## NIMBLE CODE #################################
#### try nimble approach with latent variable ###

#response variable, = eta in cr-sem
r_B<-dfB[,12]

# Other covariates are continuous : condition index
food2_B <- subset(dfB,
                  select=c(PropGrassland))
food1_B<-dfB[,44:45]
comp_B <- subset(dfB,
                 select=c(Density_km2))
weather_B<-dfB[,34:38] # 1 month
contcov_B<-cbind(food1_B,food2_B,comp_B,weather_B)
head(contcov_B)

# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov_B = apply(contcov_B,2,mean) # mean of each covariate
mean.mat_B = matrix(rep(mean.cov_B,nrow(contcov_B)),byrow=T,ncol=ncol(contcov_B))
sd.cov_B = apply(contcov_B,2,sd)
sd.mat_B = matrix(rep(sd.cov_B,nrow(contcov_B)),byrow=T,ncol=ncol(contcov_B))

stcov_B = (contcov_B-mean.mat_B)/sd.mat_B # stcov = standardized covariates values of the continuous individual covariates
dim(stcov_B) # 8 continuous covariates
names(stcov_B)
#[1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"          
#[4] "Density_km2"             "avgTemp_1M"              "avgWC_1M"               
#[7] "sumRH_month_1M"          "PrepAnom_month_1M"       "sumWS_1M" 

n_B <- nrow(contcov_B)

dfB$ObsNum<-as.numeric(dfB$ObsCatchNr)
str(dfB)
Site_B<-dfB[,46]
nsites_B <- max(Site_B)

mydata_B<-cbind(r_B,stcov_B, Site_B)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
    
    # composite variable food
    #mu6[i] <- gamma4 * cov[i,1] + gamma5 * cov[i,2] + 1 * cov[i,3] + gamma42 * pow(cov[i,1],2) + gamma52 * pow(cov[i,2],2) + gamma62 * pow(cov[i,3],2)
    #food[i] ~ dnorm(mu6[i], sd=0)
    
    # composite variable weather
    #mu7[i] <- 1 * temp[i] + gamma72 * pow(temp[i],2) +  gamma8 * prec[i] + gamma82 * pow(prec[i],2)
  }
  
  for (i in 1:n){
    #predicted.r[i] <- beta1 * temp[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta12 * pow(temp[i],2) + beta32 * pow(cov[i,1],2) + beta42 * pow(cov[i,2],2) + beta52 * pow(cov[i,3],2) + eps[Site[i]]
    predicted.r[i] <- beta0 + beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * mu6[i] + beta12 * pow(temp[i],2) + beta22 * pow(prec[i],2)
    #predicted.r[i] <- beta1 * mu6[i] + beta2 * mu7[i] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + eps[Site[i]]
    
    
    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0)
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[1] <- 0 # site 1
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta22  ~ dnorm(0,1)
  #beta32  ~ dnorm(0,1)
  #beta42  ~ dnorm(0,1)
  #beta52  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  #sig8x ~ dunif(0,10)
  #sig6x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
})

#constants
constants <- list(n=n_B, Site=Site_B, nsites=nsites_B, cov=stcov_B)

#data
mydatax_B <- list(r=r_B)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  #sig8x=runif(1,0,10), 
  #sig6x=runif(1,0,10), 
  sig7x=runif(1,0,10), 
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  #beta12=rnorm(1,0,1), beta22=rnorm(1,0,1),
  #beta32=rnorm(1,0,1),beta42=rnorm(1,0,1),beta52=rnorm(1,0,1),
  temp=rnorm(n_B,0,1), prec=rnorm(n_B,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites_B,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)

params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          #"sig8x", "sig6x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax_B,
  constants=constants,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 15000, #10000
                       nburnin = 1000, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=TRUE, summary=F,
                       setSeed = mySeed)

summary(samplesList[["samples"]][,c("beta1")]) #-0.03[-0.31,0.27]
summary(samplesList[["samples"]][,c("beta2")]) #0.24[-0.08,0.52]
summary(samplesList[["samples"]][,c("beta3")]) #0.1[-0.13,0.33]
summary(samplesList[["samples"]][,c("beta4")]) #-0.02[-0.24,0.20]
summary(samplesList[["samples"]][,c("beta5")]) #0.2[-0.2,0.42]
summary(samplesList[["samples"]][,c("beta6")]) #0.05[-0.17,0.28]

library(MCMCvis)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalEffectShellfishSpec.png", width = 5000, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6'),
         labels = c("Temperature", #expression('Temperature'^2), 
                    "Precipitation", #expression('Precipitation'^2), 
                    "Cockle availability", #expression('Cockle availability'^2), 
                    "Mussel availability", #expression('Mussel availability'^2), 
                    "Grassland proportion", #expression('Grassland proportion'^2)
                    "Conspecific density"
         ),
         main="Effect of environmental variables (standardized) on \ncondition (of shellfish specialists)")
dev.off()
#######################################################
#### generalist feeders ############################

summary(df_BillShape$BillShape)

dfI<-subset(df_BillShape, BillShape=="PB"|BillShape=="PH") #n=750

################################################## NIMBLE CODE #################################

#response variable, = eta in cr-sem
r_I<-dfI[,12]

# Other covariates are continuous : condition index
food2_I <- subset(dfI,
                  select=c(PropGrassland))
food1_I<-dfI[,44:45]
comp_I <- subset(dfI,
                 select=c(Density_km2))
weather_I<-dfI[,34:38] # 1 month
contcov_I<-cbind(food1_I,food2_I,comp_I,weather_I)
head(contcov_I)

# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov_I = apply(contcov_I,2,mean) # mean of each covariate
mean.mat_I = matrix(rep(mean.cov_I,nrow(contcov_I)),byrow=T,ncol=ncol(contcov_I))
sd.cov_I = apply(contcov_I,2,sd)
sd.mat_I = matrix(rep(sd.cov_I,nrow(contcov_I)),byrow=T,ncol=ncol(contcov_I))

stcov_I = (contcov_I-mean.mat_I)/sd.mat_I # stcov = standardized covariates values of the continuous individual covariates
dim(stcov_I) # 8 continuous covariates
names(stcov_I)
#[1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"          
#[4] "Density_km2"             "avgTemp_1M"              "avgWC_1M"               
#[7] "sumRH_month_1M"          "PrepAnom_month_1M"       "sumWS_1M"  

n_I <- nrow(contcov_I)

dfI$ObsNum<-as.numeric(dfI$ObsCatchNr)
str(dfI)
Site_I<-dfI[,46]
nsites_I <- max(Site_I)

mydata_I<-cbind(r_I,stcov_I, Site_I)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
    
    # composite variable food
    #mu6[i] <- gamma4 * cov[i,1] + gamma5 * cov[i,2] + 1 * cov[i,3] + gamma42 * pow(cov[i,1],2) + gamma52 * pow(cov[i,2],2) + gamma62 * pow(cov[i,3],2)
    #food[i] ~ dnorm(mu6[i], sd=0)
    
    # composite variable weather
    #mu7[i] <- 1 * temp[i] + gamma72 * pow(temp[i],2) +  gamma8 * prec[i] + gamma82 * pow(prec[i],2)
  }
  
  for (i in 1:n){
    #predicted.r[i] <- beta1 * temp[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta12 * pow(temp[i],2) + beta32 * pow(cov[i,1],2) + beta42 * pow(cov[i,2],2) + beta52 * pow(cov[i,3],2) + eps[Site[i]]
    predicted.r[i] <- beta0 + beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * mu6[i] + beta12 * pow(temp[i],2) + beta22 * pow(prec[i],2)
    #predicted.r[i] <- beta1 * mu6[i] + beta2 * mu7[i] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + eps[Site[i]]
    
    
    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0)
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[1] <- 0 # site 1
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta22  ~ dnorm(0,1)
  #beta32  ~ dnorm(0,1)
  #beta42  ~ dnorm(0,1)
  #beta52  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  #sig8x ~ dunif(0,10)
  #sig6x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
})

#constants
constants <- list(n=n_I, Site=Site_I, nsites=nsites_I, cov=stcov_I)

#data
mydatax_I <- list(r=r_I)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  #sig8x=runif(1,0,10), 
  #sig6x=runif(1,0,10), 
  sig7x=runif(1,0,10),
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  #beta12=rnorm(1,0,1), beta22=rnorm(1,0,1),
  #beta32=rnorm(1,0,1),beta42=rnorm(1,0,1),beta52=rnorm(1,0,1),
  temp=rnorm(n_I,0,1), prec=rnorm(n_I,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites_I,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)

params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          #"sig8x", "sig6x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax_I,
  constants=constants,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 15000, #10000
                       nburnin = 1000, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=TRUE, summary=F,
                       setSeed = mySeed)

summary(samplesList[["samples"]][,c("beta1")]) #0.11[-0.14,0.35]
summary(samplesList[["samples"]][,c("beta2")]) #0.08[-0.16,0.33]
summary(samplesList[["samples"]][,c("beta3")]) #-0.24[-0.06,0.10]
summary(samplesList[["samples"]][,c("beta4")]) #-0.07[-0.23,0.12]
summary(samplesList[["samples"]][,c("beta5")]) #1.17[-0.03,0.36]

library(MCMCvis)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalEffectIntFeeder.png", width = 5000, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6'),
         labels = c("Temperature", #expression('Temperature'^2), 
                    "Precipitation", #expression('Precipitation'^2), 
                    "Cockle availability", #expression('Cockle availability'^2), 
                    "Mussel availability", #expression('Mussel availability'^2), 
                    "Grassland proportion", #expression('Grassland proportion'^2)
                    "Conspecific density"
         ),
         main="Effect of environmental variables (standardized) on \ncondition (of generalist feeders)")
dev.off()
########################################################################################################
############################## supplements ############################################################
########################################################################################################
# environmental variables per period without regression line
str(df$YearShort)
df$Period<-ifelse(df$YearShort=="2001", 1,
                  ifelse(df$YearShort=="2002",1,
                         ifelse(df$YearShort=="2003",1,2)))
df$Period<-factor(df$Period)
head(df$Period)

#' Now we calculate posterior mean and the credible interval. Note the ordering.
dfT <- data.frame(t = samplesSummaryTemp[,1][ord],
                  cond = mean_conditionT[ord],
                  lciT = lciT[ord],
                  uciT = uciT[ord],
                  P=df$Period[ord])
head(dfT)

#' Now time to visualize. 
p_T<-ggplot(dfT, aes(x = t, y = cond)) + 
  geom_point(mapping=aes(x=samplesSummaryTemp[,1],y=samplesSummaryPredictedCond[,1], col=P), alpha=0.5, cex=3)+
  #geom_line(data=dfT,mapping=aes(x = t, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  #geom_line(data=dfT,mapping=aes(x = t, y = cond), cex=1.2) + 
  #geom_ribbon(data=dfT,aes(ymin = lciT2, ymax = uciT2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfT,aes(ymin = lciT, ymax = uciT), alpha = 0.25)+
  labs(x = "temperature (standardized)", y = "body condition")+
  theme_bw()+
  theme(legend.position="right", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16))+
  labs(col = "Period")+
  scale_colour_discrete(labels = c("2001-2003", "2016-2017"))
p_T


dfP <- data.frame(p = samplesSummaryPrec[,1][ord],
                  cond = mean_conditionP[ord],
                  lciP = lciP[ord],
                  uciP = uciP[ord],
                  P=df$Period[ord])
head(dfP)

p_P<-ggplot(dfP, aes(x = p, y = cond)) + 
  geom_point(mapping=aes(x=samplesSummaryPrec[,1],y=samplesSummaryPredictedCond[,1], col=P), alpha=0.5, cex=3)+
  #geom_line(data=dfP2,mapping=aes(x = p, y = cond, col="#E69F00"),cex=1.2) + 
  #geom_line(data=dfP,mapping=aes(x = p, y = cond), cex=1.2) + 
  #geom_ribbon(data=dfP2,aes(ymin = lciP2, ymax = uciP2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfP,aes(ymin = lciP, ymax = uciP), alpha = 0.25)+
  labs(x = "precipitation (standardized)", y = "body condition")+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  labs(col = "Period")+
  scale_colour_discrete(labels = c("2001-2003", "2016-2017"))
#scale_color_identity(name = "Term",
#                     labels = c("linear", "quadratic"),
#                     breaks = c("#56B4E9", "#E69F00"),
#                     guide = "legend")+
 #guides(colour = guide_legend(override.aes = list(size=4)))
p_P


dfC <- data.frame(c = contcov[,1][ord], #stcov for standardized
                  cond = mean_conditionC[ord],
                  lciC = lciC[ord],
                  uciC = uciC[ord],
                  P=df$Period[ord])
head(dfC)

p_C<-ggplot(dfC, aes(x = c, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,1],y=samplesSummaryPredictedCond[,1], col=P), alpha=0.5, cex=3)+ #stcov for standardized plot
  #geom_line(data=dfC2,mapping=aes(x = c, y = cond),cex=1.2, linetype="dotted") + 
  #geom_line(data=dfC,mapping=aes(x = c, y = cond), cex=1.2) + 
  #geom_ribbon(data=dfC2,aes(ymin = lciC2, ymax = uciC2), alpha = 0.25)+
  #geom_ribbon(data=dfC,aes(ymin = lciC, ymax = uciC), alpha = 0.25)+
  labs(x = "cockle abundance (g AFDM/m2)", y = "body condition")+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16))+
  labs(col = "Period")+
  scale_colour_discrete(labels = c("2001-2003", "2016-2017"))
#axis.title.y=element_blank())+
#scale_color_identity(name = "Term",
#                     labels = c("linear", "quadratic"),
#                     breaks = c("#56B4E9", "#E69F00"),
#                     guide = "legend")+
#guides(colour = guide_legend(override.aes = list(size=4)))
p_C

dfM <- data.frame(m = contcov[,2][ord], #stcov for standardized
                  cond = mean_conditionM[ord],
                  lciM = lciM[ord],
                  uciM = uciM[ord],
                  P=df$Period[ord])
head(dfM)

p_M<-ggplot(dfM, aes(x = m, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,2],y=samplesSummaryPredictedCond[,1], col=P), alpha=0.5, cex=3)+
  #geom_line(data=dfM2,mapping=aes(x = m, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  #geom_line(data=dfM,mapping=aes(x = m, y = cond), cex=1.2) + 
  #geom_ribbon(data=dfM2,aes(ymin = lciM2, ymax = uciM2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfM,aes(ymin = lciM, ymax = uciM), alpha = 0.25)+
  labs(x = "mussel abundance (g AFDM/m2)", y = "body condition")+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  labs(col = "Period")+
  scale_colour_discrete(labels = c("2001-2003", "2016-2017"))
#scale_color_identity(name = "Term",
#                     labels = c("linear", "quadratic"),
#                     breaks = c("#56B4E9", "#E69F00"),
#                     guide = "legend")+
#guides(colour = guide_legend(override.aes = list(size=4)))
p_M


dfG <- data.frame(g = contcov[,3][ord], #stcov for standardized
                  cond = mean_conditionG[ord],
                  lciG = lciG[ord],
                  uciG = uciG[ord],
                  P=df$Period[ord])
head(dfG)

p_G<-ggplot(dfG, aes(x = g, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,3],y=samplesSummaryPredictedCond[,1], col=P), alpha=0.5, cex=3)+ #stcov for standardized
  #geom_line(data=dfG2,mapping=aes(x = g, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  #geom_line(data=dfG,mapping=aes(x = g, y = cond), cex=1.2) + 
  #geom_ribbon(data=dfG2,aes(ymin = lciG2, ymax = uciG2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfG,aes(ymin = lciG, ymax = uciG), alpha = 0.25)+
  labs(x = "grassland proportion", y = "body condition")+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  labs(col = "Period")+
  scale_colour_discrete(labels = c("2001-2003", "2016-2017"))
#scale_color_identity(name = "Term",
##                     labels = c("linear", "quadratic"),
#                     breaks = c("#56B4E9", "#E69F00"),
#                     guide = "legend")+
#guides(colour = guide_legend(override.aes = list(size=4)))
p_G

dfD <- data.frame(d = contcov[,4][ord], #stcov for standardized
                  cond = mean_conditionD[ord],
                  lciD = lciD[ord],
                  uciD = uciD[ord],
                  P=df$Period[ord])
head(dfD)

p_D<-ggplot(dfD, aes(x = d, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,3],y=samplesSummaryPredictedCond[,1], col=P), alpha=0.5, cex=3)+ #stcov for standardized
  #geom_line(data=dfG2,mapping=aes(x = g, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  #geom_line(data=dfG,mapping=aes(x = g, y = cond), cex=1.2) + 
  #geom_ribbon(data=dfG2,aes(ymin = lciG2, ymax = uciG2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfG,aes(ymin = lciG, ymax = uciG), alpha = 0.25)+
  labs(x = "oystercatcher density (ind/km2)", y = "body condition")+
  theme_bw()+
  theme(legend.position="none", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  labs(col = "Period")+
  scale_colour_discrete(labels = c("2001-2003", "2016-2017"))
#scale_color_identity(name = "Term",
##                     labels = c("linear", "quadratic"),
#                     breaks = c("#56B4E9", "#E69F00"),
#                     guide = "legend")+
#guides(colour = guide_legend(override.aes = list(size=4)))
p_D

############# combine plots #############
library(patchwork)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalVariablesBothPeriods.png", width = 8500, height = 5000,units = 'px', res = 600)
patchwork <- (p_T|p_P|p_D)/(p_C|p_M|p_G)
patchwork<-patchwork +  plot_layout(guide='collect') & theme(legend.position = 'bottom')
patchwork+plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 18))
dev.off() 

##########################################################################################################
############################# supplements food sample size ###############################################
##########################################################################################################
 # cockles and mussels

p_C_Ss<-ggplot(dfC, aes(x = c, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,1],y=samplesSummaryPredictedCond[,1],color=df$StateText,shape=df$YearShort, size= df$NrSamplesFood_C), alpha=0.5)+ #stcov for standardized plot
  #geom_line(data=dfC2,mapping=aes(x = c, y = cond),cex=1.2, linetype="dotted") + 
  #geom_line(data=dfC,mapping=aes(x = c, y = cond),linetype="dashed", cex=1.2) + 
  #geom_ribbon(data=dfC2,aes(ymin = lciC2, ymax = uciC2), alpha = 0.25)+
  #geom_ribbon(data=dfC,aes(ymin = lciC, ymax = uciC), alpha = 0.25)+
  labs(x = "cockle abundance (g AFDM/m2)", y = "body condition", tag="a")+
  theme_bw()+
  ylim(-1.32,1.23)+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  scale_size(name = "Number food samples taken (cockles)")+
  theme(legend.position="right", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16))
  #annotation_custom(rasterGrob(imgCock, 
  #                             width = unit(1.5,"npc"),
  #                             height = unit(1.5,"npc")), 
  #                  246,303, .82, 1.1)
p_C_Ss

## mussels
p_M_Ss<-ggplot(dfM, aes(x = m, y = cond)) + 
  geom_point(mapping=aes(x=contcov[,2],y=samplesSummaryPredictedCond[,1], color=df$StateText,shape=df$YearShort, size=df$NrSamplesFood_M), alpha=0.5)+
  #geom_line(data=dfM2,mapping=aes(x = m, y = cond, col="#E69F00"),cex=1.2, linetype="dotted") + 
  #geom_line(data=dfM,mapping=aes(x = m, y = cond), linetype="dashed",cex=1.2) + 
  #geom_ribbon(data=dfM2,aes(ymin = lciM2, ymax = uciM2), fill = "#E69F00", alpha = 0.25)+
  #geom_ribbon(data=dfM,aes(ymin = lciM, ymax = uciM), alpha = 0.25)+
  labs(x = "mussel abundance (gg AFDM/m2)", y = "body condition", tag="b")+
  theme_bw()+
  ylim(-1.32,1.23)+
  scale_color_manual(name = "State (tidal basin)", 
                     #labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018", "test"),
                     values=c("#E69F00", "#56B4E9",  "#009E73", "#0072B2","#D55E00", "#CC79A7")) +
  scale_shape_manual(name = "Year (winter)", 
                     labels = c("2000-2001", "2001-2002", "2002-2003", "2016-2017", "2017-2018"),
                     values = c(15, 8,16, 17,18))+
  scale_size(name = "Number food samples taken (mussels)")+
  theme(legend.position="right", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=16),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())
  #annotation_custom(rasterGrob(imgMussel, 
  #                             width = unit(1.5,"npc"),
  #                             height = unit(1.5,"npc")), 
  #                  2600,3050, .89, 1.1)
p_M_Ss


############# combine plots #############
library(patchwork)
png("P:/CHIRP/Carry-over/Analysis/Figures/Supplements/Environment/EnvironmentalVariablesFoodSamples.png", width = 9000, height = 4000,units = 'px', res = 600)
patchwork <- (p_C_Ss|p_M_Ss)
patchwork<-patchwork +  plot_layout(guide='collect') & theme(legend.position = 'right')
patchwork+plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 18))
dev.off() 

library(lmerTest)
library(lme4)
summary(lmer(Mean~AFDMgrm2_C_CondLoss_exp+(1|ObsCatchNr), data=df))

df_test<-subset(df, NrSamplesFood_C>20) # remove low sample size
summary(lmer(Mean~AFDMgrm2_C_CondLoss_exp+(1|ObsCatchNr), data=df_test))

summary(lmer(Mean~AFDMgrm2_M_CondLoss_exp+(1|ObsCatchNr), data=df))
df_testM<-subset(df, NrSamplesFood_M>20) # remove low sample size
summary(lmer(Mean~AFDMgrm2_M_CondLoss_exp+(1|ObsCatchNr), data=df_testM))

################################################
###### rerun with at least 20 samples for food!
################################################
df_ss<-subset(df, NrSamplesFood_C>20&NrSamplesFood_C>20) # remove low sample size

#response variable, = eta in cr-sem
r_ss<-df_ss[,2]

# Other covariates are continuous : condition index
food2_ss <- subset(df_ss,
                  select=c(PropGrassland))
food1_ss<-df_ss[,34:35]
comp_ss <- subset(df_ss,
                 select=c(Density_km2))
weather_ss<-df_ss[,24:28] # 1 month
contcov_ss<-cbind(food1_ss,food2_ss,comp_ss,weather_ss)
head(contcov_ss)

# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov_ss = apply(contcov_ss,2,mean) # mean of each covariate
mean.mat_ss = matrix(rep(mean.cov_ss,nrow(contcov_ss)),byrow=T,ncol=ncol(contcov_ss))
sd.cov_ss = apply(contcov_ss,2,sd)
sd.mat_ss = matrix(rep(sd.cov_ss,nrow(contcov_ss)),byrow=T,ncol=ncol(contcov_ss))

stcov_ss = (contcov_ss-mean.mat_ss)/sd.mat_ss # stcov = standardized covariates values of the continuous individual covariates
dim(stcov_ss) # 8 continuous covariates
names(stcov_ss)
#[1] "AFDMgrm2_C_CondLoss_exp" "AFDMgrm2_M_CondLoss_exp" "PropGrassland"          
#[4] "Density_km2"             "avgTemp_1M"              "avgWC_1M"               
#[7] "sumRH_month_1M"          "PrepAnom_month_1M"       "sumWS_1M"   

n <- nrow(contcov_ss)

df_ss$ObsNum<-as.numeric(df_ss$ObsCatchNr)
Site_ss<-df_ss[,36]
nsites_ss <- max(Site_ss)

mydata_ss<-cbind(r_ss,stcov_ss, Site_ss)

#### run model 
library(nimble)
NimbleCode<-nimbleCode({
  
  for (i in 1:n){
    # latent variable temperature
    mu1[i] <- temp[i]
    cov[i,5] ~ dnorm(mu1[i],sd=sig5x)
    
    mu2[i] <- gamma1 * temp[i]
    cov[i,6] ~ dnorm(mu2[i],sd=sig5x)
    
    mu3[i] <- gamma2 * temp[i]
    cov[i,9] ~ dnorm(mu3[i],sd=sig9x)
    
    # latent variable precipitation
    mu4[i] <-  prec[i]
    cov[i,7] ~ dnorm(mu4[i],sd=sig7x)
    
    mu5[i] <- gamma3 * prec[i]
    cov[i,8] ~ dnorm(mu5[i],sd=sig7x)
    
    # composite variable food
    #mu6[i] <- gamma4 * cov[i,1] + gamma5 * cov[i,2] + 1 * cov[i,3] + gamma42 * pow(cov[i,1],2) + gamma52 * pow(cov[i,2],2) + gamma62 * pow(cov[i,3],2)
    #food[i] ~ dnorm(mu6[i], sd=0)
    
    # composite variable weather
    #mu7[i] <- 1 * temp[i] + gamma72 * pow(temp[i],2) +  gamma8 * prec[i] + gamma82 * pow(prec[i],2)
  }
  
  for (i in 1:n){
    #predicted.r[i] <- beta1 * temp[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta12 * pow(temp[i],2) + beta32 * pow(cov[i,1],2) + beta42 * pow(cov[i,2],2) + beta52 * pow(cov[i,3],2) + eps[Site[i]]
    predicted.r[i] <- beta0 + beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + beta6 * cov[i,4] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * mu6[i] + beta12 * pow(temp[i],2) + beta22 * pow(prec[i],2)
    #predicted.r[i] <- beta1 * mu6[i] + beta2 * mu7[i] + eps[Site[i]]
    #predicted.r[i] <- beta1 * temp[i] + beta2 * prec[i] + beta3 * cov[i,1] + beta4 * cov[i,2] + beta5 * cov[i,3] + eps[Site[i]]
    
    
    r[i] ~ dnorm(predicted.r[i], sd = sig.r)
  } #i
  
  # prior random site effect
  for (j in 1:nsites-1){ #site 36 is reference level (=beta0)
    eps[j] ~ dnorm(mu.eps, sd=sig.eps) #
  }
  # fix the effect of the first level (reference group: eps1) to 0 because we use intercept beta0
  #eps[1] <- 0 # site 1
  
  #priors for continuous fixed effects and latent variable
  for (i in 1:n){
    temp[i] ~ dnorm(0, sd=sig.temp)
    prec[i] ~ dnorm(0, sd=sig.prec)
    cov[i,1] ~ dnorm(0, sd=sig1x)
    cov[i,2] ~ dnorm(0, sd=sig2x)
    cov[i,3] ~ dnorm(0, sd=sig3x)
    cov[i,4] ~ dnorm(0, sd=sig4x)
    
  } 
  
  #priors
  beta0 ~ dnorm(0,1)
  gamma1 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma4 ~ dnorm(0,1)
  #gamma42 ~ dnorm(0,1)
  #gamma5 ~ dnorm(0,1)
  #gamma52 ~ dnorm(0,1)
  #gamma62 ~ dnorm(0,1)
  #gamma72 ~ dnorm(0,1)
  #gamma8 ~ dnorm(0,1)
  #gamma82 ~ dnorm(0,1)
  #beta0 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  beta3  ~ dnorm(0,1)
  beta4  ~ dnorm(0,1)
  beta5  ~ dnorm(0,1)
  beta6  ~ dnorm(0,1)
  #beta12  ~ dnorm(0,1)
  #beta22  ~ dnorm(0,1)
  #beta32  ~ dnorm(0,1)
  #beta42  ~ dnorm(0,1)
  #beta52  ~ dnorm(0,1)
  sig1x ~ dunif(0,10)
  sig2x ~ dunif(0,10)
  sig3x ~ dunif(0,10)
  sig4x ~ dunif(0,10)
  sig5x ~ dunif(0,10)
  #sig8x ~ dunif(0,10)
  #sig6x ~ dunif(0,10)
  sig7x ~ dunif(0,10)
  sig9x ~ dunif(0,10)
  sig.temp ~ dunif(0,10)
  sig.r ~ dunif(0,10)
  sig.prec ~ dunif(0,10)
  mu.eps ~ dnorm(0,1)
  sig.eps ~ dunif(0,10)
  
})

#constants
constants_ss <- list(n=n, Site=Site_ss, nsites=nsites_ss, cov=stcov_ss)

#data
mydatax_ss <- list(r=r_ss)

mySeed <- 124
set.seed(mySeed)

# Initial values
initsFunction <- function() list(
  gamma1=rnorm(1,0,1), gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), 
  #gamma4=rnorm(1,0,1), gamma5=rnorm(1,0,1), #gamma42=rnorm(1,0,1), gamma52=rnorm(1,0,1), 
  #gamma62=rnorm(1,0,1), gamma72=rnorm(1,0,1), 
  #gamma8=rnorm(1,0,1), gamma82=rnorm(1,0,1), 
  sig1x=runif(1,0,10), sig2x=runif(1,0,10), sig3x=runif(1,0,10),
  sig4x=runif(1,0,10), sig5x=runif(1,0,10), 
  #sig8x=runif(1,0,10), 
  #sig6x=runif(1,0,10), 
  sig7x=runif(1,0,10), 
  sig9x=runif(1,0,10), 
  sig.r=runif(1,0,10),
  beta0=rnorm(1,0,1),
  beta1=rnorm(1,0,1), #beta0=rnorm(1,0,1),
  beta2=rnorm(1,0,1),
  beta3=rnorm(1,0,1),
  beta4=rnorm(1,0,1),
  beta5=rnorm(1,0,1),
  #beta12=rnorm(1,0,1), beta22=rnorm(1,0,1),
  #beta32=rnorm(1,0,1),beta42=rnorm(1,0,1),beta52=rnorm(1,0,1),
  temp=rnorm(n,0,1), prec=rnorm(n,0,1),
  sig.temp=runif(1,0,10), sig.prec=runif(1,0,10), 
  sig.eps=runif(1,0,10),
  mu.eps=rnorm(1,0,1) , eps=rnorm(nsites,0,1)
  #food=rnorm(n,0,1)
  #r=rnorm(56664,0,1)
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)


params<-c("cov",
          "gamma1", "gamma2", "gamma3",
          #"gamma4","gamma5",
          #"gamma42","gamma52",
          #"gamma62","gamma72",
          #"gamma8","gamma82",
          "beta0",
          "beta1", "beta2",
          "beta3",
          "beta4","beta5",
          "beta6",
          #"beta12", "beta22",
          #"beta32","beta42","beta52",
          "sig1x","sig2x","sig3x",
          "sig4x", "sig5x", 
          #"sig8x", "sig6x",
          "sig7x", 
          "sig9x",
          "eps",
          "temp", "prec", 
          "sig.temp", "sig.prec", 
          "sig.r", "mu.eps", "sig.eps",
          "predicted.r")

RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax_ss,
  constants=constants_ss,
  inits = initVals, 
  calculate=F)

Cmodel <- compileNimble(RModel)

conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE

Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)

Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)

memory.limit()
memory.limit(80000)
samplesList <- runMCMC(Cmcmc,
                       niter = 15000, #10000
                       nburnin = 1000, #1000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=TRUE, summary=F)

library(MCMCvis)
png("YOUR_PATH/EnvironmentalEffectFoodSampleSize.png", width = 5000, height = 5000,units = 'px', res = 600)
MCMCplot(samplesList$samples, params =c('beta1', 'beta2',  'beta3', 'beta4','beta5', 'beta6'),
         labels = c("Temperature", #expression('Temperature'^2), 
                    "Precipitation", #expression('Precipitation'^2), 
                    "Cockle availability", #expression('Cockle availability'^2), 
                    "Mussel availability", #expression('Mussel availability'^2), 
                    "Grassland proportion", #expression('Grassland proportion'^2)
                    "Conspecific density"
         ),
         main="Effect of environmental variables (standardized) on \ncondition (excluding food samples <20 within a buffer)")
dev.off()

