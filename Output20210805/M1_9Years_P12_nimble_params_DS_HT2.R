### M1 nimble, with only 9 years of capture history for both periods
library(nimble)
library(igraph)
library(coda)
library(R6)

setwd("~/Analysis/Nimble_M1_P12")
#setwd("P:/CHIRP/JAGS/JagsModels/M1_4Years_Period2")
#load('NimbleModel_M1_P12_params_5000iter.RData')
load('NimbleModel_M1_P12_params_10000iter.RData')

# download and install RTools40: https://cran.r-project.org/bin/windows/Rtools/
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

d <- read.csv(file="StateMatrix2000_2019Cond_3class9Years.csv")
str(d)
CH<-d[,12:29]
str(CH)
# one digit per capture occasion
# 1 - 9 for live captures based on site
# 10 for dead recovery
# 0 not capture

# calculate useful quantities
n <- nrow(CH) #number of individuals
T <- ncol(CH) # number of sampling occasions
# first and last captures for censored individuals
first <- NULL # time of first capture
last <- NULL # recovery time or last occasion of the study (if not recovered dead)

for(i in 1:n){
  first <- c( first, min(which(CH[i,]>0)) )
  last <- c( last, min(which(CH[i,]==10),T) )
}

# Index for summers and winters
SW <- WS <- rep(0,T) 
SW[seq(1,T,by=2)] <- 1 # SW equals 1 in summer, 0 in winter
WS[seq(2,T,by=2)  ] <- 1 # WS equals 1 in winter, 0 in summer

## Individual covariates - individuals (lines) in same order than CH!
COV<-d[,1:11]
str(COV)
summary(COV)
# Sex is a categorial and time-invariant covariate
Sex <- COV$SexN #1=female, 2= male

# AgeH (age at Handling) is a categorial variable for age at condition measurement
length(d$AgeN[d$AgeN==1]) #n=244
length(d$AgeN[d$AgeN==2]) #n=153
length(d$AgeN[d$AgeN==3]) #n=16
length(d$AgeN[d$AgeN==4]) #n=1181
AgeH <- COV$AgeN
AgeH[AgeH==1] <- 1 #1st year=1
AgeH[AgeH==2] <- 2 #2nd year=2
AgeH[AgeH==3] <- 3 #3rd year and adult=3
AgeH[AgeH==4] <- 3 #3rd year and adult=3, 3rd year and adult combined because only few indivivudals from 3rd year. 
summary(AgeH)

# Age at observation (for survival analysis) is a categorial covariate varying with time : so we load age as a matrix
age2 <- read.csv(file="AgeMatrix2000_2019_Cond3class9Years.csv")# 
age <- age2 
head(age2) #age classes are: 1=1st, 2=2nd and 3rd year, 3=adults
age[age2==0] <- NA   # replace NA by 0 (before first capture)

# Other covariates are continuous : condition index
contcov <- COV[,3:11]
head(contcov)
# we need to standardize each value ( (x- mean(x) /sd(x) )
mean.cov = apply(contcov,2,mean) # mean of each covariate
mean.mat = matrix(rep(mean.cov,nrow(contcov)),byrow=T,ncol=ncol(contcov))
sd.cov = apply(contcov,2,sd)
sd.mat = matrix(rep(sd.cov,nrow(contcov)),byrow=T,ncol=ncol(contcov))

stcov = (contcov-mean.mat)/sd.mat # stcov = standardized covariates values of the continuous individual covariates
dim(stcov) # 9 continuous covariates
names(stcov)
#"MS"     "LogitH" "LogitB" "TA"     "TH"     "WL"     "BH"     "HT"     "DS"     
# we use stcov in the model for continuous covariates

# import catching time occasion per individual
TCatch1 <- read.csv(file="TCatch_P12.csv")# 
TCatch<-TCatch1[,1]

# use informative priors to estimate the survival (phi) and resighting prop. (p) with information from Andys Mark model (14 000 datapoints)
# 9 states: D, P, N, B, V, T, S, R, X; second period taken (2010-2019) because more precise estimates
# but also used un-informative priors for those combinations that were not well estimated by andys model using dnorm(0, 0.1) (meaning variance of 100 (inverse variance=precision))
# combinations state 3 age 2, state 9 age 2 and state 9 age 3 cannot be estimated in this model, because those combinations do not exist in
# the condition dataset. Thus, even though they were not estimable in MARK we use a 0 for beta and sd prior of 0.1 (uninformative)

# beta estimated in mark - or 0 if no prior info for certain age/state combinations --> for normal distribution
# we use a beta distribution, therefore we use dbeta(1,1) as uninformative prior, where alpha=beta=1
# https://projecteuclid.org/journals/electronic-journal-of-statistics/volume-5/issue-none/Neutral-noninformative-and-informative-conjugate-beta-and-gamma-prior-distributions/10.1214/11-EJS648.full
# alpha=beta=1.000006 results if mean=0.5 and var=0.083333 (sd=0.2886746) for uninformative priors (estimates)
# from beta estimates from andy we need to back transform the estimates
# 0.6905,0.8921,0.9461,   #state 1, age 1,2,3
# 0.4594,0.8255,0.9151,   #state 2, age 1,2,3
# 0.5463,0.8255,0.9428,   #state 3, age 1,2,3
# 0.6351,0.8971,0.92,     #state 4, age 1,2,3
# 0.3478,0.7326,0.9285,   #state 5, age 1,2,3
# 0.7129,0.8643,0.9299,   #state 6, age 1,2,3
# 0.4502,0.6899,0.946,    #state 7, age 1,2,3
# 0.2281,0.8204,0.8703,   #state 8, age 1,2,3
# 0.4901,0.8407,0.8788    #state 9, age 1,2,3

int.phi.meanprior <- matrix(c(0.6905,0.4594,0.5463, 0.6351, 0.3478,0.7129,0.4502,0.2281,0.4901,
                              0.8921,0.8255,0.8255,0.8971,0.7326,0.8643,0.6899,0.8204,0.8407,
                              0.9461,0.9151,0.9428,0.92,0.9285,0.9299,0.946,0.8703,0.8788
), ncol=3,nrow=9)

# standard error: 0.001 if prior (precise), or 0.1 if no prior info (unprecise) -> normal distr
# standard error: 0.001 if prior (precise), or 0.2886746 if no prior info (unprecise) --> beta distr
# 0.001,0.001,0.001,      #state 1, age 1,2,3
# 0.001,0.001,0.001,      #state 2, age 1,2,3
# 0.001,0.001,0.001,      #state 3, age 1,2,3
# 0.001,0.001,0.001,      #state 4, age 1,2,3
# 0.001,0.001,0.001,      #state 5, age 1,2,3
# 0.001,0.001,0.001,      #state 6, age 1,2,3
# 0.001,0.001,0.001,      #state 7, age 1,2,3
# 0.001,0.001,0.001,      #state 8, age 1,2,3
# 0.001,0.001,0.001       #state 9, age 1,2,3

int.phi.sdprior <- matrix(c(0.001,0.001,0.001, 0.001, 0.001,0.001,0.001,0.001,0.001,
                            0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,
                            0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001
), ncol=3,nrow=9)

#same for resighting probability (p), # 9 states: D, P, N, B, V, T, S, R, X
#for beta distribution
# 0.7154,0.7305,0.8153,     #state 1, age 1,2,3
# 0.5, 0.4435,0.4725,       #state 2, age 1,2,3
# 0.5,0.3509,0.5075,        #state 3, age 1,2,3
# 0.6438,0.6372,0.5756,     #state 4, age 1,2,3
# 0.2176,0.4139,0.4166,     #state 5, age 1,2,3
# 0.4877,0.4432,0.5929,     #state 6, age 1,2,3
# 0.4219,0.4668,0.585,      #state 7, age 1,2,3
# 0.3055,0.3339,0.5097,     #state 8, age 1,2,3
# 0.5358,0.4711,0.4156      #state 9, age 1,2,3 

int.p.meanprior <- matrix(c(0.7154,0.5,0.5,0.6438,0.2176,0.4877,0.4219,0.3055,0.5358,
                            0.7305,0.4435,0.3509,0.6372,0.4139,0.4432,0.4668,0.3339,0.4711,
                            0.8153,0.4725, 0.5075, 0.5756,0.4166,0.5929,0.585,0.5097,0.4156
), ncol=3,nrow=9)

# 0.001,0.001,0.001,            #state 1, age 1,2,3
# 0.2886746,0.001,0.001,        #state 2, age 1,2,3
# 0.2886746,0.001,0.001,        #state 3, age 1,2,3
# 0.001,0.001,0.001,            #state 4, age 1,2,3
# 0.001,0.001,0.001,            #state 5, age 1,2,3
# 0.001,0.001,0.001,            #state 6, age 1,2,3
# 0.001,0.001,0.001,            #state 7, age 1,2,3
# 0.001,0.001,0.001,            #state 8, age 1,2,3
# 0.001,0.001,0.001             #state 9, age 1,2,3

int.p.sdprior <- matrix(c(0.001,0.2886746,0.2886746,0.001,0.001,0.001,0.001,0.001,0.001,
                          0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,
                          0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001
), ncol=3,nrow=9)

## Laod the SEM-CR model
NimbleCode<-nimbleCode({
  ###
  ###
  ###  
  # 10 states (S) :  
  # 9 alive states : D, P, N, B, V, T, S, R, X
  # 1 just dead state for dead-recovery : JD
  # plus 1 already dead state : AD 
  #
  # 10 events (E) also calld observations (O):
  # 0 not seen
  # 1 to 9 seen alive in state D, P, N, B, V, T, S, R, X
  # 10 recovered dead (no info on state where it was recovered)
  ###
  ###
  ###
  
  # Probabilities of events given states and states given states    
  
  # vector of initial states are the proportions in each state at first capture occasion PI
  S0[1] <- prop[1] / (1 + sum(prop[1:8])) # prob. of being in initial state D
  S0[2] <- prop[2] / (1 + sum(prop[1:8])) # prob. of being in initial state P
  S0[3] <- prop[3] / (1 + sum(prop[1:8])) # prob. of being in initial state N
  S0[4] <- prop[4] / (1 + sum(prop[1:8])) # prob. of being in initial state B
  S0[5] <- prop[5] / (1 + sum(prop[1:8])) # prob. of being in initial state V
  S0[6] <- prop[6] / (1 + sum(prop[1:8])) # prob. of being in initial state T
  S0[7] <- prop[7] / (1 + sum(prop[1:8])) # prob. of being in initial state S
  S0[8] <- prop[8] / (1 + sum(prop[1:8])) # prob. of being in initial state R
  S0[9] <- 1 / (1 + sum(prop[1:8]))       # prob. of being in initial state X
  S0[10] <- 0 # prob. of being in initial state JD
  S0[11] <- 0 # prob. of being in initial state AD (already dead)
  
  # State process: define probabilities of S(t+1) given S(t)
  
  # Define PSI matrix gathering state-dependent survival parameters (phi) and movement (psi)
  for(i in 1:n){
    for(j in (first[i]+1):T){
      # for birds in state 1 at time t:
      PSI[ 1	,	1	,i,j]<-	phi[1,i,j] * ( SW[j] * (1/(sum(psiSW1[1:8]) ))         + WS[j] * (1/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	2	,i,j]<-	phi[1,i,j] * ( SW[j] * (psiSW1[1]/(sum(psiSW1[1:8]) )) + WS[j] * (psiWS1[1]/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	3	,i,j]<-	phi[1,i,j] * ( SW[j] * (psiSW1[2]/(sum(psiSW1[1:8]) )) + WS[j] * (psiWS1[2]/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	4	,i,j]<-	phi[1,i,j] * ( SW[j] * (psiSW1[3]/(sum(psiSW1[1:8]) )) + WS[j] * (psiWS1[3]/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	5	,i,j]<-	phi[1,i,j] * ( SW[j] * (psiSW1[4]/(sum(psiSW1[1:8]) )) + WS[j] * (psiWS1[4]/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	6	,i,j]<-	phi[1,i,j] * ( SW[j] * (psiSW1[5]/(sum(psiSW1[1:8]) )) + WS[j] * (psiWS1[5]/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	7	,i,j]<-	phi[1,i,j] * ( SW[j] * (psiSW1[6]/(sum(psiSW1[1:8]) )) + WS[j] * (psiWS1[6]/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	8	,i,j]<-	phi[1,i,j] * ( SW[j] * (psiSW1[7]/(sum(psiSW1[1:8]) )) + WS[j] * (psiWS1[7]/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	9	,i,j]<-	phi[1,i,j] * ( SW[j] * (psiSW1[8]/(sum(psiSW1[1:8]) )) + WS[j] * (psiWS1[8]/(sum(psiWS1[1:8]) ))  )
      PSI[ 1	,	10	,i,j]<-	(1 - phi[1,i,j]) * r
      PSI[ 1	,	11	,i,j]<-	(1 - phi[1,i,j]) * (1-r)
      
      # same for birds in state 2 at time t:
      PSI[ 2	,	1	,i,j]<-	phi[2,i,j] * ( SW[j] * (psiSW2[1]/(sum(psiSW2[1:8]) )) + WS[j] * (psiWS2[1]/(sum(psiWS2[1:8]) ))  )
      PSI[ 2	,	2	,i,j]<-	phi[2,i,j] * ( SW[j] * (1/(sum(psiSW2[1:8]) ))         + WS[j] * (1/(sum(psiWS2[1:8])         ))  )
      PSI[ 2	,	3	,i,j]<-	phi[2,i,j] * ( SW[j] * (psiSW2[2]/(sum(psiSW2[1:8]) )) + WS[j] * (psiWS2[2]/(sum(psiWS2[1:8]) ))  )
      PSI[ 2	,	4	,i,j]<-	phi[2,i,j] * ( SW[j] * (psiSW2[3]/(sum(psiSW2[1:8]) )) + WS[j] * (psiWS2[3]/(sum(psiWS2[1:8]) ))  )
      PSI[ 2	,	5	,i,j]<-	phi[2,i,j] * ( SW[j] * (psiSW2[4]/(sum(psiSW2[1:8]) )) + WS[j] * (psiWS2[4]/(sum(psiWS2[1:8]) ))  )
      PSI[ 2	,	6	,i,j]<-	phi[2,i,j] * ( SW[j] * (psiSW2[5]/(sum(psiSW2[1:8]) )) + WS[j] * (psiWS2[5]/(sum(psiWS2[1:8]) ))  )
      PSI[ 2	,	7	,i,j]<-	phi[2,i,j] * ( SW[j] * (psiSW2[6]/(sum(psiSW2[1:8]) )) + WS[j] * (psiWS2[6]/(sum(psiWS2[1:8]) ))  )
      PSI[ 2	,	8	,i,j]<-	phi[2,i,j] * ( SW[j] * (psiSW2[7]/(sum(psiSW2[1:8]) )) + WS[j] * (psiWS2[7]/(sum(psiWS2[1:8]) ))  )
      PSI[ 2	,	9	,i,j]<-	phi[2,i,j] * ( SW[j] * (psiSW2[8]/(sum(psiSW2[1:8]) )) + WS[j] * (psiWS2[8]/(sum(psiWS2[1:8]) ))  )
      PSI[ 2	,	10	,i,j]<-	(1 - phi[2,i,j]) * r
      PSI[ 2	,	11	,i,j]<-	(1 - phi[2,i,j]) * (1-r)
      
      # etc for birds in state 3 at time t:
      PSI[ 3	,	1	,i,j]<-	phi[3,i,j] * ( SW[j] * (psiSW3[1]/(sum(psiSW3[1:8]) )) + WS[j] * (psiWS3[1]/(sum(psiWS3[1:8]) ))  )
      PSI[ 3	,	2	,i,j]<-	phi[3,i,j] * ( SW[j] * (psiSW3[2]/(sum(psiSW3[1:8]) )) + WS[j] * (psiWS3[2]/(sum(psiWS3[1:8]) ))  )
      PSI[ 3	,	3	,i,j]<-	phi[3,i,j] * ( SW[j] * (1/(sum(psiSW3[1:8]) ))         + WS[j] * (1/(sum(psiWS3[1:8])         ))  )
      PSI[ 3	,	4	,i,j]<-	phi[3,i,j] * ( SW[j] * (psiSW3[3]/(sum(psiSW3[1:8]) )) + WS[j] * (psiWS3[3]/(sum(psiWS3[1:8]) ))  )
      PSI[ 3	,	5	,i,j]<-	phi[3,i,j] * ( SW[j] * (psiSW3[4]/(sum(psiSW3[1:8]) )) + WS[j] * (psiWS3[4]/(sum(psiWS3[1:8]) ))  )
      PSI[ 3	,	6	,i,j]<-	phi[3,i,j] * ( SW[j] * (psiSW3[5]/(sum(psiSW3[1:8]) )) + WS[j] * (psiWS3[5]/(sum(psiWS3[1:8]) ))  )
      PSI[ 3	,	7	,i,j]<-	phi[3,i,j] * ( SW[j] * (psiSW3[6]/(sum(psiSW3[1:8]) )) + WS[j] * (psiWS3[6]/(sum(psiWS3[1:8]) ))  )
      PSI[ 3	,	8	,i,j]<-	phi[3,i,j] * ( SW[j] * (psiSW3[7]/(sum(psiSW3[1:8]) )) + WS[j] * (psiWS3[7]/(sum(psiWS3[1:8]) ))  )
      PSI[ 3	,	9	,i,j]<-	phi[3,i,j] * ( SW[j] * (psiSW3[8]/(sum(psiSW3[1:8]) )) + WS[j] * (psiWS3[8]/(sum(psiWS3[1:8]) ))  )
      PSI[ 3	,	10	,i,j]<-	(1 - phi[3,i,j]) * r
      PSI[ 3	,	11	,i,j]<-	(1 - phi[3,i,j]) * (1-r)
      
      # for birds in state 4 at time t:
      PSI[ 4	,	1	,i,j]<-	phi[4,i,j] * ( SW[j] * (psiSW4[1]/(sum(psiSW4[1:8]) )) + WS[j] * (psiWS4[1]/(sum(psiWS4[1:8]) ))  )
      PSI[ 4	,	2	,i,j]<-	phi[4,i,j] * ( SW[j] * (psiSW4[2]/(sum(psiSW4[1:8]) )) + WS[j] * (psiWS4[2]/(sum(psiWS4[1:8]) ))  )
      PSI[ 4	,	3	,i,j]<-	phi[4,i,j] * ( SW[j] * (psiSW4[3]/(sum(psiSW4[1:8]) )) + WS[j] * (psiWS4[3]/(sum(psiWS4[1:8]) ))  )
      PSI[ 4	,	4	,i,j]<-	phi[4,i,j] * ( SW[j] * (1/(sum(psiSW4[1:8])         )) + WS[j] * (1/(sum(psiWS4[1:8])         ))  )
      PSI[ 4	,	5	,i,j]<-	phi[4,i,j] * ( SW[j] * (psiSW4[4]/(sum(psiSW4[1:8]) )) + WS[j] * (psiWS4[4]/(sum(psiWS4[1:8]) ))  )
      PSI[ 4	,	6	,i,j]<-	phi[4,i,j] * ( SW[j] * (psiSW4[5]/(sum(psiSW4[1:8]) )) + WS[j] * (psiWS4[5]/(sum(psiWS4[1:8]) ))  )
      PSI[ 4	,	7	,i,j]<-	phi[4,i,j] * ( SW[j] * (psiSW4[6]/(sum(psiSW4[1:8]) )) + WS[j] * (psiWS4[6]/(sum(psiWS4[1:8]) ))  )
      PSI[ 4	,	8	,i,j]<-	phi[4,i,j] * ( SW[j] * (psiSW4[7]/(sum(psiSW4[1:8]) )) + WS[j] * (psiWS4[7]/(sum(psiWS4[1:8]) ))  )
      PSI[ 4	,	9	,i,j]<-	phi[4,i,j] * ( SW[j] * (psiSW4[8]/(sum(psiSW4[1:8]) )) + WS[j] * (psiWS4[8]/(sum(psiWS4[1:8]) ))  )
      PSI[ 4	,	10	,i,j]<-	(1 - phi[4,i,j]) * r
      PSI[ 4	,	11	,i,j]<-	(1 - phi[4,i,j]) * (1-r)
      
      # for birds in state 5 at time t:
      PSI[ 5	,	1	,i,j]<-	phi[5,i,j] * ( SW[j] * (psiSW5[1]/(sum(psiSW5[1:8]) )) + WS[j] * (psiWS5[1]/(sum(psiWS5[1:8]) ))  )
      PSI[ 5	,	2	,i,j]<-	phi[5,i,j] * ( SW[j] * (psiSW5[2]/(sum(psiSW5[1:8]) )) + WS[j] * (psiWS5[2]/(sum(psiWS5[1:8]) ))  )
      PSI[ 5	,	3	,i,j]<-	phi[5,i,j] * ( SW[j] * (psiSW5[3]/(sum(psiSW5[1:8]) )) + WS[j] * (psiWS5[3]/(sum(psiWS5[1:8]) ))  )
      PSI[ 5	,	4	,i,j]<-	phi[5,i,j] * ( SW[j] * (psiSW5[4]/(sum(psiSW5[1:8]) )) + WS[j] * (psiWS5[4]/(sum(psiWS5[1:8]) ))  )                             
      PSI[ 5	,	5	,i,j]<-	phi[5,i,j] * ( SW[j] * (1/(sum(psiSW5[1:8])         )) + WS[j] * (1/(sum(psiWS5[1:8])         ))  )
      PSI[ 5	,	6	,i,j]<-	phi[5,i,j] * ( SW[j] * (psiSW5[5]/(sum(psiSW5[1:8]) )) + WS[j] * (psiWS5[5]/(sum(psiWS5[1:8]) ))  )                              
      PSI[ 5	,	7	,i,j]<-	phi[5,i,j] * ( SW[j] * (psiSW5[6]/(sum(psiSW5[1:8]) )) + WS[j] * (psiWS5[6]/(sum(psiWS5[1:8]) ))  )                              
      PSI[ 5	,	8	,i,j]<-	phi[5,i,j] * ( SW[j] * (psiSW5[7]/(sum(psiSW5[1:8]) )) + WS[j] * (psiWS5[7]/(sum(psiWS5[1:8]) ))  )                              
      PSI[ 5	,	9	,i,j]<-	phi[5,i,j] * ( SW[j] * (psiSW5[8]/(sum(psiSW5[1:8]) )) + WS[j] * (psiWS5[8]/(sum(psiWS5[1:8]) ))  )
      PSI[ 5	,	10	,i,j]<-	(1 - phi[5,i,j]) * r
      PSI[ 5	,	11	,i,j]<-	(1 - phi[5,i,j]) * (1-r)
      
      # for birds in state 6 at time t:
      PSI[ 6	,	1	,i,j]<-	phi[6,i,j] * ( SW[j] * (psiSW6[1]/(sum(psiSW6[1:8]) )) + WS[j] * (psiWS6[1]/(sum(psiWS6[1:8]) ))  )
      PSI[ 6	,	2	,i,j]<-	phi[6,i,j] * ( SW[j] * (psiSW6[2]/(sum(psiSW6[1:8]) )) + WS[j] * (psiWS6[2]/(sum(psiWS6[1:8]) ))  )
      PSI[ 6	,	3	,i,j]<-	phi[6,i,j] * ( SW[j] * (psiSW6[3]/(sum(psiSW6[1:8]) )) + WS[j] * (psiWS6[3]/(sum(psiWS6[1:8]) ))  )
      PSI[ 6	,	4	,i,j]<-	phi[6,i,j] * ( SW[j] * (psiSW6[4]/(sum(psiSW6[1:8]) )) + WS[j] * (psiWS6[4]/(sum(psiWS6[1:8]) ))  )
      PSI[ 6	,	5	,i,j]<-	phi[6,i,j] * ( SW[j] * (psiSW6[5]/(sum(psiSW6[1:8]) )) + WS[j] * (psiWS6[5]/(sum(psiWS6[1:8]) ))  )
      PSI[ 6	,	6	,i,j]<-	phi[6,i,j] * ( SW[j] * (1/(sum(psiSW6[1:8])         )) + WS[j] * (1/(sum(psiWS6[1:8])         ))  )
      PSI[ 6	,	7	,i,j]<-	phi[6,i,j] * ( SW[j] * (psiSW6[6]/(sum(psiSW6[1:8]) )) + WS[j] * (psiWS6[6]/(sum(psiWS6[1:8]) ))  )
      PSI[ 6	,	8	,i,j]<-	phi[6,i,j] * ( SW[j] * (psiSW6[7]/(sum(psiSW6[1:8]) )) + WS[j] * (psiWS6[7]/(sum(psiWS6[1:8]) ))  )
      PSI[ 6	,	9	,i,j]<-	phi[6,i,j] * ( SW[j] * (psiSW6[8]/(sum(psiSW6[1:8]) )) + WS[j] * (psiWS6[8]/(sum(psiWS6[1:8]) ))  )
      PSI[ 6	,	10	,i,j]<-	(1 - phi[6,i,j]) * r
      PSI[ 6	,	11	,i,j]<-	(1 - phi[6,i,j]) * (1-r)
      
      # for birds in state 7 at time t:
      PSI[ 7	,	1	,i,j]<-	phi[7,i,j] * ( SW[j] * (psiSW7[1]/(sum(psiSW7[1:8]) )) + WS[j] * (psiWS7[1]/(sum(psiWS7[1:8]) ))  )
      PSI[ 7	,	2	,i,j]<-	phi[7,i,j] * ( SW[j] * (psiSW7[2]/(sum(psiSW7[1:8]) )) + WS[j] * (psiWS7[2]/(sum(psiWS7[1:8]) ))  )
      PSI[ 7	,	3	,i,j]<-	phi[7,i,j] * ( SW[j] * (psiSW7[3]/(sum(psiSW7[1:8]) )) + WS[j] * (psiWS7[3]/(sum(psiWS7[1:8]) ))  )
      PSI[ 7	,	4	,i,j]<-	phi[7,i,j] * ( SW[j] * (psiSW7[4]/(sum(psiSW7[1:8]) )) + WS[j] * (psiWS7[4]/(sum(psiWS7[1:8]) ))  )
      PSI[ 7	,	5	,i,j]<-	phi[7,i,j] * ( SW[j] * (psiSW7[5]/(sum(psiSW7[1:8]) )) + WS[j] * (psiWS7[5]/(sum(psiWS7[1:8]) ))  )
      PSI[ 7	,	6	,i,j]<-	phi[7,i,j] * ( SW[j] * (psiSW7[6]/(sum(psiSW7[1:8]) )) + WS[j] * (psiWS7[6]/(sum(psiWS7[1:8]) ))  )
      PSI[ 7	,	7	,i,j]<-	phi[7,i,j] * ( SW[j] * (1/(sum(psiSW7[1:8]) ))         + WS[j] * (1/(sum(psiWS7[1:8])         ))  )
      PSI[ 7	,	8	,i,j]<-	phi[7,i,j] * ( SW[j] * (psiSW7[7]/(sum(psiSW7[1:8]) )) + WS[j] * (psiWS7[7]/(sum(psiWS7[1:8]) ))  )
      PSI[ 7	,	9	,i,j]<-	phi[7,i,j] * ( SW[j] * (psiSW7[8]/(sum(psiSW7[1:8]) )) + WS[j] * (psiWS7[8]/(sum(psiWS7[1:8]) ))  )
      PSI[ 7	,	10	,i,j]<-	(1 - phi[7,i,j]) * r
      PSI[ 7	,	11	,i,j]<-	(1 - phi[7,i,j]) * (1-r)
      
      # for birds in state 8 at time t:
      PSI[ 8	,	1	,i,j]<-	phi[8,i,j] * ( SW[j] * (psiSW8[1]/(sum(psiSW8[1:8]) )) + WS[j] * (psiWS8[1]/(sum(psiWS8[1:8]) ))  )
      PSI[ 8	,	2	,i,j]<-	phi[8,i,j] * ( SW[j] * (psiSW8[2]/(sum(psiSW8[1:8]) )) + WS[j] * (psiWS8[2]/(sum(psiWS8[1:8]) ))  )
      PSI[ 8	,	3	,i,j]<-	phi[8,i,j] * ( SW[j] * (psiSW8[3]/(sum(psiSW8[1:8]) )) + WS[j] * (psiWS8[3]/(sum(psiWS8[1:8]) ))  )
      PSI[ 8	,	4	,i,j]<-	phi[8,i,j] * ( SW[j] * (psiSW8[4]/(sum(psiSW8[1:8]) )) + WS[j] * (psiWS8[4]/(sum(psiWS8[1:8]) ))  )
      PSI[ 8	,	5	,i,j]<-	phi[8,i,j] * ( SW[j] * (psiSW8[5]/(sum(psiSW8[1:8]) )) + WS[j] * (psiWS8[5]/(sum(psiWS8[1:8]) ))  )
      PSI[ 8	,	6	,i,j]<-	phi[8,i,j] * ( SW[j] * (psiSW8[6]/(sum(psiSW8[1:8]) )) + WS[j] * (psiWS8[6]/(sum(psiWS8[1:8]) ))  )
      PSI[ 8	,	7	,i,j]<-	phi[8,i,j] * ( SW[j] * (psiSW8[7]/(sum(psiSW8[1:8]) )) + WS[j] * (psiWS8[7]/(sum(psiWS8[1:8]) ))  )
      PSI[ 8	,	8	,i,j]<-	phi[8,i,j] * ( SW[j] * (1/(sum(psiSW8[1:8])         )) + WS[j] * (1/(sum(psiWS8[1:8])         ))  )
      PSI[ 8	,	9	,i,j]<-	phi[8,i,j] * ( SW[j] * (psiSW8[8]/(sum(psiSW8[1:8]) )) + WS[j] * (psiWS8[8]/(sum(psiWS8[1:8]) ))  )
      PSI[ 8	,	10	,i,j]<-	(1 - phi[8,i,j]) * r
      PSI[ 8	,	11	,i,j]<-	(1 - phi[8,i,j]) * (1-r)
      
      # for birds in state 9 at time t:
      PSI[ 9	,	1	,i,j]<-	phi[9,i,j] * ( SW[j] * (psiSW9[1]/(sum(psiSW9[1:8]) )) + WS[j] * (psiWS9[1]/(sum(psiWS9[1:8]) ))  )
      PSI[ 9	,	2	,i,j]<-	phi[9,i,j] * ( SW[j] * (psiSW9[2]/(sum(psiSW9[1:8]) )) + WS[j] * (psiWS9[2]/(sum(psiWS9[1:8]) ))  )
      PSI[ 9	,	3	,i,j]<-	phi[9,i,j] * ( SW[j] * (psiSW9[3]/(sum(psiSW9[1:8]) )) + WS[j] * (psiWS9[3]/(sum(psiWS9[1:8]) ))  )
      PSI[ 9	,	4	,i,j]<-	phi[9,i,j] * ( SW[j] * (psiSW9[4]/(sum(psiSW9[1:8]) )) + WS[j] * (psiWS9[4]/(sum(psiWS9[1:8]) ))  )
      PSI[ 9	,	5	,i,j]<-	phi[9,i,j] * ( SW[j] * (psiSW9[5]/(sum(psiSW9[1:8]) )) + WS[j] * (psiWS9[5]/(sum(psiWS9[1:8]) ))  )
      PSI[ 9	,	6	,i,j]<-	phi[9,i,j] * ( SW[j] * (psiSW9[6]/(sum(psiSW9[1:8]) )) + WS[j] * (psiWS9[6]/(sum(psiWS9[1:8]) ))  )
      PSI[ 9	,	7	,i,j]<-	phi[9,i,j] * ( SW[j] * (psiSW9[7]/(sum(psiSW9[1:8]) )) + WS[j] * (psiWS9[7]/(sum(psiWS9[1:8]) ))  )
      PSI[ 9	,	8	,i,j]<-	phi[9,i,j] * ( SW[j] * (psiSW9[8]/(sum(psiSW9[1:8]) )) + WS[j] * (psiWS9[8]/(sum(psiWS9[1:8]) ))  )
      PSI[ 9	,	9	,i,j]<-	phi[9,i,j] * ( SW[j] * (1/(sum(psiSW9[1:8])         )) + WS[j] * (1/(sum(psiWS9[1:8])         ))  )
      PSI[ 9	,	10	,i,j]<-	(1 - phi[9,i,j]) * r
      PSI[ 9	,	11	,i,j]<-	(1 - phi[9,i,j]) * (1-r)
      
      PSI[ 10	,	1	,i,j]<- 0
      PSI[ 10	,	2	,i,j]<- 0
      PSI[ 10	,	3	,i,j]<- 0
      PSI[ 10	,	4	,i,j]<- 0
      PSI[ 10	,	5	,i,j]<- 0
      PSI[ 10	,	6	,i,j]<- 0
      PSI[ 10	,	7	,i,j]<- 0
      PSI[ 10	,	8	,i,j]<- 0
      PSI[ 10	,	9	,i,j]<- 0
      PSI[ 10	,	10	,i,j]<- 0
      PSI[ 10	,	11	,i,j]<- 1
      
      PSI[ 11	,	1	,i,j]<- 0
      PSI[ 11	,	2	,i,j]<- 0
      PSI[ 11	,	3	,i,j]<- 0
      PSI[ 11	,	4	,i,j]<- 0
      PSI[ 11	,	5	,i,j]<- 0
      PSI[ 11	,	6	,i,j]<- 0
      PSI[ 11	,	7	,i,j]<- 0
      PSI[ 11	,	8	,i,j]<- 0
      PSI[ 11	,	9	,i,j]<- 0
      PSI[ 11	,	10	,i,j]<- 0
      PSI[ 11	,	11	,i,j]<- 1
      
    }}
  
  
  ## for subsequent captures, depends on detection probabilities (p): 
  for(i in 1:n){
    for(j in (first[i]+1):T){
      
      E[	1	,	1	,i,j]<-	1-p[1,i,j]
      E[	1	,	2	,i,j]<-	p[1,i,j]
      E[	1	,	3	,i,j]<-	0
      E[	1	,	4	,i,j]<-	0
      E[	1	,	5	,i,j]<-	0
      E[	1	,	6	,i,j]<-	0
      E[	1	,	7	,i,j]<-	0
      E[	1	,	8	,i,j]<-	0
      E[	1	,	9	,i,j]<-	0
      E[	1	,	10	,i,j]<-	0
      E[	1	,	11	,i,j]<-	0
      
      E[	2	,	1	,i,j]<-	1-p[2,i,j]
      E[	2	,	2	,i,j]<-	0
      E[	2	,	3	,i,j]<-	p[2,i,j]
      E[	2	,	4	,i,j]<-	0
      E[	2	,	5	,i,j]<-	0
      E[	2	,	6	,i,j]<-	0
      E[	2	,	7	,i,j]<-	0
      E[	2	,	8	,i,j]<-	0
      E[	2	,	9	,i,j]<-	0
      E[	2	,	10	,i,j]<-	0
      E[	2	,	11	,i,j]<-	0
      
      E[	3	,	1	,i,j]<-	1-p[3,i,j]
      E[	3	,	2	,i,j]<-	0
      E[	3	,	3	,i,j]<-	0
      E[	3	,	4	,i,j]<-	p[3,i,j]
      E[	3	,	5	,i,j]<-	0
      E[	3	,	6	,i,j]<-	0
      E[	3	,	7	,i,j]<-	0
      E[	3	,	8	,i,j]<-	0
      E[	3	,	9	,i,j]<-	0
      E[	3	,	10	,i,j]<-	0
      E[	3	,	11	,i,j]<-	0
      
      E[	4	,	1	,i,j]<-	1-p[4,i,j]
      E[	4	,	2	,i,j]<-	0
      E[	4	,	3	,i,j]<-	0
      E[	4	,	4	,i,j]<-	0
      E[	4	,	5	,i,j]<-	p[4,i,j]
      E[	4	,	6	,i,j]<-	0
      E[	4	,	7	,i,j]<-	0
      E[	4	,	8	,i,j]<-	0
      E[	4	,	9	,i,j]<-	0
      E[	4	,	10	,i,j]<-	0
      E[	4	,	11	,i,j]<-	0
      
      E[	5	,	1	,i,j]<-	1-p[5,i,j]
      E[	5	,	2	,i,j]<-	0
      E[	5	,	3	,i,j]<-	0
      E[	5	,	4	,i,j]<-	0
      E[	5	,	5	,i,j]<-	0
      E[	5	,	6	,i,j]<-	p[5,i,j]
      E[	5	,	7	,i,j]<-	0
      E[	5	,	8	,i,j]<-	0
      E[	5	,	9	,i,j]<-	0
      E[	5	,	10	,i,j]<-	0
      E[	5	,	11	,i,j]<-	0
      
      E[	6	,	1	,i,j]<-	1-p[6,i,j]
      E[	6	,	2	,i,j]<-	0
      E[	6	,	3	,i,j]<-	0
      E[	6	,	4	,i,j]<-	0
      E[	6	,	5	,i,j]<-	0
      E[	6	,	6	,i,j]<-	0
      E[	6	,	7	,i,j]<-	p[6,i,j]
      E[	6	,	8	,i,j]<-	0
      E[	6	,	9	,i,j]<-	0
      E[	6	,	10	,i,j]<-	0
      E[	6	,	11	,i,j]<-	0
      
      E[	7	,	1	,i,j]<-	1-p[7,i,j]
      E[	7	,	2	,i,j]<-	0
      E[	7	,	3	,i,j]<-	0
      E[	7	,	4	,i,j]<-	0
      E[	7	,	5	,i,j]<-	0
      E[	7	,	6	,i,j]<-	0
      E[	7	,	7	,i,j]<-	0
      E[	7	,	8	,i,j]<-	p[7,i,j]
      E[	7	,	9	,i,j]<-	0
      E[	7	,	10	,i,j]<-	0
      E[	7	,	11	,i,j]<-	0
      
      E[	8	,	1	,i,j]<-	1-p[8,i,j]
      E[	8	,	2	,i,j]<-	0
      E[	8	,	3	,i,j]<-	0
      E[	8	,	4	,i,j]<-	0
      E[	8	,	5	,i,j]<-	0
      E[	8	,	6	,i,j]<-	0
      E[	8	,	7	,i,j]<-	0
      E[	8	,	8	,i,j]<-	0
      E[	8	,	9	,i,j]<-	p[8,i,j]
      E[	8	,	10	,i,j]<-	0
      E[	8	,	11	,i,j]<-	0
      
      E[	9	,	1	,i,j]<-	1-p[9,i,j]
      E[	9	,	2	,i,j]<-	0
      E[	9	,	3	,i,j]<-	0
      E[	9	,	4	,i,j]<-	0
      E[	9	,	5	,i,j]<-	0
      E[	9	,	6	,i,j]<-	0
      E[	9	,	7	,i,j]<-	0
      E[	9	,	8	,i,j]<-	0
      E[	9	,	9	,i,j]<-	0
      E[	9	,	10	,i,j]<-	p[9,i,j]
      E[	9	,	11	,i,j]<-	0
      
      E[	10	,	1	,i,j]<-	0
      E[	10	,	2	,i,j]<-	0
      E[	10	,	3	,i,j]<-	0
      E[	10	,	4	,i,j]<-	0
      E[	10	,	5	,i,j]<-	0
      E[	10	,	6	,i,j]<-	0
      E[	10	,	7	,i,j]<-	0
      E[	10	,	8	,i,j]<-	0
      E[	10	,	9	,i,j]<-	0
      E[	10	,	10	,i,j]<-	0
      E[	10	,	11	,i,j]<-	1
      
      E[	11	,	1	,i,j]<-	1
      E[	11	,	2	,i,j]<-	0
      E[	11	,	3	,i,j]<-	0
      E[	11	,	4	,i,j]<-	0
      E[	11	,	5	,i,j]<-	0
      E[	11	,	6	,i,j]<-	0
      E[	11	,	7	,i,j]<-	0
      E[	11	,	8	,i,j]<-	0
      E[	11	,	9	,i,j]<-	0
      E[	11	,	10	,i,j]<-	0
      E[	11	,	11	,i,j]<-	0
      
    }}
  
  ## LIKELIHOOD
  
  for (i in 1:n)  # for each individual
  {
    # The estimated probabilities of initial states S0 are the proportions in each state at first capture occasion
    alive[i,first[i]] ~ dcat(S0[1:11])         
    mydata[i,first[i]] ~ dcat(E0[alive[i,first[i]],1:11])
    
    for (j in (first[i]+1):T)
    {
      
      ## STATE EQUATIONS ##
      # draw states S(t) given states S(t-1)
      alive[i,j] ~ dcat(PSI[alive[i,j-1],1:11,i,j])
      
      ## OBSERVATION EQUATIONS ##
      # draw events E(t) given states S(t)
      
      mydata[i,j] ~ dcat(E[alive[i,j],1:11,i,j])
      
    }
  }
  
  
  ## SEM MODEL
  
  # measurement model
  # cov[i,1] is MS=mass
  # cov[i,2] is LogitH=haematocrit
  # cov[i,3] is LogitB=buffy coat
  # cov[i,4] is TA=tarsus toe length
  # cov[i,5] is TH=total head length (head+bill)
  # cov[i,6] is WL=wing length
  # cov[i,7] is BH=bill tip height
  # cov[i,8] is HT=handling time
  # cov[i,9] is DS=day in season 
  
  for(i in 1:n){  # for each individual
    
    # for mass 
    mu1[i] <- alpha0 + alpha1*cov[i,4] + alpha2*cov[i,5] + alpha3*cov[i,6] + alpha4*cov[i,7] + alpha5*cov[i,8] + alpha52*pow(cov[i,8],2) + alpha6*cov[i,9] + alpha7[AgeH[i]] 
    # for hematocrit
    mu2[i] <- delta0 + delta1*cov[i,7] + delta2*cov[i,8] + delta22*pow(cov[i,8],2) + delta3*cov[i,9] + delta4[AgeH[i]] + delta5[Sex[i]] 
    # for buffy coat
    mu3[i] <- theta0 + theta1*cov[i,7] + theta2*cov[i,8] + theta22*pow(cov[i,8],2) + theta3*cov[i,9] + theta4[AgeH[i]] + theta5[Sex[i]]
    
    # structural model
    # for composite variable condition :
    eta1[i] <- -1 * cov[i,1] +  gamma2 * cov[i,2] + gamma3 * cov[i,3] #scale on hema
  } #i=n
  
  # for CR model : survival and detection probabilities
  for(k in 1:9){ # for each state
    for(i in 1:n){ # for each individual
      for(j in (first[i]+1):T){ # for each sampling after first capture
        
        logit(phi[k,i,j]) <- mu.phi[k,i,j]  + epst.phi[k,j-1]   # age and state specific survival probability,  with site-specific time random effect
        
        mu.phi[k,i,j] <- logit(int.phi[k,age[i,j-1]]) + (j == TCatch[i]+1) * beta1 * eta1[i] + (j == TCatch[i]+1) * beta2 * cov[i,9]
        
        logit(p[k,i,j]) <- logit(int.p[k,age[i,j-1]]) + epst.p[k,j-1] # age and state specific detection probability, with site-specific time random effect
        # int.p and int.phi capture the intercepts, of age and site effect which are categorical - i.e. average survival rate for each combination of age class and site 
        
      } #j
    } #i=n
  } #k
  
  
  ## PRIORS
  
  for(i in 1:n){ # for each individual
    
    # For continuous covariates of the SEM
    cov[i,1] ~ dnorm(mu1[i],sd=sig1x)        #MS
    cov[i,2] ~ dnorm(mu2[i],sd=sig2x)        #LogitH
    cov[i,3] ~ dnorm(mu3[i],sd=sig3x)        #LogitB
    cov[i,4] ~ dnorm(0, sd=sig4x)            #TA
    cov[i,5] ~ dnorm(0, sd=sig5x)            #TH
    cov[i,6] ~ dnorm(0, sd=sig6x)            #WL
    cov[i,7] ~ dnorm(0, sd=sig7x)            #BH
    cov[i,8] ~ dnorm(0, sd=sig8x)            #HT
    cov[i,9] ~ dnorm(0, sd=sig9x)            #DS
    #eta1[i] ~ dnorm(mu.eta1[i], sd=0) #Condition
  }
  
  # proportion of individuals in initial states at first capture estimated from the data
  for (j in 1:8){ 
    log(prop[j]) <- pi[j] # mulitnomial logit link
    pi[j] ~ dnorm(0,1)
  }
  
  for(k in 1:9){ # for each state
    for(a in 1:3){ # each age
      #int.phi[k,a] ~ dnorm(int.phi.meanprior[k,a], sd=int.phi.sdprior[k,a])
      #int.p[k,a] ~ dnorm(int.p.meanprior[k,a], sd=int.p.sdprior[k,a]) 
      int.phi.alpha[k,a] <- (((1-int.phi.meanprior[k,a])/(int.phi.sdprior[k,a]*int.phi.sdprior[k,a]))-(1/int.phi.meanprior[k,a]))*(int.phi.meanprior[k,a]^2)
      int.phi.beta[k,a] <- int.phi.alpha[k,a] * ((1/int.phi.meanprior[k,a])-1)
      int.phi[k,a] ~ dbeta(int.phi.alpha[k,a], int.phi.beta[k,a])
      
      int.p.alpha[k,a] <- (((1-int.p.meanprior[k,a])/(int.p.sdprior[k,a]*int.p.sdprior[k,a]))-(1/int.p.meanprior[k,a]))*(int.p.meanprior[k,a]^2)
      int.p.beta[k,a] <- int.p.alpha[k,a] * ((1/int.p.meanprior[k,a])-1)
      int.p[k,a] ~ dbeta(int.p.alpha[k,a], int.p.beta[k,a])
    }#a
  }#k
  
  # Prior for random effects
  for(k in 1:9){ # for each site
    for (t in 1:(T-1)){ # nb of occasions - 1 
      epst.phi[k,t]~ dnorm(0,sd=sigmat.phi[k])
      epst.p[k,t]~ dnorm(0,sd=sigmat.p[k])
    }#t
    #taut.phi[k]<-pow(sigmat.phi[k],-2)  # precision
    sigmat.phi[k]~dunif(0,10)           # sd
    #vart.phi[k] <-pow(sigmat.phi[k],2)  # variance
    
    #taut.p[k]<-pow(sigmat.p[k],-2)      # precision
    sigmat.p[k]~dunif(0,10)             # sd
    #vart.p[k] <-pow(sigmat.p[k],2)      # variance
  }#k
  
  # priors for the factor loadings SEM
  #gamma1 ~ dnorm(0,1)
  #gamma12 ~ dnorm(0,1)
  gamma2 ~ dnorm(0,1)
  #gamma22 ~ dnorm(0,1)
  gamma3 ~ dnorm(0,1)
  #gamma32 ~ dnorm(0,1)
  beta1  ~ dnorm(0,1)
  beta2  ~ dnorm(0,1)
  alpha0 ~ dnorm(0,1)
  alpha1 ~ dnorm(0,1)
  alpha2 ~ dnorm(0,1)
  alpha3 ~ dnorm(0,1)
  alpha4 ~ dnorm(0,1)
  alpha5 ~ dnorm(0,1)
  alpha52 ~ dnorm(0,1)
  alpha6 ~ dnorm(0,1)
  delta0 ~ dnorm(0,1)
  delta1 ~ dnorm(0,1)
  delta2 ~ dnorm(0,1)
  delta22 ~ dnorm(0,1)
  delta3 ~ dnorm(0,1)
  theta0 ~ dnorm(0,1)
  theta1 ~ dnorm(0,1)
  theta2 ~ dnorm(0,1)
  theta22 ~ dnorm(0,1)
  theta3 ~ dnorm(0,1)
  
  # for sex
  #for(s in 1:2){ 
  delta5[2] ~ dnorm(0,1)
  theta5[2] ~ dnorm(0,1)
  #}
  
  #for AgeH
  #for(s in 1:3){ 
  alpha7[2] ~ dnorm(0,1)
  alpha7[3] ~ dnorm(0,1)
  delta4[2] ~ dnorm(0,1)
  delta4[3] ~ dnorm(0,1)
  theta4[2] ~ dnorm(0,1)
  theta4[3] ~ dnorm(0,1)
  #}
  
  # fix the effect of the first level (reference groups: juv for age and female for sex) to 0 because we use intercepts alpha0, delta0 and theta0
  alpha7[1] <- 0 # juveniles
  delta4[1] <- 0 # juveniles
  theta4[1] <- 0 # juveniles
  delta5[1] <- 0 # females
  theta5[1] <- 0 # females
  
  # for convenience directly calculate alpha, delta and theta for each age and sex
  Juv_MS <- alpha0             # juvenile mass
  sAd_MS <- alpha0 + alpha7[2] # subadult mass 
  Ad_MS <- alpha0 + alpha7[3]  # adult mass
  
  F_Juv_H <- delta0                           # juvenile female haematocrit
  F_sAd_H <- delta0 + delta4[2]               # subadult female haematocrit 
  F_Ad_H <- delta0 + delta4[3]                # adult female haematocrit
  M_Juv_H <- delta0 + delta5[2]               # juvenile male haematocrit
  M_sAd_H <- delta0 + delta4[2] + delta5[2]   # subadult male haematocrit
  M_Ad_H <- delta0 + delta4[3] + delta5[2]    # adult male haematocrit
  
  F_Juv_B <- theta0                           # juvenile female buffy coat
  F_sAd_B <- theta0 + theta4[2]               # subadult female buffy coat 
  F_Ad_B <- theta0 + theta4[3]                # adult female buffy coat
  M_Juv_B <- theta0 + theta5[2]               # juvenile male buffy coat
  M_sAd_B <- theta0 + theta4[2] + theta5[2]   # subadult male buffy coat
  M_Ad_B <- theta0 + theta4[3] + theta5[2]    # adult male buffy coat
  
  Juv_H <- mean(c(F_Juv_H,M_Juv_H))
  sAd_H <- mean(c(F_sAd_H,M_sAd_H))
  Ad_H <- mean(c(F_Ad_H,M_Ad_H))
  F_H <- mean(c(F_Juv_H,F_sAd_H,F_Ad_H))
  M_H <- mean(c(M_Juv_H,M_sAd_H,M_Ad_H))
  
  Juv_B <- mean(c(F_Juv_B,M_Juv_B))
  sAd_B <- mean(c(F_sAd_B,M_sAd_B))
  Ad_B <- mean(c(F_Ad_B,M_Ad_B))
  F_B <- mean(c(F_Juv_B,F_sAd_B,F_Ad_B))
  M_B <- mean(c(M_Juv_B,M_sAd_B,M_Ad_B))
  
  ## Prior distributions of the precision parameters SEM
  sig1x ~ dunif(0,10)
  #tau1x <- 1/(sig1x*sig1x)
  sig2x ~ dunif(0,10)
  #tau2x <- 1/(sig2x*sig2x)
  sig3x ~ dunif(0,10)
  #tau3x <- 1/(sig3x*sig3x)
  sig4x ~ dunif(0,10)
  #tau4x <- 1/(sig4x*sig4x)
  sig5x ~ dunif(0,10)
  #tau5x <- 1/(sig5x*sig5x)
  sig6x ~ dunif(0,10)
  #tau6x <- 1/(sig6x*sig6x)
  sig7x ~ dunif(0,10)
  #tau7x <- 1/(sig7x*sig7x)
  sig8x ~ dunif(0,10)
  #tau8x <- 1/(sig8x*sig8x)
  sig9x ~ dunif(0,10)
  #tau9x <- 1/(sig9x*sig9x)
  #tau.eta1 <- 1000000000000       # fixing tau.eta1 (=precision) so that the error variance for composite variable is fixed to (close to) 0, precision=inverse variance
  
  # state-to-state movement probability
  for (s in 1:8){    
    log(psiSW1[s]) <- betaSW1[s] # from state 1 in summer -> winter
    betaSW1[s] ~ dnorm(0,1)
    log(psiSW2[s]) <- betaSW2[s] # from state 2 in summer -> winter
    betaSW2[s] ~ dnorm(0,1)
    log(psiSW3[s]) <- betaSW3[s] # from state 3 in summer -> winter
    betaSW3[s] ~ dnorm(0,1)
    log(psiSW4[s]) <- betaSW4[s] # from state 4 in summer -> winter
    betaSW4[s] ~ dnorm(0,1)
    log(psiSW5[s]) <- betaSW5[s] # from state 5 in summer -> winter
    betaSW5[s] ~ dnorm(0,1)
    log(psiSW6[s]) <- betaSW6[s] # from state 6 in summer -> winter
    betaSW6[s] ~ dnorm(0,1)
    log(psiSW7[s]) <- betaSW7[s] # from state 7 in summer -> winter
    betaSW7[s] ~ dnorm(0,1)
    log(psiSW8[s]) <- betaSW8[s] # from state 8 in summer -> winter
    betaSW8[s] ~ dnorm(0,1)
    log(psiSW9[s])<- betaSW9[s] # from state 9 in summer -> winter
    betaSW9[s] ~ dnorm(0,1)
    log(psiWS1[s]) <- betaWS1[s] # from state 1 in winter -> summer
    betaWS1[s] ~ dnorm(0,1)
    log(psiWS2[s]) <- betaWS2[s] # from state 2 in winter -> summer
    betaWS2[s] ~ dnorm(0,1)
    log(psiWS3[s]) <- betaWS3[s] # from state 3 in winter -> summer
    betaWS3[s] ~ dnorm(0,1)
    log(psiWS4[s]) <- betaWS4[s] # from state 4 in winter -> summer
    betaWS4[s] ~ dnorm(0,1)
    log(psiWS5[s]) <- betaWS5[s] # from state 5 in winter -> summer
    betaWS5[s] ~ dnorm(0,1)
    log(psiWS6[s]) <- betaWS6[s] # from state 6 in winter -> summer
    betaWS6[s] ~ dnorm(0,1)
    log(psiWS7[s]) <- betaWS7[s] # from state 7 in winter -> summer
    betaWS7[s] ~ dnorm(0,1)
    log(psiWS8[s]) <- betaWS8[s] # from state 8 in winter -> summer
    betaWS8[s] ~ dnorm(0,1)
    log(psiWS9[s]) <- betaWS9[s] # from state 9 in winter -> summer
    betaWS9[s] ~ dnorm(0,1)
  }
  
  r ~ dunif(0,1) # dead-recovery rate
  
  # for convenience, directly predict (for an average year - i.e. without time random effect) survival rate on natural scale
  #for(k in 1:9){
  #  for(a in 1:3){
  #    S[k,a] <- exp(int.phi[k,a])/(1+exp(int.phi[k,a])) # survival rate (correponds to average year and condition )
  #    P[k,a] <- exp(int.p[k,a])/(1+exp(int.p[k,a])) # detection probability (correponds to average year )
  #  } #a
  #} #k
})

## Observation process: Define probabilities of E(t) given S(t).
#for initial capture, conditional on first capture
#make empty matrix
E0 <- matrix(nrow = 11, ncol = 11)

#fill matrix
E0	[	1	,	1	]<-	0
E0	[	1	,	2	]<-	1
E0	[	1	,	3	]<-	0
E0	[	1	,	4	]<-	0
E0	[	1	,	5	]<-	0
E0	[	1	,	6	]<-	0
E0	[	1	,	7	]<-	0
E0	[	1	,	8	]<-	0
E0	[	1	,	9	]<-	0
E0	[	1	,	10	]<-	0
E0	[	1	,	11	]<-	0

E0	[	2	,	1	]<-	0
E0	[	2	,	2	]<-	0
E0	[	2	,	3	]<-	1
E0	[	2	,	4	]<-	0
E0	[	2	,	5	]<-	0
E0	[	2	,	6	]<-	0
E0	[	2	,	7	]<-	0
E0	[	2	,	8	]<-	0
E0	[	2	,	9	]<-	0
E0	[	2	,	10	]<-	0
E0	[	2	,	11	]<-	0

E0	[	3	,	1	]<-	0
E0	[	3	,	2	]<-	0
E0	[	3	,	3	]<-	0
E0	[	3	,	4	]<-	1
E0	[	3	,	5	]<-	0
E0	[	3	,	6	]<-	0
E0	[	3	,	7	]<-	0
E0	[	3	,	8	]<-	0
E0	[	3	,	9	]<-	0
E0	[	3	,	10	]<-	0
E0	[	3	,	11	]<-	0

E0	[	4	,	1	]<-	0
E0	[	4	,	2	]<-	0
E0	[	4	,	3	]<-	0
E0	[	4	,	4	]<-	0
E0	[	4	,	5	]<-	1
E0	[	4	,	6	]<-	0
E0	[	4	,	7	]<-	0
E0	[	4	,	8	]<-	0
E0	[	4	,	9	]<-	0
E0	[	4	,	10	]<-	0
E0	[	4	,	11	]<-	0

E0	[	5	,	1	]<-	0
E0	[	5	,	2	]<-	0
E0	[	5	,	3	]<-	0
E0	[	5	,	4	]<-	0
E0	[	5	,	5	]<-	0
E0	[	5	,	6	]<-	1
E0	[	5	,	7	]<-	0
E0	[	5	,	8	]<-	0
E0	[	5	,	9	]<-	0
E0	[	5	,	10	]<-	0
E0	[	5	,	11	]<-	0

E0	[	6	,	1	]<-	0
E0	[	6	,	2	]<-	0
E0	[	6	,	3	]<-	0
E0	[	6	,	4	]<-	0
E0	[	6	,	5	]<-	0
E0	[	6	,	6	]<-	0
E0	[	6	,	7	]<-	1
E0	[	6	,	8	]<-	0
E0	[	6	,	9	]<-	0
E0	[	6	,	10	]<-	0
E0	[	6	,	11	]<-	0

E0	[	7	,	1	]<-	0
E0	[	7	,	2	]<-	0
E0	[	7	,	3	]<-	0
E0	[	7	,	4	]<-	0
E0	[	7	,	5	]<-	0
E0	[	7	,	6	]<-	0
E0	[	7	,	7	]<-	0
E0	[	7	,	8	]<-	1
E0	[	7	,	9	]<-	0
E0	[	7	,	10	]<-	0
E0	[	7	,	11	]<-	0

E0	[	8	,	1	]<-	0
E0	[	8	,	2	]<-	0
E0	[	8	,	3	]<-	0
E0	[	8	,	4	]<-	0
E0	[	8	,	5	]<-	0
E0	[	8	,	6	]<-	0
E0	[	8	,	7	]<-	0
E0	[	8	,	8	]<-	0
E0	[	8	,	9	]<-	1
E0	[	8	,	10	]<-	0
E0	[	8	,	11	]<-	0

E0	[	9	,	1	]<-	0
E0	[	9	,	2	]<-	0
E0	[	9	,	3	]<-	0
E0	[	9	,	4	]<-	0
E0	[	9	,	5	]<-	0
E0	[	9	,	6	]<-	0
E0	[	9	,	7	]<-	0
E0	[	9	,	8	]<-	0
E0	[	9	,	9	]<-	0
E0	[	9	,	10	]<-	1
E0	[	9	,	11	]<-	0

E0	[	10	,	1	]<-	0
E0	[	10	,	2	]<-	0
E0	[	10	,	3	]<-	0
E0	[	10	,	4	]<-	0
E0	[	10	,	5	]<-	0
E0	[	10	,	6	]<-	0
E0	[	10	,	7	]<-	0
E0	[	10	,	8	]<-	0
E0	[	10	,	9	]<-	0
E0	[	10	,	10	]<-	0
E0	[	10	,	11	]<-	0

E0	[	11	,	1	]<-	0
E0	[	11	,	2	]<-	0
E0	[	11	,	3	]<-	0
E0	[	11	,	4	]<-	0
E0	[	11	,	5	]<-	0
E0	[	11	,	6	]<-	0
E0	[	11	,	7	]<-	0
E0	[	11	,	8	]<-	0
E0	[	11	,	9	]<-	0
E0	[	11	,	10	]<-	0
E0	[	11	,	11	]<-	0

#constants
constants <- list(n=n,first=first,T=T,age=age,SW = SW, WS= WS, cov=stcov , Sex=Sex, AgeH=AgeH, TCatch=TCatch,
                  int.phi.meanprior=int.phi.meanprior, int.phi.sdprior=int.phi.sdprior,
                  int.p.meanprior=int.p.meanprior, int.p.sdprior=int.p.sdprior)

#data
mydatax <- list(mydata = data.matrix(CH+1), E0=E0)

# initial states matrix
alive1 <- CH
colnames(alive1) <- NULL
alive1[CH==0] <- 1

for(i in 1:n){
  if(first[i]>1){alive1[i,1:(first[i]-1)] <- NA}
  if(last[i]==(T-1)){alive1[i,T] <- 11} 
  if(last[i]<(T-1)){alive1[i,(last[i]+1):T] <- 11} 
}  

vector.alive1<-as.vector(t(alive1))
alive2 <- matrix(vector.alive1, nrow = T, ncol = n)
alive2 <- t(alive2)  # matrix transposed

# for reproducibility
mySeed <- 9999
set.seed(mySeed)

# Initial values
initsFunction <- function() list(alive=alive2, 
                                 pi=rnorm(8,0,1), r=runif(1,0,1),sigmat.phi=runif(9,0,10), sigmat.p=runif(9,0,10),
                                 sig1x=runif(1,0,10),sig2x=runif(1,0,10),sig3x=runif(1,0,10),sig4x=runif(1,0,10),sig5x=runif(1,0,10),sig6x=runif(1,0,10), sig7x=runif(1,0,10), sig8x=runif(1,0,10), sig9x=runif(1,0,10), 
                                 #gamma1=rnorm(1,0,1), 
                                 #gamma12=rnorm(1,0,1), 
                                 gamma2=rnorm(1,0,1), 
                                 #gamma22=rnorm(1,0,1), 
                                 gamma3=rnorm(1,0,1),  #gamma32=rnorm(1,0,1), 
                                 beta1=rnorm(1,0,1), beta2=rnorm(1,0,1), 
                                 eta1=rnorm(i,0,1),
                                 alpha0=rnorm(1,0,1), alpha1=rnorm(1,0,1),alpha2=rnorm(1,0,1),alpha3=rnorm(1,0,1),alpha4=rnorm(1,0,1),alpha5=rnorm(1,0,1),alpha52=rnorm(1,0,1),alpha6=rnorm(1,0,1),alpha7=rnorm(3,0,1),
                                 delta0=rnorm(1,0,1), delta1=rnorm(1,0,1),delta2=rnorm(1,0,1),delta22=rnorm(1,0,1),delta3=rnorm(1,0,1),delta4=rnorm(3,0,1),delta5=rnorm(2,0,1),
                                 theta0=rnorm(1,0,1), theta1=rnorm(1,0,1), theta2=rnorm(1,0,1),  theta22=rnorm(1,0,1), theta3=rnorm(1,0,1), theta4=rnorm(3,0,1), theta5=rnorm(2,0,1),
                                 betaSW1=rnorm(8,0,1),betaSW2=rnorm(8,0,1),betaSW3=rnorm(8,0,1),betaSW4=rnorm(8,0,1),betaSW5=rnorm(8,0,1),betaSW6=rnorm(8,0,1),betaSW7=rnorm(8,0,1),betaSW8=rnorm(8,0,1),betaSW9=rnorm(8,0,1),
                                 betaWS1=rnorm(8,0,1),betaWS2=rnorm(8,0,1),betaWS3=rnorm(8,0,1),betaWS4=rnorm(8,0,1),betaWS5=rnorm(8,0,1),betaWS6=rnorm(8,0,1),betaWS7=rnorm(8,0,1),betaWS8=rnorm(8,0,1),betaWS9=rnorm(8,0,1),
                                 mu1=rnorm(i,0,1), mu2=rnorm(i,0,1),mu3=rnorm(i,0,1),
                                 psiSW1=rnorm(8,0,1),psiSW2=rnorm(8,0,1),psiSW3=rnorm(8,0,1),psiSW4=rnorm(8,0,1),psiSW5=rnorm(8,0,1),psiSW6=rnorm(8,0,1), psiSW7=rnorm(8,0,1), psiSW8=rnorm(8,0,1),psiSW9=rnorm(8,0,1),
                                 psiWS1=rnorm(8,0,1), psiWS2=rnorm(8,0,1),psiWS3=rnorm(8,0,1),psiWS4=rnorm(8,0,1),psiWS5=rnorm(8,0,1),psiWS6=rnorm(8,0,1),psiWS7=rnorm(8,0,1), psiWS8=rnorm(8,0,1),psiWS9=rnorm(8,0,1),
                                 prop=runif(8,0,1), S0=runif(11,0,1),
                                 mu.phi=array(c(matrix(runif(14166,0,1))), dim = c(9, i, T)),
                                 p=array(c(matrix(runif(14166,0,1))), dim = c(9, i, T)),
                                 phi=array(c(matrix(runif(14166,0,1))), dim = c(9, i, T)),
                                 epst.phi=matrix(rnorm(162, 0, 1), 9,T), epst.p=matrix(rnorm(162, 0, 1), 9,T),
                                 #P=matrix(rnorm(27,0,1), 9, 3) , S=matrix(rnorm(27,0,1), 9, 3),
                                 int.p=matrix(runif(27,0,1), 9, 3), int.phi=matrix(runif(27,0,1), 9, 3),
                                 int.p.alpha=matrix(runif(27,0,100000), 9, 3), int.phi.alpha=matrix(runif(27,0,100000), 9, 3),
                                 int.p.beta=matrix(runif(27,0,100000), 9, 3), int.phi.beta=matrix(runif(27,0,100000), 9, 3),
                                 PSI=array(c(matrix(runif(121,0,1))), dim = c(11,11,i,T)),
                                 E=array(c(matrix(runif(121,0,1))), dim = c(11,11,i,T))
)

initsList1 <- initsFunction()
initsList2 <- initsFunction()
initVals <- c(initsList1,initsList2)

params<-c(#"S","P",
  #"alive", "cov",
  "phi", "p", 
  "int.phi","int.p",
  "r",
  "pi",
  "sigmat.p", "sigmat.phi",
  "epst.phi", "epst.p", 
  "betaSW1","betaSW2","betaSW3","betaSW4","betaSW5","betaSW6","betaSW7","betaSW8", "betaSW9",
  "betaWS1","betaWS2","betaWS3","betaWS4","betaWS5","betaWS6","betaWS7","betaWS8","betaWS9",
  "beta1","beta2",
  #"gamma1", 
  #"gamma12",
  "gamma2",
  #"gamma22",
  "gamma3",#"gamma32",
  "eta1",
  "mu1", "mu2", "mu3",
  "alpha0", "alpha1", "alpha2", "alpha3", "alpha4","alpha5","alpha52", "alpha6", "alpha7",
  "delta0", "delta1","delta2","delta22", "delta3","delta4","delta5",
  "theta0", "theta1","theta2","theta22","theta3","theta4","theta5", 
  "sig1x","sig2x","sig3x","sig4x","sig5x","sig6x","sig7x","sig8x","sig9x",
  "Juv_MS", "sAd_MS", "Ad_MS", 
  "F_Juv_H", "F_sAd_H", "F_Ad_H",
  "M_Juv_H", "M_sAd_H", "M_Ad_H",
  "F_Juv_B", "F_sAd_B", "F_Ad_B",
  "M_Juv_B", "M_sAd_B", "M_Ad_B",
  "Juv_H", "sAd_H", "Ad_H","F_H", "M_H",
  "Juv_B", "sAd_B", "Ad_B","F_B", "M_B" 
)

#memory.limit(size=1000000)

# build model (not compiled yet)
#deb = Sys.time()
#RModel<-nimbleModel(
#  code = NimbleCode,
#  data=mydatax,
#  constants = constants, 
#  inits = initVals, 
#  calculate=F) #eventually calculate=F to make it faster
#fin = Sys.time()
#duration1=fin - deb
#duration1 #with calc=F 50 minutes

#deb = Sys.time()
#mcmc.out <- nimbleMCMC(model = RModel, data = mydatax, inits = initVals,
#                       constants = constants,
#                       monitors = params, thin = 1,
#                       niter = 2, nburnin=1, nchains = 2, setSeed = mySeed,
#                       summary=TRUE, WAIC=FALSE, samples=TRUE,
#                       progressBar = getNimbleOption("MCMCprogressBar")
#                       ) # progressBar=T
#fin = Sys.time()
#duration2=fin - deb
#duration2 #

# build model (not compiled yet)
deb = Sys.time()
RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax,
  constants = constants, 
  inits = initVals, 
  calculate=F) #eventually calculate=F to make it faster
fin = Sys.time()
duration1=fin - deb
duration1 #with calc=F 58 minutes

#Now compile the model in `C++`.
deb = Sys.time()
Cmodel <- compileNimble(RModel)
fin = Sys.time()
duration3=fin - deb
duration3 #1.6 hrs

#Specify MCMC for SEM
deb = Sys.time()
conf <- configureMCMC(RModel, monitors=params, useConjugacy = FALSE) #eventually useConjugacy = FALSE
fin = Sys.time()
duration4=fin - deb
duration4 #33 secs 

#Build an executable MCMC for SEM
deb = Sys.time()
Rmcmc <- buildMCMC(conf, enableWAIC = FALSE) #conf, Cmodel, enableWAIC = TRUE
fin = Sys.time()
duration5=fin - deb
duration5 #48 mins

#Compile in `C++`. 
deb = Sys.time()
Cmcmc <- compileNimble(Rmcmc, project = Cmodel, showCompilerOutput = F)
fin = Sys.time()
duration6=fin - deb
duration6 #20 mins 

#Run compiled model (do not run the uncompiled model with `runMCMC(Rmcmc,100)`).
deb = Sys.time()
samplesList <- runMCMC(Cmcmc,
                       niter = 10000, #10000
                       nburnin = 1000, #5000
                       nchains = 2, samplesAsCodaMCMC=TRUE, WAIC=FALSE, 
                       thin=10, summary=TRUE, setSeed = mySeed) #WAIC=TRUE
#summary=TRUE,samplesAsCodaMCMC=TRUE)#WAIC=TRUE,
fin = Sys.time()
duration7=fin - deb
duration7 #22 hrs

# for phi, alive and p there are some NAs for different time occasions, this has to to with the first capture of specific indiviuduals
# birds caught at 2nd occasion, will have NA at 1st time step (phi[k,i,j])

# calculate WAIC for model comparison
samplesList$WAIC #44774.98, 1603720

############# SEM part ###############################
#setwd("N:/Dep.AnE/AnE-share/_Magali/Nimble_M1_P12/Output")

#Summary of posterior distributions for each parameter:
## make list with different variables
samplesSummaryEta<-round(samplesList$summary$all.chains[grep("eta1", rownames(samplesList$summary$all.chains)), ],2)
samplesSummaryEta<-samplesSummaryEta[2:1575,] #last variable is theta1
samplesSummaryMu1<-round(samplesList$summary$all.chains[grep("mu1", rownames(samplesList$summary$all.chains)), ],2)
samplesSummaryMu2<-round(samplesList$summary$all.chains[grep("mu2", rownames(samplesList$summary$all.chains)), ],2)
samplesSummaryMu3<-round(samplesList$summary$all.chains[grep("mu3", rownames(samplesList$summary$all.chains)), ],2)
samplesSummaryPathCoef <- round(samplesList$summary$all.chains[c("alpha0", "alpha1","alpha2", "alpha3","alpha4","alpha5",
                                                                 "alpha52",
                                                                 "alpha6" ,"alpha7[1]","alpha7[2]",
                                                                 "alpha7[3]", "delta0", 
                                                                 "delta1","delta2","delta22", "delta3","delta4[1]", "delta4[2]","delta4[3]",
                                                                 "delta5[1]","delta5[2]", "theta0", 
                                                                 "theta1","theta2","theta22", "theta3","theta4[1]","theta4[2]","theta4[3]",
                                                                 "theta5[1]","theta5[2]", 
                                                                 "sig1x","sig2x","sig3x","sig4x","sig5x","sig6x","sig7x","sig8x","sig9x",
                                                                 "beta1","beta2", 
                                                                 #"gamma1",
                                                                 #"gamma12",
                                                                 "gamma2",
                                                                 #"gamma22",
                                                                 "gamma3",#"gamma32",
                                                                 'F_Juv_B', 'F_sAd_B', 'F_Ad_B', 'M_Juv_B', 'M_sAd_B', 'M_Ad_B',
                                                                 'F_Juv_H', 'F_sAd_H', 'F_Ad_H', 'M_Juv_H', 'M_sAd_H', 'M_Ad_H',
                                                                 'Juv_MS', "sAd_MS", "Ad_MS",
                                                                 'Juv_H', 'sAd_H', 'Ad_H',
                                                                 'Juv_B', 'sAd_B', 'Ad_B',
                                                                 'F_B', 'M_B', 'F_H', 'M_H'
),1:5],2)

samplesSummarySurv_r <- round(samplesList$summary$all.chains[c("r"),1:5],2)
samplesSummarySurv_int.phi_int.p <- round(samplesList$summary$all.chains[grep("int.p", rownames(samplesList$summary$all.chains)), ],2)
samplesSummarySurv_sigmat.phi_sigmat.p <- round(samplesList$summary$all.chains[grep("sigmat.p", rownames(samplesList$summary$all.chains)), ],2) 
samplesSummarySurv_pi <- round(samplesList$summary$all.chains[grep("pi", rownames(samplesList$summary$all.chains)), ],2) 
samplesSummarySurv_betaSW <- round(samplesList$summary$all.chains[grep("betaSW", rownames(samplesList$summary$all.chains)), ],2) 
samplesSummarySurv_betaWS <- round(samplesList$summary$all.chains[grep("betaWS", rownames(samplesList$summary$all.chains)), ],2) 
#samplesSummarySurv_S <- round(samplesList$summary$all.chains[grep("S", rownames(samplesList$summary$all.chains)), ],2) 
#samplesSummarySurv_S<-samplesSummarySurv_S[1:27,] #last variable is theta1
#samplesSummarySurv_P <- round(samplesList$summary$all.chains[grep("P", rownames(samplesList$summary$all.chains)), ],2)
samplesSummarySurv_phi <- round(samplesList$summary$all.chains[grep("phi", rownames(samplesList$summary$all.chains)), ],2) 
samplesSummarySurv_epst.phi<-samplesSummarySurv_phi[1:162,] #last variable is theta1
samplesSummarySurv_phi<-samplesSummarySurv_phi[190:255177,] #last variable is theta1
samplesSummarySurv_Resight_p <- round(samplesList$summary$all.chains[grep('p', rownames(samplesList$summary$all.chains)), ],2)
samplesSummarySurv_epst.p<-samplesSummarySurv_Resight_p[10:171,] #last variable is theta1
samplesSummarySurv_Resight_p<-samplesSummarySurv_Resight_p[388:255375,] #last variable is theta1

# save output
write.csv(samplesSummaryPathCoef,"Output/Output_CR_SEM_nimble_PathCoef.csv", row.names =TRUE)
write.csv(samplesSummaryEta,"Output/Output_CR_SEM_nimble_Eta.csv", row.names =TRUE)
write.csv(samplesSummaryMu1,"Output/Output_CR_SEM_nimble_Mu1.csv", row.names =TRUE)
write.csv(samplesSummaryMu2,"Output/Output_CR_SEM_nimble_Mu2.csv", row.names =TRUE)
write.csv(samplesSummaryMu3,"Output/Output_CR_SEM_nimble_Mu3.csv", row.names =TRUE)

write.csv(samplesSummarySurv_r,"Output/Output_CR_SEM_nimble_Surv_r.csv", row.names =TRUE)
write.csv(samplesSummarySurv_int.phi_int.p,"Output/Output_CR_SEM_nimble_Surv_int.phi_int.p.csv", row.names =TRUE)
write.csv(samplesSummarySurv_sigmat.phi_sigmat.p,"Output/Output_CR_SEM_nimble_Surv_sigmat.phi_sigmat.p.csv", row.names =TRUE)
write.csv(samplesSummarySurv_pi,"Output/Output_CR_SEM_nimble_Surv_pi.csv", row.names =TRUE)
write.csv(samplesSummarySurv_betaSW,"Output_CR_SEM_nimble_Surv_betaSW.csv", row.names =TRUE)
write.csv(samplesSummarySurv_betaWS,"Output/Output_CR_SEM_nimble_Surv_betaWS.csv", row.names =TRUE)
#write.csv(samplesSummarySurv_S,"Output_CR_SEM_nimble_Surv_S.csv", row.names =TRUE)
#write.csv(samplesSummarySurv_P,"Output_CR_SEM_nimble_Surv_P.csv", row.names =TRUE)
write.csv(samplesSummarySurv_phi,"Output/Output_CR_SEM_nimble_Surv_phi.csv", row.names =TRUE)
write.csv(samplesSummarySurv_Resight_p,"Output/Output_CR_SEM_nimble_Surv_Resight_p.csv", row.names =TRUE)
write.csv(samplesSummarySurv_epst.p,"Output/Output_CR_SEM_nimble_Surv_epst.p.csv", row.names =TRUE)
write.csv(samplesSummarySurv_epst.phi,"Output/Output_CR_SEM_nimble_Surv_epst.phi.csv", row.names =TRUE)

##################################### convergence #########################################
##### extract parameters in different lists for plotting chains
#Produce trace an density plots. 
library(basicMCMCplots)
# SEM
chainsPlot(samplesList$samples, #width=4000, height=6000,
           var = c("alpha0", "alpha1", "alpha2", "alpha3", "alpha4"),
           file="Output/SEMparameters_convergence1.pdf")
chainsPlot(samplesList$samples,
           var = c("alpha5", "alpha52", "alpha6", "alpha7[2]", "alpha7[3]"),
           file="Output/SEMparameters_convergence2.pdf")
chainsPlot(samplesList$samples,
           var = c("delta0","delta1", "delta2","delta22", "delta3"),
           file="Output/SEMparameters_convergence3.pdf")
chainsPlot(samplesList$samples,
           var = c("delta4[2]", "delta4[3]","delta5[2]", "theta0", "theta1"),
           file="Output/SEMparameters_convergence4.pdf")
chainsPlot(samplesList$samples,
           var = c("theta2", "theta22", "theta3", "theta4[2]", "theta4[3]"),
           file="Output/SEMparameters_convergence5.pdf")
chainsPlot(samplesList$samples,
           var = c("theta5[2]", "sig1x","sig2x","sig3x","sig4x"),
           file="Output/SEMparameters_convergence6.pdf")
chainsPlot(samplesList$samples,
           var = c("sig5x","sig6x","sig7x","sig8x","sig9x"),
           file="Output/SEMparameters_convergence7.pdf")
chainsPlot(samplesList$samples,
           var = c("beta1","beta2", "gamma1", "gamma12", "gamma2"),
           file="Output/SEMparameters_convergence7a.pdf")
#chainsPlot(samplesList$samples,
#           var = c( "gamma22", "gamma3", "gamma32"),
#           file="Output/SEMparameters_convergence7b.pdf")
chainsPlot(samplesList$samples,
           var = c("eta1[160]","eta1[320]","eta1[480]","eta1[560]","eta1[720]"),
           file="Output/SEMparameters_convergence8.pdf")
chainsPlot(samplesList$samples,
           var = c("mu1[160]","mu1[320]","mu1[480]","mu1[560]","mu1[720]"),
           file="Output/SEMparameters_convergence9.pdf")
chainsPlot(samplesList$samples,
           var = c("mu2[160]","mu2[320]","mu2[480]","mu2[560]","mu2[720]"),
           file="Output/SEMparameters_convergence10.pdf")
chainsPlot(samplesList$samples,
           var = c("mu3[160]","mu3[320]","mu3[480]","mu3[560]","mu3[720]"),
           file="Output/SEMparameters_convergence11.pdf")
#Surv
chainsPlot(samplesList$samples,
           var = c("betaSW2[5]","betaSW4[2]","betaSW5[7]","betaSW7[4]","betaSW9[1]"),
           file="Output/SurvParameters_convergence1.pdf")
chainsPlot(samplesList$samples,
           var = c("betaWS2[5]","betaWS4[2]","betaWS5[7]","betaWS7[4]","betaWS9[1]"),
           file="Output/SurvParameters_convergence2.pdf")
chainsPlot(samplesList$samples,
           var = c("int.p[1, 1]","int.p[7, 1]","int.p[4, 2]","int.p[1, 3]","int.p[7, 3]"),
           file="Output/SurvParameters_convergence3.pdf")
chainsPlot(samplesList$samples,
           var = c("int.phi[1, 1]","int.phi[7, 1]","int.phi[4, 2]","int.phi[1, 3]","int.phi[7, 3]"),
           file="Output/SurvParameters_convergence4.pdf")
chainsPlot(samplesList$samples,
           var = c("pi[1]","pi[2]","pi[4]","pi[6]","pi[8]"),
           file="Output/SurvParameters_convergence5.pdf")
chainsPlot(samplesList$samples,
           var = c("sigmat.p[1]","sigmat.p[3]","sigmat.p[5]","sigmat.p[7]","sigmat.p[9]"),
           file="Output/SurvParameters_convergence6.pdf")
chainsPlot(samplesList$samples,
           var = c("sigmat.phi[1]","sigmat.phi[3]","sigmat.phi[5]","sigmat.phi[7]","sigmat.phi[9]"),
           file="Output/SurvParameters_convergence7.pdf")
#chainsPlot(samplesList$samples,
#           var = c("S[1, 1]","S[7, 1]","S[4, 2]","S[1, 3]","S[7, 3]"),
#           file="Output/SurvParameters_convergence8.pdf")
#chainsPlot(samplesList$samples,
#           var = c("P[1, 1]","P[7, 1]","P[4, 2]","P[1, 3]","P[7, 3]"),
#           file="SurvParameters_convergence7.pdf")
chainsPlot(samplesList$samples,
           var = c("phi[1, 2, 3]", "phi[3, 9, 5]", "phi[5, 566, 7]","phi[6, 340, 7]","phi[5, 682, 8]"),
           file="Output/SurvParameters_convergence8.pdf")
chainsPlot(samplesList$samples,
           var = c("p[1, 2, 3]", "p[3, 9, 5]", "p[5, 566, 7]","p[6, 340, 7]","p[5, 682, 8]"),
           file="Output/SurvParameters_convergence9.pdf")
chainsPlot(samplesList$samples,
           var = c("epst.p[4, 2]","epst.p[1, 4]","epst.p[5, 5]","epst.p[3, 7]","epst.p[9, 7]"),
           file="Output/SurvParameters_convergence10.pdf")
chainsPlot(samplesList$samples,
           var = c("epst.phi[4, 2]","epst.phi[1, 4]","epst.phi[5, 5]","epst.phi[3, 7]","epst.phi[9, 7]"),
           file="Output/SurvParameters_convergence11.pdf")

#combine all separate pdfs
library(pdftools)
pdf_combine(c("SEMparameters_convergence1.pdf","SEMparameters_convergence2.pdf",
              "SEMparameters_convergence3.pdf","SEMparameters_convergence4.pdf",
              "SEMparameters_convergence5.pdf","SEMparameters_convergence6.pdf",
              "SEMparameters_convergence7.pdf","SEMparameters_convergence8.pdf",
              "SEMparameters_convergence9.pdf","SEMparameters_convergence11.pdf",
              "SEMparameters_convergence11.pdf","SEMparameters_convergence12.pdf"), 
            output = "SEMparameters_convergence_joined.pdf")

#R-hat SEM
prhat_SEM = gelman.diag(samplesList$samples
                        [,c("alpha0", "alpha1", "alpha2", "alpha3", "alpha4", "alpha5","alpha52","alpha6", 
                            "alpha7[2]","alpha7[3]","delta0", "delta1", "delta2", "delta22",
                            "delta3", "delta4[2]", "delta4[3]", "delta5[2]", "theta0", 
                            "theta1", "theta2", "theta22", "theta3", "theta4[2]", "theta4[3]", 
                            "theta5[2]", "sig1x","sig2x","sig3x","sig4x","sig5x","sig6x",
                            "sig7x","sig8x","sig9x" ,"beta1","beta2",
                            #"gamma1", 
                            "gamma2", #"gamma22", "gamma32",
                            "gamma3", "eta1[160]", "eta1[320]", 
                            "eta1[480]", "eta1[560]", "eta1[720]","mu1[160]", "mu1[320]", "mu1[480]", 
                            "mu1[560]", "mu1[720]", "mu2[160]", "mu2[320]", "mu2[480]", "mu2[560]", 
                            "mu2[720]", "mu3[160]", "mu3[320]", "mu3[480]", "mu3[560]", "mu3[720]"
                        )],multivariate = FALSE)
prhat_SEM$psrf[,1]
GelRubSem<-as.data.frame(prhat_SEM$psrf[,1])
names(GelRubSem)[1] <- "PSRF" #potential scale reduction factor 
write.csv2(GelRubSem,"Output/GelRubSem.csv", 
           row.names =TRUE)

#R-hat SEM
prhat_Surv = gelman.diag(samplesList$samples
                         [,c("betaSW2[5]","betaSW4[2]","betaSW5[7]","betaSW7[4]","betaSW9[1]",
                             "betaWS2[5]","betaWS4[2]","betaWS5[7]","betaWS7[4]","betaWS9[1]",
                             #"S[1, 1]","S[7, 1]","S[4, 2]","S[1, 3]","S[7, 3]",
                             #"P[1, 1]","P[7, 1]","P[4, 2]","P[1, 3]","P[7, 3]", 
                             "pi[1]","pi[2]","pi[4]","pi[6]","pi[8]",
                             "sigmat.p[1]","sigmat.p[3]","sigmat.p[5]","sigmat.p[7]","sigmat.p[9]", 
                             "sigmat.phi[1]","sigmat.phi[3]","sigmat.phi[5]","sigmat.phi[7]","sigmat.phi[9]", 
                             "phi[1, 2, 3]", "phi[3, 9, 5]", "phi[5, 566, 7]","phi[6, 340, 7]","phi[5, 682, 8]",
                             "p[1, 2, 3]", "p[3, 9, 5]", "p[5, 566, 7]","p[6, 340, 7]","p[5, 682, 8]",
                             "epst.p[4, 2]","epst.p[1, 4]","epst.p[5, 5]","epst.p[3, 7]","epst.p[9, 7]",
                             "epst.phi[4, 2]","epst.phi[1, 4]","epst.phi[5, 5]","epst.phi[3, 7]","epst.phi[9, 7]"
                         )],multivariate = FALSE)
prhat_Surv$psrf[,1]
GelRubSurv<-as.data.frame(prhat_Surv$psrf[,1])
names(GelRubSurv)[1] <- "PSRF" #potential scale reduction factor 
write.csv2(GelRubSurv,"Output/GelRubSurv.csv", 
           row.names =TRUE)

# plot coefficients per chain
library(mcmcplots)
# Open a pdf file
png("Output/SEMparameters_CoefChains.png",
    width = 5000, height = 5000, res = 600) 
par(mfrow=c(3,2))
par(mar = c(4.5, 4.5, 0.5, 0.5))
caterplot(samplesList$samples, parms = c("alpha0", "alpha1", "alpha2", "alpha3", "alpha4", "alpha5", "alpha52", "alpha6", "alpha7[2]","alpha7[3]"),
          labels=c("alpha[0]", "alpha[1]", "alpha[2]", "alpha[3]", "alpha[4]", "alpha[5]", "alpha[52]", "alpha[6]", "alpha[7][2]","alpha[7][3]"),
          #labels = c("tato", "tohe", "wl", "bh", "ht", "ds", "Juv", "sAd", "Ad"),
          collapse=F, greek=T, cex.labels = 1.2)

caterplot(samplesList$samples, parms = c("delta0", "delta1", "delta2", "delta22", "delta3", "delta4[2]", "delta4[3]", "delta5[2]"),
          labels=c("delta[0]", "delta[1]", "delta[2]", "delta[22]", "delta[3]", "delta[4][2]", "delta[4][3]", "delta[5][2]"),
          #labels = c("bh", "ht", "ds", "F_Juv", "F_sAd", "F_Ad", "M_Juv", "M_sAd", "M_Ad"),
          collapse=F, greek=T, cex.labels = 1.2)

caterplot(samplesList$samples, parms = c("theta0", "theta1", "theta2", "theta22", "theta3", "theta4[2]", "theta4[3]", "theta5[2]"),
          labels=c("theta[0]","theta[1]", "theta[2]", "theta[22]", "theta[3]", "theta[4][2]", "theta[4][3]", "theta[5][2]"),
          #labels = c("intercept", "bh", "ht", "ds", "ageJuv", "ageSA", "ageAd", "females", "males"),
          collapse=F, greek=T, cex.labels = 1.2)

caterplot(samplesList$samples, parms = c("sig1x","sig2x","sig3x","sig4x","sig5x","sig6x","sig7x","sig8x","sig9x"),
          labels=c("sigma[1][x]","sigma[2][x]","sigma[3][x]","sigma[4][x]","sigma[5][x]","sigma[6][x]","sigma[7][x]","sigma[8][x]","sigma[9][x]"),
          #labels = c("SD_mass", "SD_Haema", "SD_BuffyCoat", "SD_tato", "SD_tohe", "SD_WL", "SD_BH", "SD_HT", "SD_DS"),
          collapse=F, greek=T, cex.labels = 1.2)

caterplot(samplesList$samples, parms = c("beta1","beta2", "gamma1","gamma12", "gamma2", "gamma22", "gamma3", "gamma32"), 
          #labels = c("condition","mass", "haema", "buffy coat"),# 
          collapse=F)

mtext("a)", side=3, adj=-2.65, line=2, cex=1.3)
mtext("b)", side=3, adj=-1.4, line=2, cex=1.3)
mtext("c)", side=3, adj=-0.15, line=2, cex=1.3)
mtext("d)", side=3, adj=-0.15, line=2, cex=1.3)
mtext("e)", side=3, adj=-0.15, line=2, cex=1.3)

# Close the png file
dev.off() 

###### coefficient plots without chain
library(MCMCvis)
png("Output/SEMparameters_Coef1.png",
    width = 11000, height = 4500, res = 600) 
par(mfrow=c(1,3),
    oma = c(0,2,0,0))
#MCMCplot(samplesList$samples, params=c("alpha7[1]"))

MCMCplot(samplesList$samples, params =c('alpha1', 'alpha2', 'alpha3','alpha4', 'alpha5', 'alpha52', 'alpha6', 'Juv_MS', 'sAd_MS', 'Ad_MS'),
         labels = c("Tarsus-toe", "Total-head", "Winglength", "Billtipheight", "Handling time", expression('Handling time'^2), "Catch day", "1-yr-olds", 
                    "2-yr-olds", ">3 yr-olds"),
         main="Effect on body mass", sz_labels=1.4, sz_med=2, sz_main_txt=1.5)

MCMCplot(samplesList$samples, params =c('delta1', 'delta2', 'delta22', 'delta3','Juv_H', 'sAd_H', 'Ad_H', 'F_H', 'M_H'),
         labels = c("Billtipheight", "Handling time",expression('Handling time'^2), "Catch day", "1-yr-olds", 
                    "2-yr-olds", ">3 yr-olds", "Females", "Males"),
         main="Effect on haematocrit", sz_labels=1.4, sz_med=2, sz_main_txt=1.5)

MCMCplot(samplesList$samples, params =c('theta1', 'theta2','theta22', 'theta3','Juv_B', 'sAd_B', 'Ad_B', 'F_B', 'M_B'),
         labels = c("Billtipheight", "Handling time", expression('Handling time'^2),"Catch day", "1-yr-olds", 
                    "2-yr-olds", ">3 yr-olds", "Females", "Males"),
         main="Effect on buffy coat", sz_labels=1.4, sz_med=2, sz_main_txt=1.5)

mtext("a)", side=3, adj=-2.65, line=2, cex=1.3)
mtext("b)", side=3, adj=-1.4, line=2, cex=1.3)
mtext("c)", side=3, adj=-0.15, line=2, cex=1.3)

# Close the png file
dev.off() 

png("Output/SEMparameters_Coef2.png",
    width = 7000, height = 5000, res = 600) 
par(mfrow=c(1,2))

MCMCplot(samplesList$samples, params =c('gamma2', 'gamma3'),
  labels=c(#"Mass", 
    #expression('Mass'^2) ,
      "Haematocrit", #expression('Haematocrit'^2), 
      "Buffy coat" #expression('Buffy coat'^2)
      ),
  main="Effect on condition")

MCMCplot(samplesList$samples, params =c('beta1', 'beta2'),
         labels=c("Condition", 'Catch day'),
         main="Effect on survival")

mtext("d)", side=3, adj=-1.6, line=1.7, cex=1.5)
mtext("e)", side=3, adj=-0.2, line=1.7, cex=1.5)
dev.off() 


png("Output/SEMparameters_Sig.png",
    width = 5000, height = 5000, res = 600) 
par(mfrow=c(1,1))
MCMCplot(samplesList$samples, params =c("sig1x","sig2x","sig3x","sig4x","sig5x","sig6x","sig7x","sig8x","sig9x"),
         labels = c("sig1x","sig2x","sig3x","sig4x","sig5x","sig6x","sig7x","sig8x","sig9x"))
dev.off() 


#save.image(file='NimbleModel_M1_P12_params.RData')

### ESS ##
samplesAlpha0 <- list(samplesList$samples$chain1[,"alpha0"],
                      samplesList$samples$chain2[,"alpha0"])
ESS_Alpha0<-effectiveSize(samplesAlpha0) 
samplesAlpha1 <- list(samplesList$samples$chain1[,"alpha1"],
                      samplesList$samples$chain2[,"alpha1"])
ESS_Alpha1<-effectiveSize(samplesAlpha1)
samplesAlpha2 <- list(samplesList$samples$chain1[,"alpha2"],
                      samplesList$samples$chain2[,"alpha2"])
ESS_Alpha2<-effectiveSize(samplesAlpha2)
samplesAlpha3 <- list(samplesList$samples$chain1[,"alpha3"],
                      samplesList$samples$chain2[,"alpha3"])
ESS_Alpha3<-effectiveSize(samplesAlpha3)
samplesAlpha4 <- list(samplesList$samples$chain1[,"alpha4"],
                      samplesList$samples$chain2[,"alpha4"])
ESS_Alpha4<-effectiveSize(samplesAlpha4)
samplesAlpha5 <- list(samplesList$samples$chain1[,"alpha5"],
                      samplesList$samples$chain2[,"alpha5"])
ESS_Alpha5<-effectiveSize(samplesAlpha5)
samplesAlpha6 <- list(samplesList$samples$chain1[,"alpha6"],
                      samplesList$samples$chain2[,"alpha6"])
ESS_Alpha6<-effectiveSize(samplesAlpha6)
samplesAlpha7_2 <- list(samplesList$samples$chain1[,"alpha7[2]"],
                        samplesList$samples$chain2[,"alpha7[2]"])
ESS_Alpha7_2<-effectiveSize(samplesAlpha7_2) 
samplesAlpha7_3 <- list(samplesList$samples$chain1[,"alpha7[3]"],
                        samplesList$samples$chain2[,"alpha7[3]"])
ESS_Alpha7_3<-effectiveSize(samplesAlpha7_3) 

samplesDelta0 <- list(samplesList$samples$chain1[,"delta0"],
                      samplesList$samples$chain2[,"delta0"])
ESS_Delta0<-effectiveSize(samplesDelta0) 
samplesDelta1 <- list(samplesList$samples$chain1[,"delta1"],
                      samplesList$samples$chain2[,"delta1"])
ESS_Delta1<-effectiveSize(samplesDelta1)
samplesDelta2 <- list(samplesList$samples$chain1[,"delta2"],
                      samplesList$samples$chain2[,"delta2"])
ESS_Delta2<-effectiveSize(samplesDelta2)
samplesDelta3 <- list(samplesList$samples$chain1[,"delta3"],
                      samplesList$samples$chain2[,"delta3"])
ESS_Delta3<-effectiveSize(samplesDelta3)
samplesDelta4_2 <- list(samplesList$samples$chain1[,"delta4[2]"],
                        samplesList$samples$chain2[,"delta4[2]"])
ESS_Delta4_2<-effectiveSize(samplesDelta4_2)
samplesDelta4_3 <- list(samplesList$samples$chain1[,"delta4[3]"],
                        samplesList$samples$chain2[,"delta4[3]"])
ESS_Delta4_3<-effectiveSize(samplesDelta4_3)
samplesDelta5_2 <- list(samplesList$samples$chain1[,"delta5[2]"],
                        samplesList$samples$chain2[,"delta5[2]"])
ESS_Delta5_2<-effectiveSize(samplesDelta5_2)

samplesTheta0 <- list(samplesList$samples$chain1[,"theta0"],
                      samplesList$samples$chain2[,"theta0"])
ESS_Theta0<-effectiveSize(samplesTheta0) 
samplesTheta1 <- list(samplesList$samples$chain1[,"theta1"],
                      samplesList$samples$chain2[,"theta1"])
ESS_Theta1<-effectiveSize(samplesTheta1)
samplesTheta2 <- list(samplesList$samples$chain1[,"theta2"],
                      samplesList$samples$chain2[,"theta2"])
ESS_Theta2<-effectiveSize(samplesTheta2)
samplesTheta3 <- list(samplesList$samples$chain1[,"theta3"],
                      samplesList$samples$chain2[,"theta3"])
ESS_Theta3<-effectiveSize(samplesTheta3)
samplesTheta4_2 <- list(samplesList$samples$chain1[,"theta4[2]"],
                        samplesList$samples$chain2[,"theta4[2]"])
ESS_Theta4_2<-effectiveSize(samplesTheta4_2)
samplesTheta4_3 <- list(samplesList$samples$chain1[,"theta4[3]"],
                        samplesList$samples$chain2[,"theta4[3]"])
ESS_Theta4_3<-effectiveSize(samplesTheta4_3)
samplesTheta5_2 <- list(samplesList$samples$chain1[,"theta5[2]"],
                        samplesList$samples$chain2[,"theta5[2]"])
ESS_Theta5_2<-effectiveSize(samplesTheta5_2)

samplesSig1 <- list(samplesList$samples$chain1[,"sig1x"],
                    samplesList$samples$chain2[,"sig1x"])
ESS_Sig1<-effectiveSize(samplesSig1)
samplesSig2 <- list(samplesList$samples$chain1[,"sig2x"],
                    samplesList$samples$chain2[,"sig2x"])
ESS_Sig2<-effectiveSize(samplesSig2)

samplesSig3 <- list(samplesList$samples$chain1[,"sig3x"],
                    samplesList$samples$chain2[,"sig3x"])
ESS_Sig3<-effectiveSize(samplesSig3)

samplesSig4 <- list(samplesList$samples$chain1[,"sig4x"],
                    samplesList$samples$chain2[,"sig4x"])
ESS_Sig4<-effectiveSize(samplesSig4)

samplesSig5 <- list(samplesList$samples$chain1[,"sig5x"],
                    samplesList$samples$chain2[,"sig5x"])
ESS_Sig5<-effectiveSize(samplesSig5)

samplesSig6 <- list(samplesList$samples$chain1[,"sig6x"],
                    samplesList$samples$chain2[,"sig6x"])
ESS_Sig6<-effectiveSize(samplesSig6)

samplesSig7 <- list(samplesList$samples$chain1[,"sig7x"],
                    samplesList$samples$chain2[,"sig7x"])
ESS_Sig7<-effectiveSize(samplesSig7)

samplesSig8 <- list(samplesList$samples$chain1[,"sig8x"],
                    samplesList$samples$chain2[,"sig8x"])
ESS_Sig8<-effectiveSize(samplesSig8)

samplesSig9 <- list(samplesList$samples$chain1[,"sig9x"],
                    samplesList$samples$chain2[,"sig9x"])
ESS_Sig9<-effectiveSize(samplesSig9)

samplesGamma2 <- list(samplesList$samples$chain1[,"gamma2"],
                      samplesList$samples$chain2[,"gamma2"])
ESS_Gamma2<-effectiveSize(samplesGamma2) 

samplesGamma3 <- list(samplesList$samples$chain1[,"gamma3"],
                      samplesList$samples$chain2[,"gamma3"])
ESS_Gamma3<-effectiveSize(samplesGamma3) 

samplesBeta1 <- list(samplesList$samples$chain1[,"beta1"],
                     samplesList$samples$chain2[,"beta1"])
ESS_Beta1<-effectiveSize(samplesBeta1) 

samplesBeta2 <- list(samplesList$samples$chain1[,"beta2"],
                     samplesList$samples$chain2[,"beta2"])
ESS_Beta2<-effectiveSize(samplesBeta2) 


ESS<-data.frame(Parameter = c("alpha0", "alpha1", "alpha2", "alpha3", "alpha4", "alpha5", "alpha6", "alpha7[2]","alpha7[3]", 
                              "delta0", "delta1", "delta2", "delta3", "delta4[2]", "delta4[3]", "delta5[2]", 
                              "theta0", "theta1", "theta2", "theta3", "theta4[2]", "theta4[3]", "theta5[2]", 
                              "sig1x","sig2x","sig3x","sig4x","sig5x","sig6x","sig7x","sig8x","sig9x","gamma2",
                              #"gamma1", 
                              "gamma3", "beta1", "beta2"),
                ESS = c(ESS_Alpha0, ESS_Alpha1,ESS_Alpha2,ESS_Alpha3,ESS_Alpha4,ESS_Alpha5,ESS_Alpha6,ESS_Alpha7_2, ESS_Alpha7_3,
                        ESS_Delta0,ESS_Delta1,ESS_Delta2,ESS_Delta3,ESS_Delta4_2,ESS_Delta4_3,ESS_Delta5_2, 
                        ESS_Theta0,ESS_Theta1,ESS_Theta2,ESS_Theta3,ESS_Theta4_2,ESS_Theta4_3,ESS_Theta5_2,
                        ESS_Sig1, ESS_Sig2,ESS_Sig3,ESS_Sig4,ESS_Sig5,ESS_Sig6,ESS_Sig7,ESS_Sig8,ESS_Sig9, 
                        ESS_Gamma2,
                        ESS_Gamma3,ESS_Beta1, ESS_Beta2))

write.table(ESS, file = "Output/ESS.csv", sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)


### autocorrelation
png("Output/Autocor_Cond.png",
    width = 6000, height = 10000, res = 600) 
par(mfrow=c(3,2))
autocorr.plot(samplesList$samples$chain1
              [,c("beta1")],auto.layout=F, main="beta1 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("beta1")],auto.layout=F, main="beta1 (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("gamma2")],auto.layout=F, main="gamma1 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("gamma2")],auto.layout=F, main="gamma1 (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("gamma3")],auto.layout=F, main="gamma3 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("gamma3")],auto.layout=F, main="gamma3 (chain 2)")
dev.off()

#alpha
png("Output/Autocor_Alpha1.png",
    width = 6000, height = 10000, res = 600) 
par(mfrow=c(4,2))
autocorr.plot(samplesList$samples$chain1
              [,c("alpha0")],auto.layout=F, main="alpha0 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha0")],auto.layout=F, main="alpha0 (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("alpha1")],auto.layout=F, main="alpha1 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha1")],auto.layout=F, main="alpha1 (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("alpha2")],auto.layout=F, main="alpha2 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha2")],auto.layout=F, main="alpha2 (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("alpha3")],auto.layout=F, main="alpha3 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha3")],auto.layout=F, main="alpha3 (chain 2)")
dev.off()

#alpha
png("Output/Autocor_Alpha2.png",
    width = 6000, height = 10000, res = 600) 
par(mfrow=c(5,2))
autocorr.plot(samplesList$samples$chain1
              [,c("alpha4")],auto.layout=F, main="alpha4 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha4")],auto.layout=F, main="alpha4 (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("alpha5")],auto.layout=F, main="alpha5 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha5")],auto.layout=F, main="alpha5 (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("alpha6")],auto.layout=F, main="alpha6 (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha6")],auto.layout=F, main="alpha6 (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("alpha7[2]")],auto.layout=F, main="alpha7[2] (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha7[2]")],auto.layout=F, main="alpha7[2] (chain 2)")
autocorr.plot(samplesList$samples$chain1
              [,c("alpha7[3]")],auto.layout=F, main="alpha7[3] (chain 1)")
autocorr.plot(samplesList$samples$chain2
              [,c("alpha7[3]")],auto.layout=F, main="alpha7[3] (chain 2)")
dev.off()

###################### PLOTTING TIME #############################################################
####### plot relationships cond variables and composite condition

#eta1[i] <- -1 * cov[i,1] +  gamma2 * cov[i,2] + gamma3 * cov[i,3] #scale on hema

############## mass and condition ##########
#' Let's plot the relationship. First, we gather the values generated from the posterior distribution of the regression parameters in the two chains. 
## ---------------------------------------------------------------------------------------------------------
#gamma12 <- c(samplesList$samples$chain1[,'gamma12'], samplesList$samples$chain2[,'gamma12'])
gamma2 <- c(samplesList$samples$chain1[,'gamma2'], samplesList$samples$chain2[,'gamma2'])
#gamma22 <- c(samplesList$samples$chain1[,'gamma22'], samplesList$samples$chain2[,'gamma22'])
gamma3 <- c(samplesList$samples$chain1[,'gamma3'], samplesList$samples$chain2[,'gamma3'])
#gamma32 <- c(samplesList$samples$chain1[,'gamma32'], samplesList$samples$chain2[,'gamma32'])

## standardized mean of covariates
mean.stcov = apply(stcov,2,mean) # mean of each covariate
mean.mat.stcov = matrix(rep(mean.stcov,nrow(stcov)),byrow=T,ncol=ncol(stcov))

#' Then we define a grid of values for squared mass, and predict condition for each MCMC iteration. 
## ---------------------------------------------------------------------------------------------------------
##predicted_conditionMS2 <- matrix(NA, nrow = length(gamma12), ncol = length(mean.mat.stcov[,1]))
#for (i in 1:length(gamma12)){ #for each iteration
#  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
#    predicted_conditionMS2[i,j] <- -1 * mean.mat.stcov[j,1] + gamma12[i] * pow(stcov[j,1],2) + 
#      gamma2[i] * mean.mat.stcov[j,2] + gamma22[i] * pow(mean.mat.stcov[j,2],2) + 
#      gamma3[i] * mean.mat.stcov[j,3] + gamma32[i] * pow(mean.mat.stcov[j,3],2)
#  }
#}

#' Now we calculate posterior mean and the credible interval. Note the ordering.
## ---------------------------------------------------------------------------------------------------------
#mean_conditionMS2 <- apply(predicted_conditionMS2, 2, mean)
#lciMS2 <- apply(predicted_conditionMS2, 2, quantile, prob = 2.5/100) #2.5, 25
#uciMS2 <- apply(predicted_conditionMS2, 2, quantile, prob = 97.5/100)#97.5, 75
#ord <- order(stcov[,1])
#dfMS2 <- data.frame(mass = stcov[,1][ord],
#                    cond = mean_conditionMS2[ord],
#                    lciMS2 = lciMS2[ord],
#                    uciMS2 = uciMS2[ord])

#head(dfMS2)

#mass linear effect
predicted_conditionMS <- matrix(NA, nrow = length(gamma2), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(gamma2)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionMS[i,j] <- -1 * stcov[j,1] + #gamma12[i] * pow(mean.mat.stcov[j,1],2) + 
      gamma2[i] * mean.mat.stcov[j,2] + #gamma22[i] * pow(mean.mat.stcov[j,2],2) + 
      gamma3[i] * mean.mat.stcov[j,3] #+ gamma32[i] * pow(mean.mat.stcov[j,3],2)
  }
}

#' Now we calculate posterior mean and the credible interval. Note the ordering.
## ---------------------------------------------------------------------------------------------------------
mean_conditionMS <- apply(predicted_conditionMS, 2, mean)
lciMS <- apply(predicted_conditionMS, 2, quantile, prob = 2.5/100) #2.5, 25
uciMS <- apply(predicted_conditionMS, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(stcov[,1])
dfMS <- data.frame(mass = stcov[,1][ord],
                   cond = mean_conditionMS[ord],
                   lciMS = lciMS[ord],
                   uciMS = uciMS[ord])

head(dfMS)

#' 
#' Now time to visualize. 
library(ggplot2)
p_1<-ggplot(dfMS, aes(x=mass, y=cond)) + 
  #geom_point(mapping=aes(x=stcov[,1],y=samplesSummaryEta[,1]), alpha=0.5)+ #stcov[,1] samplesSummaryMu1[,1]
  #geom_line(data=dfMS2,mapping=aes(x = mass, y = cond, col="#E69F00"),cex=1.2) + 
  geom_line(data=dfMS,mapping=aes(x = mass, y = cond, col="#56B4E9"), cex=1.2) + 
  #geom_ribbon(data=dfMS2,aes(ymin = lciMS2, ymax = uciMS2), fill = "#E69F00", alpha = 0.25)+
  labs(x = "standardized mass", y = "predicted condition")+
  theme_bw()+
  theme(legend.position="bottom", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16))+
  #axis.title.y=element_blank())+
  scale_color_identity(name = "Model",
                       labels = c("linear", "quadratic"),
                       breaks = c("#56B4E9", "#E69F00"),
                       guide = "legend")+
  guides(colour = guide_legend(override.aes = list(size=4)))
p_1

############## hameatocrit and condition ##########

#' Then we define a grid of values for squared hema, and predict condition for each MCMC iteration. 
## ---------------------------------------------------------------------------------------------------------
#predicted_conditionH2 <- matrix(NA, nrow = length(gamma2), ncol = length(mean.mat.stcov[,1]))
#for (i in 1:length(gamma2)){ #for each iteration
#  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
#    predicted_conditionH2[i,j] <- -1 * mean.mat.stcov[j,1] + gamma12[i] * pow(mean.mat.stcov[j,1],2) + 
#      gamma2[i] * mean.mat.stcov[j,2] + gamma22[i] * pow(stcov[j,2],2) + 
#      gamma3[i] * mean.mat.stcov[j,3] + gamma32[i] * pow(mean.mat.stcov[j,3],2)
#  }
#}

#' Now we calculate posterior mean and the credible interval. Note the ordering.
## ---------------------------------------------------------------------------------------------------------
#mean_conditionH2 <- apply(predicted_conditionH2, 2, mean)
#lciH2 <- apply(predicted_conditionH2, 2, quantile, prob = 2.5/100) #2.5, 25
#uciH2 <- apply(predicted_conditionH2, 2, quantile, prob = 97.5/100)#97.5, 75
#ord <- order(stcov[,2])
#dfH2 <- data.frame(hema = stcov[,2][ord],
#                   cond = mean_conditionH2[ord],
#                   lciH2 = lciH2[ord],
#                   uciH2 = uciH2[ord])#

#head(dfH2)

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
## ---------------------------------------------------------------------------------------------------------
mean_conditionH <- apply(predicted_conditionH, 2, mean)
lciH <- apply(predicted_conditionH, 2, quantile, prob = 2.5/100) #2.5, 25
uciH <- apply(predicted_conditionH, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(stcov[,2])
dfH <- data.frame(hema = stcov[,2][ord],
                  cond = mean_conditionH[ord],
                  lciH = lciH[ord],
                  uciH = uciH[ord])

head(dfH)

#' 
#' Now time to visualize. 
## ---- fig.width = 7.5, fig.asp = 0.618, dev = "svg"-------------------------------------------------------
library(ggplot2)
p_2<-ggplot(dfH, aes(x=hema, y=cond)) + 
  #geom_point(mapping=aes(x=stcov[,2],y=samplesSummaryEta[,1]), alpha=0.5)+
  #geom_line(data=dfH2,mapping=aes(x = hema, y = cond,col="#E69F00"), cex=1.2) + 
  geom_line(data=dfH,mapping=aes(x = hema, y = cond, col="#56B4E9"), cex=1.2) + 
  #geom_ribbon(data=dfH2,aes(ymin = lciH2, ymax = uciH2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfH,aes(ymin = lciH, ymax = uciH), fill = "#56B4E9", alpha = 0.25)+
  labs(x = "standardized haematocrit", y = "predicted condition")+
  theme_bw()+
  theme(legend.position="bottom", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  scale_color_identity(name = "Model",
                       labels = c("linear", "quadratic"),
                       breaks = c("#56B4E9", "#E69F00"),
                       guide = "legend")+
  guides(colour = guide_legend(override.aes = list(size=4)))
p_2

############## buffy coat and condition ##########

#' Then we define a grid of values for squared hema, and predict condition for each MCMC iteration. 
## ---------------------------------------------------------------------------------------------------------
#predicted_conditionB2 <- matrix(NA, nrow = length(gamma12), ncol = length(mean.mat.stcov[,1]))
#for (i in 1:length(gamma12)){ #for each iteration
#  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
#    predicted_conditionB2[i,j] <- -1 * mean.mat.stcov[j,1] + gamma12[i] * pow(mean.mat.stcov[j,1],2) + 
#      gamma2[i] * mean.mat.stcov[j,2] + gamma22[i] * pow(mean.mat.stcov[j,2],2) + 
#      gamma3[i] * mean.mat.stcov[j,3] + gamma32[i] * pow(stcov[j,3],2)
#  }
#}

#' Now we calculate posterior mean and the credible interval. Note the ordering.
## ---------------------------------------------------------------------------------------------------------
#mean_conditionB2 <- apply(predicted_conditionB2, 2, mean)
#lciB2 <- apply(predicted_conditionB2, 2, quantile, prob = 2.5/100) #2.5, 25
#uciB2 <- apply(predicted_conditionB2, 2, quantile, prob = 97.5/100)#97.5, 75
#ord <- order(stcov[,3])
#dfB2 <- data.frame(buffy = stcov[,3][ord],
#                   cond = mean_conditionB2[ord],
#                   lciB2 = lciB2[ord],
#                   uciB2 = uciB2[ord])

#head(dfB2)

#hema linear effect
predicted_conditionB <- matrix(NA, nrow = length(gamma2), ncol = length(mean.mat.stcov[,1]))
for (i in 1:length(gamma2)){ #for each iteration
  for (j in 1:length((mean.mat.stcov[,1]))){ # for each individual
    predicted_conditionB[i,j] <- -1 * mean.mat.stcov[j,1] + #gamma12[i] * pow(mean.mat.stcov[j,1],2) + 
      gamma2[i] * mean.mat.stcov[j,2] + #gamma22[i] * pow(mean.mat.stcov[j,2],2) + 
      gamma3[i] * stcov[j,3] #+ gamma32[i] * pow(mean.mat.stcov[j,3],2)
  }
}

#' Now we calculate posterior mean and the credible interval. Note the ordering.
## ---------------------------------------------------------------------------------------------------------
mean_conditionB <- apply(predicted_conditionB, 2, mean)
lciB <- apply(predicted_conditionB, 2, quantile, prob = 2.5/100) #2.5, 25
uciB <- apply(predicted_conditionB, 2, quantile, prob = 97.5/100)#97.5, 75
ord <- order(stcov[,3])
dfB <- data.frame(buffy = stcov[,3][ord],
                  cond = mean_conditionB[ord],
                  lciB = lciB[ord],
                  uciB = uciB[ord])

head(dfB)

#' 
#' Now time to visualize. 
## ---- fig.width = 7.5, fig.asp = 0.618, dev = "svg"-------------------------------------------------------
library(ggplot2)
p_3<-ggplot(dfB, aes(x=buffy, y=cond)) + 
  #geom_point(mapping=aes(x=stcov[,3],y=samplesSummaryEta[,1]), alpha=0.5)+
  #geom_line(data=dfB2,mapping=aes(x = buffy, y = cond, col="#E69F00"), cex=1.2) + 
  geom_line(data=dfB,mapping=aes(x = buffy, y = cond, col="#56B4E9"), cex=1.2) + 
  #geom_ribbon(data=dfB2,aes(ymin = lciB2, ymax = uciB2), fill = "#E69F00", alpha = 0.25)+
  geom_ribbon(data=dfB,aes(ymin = lciB, ymax = uciB), fill = "#56B4E9", alpha = 0.25)+
  labs(x = "standardized buffy coat", y = "predicted condition")+
  theme_bw()+
  theme(legend.position="bottom", 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        text = element_text(size=18),
        axis.title=element_text(size=16),
        axis.title.y=element_blank())+
  scale_color_identity(name = "Model",
                       labels = c("linear", "quadratic"),
                       breaks = c("#56B4E9", "#E69F00"),
                       guide = "legend")+
  guides(colour = guide_legend(override.aes = list(size=4)))
p_3

######## combine plots ###########
library(patchwork)
png("Output/ConditionVariablesNoPoints.png", width = 9500, height = 4500,units = 'px', res = 600)
patchwork <- p_1|p_2|p_3
patchwork<-patchwork +  plot_layout(guide='collect') & theme(legend.position = 'bottom')
patchwork+plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 18))
dev.off() 

################# plot survival and condition in other script #########################
logit(phi[k,i,j]) <- mu.phi[k,i,j]  + epst.phi[k,j-1]   # age and state specific survival probability,  with site-specific time random effect

mu.phi[k,i,j] <- logit(int.phi[k,age[i,j-1]]) + (j == TCatch[i]+1) * beta1 * eta1[i] + (j == TCatch[i]+1) * beta2 * cov[i,9]

beta1 <- c(samplesList$samples$chain1[,'beta1'], samplesList$samples$chain2[,'beta1'])
beta2 <- c(samplesList$samples$chain1[,'beta2'], samplesList$samples$chain2[,'beta2'])

###########
beta1 <- c(samplesList$samples$chain1[,'beta1'], samplesList$samples$chain2[,'beta1'])
beta2 <- c(samplesList$samples$chain1[,'beta2'], samplesList$samples$chain2[,'beta2'])
gamma2 <- c(samplesList$samples$chain1[,'gamma2'], samplesList$samples$chain2[,'gamma2'])
#gamma22 <- c(samplesList$samples$chain1[,'gamma22'], samplesList$samples$chain2[,'gamma22'])
gamma3 <- c(samplesList$samples$chain1[,'gamma3'], samplesList$samples$chain2[,'gamma3'])
#gamma32 <- c(samplesList$samples$chain1[,'gamma32'], samplesList$samples$chain2[,'gamma32'])
## standardized mean of covariates
mean.stcov = apply(stcov,2,mean) # mean of each covariate
mean.mat.stcov = matrix(rep(mean.stcov,nrow(stcov)),byrow=T,ncol=ncol(stcov))

########### export for plotting without loading Rdata ############
#write.csv(gamma12,"Output/gamma12.csv", row.names =TRUE)
write.csv(gamma2,"Output/gamma2.csv", row.names =TRUE)
#write.csv(gamma22,"Output/gamma22.csv", row.names =TRUE)
write.csv(gamma3,"Output/gamma3.csv", row.names =TRUE)
#write.csv(gamma32,"Output/gamma32.csv", row.names =TRUE)
write.csv(mean.mat.stcov,"Output/mean.mat.stcov.csv", row.names =TRUE)
write.csv(stcov,"Output/stcov.csv", row.names =TRUE)
write.csv(beta1,"Output/beta1.csv", row.names =TRUE)
write.csv(beta2,"Output/beta2.csv", row.names =TRUE)


save.image(file='NimbleModel_M1_P12_params_10000iter.RData')
