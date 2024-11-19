### M1 nimble, with only 9 years of capture history for both periods
library(nimble)
library(igraph)
library(coda)
library(R6)
setwd("YOUR_PATH")

# download and install RTools40: https://cran.r-project.org/bin/windows/Rtools/
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

d <- read.csv(file="StateMatrix.csv")
str(d)
CH1<-d[,12:21] #period 1
CH2<-d[,44:51] #period 2
CH<-cbind(CH1,CH2)
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
age2 <- read.csv(file="AgeMatrix2000_2019_Cond3class.csv")# 
age <- age2 
head(age2) #age classes are: 1=1st, 2=2nd and 3rd year, 3=adults
age[age2==0] <- NA   # replace NA by 0 (before first capture)
age2_1<-age2[,1:10] #period 1
age2_2<-age2[,33:40] #period 2
age2<-cbind(age2_1,age2_2)

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

# use informative priors to estimate the survival (phi) and resighting prop. (p) with information from Andys Mark model (14 000 datapoints)
# 9 states: D, P, N, B, V, T, S, R, X; second period taken (2010-2019) because more precise estimates
# but also used un-informative priors for those combinations that were not well estimated by andys model using dnorm(0, 0.1) (meaning variance of 100 (inverse variance=precision))
# combinations state 3 age 2, state 9 age 2 and state 9 age 3 cannot be estimated in this model, because those combinations do not exist in
# the condition dataset. Thus, even though they were not estimable in MARK we use a 0 for beta and sd prior of 0.1 (uninformative)

# beta estimated in mark - or 0 if no prior info for certain age/state combinations 
int.phi.meanprior <- matrix(c(0.7979485,1.1765438,3.9646808,          #state 1, age 1,2,3
                              0,0.873906,1.0615756,                   #state 2, age 1,2,3
                              -2.2365741, 0,0.9154164,                #state 3, age 1,2,3
                              0,0,2.2753403,                          #state 4, age 1,2,3
                              0,3.6303479,0,                          #state 5, age 1,2,3
                              -0.2539863,0,3.0473434,                 #state 6, age 1,2,3
                              0.8329513,0,2.2105682,                  #state 7, age 1,2,3
                              2.9337983,5.0716794,1.6358572,          #state 8, age 1,2,3
                              1.9594179,0,1.8401387                   #state 9, age 1,2,3
), ncol=3,nrow=9)  
# standard error: 0.001 if prior (precise), or 0.1 if no prior info (unprecise)
int.phi.sdprior <- matrix(c(0.001,0.001,0.001,                        #state 1, age 1,2,3
                            0.1,0.001,0.001,                          #state 2, age 1,2,3
                            0.001,0.1,0.001,                          #state 3, age 1,2,3
                            0.1,0.1,0.001,                            #state 4, age 1,2,3
                            0.1,0.001,0.1,                            #state 5, age 1,2,3
                            0.001,0.1,0.001,                          #state 6, age 1,2,3
                            0.001,0.1,0.001,                          #state 7, age 1,2,3
                            0.001,0.001,0.001,                        #state 8, age 1,2,3
                            0.001,0.1,0.001                           #state 9, age 1,2,3
), ncol=3,nrow=9)

#same for resighting probability (p)
int.p.meanprior <- matrix(c(0.4226055,0.9949699,0.5552625,            #state 1, age 1,2,3
                            0,-2.2944491,0.1951106,                   #state 2, age 1,2,3
                            0,-4.2865546,0.3050049,                   #state 3, age 1,2,3
                            -1.2290286,-0.4012559,-1.6948417,         #state 4, age 1,2,3
                            0,-0.8366816,-3.6535485,                  #state 5, age 1,2,3
                            -0.0732934,-1.5325679,0.9220549,          #state 6, age 1,2,3
                            0,-1.0938781,-1.9021636,                  #state 7, age 1,2,3
                            0,-3.3188883,0,                           #state 8, age 1,2,3
                            0,0,-1.8367747                            #state 9, age 1,2,3 
), ncol=3,nrow=9)

int.p.sdprior <- matrix(c(0.001,0.001,0.001,                          #state 1, age 1,2,3
                          0.001,0.001,0.001,                          #state 2, age 1,2,3
                          0.001,0.001,0.001,                          #state 3, age 1,2,3
                          0.001,0.001,0.001,                          #state 4, age 1,2,3
                          0.1,0.001,0.001,                            #state 5, age 1,2,3
                          0.001,0.001,0.001,                          #state 6, age 1,2,3
                          0.1,0.001,0.001,                            #state 7, age 1,2,3
                          0.1,0.001,0.1,                              #state 8, age 1,2,3
                          0.1,0.1,0.001                               #state 9, age 1,2,3
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
    # 10 events (E) also called observations (O):
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
    mu1[i] <- alpha0 + alpha1*cov[i,4] + alpha2*cov[i,5] + alpha3*cov[i,6] + alpha4*cov[i,7] + alpha5*cov[i,8] + alpha6*cov[i,9] + alpha7[AgeH[i]] 
    # for hematocrit
    mu2[i] <- delta0 + delta1*cov[i,7] + delta2*cov[i,8] + delta3*cov[i,9] + delta4[AgeH[i]] + delta5[Sex[i]] 
    # for buffy coat
    mu3[i] <- theta0 + theta1*cov[i,7] + theta2*cov[i,8] + theta3*cov[i,9] + theta4[AgeH[i]] + theta5[Sex[i]]
    
    # structural model
    # for composite variable condition :
    mu.eta1[i] <- 1 * cov[i,1] + gamma2 * cov[i,2] + gamma3 * cov[i,3] 
    } #i=n
    
    # for CR model : survival and detection probabilities
    for(k in 1:9){ # for each state
    for(i in 1:n){ # for each individual
    for(j in (first[i]+1):T){ # for each sampling after first capture
    
    logit(phi[k,i,j]) <- mu.phi[k,i,j]  + epst.phi[k,j-1]   # age and state specific survival probability,  with site-specific time random effect
    
    mu.phi[k,i,j] <- int.phi[k,age[i,j-1]] + beta1 * eta1[i] 
    
    logit(p[k,i,j]) <- int.p[k,age[i,j-1]] + epst.p[k,j-1] # age and state specific detection probability, with site-specific time random effect
    # int.p and int.phi capture the intercepts, of age and site effect which are categorial - i.e. average survival rate for each combination of age class and site 
    
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
    eta1[i] ~ dnorm(mu.eta1[i], sd=0) #Condition
    }
    
    # proportion of individuals in initial states at first capture estimated from the data
    for (j in 1:8){ 
    log(prop[j]) <- pi[j] # mulitnomial logit link
    pi[j] ~ dnorm(0,1)
    }
    
    for(k in 1:9){ # for each state
    for(a in 1:3){ # each age
    int.phi[k,a] ~ dnorm(int.phi.meanprior[k,a], sd=int.phi.sdprior[k,a])
    int.p[k,a] ~ dnorm(int.p.meanprior[k,a], sd=int.p.sdprior[k,a]) 
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
    gamma2 ~ dnorm(0,1)
    gamma3 ~ dnorm(0,1)
    beta1  ~ dnorm(0,1)
    alpha0 ~ dnorm(0,1)
    alpha1 ~ dnorm(0,1)
    alpha2 ~ dnorm(0,1)
    alpha3 ~ dnorm(0,1)
    alpha4 ~ dnorm(0,1)
    alpha5 ~ dnorm(0,1)
    alpha6 ~ dnorm(0,1)
    delta0 ~ dnorm(0,1)
    delta1 ~ dnorm(0,1)
    delta2 ~ dnorm(0,1)
    delta3 ~ dnorm(0,1)
    theta0 ~ dnorm(0,1)
    theta1 ~ dnorm(0,1)
    theta2 ~ dnorm(0,1)
    theta3 ~ dnorm(0,1)
    
    # for sex
    for(s in 1:2){ 
    delta5[s] ~ dnorm(0,1)
    theta5[s] ~ dnorm(0,1)}
    
    #for AgeH
    for(s in 1:3){ 
    alpha7[s] ~ dnorm(0,1)
    delta4[s] ~ dnorm(0,1)
    theta4[s] ~ dnorm(0,1)}
    
    ## Prior distributions of the precision parameters SEM
    sig1x ~ dunif(0,10)
    sig2x ~ dunif(0,10)
    sig3x ~ dunif(0,10)
    sig4x ~ dunif(0,10)
    sig5x ~ dunif(0,10)
    sig6x ~ dunif(0,10)
    sig7x ~ dunif(0,10)
    sig8x ~ dunif(0,10)
    sig9x ~ dunif(0,10)
    
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
    for(k in 1:9){
    for(a in 1:3){
    for(j in 1:T){
    S[k,a,j] <- exp(int.phi[k,a])/(1+exp(int.phi[k,a])) # survival rate (correponds to average year and condition )
    P[k,a,j] <- exp(int.p[k,a])/(1+exp(int.p[k,a])) # detection probability (correponds to average year )
    } #j
    } #a
    } #k
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
constants <- list(n=n,first=first,T=T,age=age,SW = SW, WS= WS, cov=stcov , Sex=Sex, AgeH=AgeH,
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
alive2 <- matrix(vector.alive1, nrow = 18, ncol = 1574)
alive2 <- t(alive2)  # matrix transposed

# Initial values
initsFunction <- function() list(alive=alive2)#, 
                                 pi=rnorm(8,0,1), r=runif(1,0,1),sigmat.phi=runif(9,0,10), sigmat.p=runif(9,0,10),
                                 sig1x=runif(1,0,10),sig2x=runif(1,0,10),sig3x=runif(1,0,10),sig4x=runif(1,0,10),sig5x=runif(1,0,10),sig6x=runif(1,0,10), sig7x=runif(1,0,10), sig8x=runif(1,0,10), sig9x=runif(1,0,10), 
                                 #sig.eta1=0,
                                 gamma2=rnorm(1,0,1), gamma3=rnorm(1,0,1), beta1=rnorm(1,0,1),
                                 alpha0=rnorm(1,0,1),alpha1=rnorm(1,0,1),alpha2=rnorm(1,0,1),alpha3=rnorm(1,0,1),alpha4=rnorm(1,0,1),alpha5=rnorm(1,0,1),alpha6=rnorm(1,0,1),alpha7=rnorm(3,0,1),
                                 delta0=rnorm(1,0,1),delta1=rnorm(1,0,1),delta2=rnorm(1,0,1),delta3=rnorm(1,0,1),delta4=rnorm(3,0,1),delta5=rnorm(2,0,1),
                                 theta0=rnorm(1,0,1), theta1=rnorm(1,0,1), theta2=rnorm(1,0,1), theta3=rnorm(1,0,1), theta4=rnorm(3,0,1), theta5=rnorm(2,0,1),
                                 betaSW1=rnorm(8,0,1),betaSW2=rnorm(8,0,1),betaSW3=rnorm(8,0,1),betaSW4=rnorm(8,0,1),betaSW5=rnorm(8,0,1),betaSW6=rnorm(8,0,1),betaSW7=rnorm(8,0,1),betaSW8=rnorm(8,0,1),betaSW9=rnorm(8,0,1),
                                 betaWS1=rnorm(8,0,1),betaWS2=rnorm(8,0,1),betaWS3=rnorm(8,0,1),betaWS4=rnorm(8,0,1),betaWS5=rnorm(8,0,1),betaWS6=rnorm(8,0,1),betaWS7=rnorm(8,0,1),betaWS8=rnorm(8,0,1),betaWS9=rnorm(8,0,1),
                                 mu1=rnorm(1,0,1), mu2=rnorm(1,0,1),mu3=rnorm(1,0,1),mu.eta1=rnorm(1,0,1),
                                 psiSW1=rnorm(8,0,1),psiSW2=rnorm(8,0,1),psiSW3=rnorm(8,0,1),psiSW4=rnorm(8,0,1),psiSW5=rnorm(8,0,1),psiSW6=rnorm(8,0,1), psiSW7=rnorm(8,0,1), psiSW8=rnorm(8,0,1),psiSW9=rnorm(8,0,1),
                                 psiWS1=rnorm(8,0,1), psiWS2=rnorm(8,0,1),psiWS3=rnorm(8,0,1),psiWS4=rnorm(8,0,1),psiWS5=rnorm(8,0,1),psiWS6=rnorm(8,0,1),psiWS7=rnorm(8,0,1), psiWS8=rnorm(8,0,1),psiWS9=rnorm(8,0,1),
                                 prop=runif(8,0,1), eta1=rnorm(1,0,1), S0=runif(11,0,1),
                                 mu.phi=array(c(matrix(runif(14166,0,1))), dim = c(9, 1574, 18)),
                                 p=array(c(matrix(runif(14166,0,1))), dim = c(9, 1574, 18)),
                                 phi=array(c(matrix(runif(14166,0,1))), dim = c(9, 1574, 18)),
                                 epst.phi=matrix(rnorm(162, 0, 1), 9,18), epst.p=matrix(rnorm(162, 0, 1), 9,18),
                                 P=array(c(matrix(runif(27,0,1))), dim = c(9, 3, 18)) , S=array(c(matrix(runif(27,0,1))), dim = c(9, 3, 18)),
                                 int.p=matrix(rnorm(27,0,1), 9, 3), int.phi=matrix(rnorm(27,0,1), 9, 3),
                                 PSI=array(c(matrix(runif(121,0,1))), dim = c(11,11,1574,18)),
                                 E=array(c(matrix(runif(121,0,1))), dim = c(11,11,1574,18))
)

initsList <- initsFunction()

# Parameters to be monitored
params <- c("S","P","phi", "p", "int.phi","int.p","r","pi","eta1",
            "mu1", "mu2", "mu3","mu.eta1",
            "sigmat.p", "sigmat.phi",
            "betaSW1","betaSW2","betaSW3","betaSW4","betaSW5","betaSW6","betaSW7","betaSW8", "betaSW9",
            "betaWS1","betaWS2","betaWS3","betaWS4","betaWS5","betaWS6","betaWS7","betaWS8","betaWS9",
            "beta1","gamma2","gamma3",
            "alpha0", "alpha1", "alpha2", "alpha3", "alpha4","alpha5", "alpha6", "alpha7",
            "delta0", "delta1","delta2","delta3","delta4","delta5",
            "theta0", "theta1","theta2","theta3","theta4","theta5", 
            "sig1x","sig2x","sig3x","sig4x","sig5x","sig6x","sig7x","sig8x","sig9x")

memory.limit(size=150000)

# build model (not compiled yet)
deb = Sys.time()
RModel<-nimbleModel(
  code = NimbleCode,
  data=mydatax,
  constants = constants, 
  inits = initsList, 
  calculate=F) #eventually calculate=F to make it faster
fin = Sys.time()
duration1=fin - deb
duration1 #49 minutes

