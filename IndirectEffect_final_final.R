load("YOUR_PATH/GlobalEnvFig5.Rdata")

### Env vatriable effect on condition ###
summary(samplesList[["samples"]][,c("beta0")]) #intercept 1.4
summary(samplesList[["samples"]][,c("beta1")]) #0.13[-0.15,0.42], sd 0.14, temp
summary(samplesList[["samples"]][,c("beta2")]) #0.04[-0.26,0.32], sd 0.14, precipitation
summary(samplesList[["samples"]][,c("beta3")]) #0.09[-0.11,0.28], sd 0.10, cockle
summary(samplesList[["samples"]][,c("beta4")]) #0.02[-0.17,0.21], sd 0.10, mussel
summary(samplesList[["samples"]][,c("beta5")]) #0.24[0.02,0.45], sd 0.11, grasland
summary(samplesList[["samples"]][,c("beta6")]) #0.14[-0.06,0.35], sd 0.10, competition

## effect of cond on chick survival
# effect of cond on chick surv [days] [lower CI;upper CI]: 0.13[0.02;0.23] ; sd (model output)=0.05357143 (fig 4)
# intercept: 3.07

###########################################################################################################################################################
################################################## TEMP EFFECT ###########################################################################################
##########################################################################################################################################################

##### effect of only cond on survival
# input data
beta0A<-1.4  # intercept of regression of Cond on env
beta0B<-3.07  # intercept of regression of Surv on Cond
betaA<-0.13 # effect of temp on cond
betaB<-0.13 # effect of cond on log(surv) 
se_betaA<-0.15 # se of effect of temp on cond
se_betaB<-0.05357143 # se of effect of cond on log(surv), sd (model output)=0.05357143

# se of betaA*betaB
se_betaAB<-sqrt(betaA^2*se_betaA^2+betaB^2*se_betaB^2)

temp<-data.frame(samplesSummaryTemp[,1])
str(temp)
mean(temp$samplesSummaryTemp...1., na.rm=T) #0.0002986023
max(temp$samplesSummaryTemp...1., na.rm=T) #2.19
min(temp$samplesSummaryTemp...1., na.rm=T) #0
sd(temp$samplesSummaryTemp...1., na.rm=T) #0.99

# calculations
reference.env.driver.value<-0 # this is the mean value of the env. driver
pertubation.env.driver.value<-1 # 1sd

# effect of T on log(S) mediated by C
logSurv_combined_ref<-beta0B+beta0A*betaB+betaB*betaA*reference.env.driver.value
# upper 95%CI 
logSurv_combined_upper95_ref<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*reference.env.driver.value
# lower 95%CI 
logSurv_combined_lower95_ref<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*reference.env.driver.value

# now translate from the log(S) to the S scale
Surv_combined_ref<-exp(logSurv_combined_ref)
Surv_combined_upper95_ref<-exp(logSurv_combined_upper95_ref)
Surv_combined_lower95_ref<-exp(logSurv_combined_lower95_ref)
print(c(Surv_combined_ref,Surv_combined_lower95_ref,Surv_combined_upper95_ref))

# for below steps see word doc for explanation
# effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1); you can change T to other value if you want to look at effect of 1SD in temp
logSurv_combined<-beta0B+beta0A*betaB+betaB*betaA*(reference.env.driver.value+pertubation.env.driver.value)
# upper 95%CI 
logSurv_combined_upper95<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)
# lower 95%CI 
logSurv_combined_lower95<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)

# now translate from the log(S) to the S scale
Surv_combined<-exp(logSurv_combined)
Surv_combined_upper95<-exp(logSurv_combined_upper95)
Surv_combined_lower95<-exp(logSurv_combined_lower95)
print(c(Surv_combined,Surv_combined_lower95,Surv_combined_upper95 ))

Surv_difference<-Surv_combined-Surv_combined_ref
Surv_difference_upper95<-Surv_combined_upper95-Surv_combined_upper95_ref
Surv_difference_lower95<-Surv_combined_lower95-Surv_combined_lower95_ref
print(c(Surv_difference,Surv_difference_upper95,Surv_difference_lower95 ))
#[1]  0.4404406  1.5290362 -0.6048596

0.4404406/18.2*100 #mean increase in days, 18.2 days survival on average
#2.420003% increase

###########################################################################################################################################################
################################################## GRASSLAND EFFECT ###########################################################################################
##########################################################################################################################################################
# input data
beta0A<-1.4  # intercept of regression of grass on cond
beta0B<-3.07  # intercept of regression of Surv on Cond
betaA<-0.24 # effect of grassland on cond
betaB<-0.13 # effect of cond on log(surv) 
se_betaA<-0.11 # se of effect of grassland on cond
se_betaB<-0.05357143 # se of effect of cond on log(surv), sd (model output)=0.05357143

# se of betaA*betaB
se_betaAB<-sqrt(betaA^2*se_betaA^2+betaB^2*se_betaB^2)

#mean(contcov$PropGrassland) #0.07786175
#max(contcov$PropGrassland) #0.1693442
#min(contcov$PropGrassland) #0
#(max(contcov$PropGrassland) - min(contcov$PropGrassland)) * (10/100) #0.01692634
sd(contcov$PropGrassland) #0.05498812

# calculations
reference.env.driver.value<-0 # mean is 0 as all variable are standardized; this is the mean value of the env. driver
pertubation.env.driver.value<- 1 # increase in 1 sd means 0.05 grassland prop

#convert standardized score back to original scale
(1* sd(contcov$PropGrassland)) + mean(contcov$PropGrassland) #standardzied value is set to 1, 0.13
# 1 sd is 0.13 grassland proportion
# This calculation indicates that a one standard deviation increase from the mean on your standardized scale corresponds to an actual value of 0.13 on the original scale of the variable

# effect of grassland on log(S) mediated by Cond
logSurv_combined_ref<-beta0B+beta0A*betaB+betaB*betaA*reference.env.driver.value
# upper 95%CI 
logSurv_combined_upper95_ref<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*reference.env.driver.value
# lower 95%CI 
logSurv_combined_lower95_ref<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*reference.env.driver.value

# now translate from the log(S) to the S scale
Surv_combined_ref<-exp(logSurv_combined_ref)
Surv_combined_upper95_ref<-exp(logSurv_combined_upper95_ref)
Surv_combined_lower95_ref<-exp(logSurv_combined_lower95_ref)
print(c(Surv_combined_ref,Surv_combined_lower95_ref,Surv_combined_upper95_ref))

# effect of grassland on log(S) mediated by Cond
logSurv_combined<-beta0B+beta0A*betaB+betaB*betaA*(reference.env.driver.value+pertubation.env.driver.value)
# upper 95%CI 
logSurv_combined_upper95<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)
# lower 95%CI 
logSurv_combined_lower95<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)

# now translate from the log(S) to the S scale
Surv_combined<-exp(logSurv_combined)
Surv_combined_upper95<-exp(logSurv_combined_upper95)
Surv_combined_lower95<-exp(logSurv_combined_lower95)
print(c(Surv_combined,Surv_combined_lower95,Surv_combined_upper95 ))

Surv_difference<-Surv_combined-Surv_combined_ref
Surv_difference_upper95<-Surv_combined_upper95-Surv_combined_upper95_ref
Surv_difference_lower95<-Surv_combined_lower95-Surv_combined_lower95_ref
print(c(Surv_difference,Surv_difference_upper95,Surv_difference_lower95 ))
#[1]  0.8189792  2.2845828 -0.5702555

0.8189792/18.2*100 #mean increase in days, 18.2 days survival on average
#4.499886% increase

###########################################################################################################################################################
################################################## cockle EFFECT ###########################################################################################
##########################################################################################################################################################
# input data
beta0A<-1.4  # intercept of regression of cond on cockle
beta0B<-3.07  # intercept of regression of Surv on Cond
betaA<-0.09 # effect of cockel on cond
betaB<-0.13 # effect of cond on log(surv) 
se_betaA<-0.10 # se of effect of cockle on cond
se_betaB<-0.05357143 # se of effect of cond on log(surv), sd (model output)=0.05357143

# se of betaA*betaB
se_betaAB<-sqrt(betaA^2*se_betaA^2+betaB^2*se_betaB^2)

mean(contcov$AFDMgrm2_C_CondLoss_exp) #5.435126
max(contcov$AFDMgrm2_C_CondLoss_exp) #22.58438
min(contcov$AFDMgrm2_C_CondLoss_exp) #0.03849116
(max(contcov$AFDMgrm2_C_CondLoss_exp) - min(contcov$AFDMgrm2_C_CondLoss_exp)) * (10/100) #2.25
mean(contcov$AFDMgrm2_C_CondLoss_exp) * 0.1 #0.5435126
sd(contcov$AFDMgrm2_C_CondLoss_exp) #4.809949

# calculations
reference.env.driver.value<-0 # this is the mean value of the env. driver, which is 0 because standardized/normalized
pertubation.env.driver.value<-1 # mean and increase of 1SD

#convert standardized score back to original scale
(1* sd(contcov$AFDMgrm2_C_CondLoss_exp)) + mean(contcov$AFDMgrm2_C_CondLoss_exp) #standardzied value is set to 1
# 1 sd is 10.24508 g afdm/m2 cockle availability
# This calculation indicates that a one standard deviation increase from the mean on your standardized scale corresponds to an actual value of 10.2  on the original scale of the variable

# effect of Cockle on log(S) mediated by Cond
logSurv_combined_ref<-beta0B+beta0A*betaB+betaB*betaA*reference.env.driver.value
# upper 95%CI 
logSurv_combined_upper95_ref<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*reference.env.driver.value
# lower 95%CI 
logSurv_combined_lower95_ref<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*reference.env.driver.value

# now translate from the log(S) to the S scale
Surv_combined_ref<-exp(logSurv_combined_ref)
Surv_combined_upper95_ref<-exp(logSurv_combined_upper95_ref)
Surv_combined_lower95_ref<-exp(logSurv_combined_lower95_ref)
print(c(Surv_combined_ref,Surv_combined_lower95_ref,Surv_combined_upper95_ref))

# effect of Cockle on log(S) mediated by Conc, where we look at effect of 1 SD change 
logSurv_combined<-beta0B+beta0A*betaB+betaB*betaA*(reference.env.driver.value+pertubation.env.driver.value)
# upper 95%CI 
logSurv_combined_upper95<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)
# lower 95%CI 
logSurv_combined_lower95<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)

# now translate from the log(S) to the S scale
Surv_combined<-exp(logSurv_combined)
Surv_combined_upper95<-exp(logSurv_combined_upper95)
Surv_combined_lower95<-exp(logSurv_combined_lower95)
print(c(Surv_combined,Surv_combined_lower95,Surv_combined_upper95 ))

Surv_difference<-Surv_combined-Surv_combined_ref
Surv_difference_upper95<-Surv_combined_upper95-Surv_combined_upper95_ref
Surv_difference_lower95<-Surv_combined_lower95-Surv_combined_lower95_ref
print(c(Surv_difference,Surv_difference_upper95,Surv_difference_lower95 ))
#0.3041267  0.8938556 -0.2725941

0.3041267/18.2*100 #mean increase in days, 18.2 days survival on average
#1.671026% increase
