load("G:/NIOO drive/P drive/Personal Drive (MagaliF)/CHIRP/Carry-over/Analysis/Figures/MainDoc/Files for Fig5/GlobalEnvFig5.Rdata")

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
reference.env.driver.value<-0 # this is the mean value of the env. driver, I assume it is zero for temperature (as this is a latent, but for grassland it may be 0.17 if you have not mean-centered the grassland variable))
pertubation.env.driver.value<-1 # you may have to change this for temperature, as 1 SD is a lot, maybe use the equivalent of 1 degree?  For grassland, the pertubation may have to be not 1, but 0.17*0.1 if you want to look at the effect of 10% increase in existing grassland

### temperature is a latent variable, so we need to see how 1sd can be changed in degrees for easier interpretation
## conclusion was, latent variable was described as first measured variable average temp in degrees, where there was no factor loadinng, meaning factor loading is 1, that means unit of temp latent variable can be inferred from the
# avera ge temp (measured in degree celsius)
### ["To determine what one standard deviation change in your latent variable corresponds to in terms of degrees Celsius, you would typically utilize the standardized loading of the temperature variable on the latent variable from your SEM output. Here???s how you can do it:
#1. **Identify the Standardized Loading**: Find the standardized loading (also known as a factor loading) of the temperature variable on the latent variable. This loading represents how much a one standard deviation increase in the latent variable is associated with a change in standard deviations in the temperature variable.
#2. **Calculate the Temperature Change**: Multiply the standard deviation of the temperature variable by the standardized loading. This calculation gives you the change in temperature corresponding to a one standard deviation increase in the latent variable. For example, if the standard deviation of temperature is 10 degrees Celsius and the standardized loading of temperature on the latent variable is 0.8, then a one standard deviation increase in the latent variable corresponds to an increase of \(0.8 \times 10 = 8\) degrees Celsius.
#This method allows you to interpret the latent variable in terms of the observed variable (temperature), facilitating a more intuitive understanding of your model???s outputs."]
#["Correct, in the model you've provided, `cov[i,5] ~ dnorm(mu1[i], sd=sig5x)` where `mu1[i] <- temp[i]` indeed suggests that the latent variable associated with `cov[i,5]` (representing average temperature in this context) directly equates to the measured temperature without any transformation or scaling factor. Therefore, the factor loading for temperature on this latent variable (`mu1[i]` or average temperature) is effectively 1. This means that a one-unit change in the latent variable is directly equivalent to a one-unit change in the observed temperature variable measured in degrees Celsius. This direct 1:1 relationship simplifies the interpretation, making it straightforward to understand how changes in the observed temperature relate to changes in your latent construct of average temperature."]
#["Yes, exactly. If the standard deviation (sd) of the latent variable for average temperature is 0.99 and the factor loading is 1, this means that a one standard deviation change in the latent variable corresponds almost exactly to a one degree Celsius change in temperature. In practical terms, since the factor loading between the observed temperature variable and the latent average temperature is 1, the scale and variation of the latent variable directly reflect those of the temperature variable. Thus, a change of one standard deviation in this latent variable???being 0.99???can be interpreted as approximately a one degree change in Celsius, aligning closely with the scale of the temperature measurements."]


# effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1); you can change T to other value if you want to look at effect of 1SD in temp
logSurv_combined_ref<-beta0B+beta0A*betaB+betaB*betaA*reference.env.driver.value
# upper 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_upper95_ref<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*reference.env.driver.value
# lower 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_lower95_ref<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*reference.env.driver.value

# now translate from the log(S) to the S scale
Surv_combined_ref<-exp(logSurv_combined_ref)
Surv_combined_upper95_ref<-exp(logSurv_combined_upper95_ref)
Surv_combined_lower95_ref<-exp(logSurv_combined_lower95_ref)
print(c(Surv_combined_ref,Surv_combined_lower95_ref,Surv_combined_upper95_ref))


# for below steps see word doc for explanation
# effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1); you can change T to other value if you want to look at effect of 1SD in temp
logSurv_combined<-beta0B+beta0A*betaB+betaB*betaA*(reference.env.driver.value+pertubation.env.driver.value)
# upper 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_upper95<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)
# lower 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
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
beta0A<-1.4  # intercept of regression of Cond on Temp; this estimate was not in your file, so I just put it on zero, should be changes to correct estimate
beta0B<-3.07  # intercept of regression of Surv on Cond; this estimate was not in your file, so I just put it on zero, should be changes to correct estimate
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
reference.env.driver.value<-0 # mean is 0 as all variable are standardized; this is the mean value of the env. driver, I assume it is zero for temperature (as this is a latent, but for grassland it may be 0.17 if you have not mean-centered the grassland variable))
pertubation.env.driver.value<- 1 # increase in 1 sd means 0.05 grassland prop; max and increase of 10%; you may have to change this for temperature, as 1 SD is a lot, maybe use the equivalent of 1 degree?  For grassland, the pertubation may have to be not 1, but 0.17*0.1 if you want to look at the effect of 10% increase in existing grassland

#convert standardized score back to original scale
#Original Value=(Standardized Value ?? Original Standard Deviation)+Original Mean
(1* sd(contcov$PropGrassland)) + mean(contcov$PropGrassland) #standardzied value is set to 1, 0.13
# 1 sd is 0.13 grassland proportion
# This calculation indicates that a one standard deviation increase from the mean on your standardized scale corresponds to an actual value of 0.13 on the original scale of your variable

# what does it mean in percentage?
# percentage of total possible range: 13%
# percentage increase from mean: 0.13/0.07*100=185.7143%

# effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1); you can change T to other value if you want to look at effect of 1SD in temp
logSurv_combined_ref<-beta0B+beta0A*betaB+betaB*betaA*reference.env.driver.value
# upper 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_upper95_ref<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*reference.env.driver.value
# lower 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_lower95_ref<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*reference.env.driver.value

# now translate from the log(S) to the S scale
Surv_combined_ref<-exp(logSurv_combined_ref)
Surv_combined_upper95_ref<-exp(logSurv_combined_upper95_ref)
Surv_combined_lower95_ref<-exp(logSurv_combined_lower95_ref)
print(c(Surv_combined_ref,Surv_combined_lower95_ref,Surv_combined_upper95_ref))


# for below steps see word doc for explanation
# effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1); you can change T to other value if you want to look at effect of 1SD in temp
logSurv_combined<-beta0B+beta0A*betaB+betaB*betaA*(reference.env.driver.value+pertubation.env.driver.value)
# upper 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_upper95<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)
# lower 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
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
beta0A<-1.4  # intercept of regression of Cond on Temp; this estimate was not in your file, so I just put it on zero, should be changes to correct estimate
beta0B<-3.07  # intercept of regression of Surv on Cond; this estimate was not in your file, so I just put it on zero, should be changes to correct estimate
betaA<-0.09 # effect of cockel on cond
betaB<-0.13 # effect of cond on log(surv) 
se_betaA<-0.10 # se of effect of grassland on cond
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
reference.env.driver.value<-0 # this is the mean value of the env. driver, I assume it is zero for temperature (as this is a latent, but for grassland it may be 0.17 if you have not mean-centered the grassland variable))
pertubation.env.driver.value<-1 # mean and increase of 10%; you may have to change this for temperature, as 1 SD is a lot, maybe use the equivalent of 1 degree?  For grassland, the pertubation may have to be not 1, but 0.17*0.1 if you want to look at the effect of 10% increase in existing grassland

#convert standardized score back to original scale
#Original Value=(Standardized Value ?? Original Standard Deviation)+Original Mean
(1* sd(contcov$AFDMgrm2_C_CondLoss_exp)) + mean(contcov$AFDMgrm2_C_CondLoss_exp) #standardzied value is set to 1
# 1 sd is 10.24508 g afdm/m2 cockle availability
# This calculation indicates that a one standard deviation increase from the mean on your standardized scale corresponds to an actual value of 0.13 on the original scale of your variable

# effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1); you can change T to other value if you want to look at effect of 1SD in temp
logSurv_combined_ref<-beta0B+beta0A*betaB+betaB*betaA*reference.env.driver.value
# upper 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_upper95_ref<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*reference.env.driver.value
# lower 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_lower95_ref<-beta0B+beta0A*betaB+(betaB*betaA-1.96*se_betaAB)*reference.env.driver.value

# now translate from the log(S) to the S scale
Surv_combined_ref<-exp(logSurv_combined_ref)
Surv_combined_upper95_ref<-exp(logSurv_combined_upper95_ref)
Surv_combined_lower95_ref<-exp(logSurv_combined_lower95_ref)
print(c(Surv_combined_ref,Surv_combined_lower95_ref,Surv_combined_upper95_ref))

# for below steps see word doc for explanation
# effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1); you can change T to other value if you want to look at effect of 1SD in temp
logSurv_combined<-beta0B+beta0A*betaB+betaB*betaA*(reference.env.driver.value+pertubation.env.driver.value)
# upper 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
logSurv_combined_upper95<-beta0B+beta0A*betaB+(betaB*betaA+1.96*se_betaAB)*(reference.env.driver.value+pertubation.env.driver.value)
# lower 95%CI effect of T on log(S) mediated by C, where we look at effect of 1 degree change (T=1)
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
