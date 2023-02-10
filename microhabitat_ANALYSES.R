rm(list = ls())


# XXX Manuscript uses asreml to do models (must pay for license)
## used asreml because easiest way to get 95% CIs on variance estimates
have_asreml <- FALSE  #<-- change to TRUE if computer has the asreml software

# If do not have asreml, can still get same models with nlme, just can't do
## profile likelihood method to get 95% CIs on variance estimates.
library(nlme)


if(have_asreml){
  library(asreml)
  # create function to extract 95% CIs from profile likelihood
  ## uses nadiv:::proLik4 to profile the likelihood for a single variance
  proCI <- function(x){
    if(is(x) != "proLik") error("x is not a profile likelihood/nadiv::proLik")  
    unlist(x[c("LCL", "UCL")])
  }  #<-- end function
}
# need nadiv for use with asreml (need LRT function)
library(nadiv) 


#FIXME set to your own path here
#setwd("<< Insert path on local computer >>")

# load data
microhab <- read.table(file = "microhabitat.txt", header = TRUE)




################################################################################
# BEFORE ANALYSES, need to make categorical variables into factors
## Do this so won't make mistake using an integer and R interprets this as a covariate when you want it to be a categorical factor
str(microhab)
# Just doing the main ones, others *could* be added if used later
# Also, Mean center and standardize iButton depth and canopy openness: use as covariates
microhab <- within(microhab, {
  NestRandFac <- as.factor(NestRand)  #<-- create new column/don't write over
  SiteTypeFac <- as.factor(SiteType)
  NestClusterFac <- as.factor(NestCluster)
  scibdepth <- scale(ibdepth)
  scCanopy <- scale(Canopy)
})

# order based on nest type for consistency (and for asreml)
microhab <- microhab[order(microhab$NestRandFac), ]

############################################################################



################################
# MICROHABITAT ANALYSES
################################

#### Real vs. random locations for each microhabitat variable, plus urbanization covar. and urb*nestrand interaction
###random effect of nest cluster


##################################################
# Distance to water
##################################################
modDistTW <- lme(DistTW ~ urbPC1*NestRandFac,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac), 
  data = microhab, na.action = na.omit)
# use `modDistTW` (separate residual variances)
## but now see whether slopes differ or should we use a model with a single slope
anova(modDistTW)  #<-- no significant interaction
modDistTWb <- lme(DistTW ~ urbPC1 + NestRandFac,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac), 
  data = microhab, na.action = na.omit)
# Test for different residual variances  
modDistTWc <- lme(DistTW ~ urbPC1*NestRandFac,
  random = ~ 1 | NestClusterFac,
  data = microhab, na.action = na.omit)
anova(modDistTW, modDistTWc)
summary(modDistTWb)  #<-- XXX use no interaction but separate residual variances
##############################
# asreml
if(have_asreml){
asrDistTW <- asreml(DistTW ~ urbPC1*NestRandFac,
  random = ~ NestClusterFac,
  residual = ~ dsum( ~ units | NestRandFac),
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
asrDistTWb <- asreml(DistTW ~ urbPC1*NestRandFac,
  random = ~ NestClusterFac,
  residual = ~ idv(units), #<--necessary so model NOT parameterized ratios: var/sigma
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
# convert asreml variances into lme SD and SD ratio
## use delta method to approximate std. errors on calculated SD and SD ratio
rbind(vpredict(asrDistTW, ~ sqrt(V1)),
  vpredict(asrDistTW, ~ sqrt(V2)),
  vpredict(asrDistTW, ~ sqrt(V3) / sqrt(V2)))
summary(modDistTW)

# Profile likelihood confidence intervals
DistTW.v1 <- proLik4(asrDistTW, component = ~ V1, parallel = TRUE, ncores = 8)
DistTW.v2 <- proLik4(asrDistTW, component = ~ V2, G = FALSE,
  parallel = TRUE, ncores = 8)
DistTW.v3 <- proLik4(asrDistTW, component = ~ V3, G = FALSE,
  parallel = TRUE, ncores = 8)


rbind(vpredict(asrDistTWb, ~ sqrt(V1)),
  vpredict(asrDistTWb, ~ sqrt(V2)))
summary(modDistTWc)
# do the likelihood ratio test of asreml models to compare to anova() results
LRTest(asrDistTW$loglik, asrDistTWb$loglik, df = 1)
anova(modDistTW, modDistTWc)
#XXX asreml agrees with variance components of lme
}
##############################

 




##################################################
# Slope
##################################################
modSlope <- lme(Slope ~ urbPC1*NestRandFac,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
## see whether slopes differ or should use model with single slope
anova(modSlope)    #<-- no significant interaction
modSlopeb <- lme(Slope ~ urbPC1 + NestRandFac,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
# Test for different residual variances  
modSlopec <- lme(Slope ~ urbPC1*NestRandFac,
  random = ~ 1 | NestClusterFac,
  data = microhab, na.action = na.omit)
anova(modSlope,modSlopec)  #<-- no significant diff: use 1 residual variance
summary(modSlope)
summary(modSlopec)  #<-- XXX Use this model

##############################
# asreml
if(have_asreml){
asrSlope <- asreml(Slope ~ urbPC1*NestRandFac,
  random = ~ NestClusterFac,
  residual = ~ dsum( ~ units | NestRandFac),
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
asrSlopeb <- asreml(Slope ~ urbPC1*NestRandFac,
  random = ~ NestClusterFac,
  residual = ~ idv(units), #<--necessary so model NOT parameterized ratios: var/sigma
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
# convert asreml variances into lme SD and SD ratio
## use delta method to approximate std. errors on calculated SD and SD ratio
rbind(vpredict(asrSlope, ~ sqrt(V1)),
  vpredict(asrSlope, ~ sqrt(V2)),
  vpredict(asrSlope, ~ sqrt(V3) / sqrt(V2)))
summary(modSlope)

# Profile likelihood confidence intervals
Slope.v1 <- proLik4(asrSlope, component = ~ V1, parallel = TRUE, ncores = 8)
Slope.v2 <- proLik4(asrSlope, component = ~ V2, G = FALSE,
  parallel = TRUE, ncores = 8)
Slope.v3 <- proLik4(asrSlope, component = ~ V3, G = FALSE,
  parallel = TRUE, ncores = 8)


rbind(vpredict(asrSlopeb, ~ sqrt(V1)),
  vpredict(asrSlopeb, ~ sqrt(V2)))
summary(modSlopec)
# do the likelihood ratio test of asreml models to compare to anova() results
LRTest(asrSlope$loglik, asrSlopeb$loglik, df = 1)
anova(modSlope, modSlopec)
#XXX asreml agrees with variance components of lme
}
##############################






##################################################
# Canopy Openness
##################################################
modCanopy <- lme(Canopy ~ urbPC1*NestRandFac,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(modCanopy)
## but now see whether slopes differ or should we use a model with a single slope
anova(modCanopy)  #<-- NO significant interaction
modCanopyb <- lme(Canopy ~ urbPC1 + NestRandFac,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(modCanopyb)  #<-- XXX Use this model
# Test for different residual variances  
modCanopyc <- lme(Canopy ~ urbPC1*NestRandFac,
  random = ~ 1 | NestClusterFac,
  data = microhab, na.action = na.omit)
summary(modCanopyc)  
anova(modCanopy,modCanopyc)  #<-- XXX significant diff: use 2 residual variance

##############################
# asreml
if(have_asreml){
asrCanopy <- asreml(Canopy ~ urbPC1*NestRandFac,
  random = ~ NestClusterFac,
  residual = ~ dsum( ~ units | NestRandFac),
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
asrCanopyb <- asreml(Canopy ~ urbPC1*NestRandFac,
  random = ~ NestClusterFac,
  residual = ~ idv(units), #<--necessary so model NOT parameterized ratios: var/sigma
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
# convert asreml variances into lme SD and SD ratio
## use delta method to approximate std. errors on calculated SD and SD ratio
rbind(vpredict(asrCanopy, ~ sqrt(V1)),
  vpredict(asrCanopy, ~ sqrt(V2)),
  vpredict(asrCanopy, ~ sqrt(V3) / sqrt(V2)))
summary(modCanopy)

# Profile likelihood confidence intervals
Canopy.v1 <- proLik4(asrCanopy, component = ~ V1, parallel = TRUE, ncores = 8)
Canopy.v2 <- proLik4(asrCanopy, component = ~ V2, G = FALSE,
  parallel = TRUE, ncores = 8)
Canopy.v3 <- proLik4(asrCanopy, component = ~ V3, G = FALSE,
  parallel = TRUE, ncores = 8)


rbind(vpredict(asrCanopyb, ~ sqrt(V1)),
  vpredict(asrCanopyb, ~ sqrt(V2)))
summary(modCanopyc)
# do the likelihood ratio test of asreml models to compare to anova() results
LRTest(asrCanopy$loglik, asrCanopyb$loglik, df = 1)
anova(modCanopy, modCanopyc)
#XXX asreml agrees with variance components of lme
}
##############################
# use `modCanopyb` (separate residual variances)









##################################################
# Daily Mean Temperature
##################################################
moddailyMean_C <- lme(dailyMean_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(moddailyMean_C)
## but now see whether slopes differ or should we use a model with a single slope
anova(moddailyMean_C)  #<-- NO significant interaction
moddailyMean_Cb <- lme(dailyMean_C ~ urbPC1 + NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(moddailyMean_Cb)
# See if separate residual variances needed
moddailyMean_Cc <- lme(dailyMean_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  data = microhab, na.action = na.omit)
summary(moddailyMean_Cc)  #<-- XXX Use this model 
anova(moddailyMean_C,moddailyMean_Cc)  #<--XXX No significant diff.

##############################
# asreml
if(have_asreml){
asrdailyMean_C <- asreml(dailyMean_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ NestClusterFac,
  residual = ~ dsum( ~ units | NestRandFac),
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
asrdailyMean_C <- update(asrdailyMean_C, maxit = 10)  
asrdailyMean_C <- update(asrdailyMean_C, maxit = 10)  
asrdailyMean_Cb <- asreml(dailyMean_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ NestClusterFac,
  residual = ~ idv(units), #<--necessary so model NOT parameterized ratios: var/sigma
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
# convert asreml variances into lme SD and SD ratio
## use delta method to approximate std. errors on calculated SD and SD ratio
rbind(vpredict(asrdailyMean_C, ~ sqrt(V1)),
  vpredict(asrdailyMean_C, ~ sqrt(V3)),
  vpredict(asrdailyMean_C, ~ sqrt(V2) / sqrt(V3)))
summary(moddailyMean_C)

# Profile likelihood confidence intervals
dailyMean_C.v1 <- proLik4(asrdailyMean_C, component = ~ V1,
  parallel = TRUE, ncores = 8)
dailyMean_C.v2 <- proLik4(asrdailyMean_C, component = ~ V2, G = FALSE,
  parallel = TRUE, ncores = 8)
dailyMean_C.v3 <- proLik4(asrdailyMean_C, component = ~ V3, G = FALSE,
  parallel = TRUE, ncores = 8)
par(mfrow = c(3, 1))
  plot(dailyMean_C.v1)
  plot(dailyMean_C.v2)
  plot(dailyMean_C.v3)
    
    
rbind(vpredict(asrdailyMean_Cb, ~ sqrt(V1)),
  vpredict(asrdailyMean_Cb, ~ sqrt(V2)))
summary(moddailyMean_Cc)
# do the likelihood ratio test of asreml models to compare to anova() results
LRTest(asrdailyMean_C$loglik, asrdailyMean_Cb$loglik, df = 1)
anova(moddailyMean_C, moddailyMean_Cc)
## the test of whether or not the variances differ yields the same interpretation
### across ALL software
}
##############################
# use `moddailyMean_C` (2 residual variances) based on profile likelihood CIs




  

##################################################
# Daily Max Temperature
##################################################
moddailyMax_C <- lme(dailyMax_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(moddailyMax_C)
## see whether slopes differ or should use model with single slope
anova(moddailyMax_C)  #<-- NO significant interaction
moddailyMax_Cb <- lme(dailyMax_C ~ urbPC1 + NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(moddailyMax_Cb)
# Test for separate residual variances
moddailyMax_Cc <- lme(dailyMax_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  data = microhab, na.action = na.omit)
summary(moddailyMax_Cc) 
anova(moddailyMax_C, moddailyMax_Cc)
# use `moddailyMax_Cc` (No difference between residual variances)
summary(moddailyMax_Cc)  #<-- XXX Use this model
##############################
# asreml
if(have_asreml){
asrdailyMax_C <- asreml(dailyMax_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ NestClusterFac,
  residual = ~ dsum( ~ units | NestRandFac),
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
asrdailyMax_C <- update(asrdailyMax_C, maxit = 10)
asrdailyMax_Cb <- asreml(dailyMax_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ NestClusterFac,
  residual = ~ idv(units), #<--necessary so model NOT parameterized ratios: var/sigma
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
# convert asreml variances into lme SD and SD ratio
## use delta method to approximate std. errors on calculated SD and SD ratio
rbind(vpredict(asrdailyMax_C, ~ sqrt(V1)),
  vpredict(asrdailyMax_C, ~ sqrt(V3)),
  vpredict(asrdailyMax_C, ~ sqrt(V2) / sqrt(V3)))
summary(moddailyMax_C)

# Profile likelihood confidence intervals
dailyMax_C.v1 <- proLik4(asrdailyMax_C, component = ~ V1,
  parallel = TRUE, ncores = 8,
  nsample.units = 1, nse = 4)
dailyMax_C.v2 <- proLik4(asrdailyMax_C, component = ~ V2, G = FALSE,
  parallel = TRUE, ncores = 8)
dailyMax_C.v3 <- proLik4(asrdailyMax_C, component = ~ V3, G = FALSE,
  parallel = TRUE, ncores = 8,
  nsample.units = 1, nse = 1.5)
# Have to find upper CI limit manually  
#XXX MUST do var.estimate replacement FIRST XXX
dailyMax_C.v3$var.estimates <- dailyMax_C.v3$var.estimates[!is.na(dailyMax_C.v3$lambdas)]
#XXX
dailyMax_C.v3$lambdas <- dailyMax_C.v3$lambdas[!is.na(dailyMax_C.v3$lambdas)]
chi.val <- 0.5 * qchisq(1 - 0.05, df = 1)
fllmd <- asreml::update.asreml(object = asrdailyMax_C,
  start.values = TRUE)$vparameters.table
fllmd2 <- fllmd
  fllmd2[3, 3] <- "F"
for(v in seq(17.05, 17.15, by = 0.005)){  
  fllmd2[3, 2] <- v
  conMod <- update.asreml(asrdailyMax_C, random = ~., R.param = fllmd2)
    conMod <- update(conMod, maxiter = 10)
  lout <- asreml::lrt(conMod, asrdailyMax_C)$"LR-statistic"
  dailyMax_C.v3 <- within(dailyMax_C.v3, {
    lambdas <- c(lambdas, lout)
    var.estimates <- c(var.estimates, v)})
}
dailyMax_C.v3
chi.val
for(v in seq(17.14, 17.145, by = 0.0001)){  
  fllmd2[3, 2] <- v
  conMod <- update.asreml(asrdailyMax_C, random = ~., R.param = fllmd2)
    conMod <- update(conMod, maxiter = 10)
  lout <- asreml::lrt(conMod, asrdailyMax_C)$"LR-statistic"
  dailyMax_C.v3 <- within(dailyMax_C.v3, {
    lambdas <- c(lambdas, lout)
    var.estimates <- c(var.estimates, v)})
}
dailyMax_C.v3
chi.val
v <- 17.14016
fllmd2[3, 2] <- v
conMod <- update.asreml(asrdailyMax_C, random = ~., R.param = fllmd2)
  conMod <- update(conMod, maxiter = 10)
lout <- asreml::lrt(conMod, asrdailyMax_C)$"LR-statistic"
dailyMax_C.v3 <- within(dailyMax_C.v3, {
  lambdas <- c(lambdas, lout)
  var.estimates <- c(var.estimates, v)})
dailyMax_C.v3
chi.val
dailyMax_C.v3 <- within(dailyMax_C.v3, {
  lambdas <- lambdas[order(var.estimates)]
  var.estimates <- var.estimates[order(var.estimates)]})

dailyMax_C.v3$UCL <- with(dailyMax_C.v3, var.estimates[var.estimates > 10][which.min(abs(lambdas[var.estimates > 10] - chi.val))])
  

par(mfrow = c(3, 1))
  plot(dailyMax_C.v1)
  plot(dailyMax_C.v2)
  plot(dailyMax_C.v3)


rbind(vpredict(asrdailyMax_Cb, ~ sqrt(V1)),
  vpredict(asrdailyMax_Cb, ~ sqrt(V2)))
summary(moddailyMax_Cc)
# do the likelihood ratio test of asreml models to compare to anova() results
LRTest(asrdailyMax_C$loglik, asrdailyMax_Cb$loglik, df = 1)
anova(moddailyMax_C, moddailyMax_Cc)
#XXX asreml MOSTLY agrees with variance components of lme
## the test of whether or not the variances differ yields the same interpretation
### across ALL software
}
##############################






##################################################
# Daily Min Temperature
##################################################
moddailyMin_C <- lme(dailyMin_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(moddailyMin_C)
anova(moddailyMin_C)  #<-- NO significant interaction
moddailyMin_Cb <- lme(dailyMin_C ~ urbPC1 + NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
# Test for separate residual variances
## drop separate residual variances  
moddailyMin_Cc <- lme(dailyMin_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  data = microhab, na.action = na.omit)
summary(moddailyMin_Cc)
anova(moddailyMin_C,moddailyMin_Cc)  #<-- no significant diff. XXX p=0.08 XXX
# use `moddailyMin_C` (2 different residual variances since profile 95%CI do NOT overalp)
summary(moddailyMin_C)  #<-- XXX Use this model
##############################
# asreml
if(have_asreml){
asrdailyMin_C <- asreml(dailyMin_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ NestClusterFac,
  residual = ~ dsum( ~ units | NestRandFac),
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
asrdailyMin_C <- update(asrdailyMin_C, maxit = 10)    
asrdailyMin_Cb <- asreml(dailyMin_C ~ urbPC1*NestRandFac + scibdepth,
  random = ~ NestClusterFac,
  residual = ~ idv(units), #<--necessary so model NOT parameterized ratios: var/sigma
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
# convert asreml variances into lme SD and SD ratio
## use delta method to approximate std. errors on calculated SD and SD ratio
rbind(vpredict(asrdailyMin_C, ~ sqrt(V1)),
  vpredict(asrdailyMin_C, ~ sqrt(V3)),
  vpredict(asrdailyMin_C, ~ sqrt(V2) / sqrt(V3)))
summary(moddailyMin_C)


# Profile likelihood confidence intervals
dailyMin_C.v1 <- proLik4(asrdailyMin_C, component = ~ V1,
  parallel = TRUE, ncores = 8)
dailyMin_C.v2 <- proLik4(asrdailyMin_C, component = ~ V2, G = FALSE,
  parallel = TRUE, ncores = 8)
dailyMin_C.v3 <- proLik4(asrdailyMin_C, component = ~ V3, G = FALSE,
  parallel = TRUE, ncores = 8)
par(mfrow = c(3, 1))
  plot(dailyMin_C.v1)
  plot(dailyMin_C.v2)
  plot(dailyMin_C.v3)
    

rbind(vpredict(asrdailyMin_Cb, ~ sqrt(V1)),
  vpredict(asrdailyMin_Cb, ~ sqrt(V2)))
summary(moddailyMin_Cc)
# do the likelihood ratio test of asreml models to compare to anova() results
LRTest(asrdailyMin_C$loglik, asrdailyMin_Cb$loglik, df = 1)
anova(moddailyMin_C, moddailyMin_Cc)
#XXX asreml MOSTLY agrees with variance components of lme
## the test of whether or not the variances differ yields the same interpretation
### across ALL software
}
##############################







##################################################
# Daily Temperature Range
##################################################
modrange <- lme(range ~ urbPC1*NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(modrange)
anova(modrange)  #<-- NO significant interaction
modrangeb <- lme(range ~ urbPC1 + NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  weights = varIdent(form = ~ 1 | NestRandFac),
  data = microhab, na.action = na.omit)
summary(modrangeb)
# Now test for separate residual variances
modrangec <- lme(range ~ urbPC1*NestRandFac + scibdepth,
  random = ~ 1 | NestClusterFac,
  data = microhab, na.action = na.omit)
summary(modrangec)  #<-- XXX Use this model
anova(modrange,modrangec)  #<-- XXX Non-significance difference between residuals
# use `modrangec` (No difference between residual variances)
##############################
# asreml
if(have_asreml){
asrrange <- asreml(range ~ urbPC1*NestRandFac + scibdepth,
  random = ~ NestClusterFac,
  residual = ~ dsum( ~ units | NestRandFac),
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
asrrangeb <- asreml(range ~ urbPC1*NestRandFac + scibdepth,
  random = ~ NestClusterFac,
  residual = ~ idv(units), #<--necessary so model NOT parameterized ratios: var/sigma
  data = microhab, na.action = list(y = "omit", x = "omit"),
  maxit = 20)
# convert asreml variances into lme SD and SD ratio
## use delta method to approximate std. errors on calculated SD and SD ratio
rbind(vpredict(asrrange, ~ sqrt(V1)),
  vpredict(asrrange, ~ sqrt(V3)),
  vpredict(asrrange, ~ sqrt(V2) / sqrt(V3)))
summary(modrange)

# Profile likelihood confidence intervals
range.v1 <- proLik4(asrrange, component = ~ V1,
  parallel = TRUE, ncores = 8)
range.v2 <- proLik4(asrrange, component = ~ V2, G = FALSE,
  parallel = TRUE, ncores = 8)
range.v3 <- proLik4(asrrange, component = ~ V3, G = FALSE,
  parallel = TRUE, ncores = 8)
par(mfrow = c(3, 1))
  plot(range.v1)
  plot(range.v2)
  plot(range.v3)
    


rbind(vpredict(asrrangeb, ~ sqrt(V1)),
  vpredict(asrrangeb, ~ sqrt(V2)))
summary(modrangec)
# do the likelihood ratio test of asreml models to compare to anova() results
LRTest(asrrange$loglik, asrrangeb$loglik, df = 1)
anova(modrange, modrangec)
#XXX asreml MOSTLY agrees with variance components of lme
## the test of whether or not the variances differ yields the same interpretation
### across ALL software
}
##############################













###############################################################################
###############################################################################

#### Save asreml results if necessary
if(have_asreml){
  ciBaseNms <- c("Canopy", "dailyMax_C", "dailyMean_C", "dailyMin_C",
    "DistTW", "range", "Slope")
  # Save raw profile likelihoods
  save(list = c(paste0(ciBaseNms, ".v1"),
      paste0(ciBaseNms, ".v2"),
      paste0(ciBaseNms, ".v3")),
    file = "./microhabProfileCIs.rda")
}
###############################################################################












###############################################################################
# Figure with urbanization scores by location/location type
###############################################################################

# First, make simple dataframe with sites
locations <- microhab[which(!duplicated(microhab$siteAbbr)),
     match(c("SiteName", "siteAbbr", "SiteType", "HumDist", "urbPC1", "urbPC2"),
                                                              names(microhab))]
# Give each location a number corresponding to table in main text of Manuscript
## Missing the Agricultural Heritage park (4) and Notasulga pond (15)
locations$ind <- c(3, 2, 11, 10, 5, 7, 6, 14, 1, 13, 12, 9, 8) 
# now make small dataframe for plotting the indices at the urbPC1 scores
## and condense to a single unique urbPC1
### sort first, so indices will increase when paste duplicated ones back in
locTcks <- locations[order(locations$ind), c("urbPC1", "ind")]
locTcks <- locTcks[which(!duplicated(locTcks$urbPC1)), ]
locTcks$ind <- as.character(locTcks$ind)
# Now find matches and paste those indices in with existing ones
for(i in 1:nrow(locTcks)){
  locTcks$ind[i] <- paste(locations$ind[which(locTcks$urbPC1[i] ==
                                                              locations$urbPC1)],
    collapse = ",")
}
## order by urbPC1
locTcks <- locTcks[order(locTcks$urbPC1), ]
# Add a vertical adjustment to space out text on figure
locTcks$vadj <- rep(c(0, 0.2), nrow(locTcks))[1:nrow(locTcks)]



# Snag some colors from base R's `Okabe-Ito` palette
## suitable palette for color-related vision accessibility
### first have a look
palOI <- palette.colors(NULL, "Okabe-Ito")
pie(rep(1, length(palOI)), col = palOI, labels = names(palOI))
# grab subset of 3 colors for figure
palOI3 <- palOI[c("skyblue", "bluishgreen", "vermillion")]

dev.off()  #<-- turn off pie chart




# Need multiple "rug" plots to do different colors
## set rug plot arguments here
tcksz <- 0.05  #<-- "ticksize" positive then ticks plot in towards center of fig.
tcklwd <- 4    #<-- "lwd"
rpos <- 0.26  #<-- "pos" to make sure end of line doesn't extend below axis
xpos <- 0.25  #<-- "pos" of axis(1) to bring down to make room for indnices





#XXX for saving figures - use:
pdf(file = "./Fig1_HumDisturbCat_vs_urb.pdf",
  width = 9, height = 5)

# use `par()` to set up some features of the entire figure
## `mfrow` designates the number of rows x columns to create in the figure
par(mar = c(4.5, 5.5, 2.2, 0.1),  #<-- space around panel (bottom, left, top, right)
  mgp = c(3, 1, 0), #<-- adjustment of axis title, labels, and line
  cex.axis = 1.25, cex.lab = 1.6)  #<-- scaling for axis labels and axis title

boxplot(urbPC1 ~ SiteType, data = locations, horizontal = TRUE, axes = FALSE,
  col = palOI3,
  xlab = "Urbanization level (PC1)", ylab = "Study area disturbance level",
  xlim = c(0.5, 3.5), #<-- switched: for final y-axis when `horizontal = TRUE`
  ylim = c(-1.6, 5.3))  #<-- switched so eventual x-axis

rug(locations$urbPC1[locations$HumDist == "High"],
  side = 1,  #<-- side = 1 is bottom
  ticksize = tcksz, lwd = tcklwd, pos = rpos,
  col = palOI3[3]) #<-- color for ticks
rug(locations$urbPC1[locations$HumDist == "Intermediate"],
  side = 1,  #<-- side = 1 is bottom
  ticksize = tcksz, lwd = tcklwd, pos = rpos,
  col = palOI3[2]) #<-- color for ticks
rug(locations$urbPC1[locations$HumDist == "Low"],
  side = 1,  #<-- side = 1 is bottom
  ticksize = tcksz, lwd = tcklwd, pos = rpos,
  col = palOI3[1]) #<-- color for ticks
  
# indices to associate rug plot ticks with table in main manuscript
#text(x = locTcks$urbPC1, y = 0.5 + locTcks$vadj, labels = locTcks$ind, cex = 0.8)

axis(1, at = seq(-1.6, 5.2, 0.4), labels = FALSE, lwd = 1.6, pos = xpos)  #<-- just the axis
  axis(1, at = seq(-1.6, 5.2, 0.8), lwd = 0, pos = xpos)  #<-- just the labels
axis(2, at = seq.int(3), labels = c("Low", "Intermediate", "High"), lwd = 1.6)


dev.off() #<-- XXX MUST do this to close pdf file connection


















###############################################################################
# 7-panel Figure of all above variables
###############################################################################

## First, to create prediction lines need a new dataset that gives values over which we want predictions
### choose even spacing between minimum and maximum urbanization scores
### assign this to both random and nest sites
ndata <- with(microhab, #<--avoid `microhab$` each time
  data.frame(urbPC1 = rep(seq(from = min(urbPC1, na.rm = TRUE),
                              to = max(urbPC1, na.rm = TRUE),
                              length.out = 100), 2),
    NestRandFac = as.factor(rep(c(0, 1), each = 100)),
    scibdepth = rep(seq(from = min(scibdepth, na.rm = TRUE),
    			to = max(scibdepth, na.rm = TRUE),
    			length.out = 100), 2)))

# Set up a few things that will be used over and over again among all 8 plots
xlab_in <- "Urbanization level (PC1)"
xaxis <- seq(from = -2, to = 5, by = 1)
# what should we set x-axis limits at
range(microhab$urbPC1, na.rm = TRUE)
xlim_in <- c(-2.05, 5.2)

degCexpr <- "(\u00B0C)"  #<-- degrees Celsius expression to paste in
nestPtSymb <- 21
  nestPtCols <- c(bg = "#03244d", brd = "grey40")  #<-- "BLUE"
  nestPtCx <- 1.7
randPtSymb <- 22
  randPtCols <- c(bg = "#e86823", brd = "grey20")  #<-- "ORANGE"
  randPtCx <- 1.1
jitfac <- 3.6  #<-- jitter factor
reglinewd <- 3.2  #<-- line width of all regression lines
ptLwd <- 1.0  #<-- line width of point border





#XXX for saving figures - use:
pdf(file = "./Fig3_microhab_vs_urb.pdf",
  width = 9, height = 12)


# use `par()` to set up some features of the entire figure
## `mfrow` designates the number of rows x columns to create in the figure
par(mfrow = c(4, 2),  #<-- (rows, columns)
  mar = c(5, 6.2, 1.9, 0.5),  #<-- space around each panel (bottom, left, top, right)
  mgp = c(3, 1, 0), #<-- adjustment of axis title, labels, and line
  cex.axis = 1.5, cex.lab = 1.6)  #<-- scaling for axis labels and axis title
################################
# Distance to Water (DistTW)
################################
plot(DistTW ~ urbPC1, data = microhab, type = "n", #<-- just set up
  axes = FALSE,  #<-- make our own fancy ones
  xlim = xlim_in, ylim = c(0, 265),
  xlab = xlab_in,
  ylab = "Distance to water (m)")
  
  # points first (to put in background)
  ## Nest first
  points(DistTW ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 0,
    pch = nestPtSymb, bg = nestPtCols["bg"], col = nestPtCols["brd"],
    cex = nestPtCx, lwd = ptLwd)
  ## Random second
  points(DistTW ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 1,
    pch = randPtSymb, bg = randPtCols["bg"], col = randPtCols["brd"],
    cex = randPtCx, lwd = ptLwd)

  # Lines from model
  ## predict from the model
  ndata$pred <- predict(modDistTW, #<-- XXX change for each response variable
    newdata = ndata, level = 0)
  ## Nest first
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "0",
    lwd = reglinewd * 1.2, col = nestPtCols["bg"])  #<-- make thicker so not covered
  ## Random second
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "1",
    lwd = reglinewd, lty = "dashed", col = randPtCols["bg"])

 # X-axis followed by Y-axis
 axis(1, xaxis)  
 axis(2, seq(0, 250, 50))

mtext(text = expression((bold(A))),
  side = 3, line = -0.4, adj = -0.25, cex = 1.3)

 
################################        
# Slope
################################
plot(Slope ~ urbPC1, data = microhab, type = "n", #<-- just set up
  axes = FALSE,  #<-- make our own fancy ones
  xlim = xlim_in, ylim = c(0, 65),
  xlab = xlab_in,
  ylab = "Slope (degrees)")
  
  # points first (to put in background)
  ## Nest first
  points(Slope ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 0,
    pch = nestPtSymb, bg = nestPtCols["bg"], col = nestPtCols["brd"],
    cex = nestPtCx, lwd = ptLwd)
  ## Random second
  points(Slope ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 1,
    pch = randPtSymb, bg = randPtCols["bg"], col = randPtCols["brd"],
    cex = randPtCx, lwd = ptLwd)

  # Lines from model
  ## predict from the model
  ndata$pred <- predict(modSlope, #<-- XXX change for each response variable
    newdata = ndata, level = 0)
  ## Nest first
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "0",
    lwd = reglinewd * 1.2, col = nestPtCols["bg"])
  ## Random second
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "1",
    lwd = reglinewd, lty = "dashed", col = randPtCols["bg"])

 # X-axis followed by Y-axis
 axis(1, xaxis)  
 axis(2, seq(0, 60, 20))

mtext(text = expression((bold(B))),
  side = 3, line = -0.4, adj = -0.25, cex = 1.3)



################################
# Canopy
################################
plot(Canopy ~ urbPC1, data = microhab, type = "n", #<-- just set up
  axes = FALSE,  #<-- make our own fancy ones
  xlim = xlim_in, ylim = c(0, 100),
  xlab = xlab_in,
  ylab = "Canopy openness (%)")
  
  # points first (to put in background)
  ## Nest first
  points(Canopy ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 0,
    pch = nestPtSymb, bg = nestPtCols["bg"], col = nestPtCols["brd"],
    cex = nestPtCx, lwd = ptLwd)
  ## Random second
  points(Canopy ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 1,
    pch = randPtSymb, bg = randPtCols["bg"], col = randPtCols["brd"],
    cex = randPtCx, lwd = ptLwd)

  # Lines from model
  ## predict from the model
  ndata$pred <- predict(modCanopy, #<-- XXX change for each response variable
    newdata = ndata, level = 0)
  ## Nest first
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "0",
    lwd = reglinewd * 1.2, col = nestPtCols["bg"])
  ## Random second
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "1",
    lwd = reglinewd, lty = "dashed", col = randPtCols["bg"])

 # X-axis followed by Y-axis
 axis(1, xaxis)  
 axis(2, seq(0, 100, 20))

mtext(text = expression((bold(C))),
  side = 3, line = -0.4, adj = -0.25, cex = 1.3)





################################
# dailyMean_C
################################
plot(dailyMean_C ~ urbPC1, data = microhab, type = "n", #<-- just set up
  axes = FALSE,  #<-- make our own fancy ones
  xlim = xlim_in, ylim = c(22.5, 34),
  xlab = xlab_in,
  ylab = paste("Daily mean temp. ", degCexpr))
  
  # points first (to put in background)
  ## Nest first
  points(dailyMean_C ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 0,
    pch = nestPtSymb, bg = nestPtCols["bg"], col = nestPtCols["brd"],
    cex = nestPtCx, lwd = ptLwd)
  ## Random second
  points(dailyMean_C ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 1,
    pch = randPtSymb, bg = randPtCols["bg"], col = randPtCols["brd"],
    cex = randPtCx, lwd = ptLwd)

  # Lines from model
  ## predict from the model
  ndata$pred <- predict(moddailyMean_C, #<-- XXX change for each response variable
    newdata = ndata, level = 0)
  ## Nest first
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "0",
    lwd = reglinewd * 1.2, col = nestPtCols["bg"])
  ## Random second
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "1",
    lwd = reglinewd, lty = "dashed", col = randPtCols["bg"])

 # X-axis followed by Y-axis
 axis(1, xaxis)  
 axis(2, seq(24, 34, 2))

mtext(text = expression((bold(D))),
  side = 3, line = -0.2, adj = -0.24, cex = 1.3)
 

################################
# dailyMax_C
################################
plot(dailyMax_C ~ urbPC1, data = microhab, type = "n", #<-- just set up
  axes = FALSE,  #<-- make our own fancy ones
  xlim = xlim_in, ylim = c(23, 45),
  xlab = xlab_in,
  ylab = paste("Daily max. temp. ", degCexpr))
  
  # points first (to put in background)
  ## Nest first
  points(dailyMax_C ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 0,
    pch = nestPtSymb, bg = nestPtCols["bg"], col = nestPtCols["brd"],
    cex = nestPtCx, lwd = ptLwd)
  ## Random second
  points(dailyMax_C ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 1,
    pch = randPtSymb, bg = randPtCols["bg"], col = randPtCols["brd"],
    cex = randPtCx, lwd = ptLwd)

  # Lines from model
  ## predict from the model
  ndata$pred <- predict(moddailyMax_C, #<-- XXX change for each response variable
    newdata = ndata, level = 0)
  ## Nest first
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "0",
    lwd = reglinewd * 1.2, col = nestPtCols["bg"])
  ## Random second
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "1",
    lwd = reglinewd, lty = "dashed", col = randPtCols["bg"])

 # X-axis followed by Y-axis
 axis(1, xaxis)  
 axis(2, seq(25, 45, 5))

mtext(text = expression((bold(E))),
  side = 3, line = -0.2, adj = -0.24, cex = 1.3)
 

################################
# dailyMin_C
################################
plot(dailyMin_C ~ urbPC1, data = microhab, type = "n", #<-- just set up
  axes = FALSE,  #<-- make our own fancy ones
  xlim = xlim_in,
  xlab = xlab_in,
  ylab = paste("Daily min. temp. ", degCexpr))
  
  # points first (to put in background)
  ## Nest first
  points(dailyMin_C ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 0,
    pch = nestPtSymb, bg = nestPtCols["bg"], col = nestPtCols["brd"],
    cex = nestPtCx, lwd = ptLwd)
  ## Random second
  points(dailyMin_C ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 1,
    pch = randPtSymb, bg = randPtCols["bg"], col = randPtCols["brd"],
    cex = randPtCx, lwd = ptLwd)

  # Lines from model
  ## predict from the model
  ndata$pred <- predict(moddailyMin_C, #<-- XXX change for each response variable
    newdata = ndata, level = 0)
  ## Nest first
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "0",
    lwd = reglinewd * 1.2, col = nestPtCols["bg"])
  ## Random second
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "1",
    lwd = reglinewd, lty = "dashed", col = randPtCols["bg"])

 # X-axis followed by Y-axis
 axis(1, xaxis)  
 axis(2)

mtext(text = expression((bold(F))),
  side = 3, line = -0.2, adj = -0.24, cex = 1.3)

################################
# range
################################
plot(range ~ urbPC1, data = microhab, type = "n", #<-- just set up
  axes = FALSE,  #<-- make our own fancy ones
  xlim = xlim_in, ylim = c(0, 16),
  xlab = xlab_in,
  ylab = paste("Daily temp. range ", degCexpr))
  
  # points first (to put in background)
  ## Nest first
  points(range ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 0,
    pch = nestPtSymb, bg = nestPtCols["bg"], col = nestPtCols["brd"],
    cex = nestPtCx, lwd = ptLwd)
  ## Random second
  points(range ~ jitter(urbPC1, jitfac), data = microhab,
    subset = NestRand == 1,
    pch = randPtSymb, bg = randPtCols["bg"], col = randPtCols["brd"],
    cex = randPtCx, lwd = ptLwd)

  # Lines from model
  ## predict from the model
  ndata$pred <- predict(modrange, #<-- XXX change for each response variable
    newdata = ndata, level = 0)
  ## Nest first
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "0",
    lwd = reglinewd * 1.2, col = nestPtCols["bg"])
  ## Random second
  lines(pred ~ urbPC1, data = ndata, subset = NestRandFac == "1",
    lwd = reglinewd, lty = "dashed", col = randPtCols["bg"])

 # X-axis followed by Y-axis
 axis(1, xaxis)  
 axis(2, seq(0, 16, 4))

mtext(text = expression((bold(G))),
  side = 3, line = -0.2, adj = -0.24, cex = 1.3)

# END OF PLOTTING #############################


dev.off() #<-- XXX MUST do this to close pdf file connection
















################################################################################

################################################################################
# For results tables Using `asreml`
## "NestRandFac0" == natural and "NestRandFac1" == artifical
# Create quick function to extract fixed effect estimates and standard errors from asreml
fxdFun <- function(mod){
  fxdtmp <- cbind(rev(mod$coefficients$fixed),
                  rev(sqrt(mod$vcoeff$fixed)))
    dimnames(fxdtmp) <- list(rev(rownames(mod$coefficients$fixed)),
      c("Est", "Std.Err"))
 fxdtmp[which(fxdtmp[, 1] != 0.00 & fxdtmp[, 2] != 0.00), ]
}
#######################
fxdFun(asrDistTW)   
wald(asrDistTW)
summary(asrDistTW)$varcomp
rbind(proCI(DistTW.v2), proCI(DistTW.v3))
lrt(asrDistTWb, asrDistTW)


fxdFun(asrSlope)   
wald(asrSlope)
summary(asrSlope)$varcomp
rbind(proCI(Slope.v2), proCI(Slope.v3))
lrt(asrSlopeb, asrSlope)


fxdFun(asrCanopy)   
wald(asrCanopy)
summary(asrCanopy)$varcomp
rbind(proCI(Canopy.v2), proCI(Canopy.v3))
lrt(asrCanopyb, asrCanopy)


fxdFun(asrdailyMean_C)   
wald(asrdailyMean_C)
summary(asrdailyMean_C)$varcomp
rbind(proCI(dailyMean_C.v2), proCI(dailyMean_C.v3))
lrt(asrdailyMean_Cb, asrdailyMean_C)


fxdFun(asrdailyMax_C)   
wald(asrdailyMax_C)
summary(asrdailyMax_C)$varcomp
rbind(proCI(dailyMax_C.v2), proCI(dailyMax_C.v3))
lrt(asrdailyMax_Cb, asrdailyMax_C)


fxdFun(asrdailyMin_C)   
wald(asrdailyMin_C)
summary(asrdailyMin_C)$varcomp
rbind(proCI(dailyMin_C.v2), proCI(dailyMin_C.v3))
lrt(asrdailyMin_Cb, asrdailyMin_C)


fxdFun(asrrange)   
wald(asrrange)
summary(asrrange)$varcomp
rbind(proCI(range.v2), proCI(range.v3))
lrt(asrrangeb, asrrange)











