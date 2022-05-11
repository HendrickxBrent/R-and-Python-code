#####                                                                                      #####
#####   code for the course 'structural equation modeling' at the University of Leuven     #####
#####                                                                                      #####

# author : Brent hendrickx


library(stringr)
library(dplyr)
library(blavaan)
library(haven)
library(bayesplot)

library(semPlot)
#library(lavaan)   # do not run also lavaan here, otherwise some code won't work! (its actually also loaded when loading blavaan)
library(ggcorrplot)
library(semTools)
library(runjags)
library(rjags)

## check if you need this
# library(loo)
# library(modeest)
# library(testthat)
# library(MCMCpack)
# library(coda)
# library(bayesplot)
# library(future.apply)
# library(rstantools)
# library(nonnest2)
# library(mnormt)
# library(Rcpp)
# library(RcppEigen)
# library(BH)

getwd()
load('SEMwork.RData')





########################                  1                  ################################################
########################                                     ################################################
########################       descriptive statistics        ################################################
########################                                     ################################################
########################                                     ################################################






# check if they are indeed correlated
items1 <- vuil %>% select(DDP1:DDM4)
items1 <- vuil %>% select(DDP1:DDM4)
corr <- round(cor(items1, use = "complete.obs"),2)
corr
ggcorrplot(corr)

# check for normality
install.packages("naniar")
library(naniar)
vuil <- vuil %>% replace_with_na(replace = list(DDP1 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDP2 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDP3 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDP4 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDN1 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDN2 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDN3 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDN4 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDM1 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDM2 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDM3 = 0))
vuil <- vuil %>% replace_with_na(replace = list(DDM4 = 0))


DD <- filter(vuil, DDP1 == "1"|DDP1 == "2"| DDP1 == "3"|DDP1 == "4"|DDP1 == "5",
               DDP2 == "1"|DDP2 == "2"| DDP2 == "3"|DDP2 == "4"|DDP2 == "5",
               DDP3 == "1"|DDP3 == "2"| DDP3 == "3"|DDP3 == "4"|DDP3 == "5",
               DDP4 == "1"|DDP4 == "2"| DDP4 == "3"|DDP4 == "4"|DDP4 == "5",
               DDN1 == "1"|DDN1 == "2"| DDN1 == "3"|DDN1 == "4"|DDN1 == "5",
               DDN2 == "1"|DDN2 == "2"| DDN2 == "3"|DDN2 == "4"|DDN2 == "5",
               DDN3 == "1"|DDN3 == "2"| DDN3 == "3"|DDN3 == "4"|DDN3 == "5",
               DDN4 == "1"|DDN4 == "2"| DDN4 == "3"|DDN4 == "4"|DDN4 == "5",
               DDM1 == "1"|DDM1 == "2"| DDM1 == "3"|DDM1 == "4"|DDM1 == "5",
               DDM2 == "1"|DDM2 == "2"| DDM2 == "3"|DDM2 == "4"|DDM2 == "5",
               DDM3 == "1"|DDM3 == "2"| DDM3 == "3"|DDM3 == "4"|DDM3 == "5",
               DDM4 == "1"|DDM4 == "2"| DDM4 == "3"|DDM4 == "4"|DDM4 == "5")


par(mfrow=c(2,3))
hist(DD$DDP1, col = "royalblue", main = "Histogram of DDP1")
hist(DD$DDP2, col = "royalblue", main = "Histogram of DDP2")
hist(DD$DDP3, col = "royalblue", main = "Histogram of DDP3")
hist(DD$DDP4, col = "royalblue", main = "Histogram of DDP4")
hist(DD$DDN1, col = "royalblue", main = "Histogram of DDN1")
hist(DD$DDN2, col = "royalblue", main = "Histogram of DDN2")
hist(DD$DDN3, col = "royalblue", main = "Histogram of DDN3")
hist(DD$DDN4, col = "royalblue", main = "Histogram of DDN4")
hist(DD$DDM1, col = "royalblue", main = "Histogram of DDM1")
hist(DD$DDM2, col = "royalblue", main = "Histogram of DDM2")
hist(DD$DDM3, col = "royalblue", main = "Histogram of DDM3")
hist(DD$DDM4, col = "royalblue", main = "Histogram of DDM4")
par(mfrow=c(1,1))

summary(vuil$gender)
hist(vuil$gender)


DDage <- filter(vuil$age, age < 100)
hist(DDage$age)
boxplot(DDage$age)





########################                  1.1                ################################################
########################                                     ################################################
########################       exploring and preparing       ################################################
########################       data for CFA                  ################################################



models <-'P =~ DDP1+DDP2+DDP3+DDP4
N =~ DDN1+DDN2+DDN3+DDN4
M =~ DDM1+DDM2+DDM3+DDM4          
#DDN1 ~~ DDN2
#all =~ P+M+N
'
############################################ this one
modls <-'P =~ DDP2+DDP1+DDP3+DDP4
N =~ DDN3+DDN2+DDN1+DDN4
M =~ DDM4+DDM2+DDM3+DDM1         
#DDN1 ~~ DDN2
#all =~ P+M+N  # actually structually equivalent to 3 covariations
'
# the model is indeed an improvement to a model were they all fall in one factor
models <-'P =~ DDP1+DDP2+DDP3+DDP4+DDN3+DDN1+DDN2+DDN4+DDM1+DDM2+DDM3+DDM4 
'

# highest MI
models <-'P =~ DDP1+DDP2+DDP3+DDP4
N =~ DDN3+DDN1+DDN2+DDN4
M =~ DDM1+DDM2+DDM3+DDM4+DDN4
#DDN1 ~~ DDN2
#all =~ P+M+N
'
# but now the loading on N is only .19
models <-'P =~ DDP1+DDP2+DDP3+DDP4
N =~ DDN3+DDN1+DDN2
M =~ DDM1+DDM2+DDM3+DDM4+DDN4
#DDN1 ~~ DDN2
#all =~ P+M+N
'
summary(vuil$country)

table(US1gendr$gender)                                   # SAMPLE SIZE
MY1gendr <- filter(vuil, gender =="1", country =="MY") # 76
MY2gendr <- filter(vuil, gender =="2", country =="MY") # 80

DE1gendr <- filter(vuil, gender =="1", country =="DE") #    587
DE2gendr <- filter(vuil, gender =="2", country =="DE") #    232
FI1gendr <- filter(vuil, gender =="1", country =="FI") # 266
FI2gendr <- filter(vuil, gender =="2", country =="FI") # 106
BR1gendr <- filter(vuil, gender =="1", country =="BR") #    346
BR2gendr <- filter(vuil, gender =="2", country =="BR") #    165
SE1gendr <- filter(vuil, gender =="1", country =="SE") # 497
SE2gendr <- filter(vuil, gender =="2", country =="SE") # 120


GB1gendr <- filter(vuil, gender =="1", country =="GB") #
GB2gendr <- filter(vuil, gender =="2", country =="GB") #
CA1gendr <- filter(vuil, gender =="1", country =="CA") # 2369
CA2gendr <- filter(vuil, gender =="2", country =="CA") # 1366
AU1gendr <- filter(vuil, gender =="1", country =="AU") #      1746
AU2gendr <- filter(vuil, gender =="2", country =="AU") #      1219
IN1gendr <- filter(vuil, gender =="1", country =="IN") #      433
IN2gendr <- filter(vuil, gender =="2", country =="IN") #      218
PH1gendr <- filter(vuil, gender =="1", country =="PH") #
PH2gendr <- filter(vuil, gender =="2", country =="PH") #
PL1gendr <- filter(vuil, gender =="1", country =="PL") #
PL2gendr <- filter(vuil, gender =="2", country =="PL") #
NL1gendr <- filter(vuil, gender =="1", country =="NL") # 366
NL2gendr <- filter(vuil, gender =="2", country =="NL") # 120
NZ1gendr <- filter(vuil, gender =="1", country =="NZ") #
NZ2gendr <- filter(vuil, gender =="2", country =="NZ") #
IE1gendr <- filter(vuil, gender =="1", country =="IE") #
IE2gendr <- filter(vuil, gender =="2", country =="IE") #
SG1gendr <- filter(vuil, gender =="1", country =="SG") #
SG2gendr <- filter(vuil, gender =="2", country =="SG") #
FI1gendr <- filter(vuil, gender =="1", country =="FI") #
FI2gendr <- filter(vuil, gender =="2", country =="FI") #
FR1gendr <- filter(vuil, gender =="1", country =="FR") #
FR2gendr <- filter(vuil, gender =="2", country =="FR") #
RO1gendr <- filter(vuil, gender =="1", country =="RO") #
RO2gendr <- filter(vuil, gender =="2", country =="RO") #
MX1gendr <- filter(vuil, gender =="1", country =="MX") #
MX2gendr <- filter(vuil, gender =="2", country =="MX") #
NO1gendr <- filter(vuil, gender =="1", country =="NO") #
NO2gendr <- filter(vuil, gender =="2", country =="NO") #
ZA1gendr <- filter(vuil, gender =="1", country =="ZA") #
ZA2gendr <- filter(vuil, gender =="2", country =="ZA") #


US1gendr <- filter(vuil, gender =="1", country =="US") #16 817
US2gendr <- filter(vuil, gender =="2", country =="US") #10 657
HR1gendr <- filter(vuil, gender =="1", country =="HR") #   156
HR2gendr <- filter(vuil, gender =="2", country =="HR") #   124

table(HR2gendr$gender)

table(GB1gendr$gender) # 3883
table(GB2gendr$gender) # 2240
table(CA1gendr$gender) # 2369
table(CA2gendr$gender) # 1366
table(AU1gendr$gender) # 1746
table(AU2gendr$gender) # 1219
table(DE1gendr$gender) # 587
table(DE2gendr$gender) # 232
table(IN1gendr$gender) # 433
table(IN2gendr$gender) # 218
table(SE1gendr$gender) # 497
table(SE2gendr$gender) # 120
table(PH1gendr$gender) # 261
table(PH2gendr$gender) # 295
table(PL1gendr$gender) # 344
table(PL2gendr$gender) # 174
table(BR1gendr$gender) # 346
table(BR2gendr$gender) # 165
table(NL1gendr$gender) # 366
table(NL2gendr$gender) # 120
table(NZ1gendr$gender) # 285
table(NZ2gendr$gender) # 155
table(IE1gendr$gender) # 263
table(IE2gendr$gender) # 139
table(SG1gendr$gender) # 195
table(SG2gendr$gender) # 179
# table(FI1gendr$gender) # 266
# table(FI2gendr$gender) # 106   # this sample size is too low
table(FR1gendr$gender) # 214
table(FR2gendr$gender) # 137
table(RO1gendr$gender) # 175
table(RO2gendr$gender) # 142
# table(MX1gendr$gender) # 223
# table(MX2gendr$gender) # 88    # this sample size is too low
# table(NO1gendr$gender) # 220
# table(NO2gendr$gender) # 89    # this sample size is too low
table(ZA1gendr$gender) # 159
table(ZA2gendr$gender) # 121


####################################################################### before cloud
# sampling from US, later on compare to cloud computing

sampl <- sample(1:nrow(US1gendr), 600)
US2gendr <- US2gendr[sampl, ]
summary(vuil$gender)

sampl <- sample(1:nrow(US2gendr), 600)
US1gendr <- US1gendr[sampl, ]

USgendr <- filter(vuil, gender =="1"|gender =="2", country =="US") # need this later
sampl <- sample(1:nrow(USgendr), 2000)
USgendr <- USgendr[sampl, ]
summary(USgendr$gender)
# make separate rfile with only the code you need for the bayesian large samples estimations (US)
# so you can stay with more confidence within the free 15 hours and 1GB of Rstudio cloud
# running this document alone will make a workspace of 0.4GB
#######################################################################




########################                  2                  ################################################
########################                                     ################################################
########################           CFA US and HR             ################################################
########################                                     ################################################


########################                  2.1                ################################################
########################                                     ################################################
########################          maximum likelihood         ################################################


# CFA
# original dirty dozen
fitUS1a <- cfa(modls,data=US1gendr, estimator = 'MLM', meanstructure=T) # need meanstructure to use parameters in bcfa
fitUS2a <- cfa(modls,data=US2gendr, estimator = 'MLM', meanstructure=T)
fitHR1a <- cfa(modls,data=HR1gendr, estimator = 'MLM', meanstructure=T)
fitHR2a <- cfa(modls,data=HR2gendr, estimator = 'MLM', meanstructure=T)

# run the models with DDN1 ~~ DDN2
fitUS1 <- cfa(models,data=US1gendr, estimator = 'MLM', meanstructure=T) 
fitUS2 <- cfa(models,data=US2gendr, estimator = 'MLM', meanstructure=T)
fitHR1 <- cfa(models,data=HR1gendr, estimator = 'MLM', meanstructure=T)
fitHR2 <- cfa(models,data=HR2gendr, estimator = 'MLM', meanstructure=T)

fitUS1 <- cfa(models,data=US1gendr, meanstructure=T) 
fitUS2 <- cfa(models,data=US2gendr, meanstructure=T)
fitHR1 <- cfa(models,data=HR1gendr, meanstructure=T)
fitHR2 <- cfa(models,data=HR2gendr, meanstructure=T)

#######
####### missingness
#######

fitUS1b <- cfa(models,data=US1gendr, meanstructure=T, missing = "direct") 
fitUS2b <- cfa(models,data=US2gendr, meanstructure=T, missing = "direct")
fitHR1b <- cfa(models,data=HR1gendr, meanstructure=T, missing = "direct")
fitHR2b <- cfa(models,data=HR2gendr, meanstructure=T, missing = "direct")

fitMY1 <- cfa(models,data=MY1gendr, estimator = 'MLM', meanstructure=T)
fitMY2 <- cfa(models,data=MY2gendr, estimator = 'MLM', meanstructure=T)

fitDE1 <- cfa(models,data=DE1gendr, estimator = 'MLM', meanstructure=T)
fitDE2 <- cfa(models,data=DE2gendr, estimator = 'MLM', meanstructure=T)
fitFI1 <- cfa(models,data=FI1gendr, estimator = 'MLM', meanstructure=T)
fitFI2 <- cfa(models,data=FI2gendr, estimator = 'MLM', meanstructure=T)
fitBR1 <- cfa(models,data=BR1gendr, estimator = 'MLM', meanstructure=T)
fitBR2 <- cfa(models,data=BR2gendr, estimator = 'MLM', meanstructure=T)
fitSE1 <- cfa(models,data=SE1gendr, estimator = 'MLM', meanstructure=T)
fitSE2 <- cfa(models,data=SE2gendr, estimator = 'MLM', meanstructure=T)

fitGB1 <- cfa(models,data=GB1gendr, estimator = 'MLM', meanstructure=T)
fitGB2 <- cfa(models,data=GB2gendr, estimator = 'MLM', meanstructure=T)
fitCA1 <- cfa(models,data=CA1gendr, estimator = 'MLM', meanstructure=T)
fitCA2 <- cfa(models,data=CA2gendr, estimator = 'MLM', meanstructure=T)
fitAU1 <- cfa(models,data=AU1gendr, estimator = 'MLM', meanstructure=T)
fitAU2 <- cfa(models,data=AU2gendr, estimator = 'MLM', meanstructure=T)

fitIN1 <- cfa(models,data=IN1gendr, estimator = 'MLM', meanstructure=T)
fitIN2 <- cfa(models,data=IN2gendr, estimator = 'MLM', meanstructure=T)
fitPH1 <- cfa(models,data=PH1gendr, estimator = 'MLM', meanstructure=T)
fitPH2 <- cfa(models,data=PH2gendr, estimator = 'MLM', meanstructure=T)
fitPL1 <- cfa(models,data=PL1gendr, estimator = 'MLM', meanstructure=T)
fitPL2 <- cfa(models,data=PL2gendr, estimator = 'MLM', meanstructure=T)
fitNL1 <- cfa(models,data=NL1gendr, estimator = 'MLM', meanstructure=T)
fitNL2 <- cfa(models,data=NL2gendr, estimator = 'MLM', meanstructure=T)

fitNZ1 <- cfa(models,data=NZ1gendr, estimator = 'MLM', meanstructure=T)
fitNZ2 <- cfa(models,data=NZ2gendr, estimator = 'MLM', meanstructure=T)
fitIE1 <- cfa(models,data=IE1gendr, estimator = 'MLM', meanstructure=T)
fitIE2 <- cfa(models,data=IE2gendr, estimator = 'MLM', meanstructure=T)
fitSG1 <- cfa(models,data=SG1gendr, estimator = 'MLM', meanstructure=T)
fitSG2 <- cfa(models,data=SG2gendr, estimator = 'MLM', meanstructure=T)
fitFR1 <- cfa(models,data=FR1gendr, estimator = 'MLM', meanstructure=T)
fitFR2 <- cfa(models,data=FR2gendr, estimator = 'MLM', meanstructure=T)
fitRO1 <- cfa(models,data=RO1gendr, estimator = 'MLM', meanstructure=T)
fitRO2 <- cfa(models,data=RO2gendr, estimator = 'MLM', meanstructure=T)
fitZA1 <- cfa(models,data=ZA1gendr, estimator = 'MLM', meanstructure=T)
fitZA2 <- cfa(models,data=ZA2gendr, estimator = 'MLM', meanstructure=T)



########################                  2.1                ################################################
########################                                     ################################################
########################               BAYESIAN              ################################################



bmodels <-'P =~ DDP2+DDP1+DDP3+DDP4
N =~ DDN3+DDN2+DDN1+DDN4
M =~ DDM4+DDM2+DDM3+DDM1         
DDN1 ~~ DDN2'

#loadings
bmodels <- c(bmodels,
'DDP1 ~~ prior("dunif(0,3)[sd]") * DDP1
+ DDP2 ~~ prior("dunif(0,3)[sd]") * DDP2
+ DDP3 ~~ prior("dunif(0,3)[sd]") * DDP3
+ DDP4 ~~ prior("dunif(0,3)[sd]") * DDP4
+ DDN1 ~~ prior("dunif(0,3)[sd]") * DDN1
+ DDN2 ~~ prior("dunif(0,3)[sd]") * DDN2
+ DDN3 ~~ prior("dunif(0,3)[sd]") * DDN3
+ DDN4 ~~ prior("dunif(0,3)[sd]") * DDN4
+ DDM1 ~~ prior("dunif(0,3)[sd]") * DDM1
+ DDM2 ~~ prior("dunif(0,3)[sd]") * DDM2
+ DDM3 ~~ prior("dunif(0,3)[sd]") * DDM3
+ DDM4 ~~ prior("dunif(0,3)[sd]") * DDM4')


# factor variances
bmodels <- c(bmodels,
             'P ~~ prior("dunif(0,3)[sd]") * P
             + M ~~ prior("dunif(0,3)[sd]") * M
             + N ~~ prior("dunif(0,3)[sd]") * N')

# intercepts
bmodels <- c(bmodels,
             'DDP1 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDP2 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDP3 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDP4 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDN1 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDN2 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDN3 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDN4 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDM1 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDM2 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDM3 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
+ DDM4 ~ prior("dnorm(2.5,.25)T(1,5)") * 1')



bmodels <-'P =~ DDP2+DDP1+DDP3+DDP4
N =~ DDN3+DDN2+DDN1+DDN4
M =~ DDM4+DDM2+DDM3+DDM1         
DDN1 ~~ DDN2

# loading priors
DDP1 ~~ prior("dunif(0,3)[sd]") * DDP1
DDP2 ~~ prior("dunif(0,3)[sd]") * DDP2
DDP3 ~~ prior("dunif(0,3)[sd]") * DDP3
DDP4 ~~ prior("dunif(0,3)[sd]") * DDP4
DDN1 ~~ prior("dunif(0,3)[sd]") * DDN1
DDN2 ~~ prior("dunif(0,3)[sd]") * DDN2
DDN3 ~~ prior("dunif(0,3)[sd]") * DDN3
DDN4 ~~ prior("dunif(0,3)[sd]") * DDN4
DDM1 ~~ prior("dunif(0,3)[sd]") * DDM1
DDM2 ~~ prior("dunif(0,3)[sd]") * DDM2
DDM3 ~~ prior("dunif(0,3)[sd]") * DDM3
DDM4 ~~ prior("dunif(0,3)[sd]") * DDM4

# factor variances priors
P ~~ prior("dunif(0,3)[sd]") * P
M ~~ prior("dunif(0,3)[sd]") * M
N ~~ prior("dunif(0,3)[sd]") * N

DDP1 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDP2 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDP3 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDP4 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDN1 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDN2 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDN3 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDN4 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDM1 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDM2 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDM3 ~ prior("dnorm(2.5,.25)T(1,5)") * 1
DDM4 ~ prior("dnorm(2.5,.25)T(1,5)") * 1'


bmodels <-'P =~ DDP2+DDP1+DDP3+DDP4
N =~ DDN3+DDN2+DDN1+DDN4
M =~ DDM4+DDM2+DDM3+DDM1         
DDN1 ~~ DDN2

# loading priors
# DDP1 ~~ prior("gamma(4,1)[sd]") * DDP1
# DDP2 ~~ prior("gamma(4,1)[sd]") * DDP2
# DDP3 ~~ prior("gamma(4,1)[sd]") * DDP3
# DDP4 ~~ prior("gamma(4,1)[sd]") * DDP4
# DDN1 ~~ prior("gamma(4,1)[sd]") * DDN1
# DDN2 ~~ prior("gamma(4,1)[sd]") * DDN2
# DDN3 ~~ prior("gamma(4,1)[sd]") * DDN3
# DDN4 ~~ prior("gamma(4,1)[sd]") * DDN4
# DDM1 ~~ prior("gamma(4,1)[sd]") * DDM1
# DDM2 ~~ prior("gamma(4,1)[sd]") * DDM2
# DDM3 ~~ prior("gamma(4,1)[sd]") * DDM3
# DDM4 ~~ prior("gamma(4,1)[sd]") * DDM4

# factor variances priors
# P ~~ prior("normal(0,3)[sd]") * P
# M ~~ prior("normal(0,3)[sd]") * M
# N ~~ prior("normal(0,3)[sd]") * N

DDP1 ~ prior("normal(3,.6)T(1,5)") * 1
DDP2 ~ prior("normal(3,.6)T(1,5)") * 1
DDP3 ~ prior("normal(3,.6)T(1,5)") * 1
DDP4 ~ prior("normal(3,.6)T(1,5)") * 1
DDN1 ~ prior("normal(3,.6)T(1,5)") * 1
DDN2 ~ prior("normal(3,.6)T(1,5)") * 1
DDN3 ~ prior("normal(3,.6)T(1,5)") * 1
DDN4 ~ prior("normal(3,.6)T(1,5)") * 1
DDM1 ~ prior("normal(3,.6)T(1,5)") * 1
DDM2 ~ prior("normal(3,.6)T(1,5)") * 1
DDM3 ~ prior("normal(3,.6)T(1,5)") * 1
DDM4 ~ prior("normal(3,.6)T(1,5)") * 1'

dpriors(target='stan')
remove(bmodels)

######## without the DDN1 ~~ DDN2
bfitUS1a <- bcfa(parTable(fitUS1a),data=US1gendr, convergence = "manual", sample = 4000, burnin = 1000, bcontrol=list(cores=11))
bfitUS2a <- bcfa(parTable(fitUS2a),data=US2gendr, convergence = "manual", sample = 4000, burnin = 1000, bcontrol=list(cores=11))
bfitHR1a <- bcfa(parTable(fitHR1a),data=HR1gendr, convergence = "manual", sample = 5000, burnin = 1000, bcontrol=list(cores=11))
bfitHR2a <- bcfa(parTable(fitHR2a),data=HR2gendr, convergence = "manual", sample = 5000, burnin = 1000, bcontrol=list(cores=11))

########  with DDN1 ~~ DDN2
bfitUS1 <- bcfa(parTable(fitUS1),data=US1gendr, convergence = "manual", sample = 8000, burnin = 1000, bcontrol=list(cores=11))
bfitUS2 <- bcfa(parTable(fitUS1),data=US2gendr, convergence = "manual", sample = 8000, burnin = 1000, bcontrol=list(cores=11))

bbfitHR1 <- bcfa(parTable(fitUS1a),data=HR1gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))

bfitHR1 <- bcfa(parTable(fitHR1),data=HR1gendr, convergence = "manual", sample = 30000, burnin = 1000, bcontrol=list(cores=11))
bfitHR2 <- bcfa(parTable(fitHR2),data=HR2gendr, convergence = "manual", sample = 30000, burnin = 1000, bcontrol=list(cores=11))

# + with specified priors
bfitUS1 <- bcfa(bmodels,data=US1gendr, convergence = "manual", sample = 30000, burnin = 1000, bcontrol=list(cores=11))
bfitUS2 <- bcfa(bmodels,data=US2gendr, convergence = "manual", sample = 30000, burnin = 1000, bcontrol=list(cores=11))
#bfitHR1 <- bcfa(bmodels,data=HR1gendr, convergence = "manual", sample = 30000, burnin = 1000, bcontrol=list(cores=11))
#bfitHR2 <- bcfa(bmodels,data=HR2gendr, convergence = "manual", sample = 30000, burnin = 1000, bcontrol=list(cores=11))

# only run with low sample of 3000 
bfitSG1 <- bcfa(parTable(fitSG1),data=SG1gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitSG2 <- bcfa(parTable(fitSG2),data=SG2gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitDE1 <- bcfa(parTable(fitDE1),data=DE1gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitDE2 <- bcfa(parTable(fitDE2),data=DE2gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitFI1 <- bcfa(parTable(fitFI1),data=FI1gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitFI2 <- bcfa(parTable(fitFI2),data=FI2gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitBR1 <- bcfa(parTable(fitBR1),data=BR1gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitBR2 <- bcfa(parTable(fitBR2),data=BR2gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitSE1 <- bcfa(parTable(fitSE1),data=SE1gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))
bfitSE2 <- bcfa(parTable(fitSE2),data=SE2gendr, convergence = "manual", sample = 3000, burnin = 1000, bcontrol=list(cores=11))



########################                  2.3                ################################################
########################                                     ################################################
########################                 output              ################################################


summary(fitUS1a, standardized=T, fit.measures=T) 
summary(fitUS2a, standardized=T, fit.measures=T) 
summary(fitHR1a, standardized=T, fit.measures=T) 
summary(fitHR2a, standardized=T, fit.measures=T)

summary(fitUS1, standardized=T, fit.measures=T) 
summary(fitUS2, standardized=T, fit.measures=T) 
summary(fitHR1, standardized=T, fit.measures=T) 
summary(fitHR2, standardized=T, fit.measures=T)

summary(fitUS1b, standardized=T, fit.measures=T)  # missingness
summary(fitUS2b, standardized=T, fit.measures=T) 
summary(fitHR1b, standardized=T, fit.measures=T) 
summary(fitHR2b, standardized=T, fit.measures=T)

summary(fitSG1, standardized=T, fit.measures=T) 
summary(fitSG2, standardized=T, fit.measures=T)

summary(fitDE1, standardized=T, fit.measures=T) 
summary(fitDE2, standardized=T, fit.measures=T) 
summary(fitFI1, standardized=T, fit.measures=T) 
summary(fitFI2, standardized=T, fit.measures=T) 
summary(fitBR1, standardized=T, fit.measures=T) 
summary(fitBR2, standardized=T, fit.measures=T) 
summary(fitSE1, standardized=T, fit.measures=T) 
summary(fitSE2, standardized=T, fit.measures=T) 

summary(fitGB1, standardized=T, fit.measures=T) 
summary(fitGB2, standardized=T, fit.measures=T) 
summary(fitCA1, standardized=T, fit.measures=T) 
summary(fitCA2, standardized=T, fit.measures=T) 
summary(fitAU1, standardized=T, fit.measures=T) 
summary(fitAU2, standardized=T, fit.measures=T) 

summary(fitIN1, standardized=T, fit.measures=T) 
summary(fitIN2, standardized=T, fit.measures=T) 
summary(fitPH1, standardized=T, fit.measures=T) 
summary(fitPH2, standardized=T, fit.measures=T) 
summary(fitPL1, standardized=T, fit.measures=T) 
summary(fitPL2, standardized=T, fit.measures=T) 
summary(fitNL1, standardized=T, fit.measures=T) 
summary(fitNL2, standardized=T, fit.measures=T) 
summary(fitNZ1, standardized=T, fit.measures=T) 
summary(fitNZ2, standardized=T, fit.measures=T) 
summary(fitIE1, standardized=T, fit.measures=T) 
summary(fitIE2, standardized=T, fit.measures=T) 
summary(fitSG1, standardized=T, fit.measures=T) 
summary(fitSG2, standardized=T, fit.measures=T) 
summary(fitFR1, standardized=T, fit.measures=T) 
summary(fitFR2, standardized=T, fit.measures=T) 
summary(fitRO1, standardized=T, fit.measures=T) 
summary(fitRO2, standardized=T, fit.measures=T) 
summary(fitZA1, standardized=T, fit.measures=T) 
summary(fitZA2, standardized=T, fit.measures=T) 


# make sure the summary does not run by lavaan, otherwise less information in output
summary(bfitUS1a, standardized=T, fit.measures=T) 
summary(bfitUS2a, standardized=T, fit.measures=T) 
summary(bfitHR1a, standardized=T, fit.measures=T) 
summary(bfitHR2a, standardized=T, fit.measures=T)

summary(bfitUS1, standardized=T, fit.measures=T) 
summary(bfitUS2, standardized=T, fit.measures=T) 
summary(bfitHR1, standardized=T, fit.measures=T) 
summary(bfitHR2, standardized=T, fit.measures=T)

summary(bfitSG1, standardized=T, fit.measures=T)
summary(bfitSG2, standardized=T, fit.measures=T)
summary(bfitDE1, standardized=T, fit.measures=T)
summary(bfitDE2, standardized=T, fit.measures=T)
summary(bfitFI1, standardized=T, fit.measures=T)
summary(bfitFI2, standardized=T, fit.measures=T)
summary(bfitBR1, standardized=T, fit.measures=T)
summary(bfitBR2, standardized=T, fit.measures=T)
summary(bfitSE1, standardized=T, fit.measures=T)
summary(bfitSE2, standardized=T, fit.measures=T)

summary(bfitUS1a, psrf=TRUE, neff=TRUE)
summary(bfitUS2a, psrf=TRUE, neff=TRUE)
summary(bfitHR1a, psrf=TRUE, neff=TRUE)
summary(bfitHR2a, psrf=TRUE, neff=TRUE)

summary(bfitUS1, psrf=TRUE, neff=TRUE)
summary(bfitUS2, psrf=TRUE, neff=TRUE)
summary(bfitHR1, psrf=TRUE, neff=TRUE)
summary(bfitHR2, psrf=TRUE, neff=TRUE)

summary(bfitSG1, psrf=TRUE, neff=TRUE)
summary(bfitSG2, psrf=TRUE, neff=TRUE)
summary(bfitDE1, psrf=TRUE, neff=TRUE)
summary(bfitDE2, psrf=TRUE, neff=TRUE)
summary(bfitFI1, psrf=TRUE, neff=TRUE)
summary(bfitFI2, psrf=TRUE, neff=TRUE)
summary(bfitBR1, psrf=TRUE, neff=TRUE)
summary(bfitBR2, psrf=TRUE, neff=TRUE)
summary(bfitSE1, psrf=TRUE, neff=TRUE)
summary(bfitSE2, psrf=TRUE, neff=TRUE)

fitMeasUS1 <- fitMeasures(bfitUS1, "all")
fitMeasUS1 <- fitMeasures(bfitUS2, "all")
fitMeasures(bfitHR1, "all")
fitMeasures(bfitHR2, "all")

fitMeasures(bfitSG1, "all")
fitMeasures(bfitSG2, "all")
fitMeasures(bfitDE1, "all")
fitMeasures(bfitDE2, "all")
fitMeasures(bfitFI1, "all")
fitMeasures(bfitFI2, "all")
fitMeasures(bfitBR1, "all")
fitMeasures(bfitBR2, "all")
fitMeasures(bfitSE1, "all")
fitMeasures(bfitSE2, "all")


library("rstanarm") 

blavInspect(bfitHR1, "psrf")
blavInspect(bfitHR1, "neff")



########################                  2.3.2              ################################################
########################                                     ################################################
########################             BAYSIAN plots           ################################################



############## US

### US 1

plot(bfitUS1, pars=(1:4|10:14), plot.type="trace")

plot(bfitUS1, pars=1:40, plot.type="trace")
plot(bfitUS2, pars=1:40, plot.type="trace")
plot(bfitUS1, pars=41:80, plot.type="trace")
plot(bfitUS2, pars=41:80, plot.type="trace")

plot(bfitUS1, pars=1:5, plot.type="acf")
plot(bfitUS1, pars=1:5, plot.type="acf_bar")

plot(bfitUS1, pars=1:12, plot.type="dens_chains")
plot(bfitUS1, pars=1:25, plot.type="intervals")
plot(bfitUS1, pars=1:20, plot.type="areas")

plot(bfitUS1, pars=1:20, plot.type="areas_ridges")
plot(bfitUS1, pars=1:20, plot.type="trace_highlight")
plot(bfitUS1, pars=1:2, plot.type="pairs")

plot(bfitUS1, pars=1:5, plot.type="hist_by_chain")
plot(bfitUS1, pars=1:6, plot.type="dens_overlay")


### US 2
plot(bfitUS2, pars=(1:4|10:14), plot.type="trace")

plot(bfitUS2, pars=1:40, plot.type="trace")
plot(bfitUS2, pars=41:80, plot.type="trace")

plot(bfitUS2, pars=1:5, plot.type="acf")
plot(bfitUS2, pars=1:5, plot.type="acf_bar")

plot(bfitUS2, pars=1:12, plot.type="dens_chains")
plot(bfitUS2, pars=1:25, plot.type="intervals")
plot(bfitUS2, pars=1:20, plot.type="areas")

plot(bfitUS2, pars=1:20, plot.type="areas_ridges")
plot(bfitUS2, pars=1:20, plot.type="trace_highlight")
plot(bfitUS2, pars=1:2, plot.type="pairs")

plot(bfitUS2, pars=1:5, plot.type="hist_by_chain")
plot(bfitUS2, pars=1:5, plot.type="dens_overlay")


#does not work (you need more info from the estimation process)
# plot(bfitUS1, pars=1:20, plot.type="rhat_hist")
# plot(bfitUS1, pars=1:2, plot.type="parcoord") ??
# plot(bfitUS1, pars=1:20, plot.type="areas_data")
# plot(bfitUS1, pars=1:12, plot.type="dens_chains_data") fails
# plot(bfitUS1, pars=1:12, plot.type="violin") stom
# plot(bfitUS1, pars=1:40, plot.type="neff")



plot(bfitHR1, pars=1:40, plot.type="trace")
plot(bfitHR1, pars=1:5, plot.type="acf")
plot(bfitHR1, pars=1:25, plot.type="intervals")
plot(bfitHR1, pars=1:20, plot.type="areas")
plot(bfitHR1, pars=1:20, plot.type="areas_ridges")
plot(bfitHR1, pars=1:12, plot.type="dens_overlay")


### HR 1
plot(bfitHR1, pars=1:40, plot.type="trace")
plot(bfitHR1, pars=41:80, plot.type="trace")

plot(bfitHR1, pars=(1:4|10:14), plot.type="trace")

plot(bfitHR1, pars=1:5, plot.type="acf")
plot(bfitHR1, pars=1:5, plot.type="acf_bar")

plot(bfitHR1, pars=1:12, plot.type="dens_chains")
plot(bfitHR1, pars=1:25, plot.type="intervals")
plot(bfitHR1, pars=1:20, plot.type="areas")

plot(bfitHR1, pars=1:20, plot.type="areas_ridges")
plot(bfitHR1, pars=1:20, plot.type="trace_highlight")  # this takes long
plot(bfitHR1, pars=1:2, plot.type="pairs")   

plot(bfitHR1, pars=1:5, plot.type="hist_by_chain")
plot(bfitHR1, pars=1:12, plot.type="dens_overlay")



### HR 2
plot(bfitHR2, pars=1:40, plot.type="trace")
plot(bfitHR2, pars=41:80, plot.type="trace")

plot(bfitHR2, pars=(1:4|10:14), plot.type="trace")

plot(bfitHR2, pars=1:5, plot.type="acf")
plot(bfitHR2, pars=1:5, plot.type="acf_bar")

plot(bfitHR2, pars=1:12, plot.type="dens_chains")
plot(bfitHR2, pars=1:25, plot.type="intervals")
plot(bfitHR2, pars=1:20, plot.type="areas")

plot(bfitHR2, pars=1:20, plot.type="areas_ridges")
plot(bfitHR2, pars=1:20, plot.type="trace_highlight")
plot(bfitHR2, pars=1:2, plot.type="pairs")

plot(bfitHR2, pars=1:5, plot.type="hist_by_chain")
plot(bfitHR2, pars=1:5, plot.type="dens_overlay")

coef(fit)

#look at it
?bayesplot


########################                  2.3.3              ################################################
########################                                     ################################################
########################               BAYSIAN fit           ################################################



null.modl <- c(paste0("DDN", 1:4, " ~~ DDN", 1:4),paste0("DDP", 1:4, " ~~ DDP", 1:4),
               paste0("DDM", 1:4, " ~~ DDM", 1:4),paste0("DDN", 1:4, " ~ 1"), 
               paste0("DDP", 1:4, " ~ 1"), paste0("DDM", 1:4, " ~ 1"))

null.modl

# same sample size as your initial bfit sample size! Otherwise you wont get BCFI, BTLI, BNFI !!!
fit0US1a <- bcfa(null.modl, data = US1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0US2a <- bcfa(null.modl, data = US2gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0HR1a <- bcfa(null.modl, data = HR1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0HR2a <- bcfa(null.modl, data = HR2gendr, cp = "fa", sample = 3000, burnin = 1000)

fit0US1 <- bcfa(null.modl, data = US1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0US2 <- bcfa(null.modl, data = US2gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0HR1 <- bcfa(null.modl, data = HR1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0HR2 <- bcfa(null.modl, data = HR2gendr, cp = "fa", sample = 3000, burnin = 1000)

fit0CA1 <- bcfa(null.modl, data = CA1gendr, cp = "fa")

fit0SG1 <- bcfa(null.modl, data = SG1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0SG2 <- bcfa(null.modl, data = SG2gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0DE1 <- bcfa(null.modl, data = DE1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0DE2 <- bcfa(null.modl, data = DE2gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0FI1 <- bcfa(null.modl, data = FI1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0FI2 <- bcfa(null.modl, data = FI2gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0BR1 <- bcfa(null.modl, data = BR1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0BR2 <- bcfa(null.modl, data = BR2gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0SE1 <- bcfa(null.modl, data = SE1gendr, cp = "fa", sample = 3000, burnin = 1000)
fit0SE2 <- bcfa(null.modl, data = SE2gendr, cp = "fa", sample = 3000, burnin = 1000)


MLUS1a <- blavFitIndices(bfitUS1a, baseline.model = fit0US1a)
MLUS2a <- blavFitIndices(bfitUS2a, baseline.model = fit0US2a)
MLHR1a <- blavFitIndices(bfitHR1a, baseline.model = fit0HR1a)
MLHR2a <- blavFitIndices(bfitHR2a, baseline.model = fit0HR2a)

MLUS1 <- blavFitIndices(bfitUS1, baseline.model = fit0US1)
MLUS2 <- blavFitIndices(bfitUS2, baseline.model = fit0US2)
MLHR1 <- blavFitIndices(bfitHR1, baseline.model = fit0HR1)
MLHR2 <- blavFitIndices(bfitHR2, baseline.model = fit0HR2)

MLCA1 <- blavFitIndices(bfitCA1, baseline.model = fit0CA1)

MLSG1 <- blavFitIndices(bfitSG1, baseline.model = fit0SG1)
MLSG2 <- blavFitIndices(bfitSG2, baseline.model = fit0SG2)
MLDE1 <- blavFitIndices(bfitDE1, baseline.model = fit0DE1)
MLDE2 <- blavFitIndices(bfitDE2, baseline.model = fit0DE2)
MLFI1 <- blavFitIndices(bfitFI1, baseline.model = fit0FI1)
MLFI2 <- blavFitIndices(bfitFI2, baseline.model = fit0FI2)
MLBR1 <- blavFitIndices(bfitBR1, baseline.model = fit0BR1)
MLBR2 <- blavFitIndices(bfitBR2, baseline.model = fit0BR2)
MLSE1 <- blavFitIndices(bfitSE1, baseline.model = fit0SE1)
MLSE2 <- blavFitIndices(bfitSE2, baseline.model = fit0SE2)

MLUS1a
MLUS2a
MLHR1a
MLHR2a

MLUS1
MLUS2
MLHR1
MLHR2

MLCA1

MLSG1
MLSG2
MLDE1
MLDE2
MLFI1
MLFI2
MLBR1
MLBR2
MLSE1
MLSE2

summary(MLUS1a)
summary(MLUS2a)
summary(MLHR1a)
summary(MLHR2a)

summary(MLUS1)
summary(MLUS2)
summary(MLHR1)
summary(MLHR2)

summary(MLSG1)
summary(MLSG2)
summary(MLDE1)
summary(MLDE2)
summary(MLFI1)
summary(MLFI2)
summary(MLBR1)
summary(MLBR2)
summary(MLSE1)
summary(MLSE2)


# PPMC <- blavFitIndices(fit1, baseline.model = fit0, pD = "waic", rescale = "PPMC")
# ppmc(bfitHR1, thin = 1, fit.measures = c("srmr","chisq"), discFUN = NULL)


mi<-inspect(fit,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:15,]



########################                  2.3.4              ################################################
########################                                     ################################################
########################         Maximum likelihood fit      ################################################



semPaths(fitUS1,"model","stand",style="LISREL",rotation=1, edge.color="black",sizeLat=5,layout = 'tree', edge.label.cex=0.9,mar=c(4,1,4,1),sizeMan = 6)
semPaths(fitUS2,"model","stand",style="LISREL",rotation=1, edge.color="black",sizeLat=5,layout = 'tree', edge.label.cex=0.9,mar=c(4,1,4,1),sizeMan = 6)
semPaths(fitHR1,"model","stand",style="LISREL",rotation=1, edge.color="black",sizeLat=5,layout = 'tree', edge.label.cex=0.9,mar=c(4,1,4,1),sizeMan = 6)
semPaths(fitHR2,"model","stand",style="LISREL",rotation=1, edge.color="black",sizeLat=5,layout = 'tree', edge.label.cex=0.9,mar=c(4,1,4,1),sizeMan = 6)

semPaths(fitUS1a,"model","stand",style="LISREL",rotation=1, edge.color="black",sizeLat=5,layout = 'tree', edge.label.cex=0.9,mar=c(4,1,4,1),sizeMan = 4)
semPaths(fitUS2a,"model","stand",style="LISREL",rotation=1, edge.color="black",sizeLat=5,layout = 'tree', edge.label.cex=0.9,mar=c(4,1,4,1),sizeMan = 4)
semPaths(fitHR1a,"model","stand",style="LISREL",rotation=1, edge.color="black",sizeLat=5,layout = 'tree', edge.label.cex=0.9,mar=c(4,1,4,1),sizeMan = 4)
semPaths(fitHR2a,"model","stand",style="LISREL",rotation=1, edge.color="black",sizeLat=5,layout = 'tree', edge.label.cex=0.9,mar=c(4,1,4,1),sizeMan = 4)



anova(fit,fit1)
fitMeasures(fit, 'cfi')
fitMeasures(fit, selected)
fitMeasures(fit1, selected)
# if difference > 0.01 then its not good
fitMeasures(fit, 'cfi') - fitMeasures(fit1, 'cfi') # its 0 so thats good
lavTestScore(fit1)

selected <- c("chisq", "df", "rmsea", "cfi", "srmr","BIC","AIC")
anova(fit1,fit1.1)
fitMeasures(fit1, 'cfi')
fitMeasures(fit1, selected)
fitMeasures(fit1.1, selected)
# if difference > 0.01 then its not good
fitMeasures(fit1, 'cfi') - fitMeasures(fit1.1, 'cfi') # its 0 so thats good
lavTestScore(fit1.1)

selected <- c("chisq", "df", "cfi","tli", "rmsea","srmr","bic","aic")
anova(fit1.1,fit1.2)
fitMeasures(fit1.1, selected)
fitMeasures(fit1.2, selected)
fitMeasures(fit1.3, selected)
# if difference > 0.01 then its not good
fitMeasures(fit1, 'cfi') - fitMeasures(fit1.1, 'cfi') # its 0 so thats good
lavTestScore(fit1.1)





#############################################################################################
############################             3                ###################################
############################                              ###################################
############################        Equivalence           ###################################
############################                              ###################################
#############################################################################################




########################                  3.1                ################################################
########################                                     ################################################
########################          maximum likelihood         ################################################



HRgendr <- filter(vuil, gender =="1"|gender =="2", country =="HR")# no difference mean structure   n=282 x
USgendr <- filter(vuil, gender =="1"|gender =="2", country =="US")# difference mean structure

PHgendr <- filter(vuil, gender =="1"|gender =="2", country =="PH")#///
ZAgendr <- filter(vuil, gender =="1"|gender =="2", country =="ZA")#///
ROgendr <- filter(vuil, gender =="1"|gender =="2", country =="RO")#///
PLgendr <- filter(vuil, gender =="1"|gender =="2", country =="PL")#///
IEgendr <- filter(vuil, gender =="1"|gender =="2", country =="IE")#///

DEgendr <- filter(vuil, gender =="1"|gender =="2", country =="DE")# no difference mean structure   n=831
BRgendr <- filter(vuil, gender =="1"|gender =="2", country =="BR")# no difference mean structure   n=517
SEgendr <- filter(vuil, gender =="1"|gender =="2", country =="SE")# no difference mean structure   n=630

GBgendr <- filter(vuil, gender =="1"|gender =="2", country =="GB")
CAgendr <- filter(vuil, gender =="1"|gender =="2", country =="CA")# difference mean structure
AUgendr <- filter(vuil, gender =="1"|gender =="2", country =="AU")# difference mean structure
INgendr <- filter(vuil, gender =="1"|gender =="2", country =="IN")# difference mean structure
SGgendr <- filter(vuil, gender =="1"|gender =="2", country =="SG")# difference mean structure
NZgendr <- filter(vuil, gender =="1"|gender =="2", country =="NZ")# difference mean structure

#INgendr <- filter(vuil, gender =="1"|gender =="2", country =="MX")# difference mean structure
#INgendr <- filter(vuil, gender =="1"|gender =="2", country =="FR")# difference mean structure - just noticable difference JND
#INgendr <- filter(vuil, gender =="1"|gender =="2", country =="GR")# difference mean structure
#INgendr <- filter(vuil, gender =="1"|gender =="2", country =="BE")# difference mean structure
#INgendr <- filter(vuil, gender =="1"|gender =="2", country =="NL")# difference mean structure
#FIgendr <- filter(vuil, gender =="1"|gender =="2", country =="FI")# no difference mean structure   n=383
#NOgendr <- filter(vuil, gender =="1"|gender =="2", country =="NO")# no difference mean structure   n=317
#MYgendr <- filter(vuil, gender =="1"|gender =="2", country =="MY")# no difference mean structure   n=159
# INgendr <- filter(vuil, gender =="1"|gender =="2", country =="DK")#///

summary(vuil$country)




models <-'one =~ DDP1+DDP2+DDP3+DDP4
avoidence =~ DDN1+DDN2+DDN3+DDN4
two =~ DDM1+DDM2+DDM3+DDM4          
DDN1 ~~ DDN2
#all =~ one+two+avoidence
'
# general one
# fit <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T)
# fit1 <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
# fit1.1 <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
# fit1.2 <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
# fit1.3 <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
# fit1.4 <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
# fit1.5 <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
# fit1.6 <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))
# 
# fitMeasures(fit, selected)
# fitMeasures(fit1, selected)
# fitMeasures(fit1.1, selected)
# fitMeasures(fit1.2, selected)
# fitMeasures(fit1.3, selected)
# fitMeasures(fit1.4, selected)
# fitMeasures(fit1.5, selected)
# fitMeasures(fit1.6, selected)
# summary(fit, standardized=T, fit.measures=T, rsquare=T) 
# summary(fit1, standardized=T, fit.measures=T, rsquare=T) 
# summary(fit1.1, standardized=T, fit.measures=T, rsquare=T) 
# summary(fit1.2, standardized=T, fit.measures=T, rsquare=T) 
# summary(fit1.3, standardized=T, fit.measures=T, rsquare=T) 
# summary(fit1.4, standardized=T, fit.measures=T, rsquare=T)
# summary(fit1.5, standardized=T, fit.measures=T, rsquare=T) 
# summary(fit1.6, standardized=T, fit.measures=T, rsquare=T) 

# difference HR and US?    
HRUSgendr <- filter(vuil, gender =="1"|gender =="2", country =="HR"|country =="US"|country =="DE"|country =="SE"|country =="BR"|country =="AU"|country =="CA"|country =="GB"|country =="SG"|country =="IN")
fit <- cfa(models,data=HRUSgendr, estimator = 'MLM', group = 'country', meanstructure=T)
fit1 <- cfa(models,data=HRUSgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings"))
fit1.1 <- cfa(models,data=HRUSgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2 <- cfa(models,data=HRUSgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3 <- cfa(models,data=HRUSgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4 <- cfa(models,data=HRUSgendr, estimator = 'MLM', group = 'country',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5 <- cfa(models,data=HRUSgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6 <- cfa(models,data=HRUSgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitMeasures(fit, selected)
fitMeasures(fit1, selected)
fitMeasures(fit1.1, selected)
fitMeasures(fit1.2, selected)
fitMeasures(fit1.3, selected)
fitMeasures(fit1.4, selected)
fitMeasures(fit1.5, selected)
fitMeasures(fit1.6, selected)


#USA
fitUS <- cfa(models,data=USgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1US <- cfa(models,data=USgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1US <- cfa(models,data=USgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2US <- cfa(models,data=USgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3US <- cfa(models,data=USgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4US <- cfa(models,data=USgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5US <- cfa(models,data=USgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
save.image(file = 'SEMwork.RData')
fit1.6US <- cfa(models,data=USgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# HR
fitHR <- cfa(models,data=HRgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1HR <- cfa(models,data=HRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1HR <- cfa(models,data=HRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2HR <- cfa(models,data=HRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3HR <- cfa(models,data=HRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4HR <- cfa(models,data=HRgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5HR <- cfa(models,data=HRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6HR <- cfa(models,data=HRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# MY
fitMY <- cfa(models,data=MYgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1MY <- cfa(models,data=MYgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1MY <- cfa(models,data=MYgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2MY <- cfa(models,data=MYgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3MY <- cfa(models,data=MYgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4MY <- cfa(models,data=MYgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5MY <- cfa(models,data=MYgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6MY <- cfa(models,data=MYgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# DE
fitDE <- cfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1DE <- cfa(models,data=DEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1DE <- cfa(models,data=DEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2DE <- cfa(models,data=DEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3DE <- cfa(models,data=DEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4DE <- cfa(models,data=DEgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5DE <- cfa(models,data=DEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6DE <- cfa(models,data=DEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# FI
fitFI <- cfa(models,data=FIgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1FI <- cfa(models,data=FIgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1FI <- cfa(models,data=FIgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2FI <- cfa(models,data=FIgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3FI <- cfa(models,data=FIgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4FI <- cfa(models,data=FIgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5FI <- cfa(models,data=FIgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6FI <- cfa(models,data=FIgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# BR
fitBR <- cfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1BR <- cfa(models,data=BRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1BR <- cfa(models,data=BRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2BR <- cfa(models,data=BRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3BR <- cfa(models,data=BRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4BR <- cfa(models,data=BRgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5BR <- cfa(models,data=BRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6BR <- cfa(models,data=BRgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# SE
fitSE <- cfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1SE <- cfa(models,data=SEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1SE <- cfa(models,data=SEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2SE <- cfa(models,data=SEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3SE <- cfa(models,data=SEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4SE <- cfa(models,data=SEgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5SE <- cfa(models,data=SEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6SE <- cfa(models,data=SEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitPH <- cfa(models,data=PHgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1PH <- cfa(models,data=PHgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1PH <- cfa(models,data=PHgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2PH <- cfa(models,data=PHgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3PH <- cfa(models,data=PHgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4PH <- cfa(models,data=PHgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5PH <- cfa(models,data=PHgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6PH <- cfa(models,data=PHgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitZA <- cfa(models,data=ZAgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1ZA <- cfa(models,data=ZAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1ZA <- cfa(models,data=ZAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2ZA <- cfa(models,data=ZAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3ZA <- cfa(models,data=ZAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4ZA <- cfa(models,data=ZAgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5ZA <- cfa(models,data=ZAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6ZA <- cfa(models,data=ZAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitRO <- cfa(models,data=ROgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1RO <- cfa(models,data=ROgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1RO <- cfa(models,data=ROgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2RO <- cfa(models,data=ROgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3RO <- cfa(models,data=ROgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4RO <- cfa(models,data=ROgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5RO <- cfa(models,data=ROgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6RO <- cfa(models,data=ROgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitPL <- cfa(models,data=PLgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1PL <- cfa(models,data=PLgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1PL <- cfa(models,data=PLgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2PL <- cfa(models,data=PLgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3PL <- cfa(models,data=PLgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4PL <- cfa(models,data=PLgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5PL <- cfa(models,data=PLgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6PL <- cfa(models,data=PLgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitIE <- cfa(models,data=IEgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1IE <- cfa(models,data=IEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1IE <- cfa(models,data=IEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2IE <- cfa(models,data=IEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3IE <- cfa(models,data=IEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4IE <- cfa(models,data=IEgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5IE <- cfa(models,data=IEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6IE <- cfa(models,data=IEgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitGB <- cfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1GB <- cfa(models,data=GBgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1GB <- cfa(models,data=GBgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2GB <- cfa(models,data=GBgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3GB <- cfa(models,data=GBgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4GB <- cfa(models,data=GBgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5GB <- cfa(models,data=GBgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6GB <- cfa(models,data=GBgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitCA <- cfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1CA <- cfa(models,data=CAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1CA <- cfa(models,data=CAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2CA <- cfa(models,data=CAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3CA <- cfa(models,data=CAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4CA <- cfa(models,data=CAgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5CA <- cfa(models,data=CAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6CA <- cfa(models,data=CAgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitAU <- cfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1AU <- cfa(models,data=AUgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1AU <- cfa(models,data=AUgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2AU <- cfa(models,data=AUgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3AU <- cfa(models,data=AUgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4AU <- cfa(models,data=AUgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5AU <- cfa(models,data=AUgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6AU <- cfa(models,data=AUgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitIN <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1IN <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1IN <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2IN <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3IN <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4IN <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5IN <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6IN <- cfa(models,data=INgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitSG <- cfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1SG <- cfa(models,data=SGgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1SG <- cfa(models,data=SGgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2SG <- cfa(models,data=SGgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3SG <- cfa(models,data=SGgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4SG <- cfa(models,data=SGgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5SG <- cfa(models,data=SGgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6SG <- cfa(models,data=SGgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitNZ <- cfa(models,data=NZgendr, estimator = 'MLM', group = 'gender', meanstructure=T)
fit1NZ <- cfa(models,data=NZgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings"))
fit1.1NZ <- cfa(models,data=NZgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2NZ <- cfa(models,data=NZgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3NZ <- cfa(models,data=NZgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4NZ <- cfa(models,data=NZgendr, estimator = 'MLM', group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5NZ <- cfa(models,data=NZgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6NZ <- cfa(models,data=NZgendr, estimator = 'MLM', group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))


summary(fitUS, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1US, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.1US, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.2US, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.3US, standardized=T, fit.measures=T, rsquare=T) #c("loadings","intercepts", "residuals","residual.covariances"))
summary(fit1.4US, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.5US, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.6US, standardized=T, fit.measures=T, rsquare=T)

summary(fitHR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1HR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.1HR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.2HR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.3HR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.4HR, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.5HR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.6HR, standardized=T, fit.measures=T, rsquare=T)

summary(fitMY, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1MY, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.1MY, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.2MY, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.3MY, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.4MY, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.5MY, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.6MY, standardized=T, fit.measures=T, rsquare=T)

summary(fitFI, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1FI, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.1FI, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.2FI, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.3FI, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.4FI, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.5FI, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.6FI, standardized=T, fit.measures=T, rsquare=T)

summary(fitDE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1DE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.1DE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.2DE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.3DE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.4DE, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.5DE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.6DE, standardized=T, fit.measures=T, rsquare=T)

summary(fitBR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1BR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.1BR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.2BR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.3BR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.4BR, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.5BR, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.6BR, standardized=T, fit.measures=T, rsquare=T)

summary(fitSE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1SE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.1SE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.2SE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.3SE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.4SE, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.5SE, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.6SE, standardized=T, fit.measures=T, rsquare=T)

selected <- c("chisq", "df", "rmsea", "cfi", "srmr","BIC","AIC")

fitMeasures(fitHR, selected)
fitMeasures(fit1HR, selected)
fitMeasures(fit1.1HR, selected)
fitMeasures(fit1.2HR, selected)
fitMeasures(fit1.3HR, selected)
fitMeasures(fit1.4HR, selected)
fitMeasures(fit1.5HR, selected)
fitMeasures(fit1.6HR, selected)

fitMeasures(fitUS, selected)
fitMeasures(fit1US, selected)
fitMeasures(fit1.1US, selected)
fitMeasures(fit1.2US, selected)
fitMeasures(fit1.3US, selected)
fitMeasures(fit1.4US, selected)
fitMeasures(fit1.5US, selected)
fitMeasures(fit1.6US, selected)

?fitMeasures
fitMeasures(fitDE, "all")
fitMeasures(fit1DE, selected)
fitMeasures(fit1.1DE, selected)
fitMeasures(fit1.2DE, selected)
fitMeasures(fit1.3DE, selected)
fitMeasures(fit1.4DE, selected)
fitMeasures(fit1.5DE, selected)
fitMeasures(fit1.6DE, selected)

fitMeasures(fitBR, selected)
fitMeasures(fit1BR, selected)
fitMeasures(fit1.1BR, selected)
fitMeasures(fit1.2BR, selected)
fitMeasures(fit1.3BR, selected)
fitMeasures(fit1.4BR, selected)
fitMeasures(fit1.5BR, selected)
fitMeasures(fit1.6BR, selected)

fitMeasures(fitSE, selected)
fitMeasures(fit1SE, selected)
fitMeasures(fit1.1SE, selected)
fitMeasures(fit1.2SE, selected)
fitMeasures(fit1.3SE, selected)
fitMeasures(fit1.4SE, selected)
fitMeasures(fit1.5SE, selected)
fitMeasures(fit1.6SE, selected)

# also do anova test for DE, BR and SE 



fitMeasures(fitPH, selected)
fitMeasures(fit1PH, selected)
fitMeasures(fit1.1PH, selected) # CFI drops > 0.1
fitMeasures(fit1.2PH, selected)
fitMeasures(fit1.3PH, selected)
fitMeasures(fit1.4PH, selected)
fitMeasures(fit1.5PH, selected)
fitMeasures(fit1.6PH, selected) # difference

fitMeasures(fitZA, selected)
fitMeasures(fit1ZA, selected)
fitMeasures(fit1.1ZA, selected) # CFI drops > 0.1
fitMeasures(fit1.2ZA, selected)
fitMeasures(fit1.3ZA, selected)
fitMeasures(fit1.4ZA, selected)
fitMeasures(fit1.5ZA, selected)
fitMeasures(fit1.6ZA, selected) # difference

fitMeasures(fitRO, selected)
fitMeasures(fit1RO, selected)
fitMeasures(fit1.1RO, selected)
fitMeasures(fit1.2RO, selected) # CFI drops > 0.1
fitMeasures(fit1.3RO, selected)
fitMeasures(fit1.4RO, selected)
fitMeasures(fit1.5RO, selected)
fitMeasures(fit1.6RO, selected) # difference

fitMeasures(fitPL, selected)
fitMeasures(fit1PL, selected)
fitMeasures(fit1.1PL, selected) # CFI drops > 0.1
fitMeasures(fit1.2PL, selected)
fitMeasures(fit1.3PL, selected)
fitMeasures(fit1.4PL, selected)
fitMeasures(fit1.5PL, selected)
fitMeasures(fit1.6PL, selected) # no difference

fitMeasures(fitIE, selected)
fitMeasures(fit1IE, selected)
fitMeasures(fit1.1IE, selected)
fitMeasures(fit1.2IE, selected) # CFI drops > 0.1
fitMeasures(fit1.3IE, selected)
fitMeasures(fit1.4IE, selected)
fitMeasures(fit1.5IE, selected)
fitMeasures(fit1.6IE, selected) # difference

fitMeasures(fitNZ, selected)
fitMeasures(fit1NZ, selected)
fitMeasures(fit1.1NZ, selected) 
fitMeasures(fit1.2NZ, selected) # CFI drops > 0.1
fitMeasures(fit1.3NZ, selected)
fitMeasures(fit1.4NZ, selected)
fitMeasures(fit1.5NZ, selected)
fitMeasures(fit1.6NZ, selected) # difference

fitMeasures(fitGB, selected)
fitMeasures(fit1GB, selected)
fitMeasures(fit1.1GB, selected)
fitMeasures(fit1.2GB, selected)
fitMeasures(fit1.3GB, selected)
fitMeasures(fit1.4GB, selected)
fitMeasures(fit1.5GB, selected)
fitMeasures(fit1.6GB, selected)

fitMeasures(fitCA, selected)
fitMeasures(fit1CA, selected)
fitMeasures(fit1.1CA, selected)
fitMeasures(fit1.2CA, selected)
fitMeasures(fit1.3CA, selected)
fitMeasures(fit1.4CA, selected)
fitMeasures(fit1.5CA, selected)
fitMeasures(fit1.6CA, selected)

fitMeasures(fitAU, selected)
fitMeasures(fit1AU, selected)
fitMeasures(fit1.1AU, selected)
fitMeasures(fit1.2AU, selected)
fitMeasures(fit1.3AU, selected)
fitMeasures(fit1.4AU, selected)
fitMeasures(fit1.5AU, selected)
fitMeasures(fit1.6AU, selected)

fitMeasures(fitIN, selected)
fitMeasures(fit1IN, selected)
fitMeasures(fit1.1IN, selected)
fitMeasures(fit1.2IN, selected)
fitMeasures(fit1.3IN, selected)
fitMeasures(fit1.4IN, selected)
fitMeasures(fit1.5IN, selected)
fitMeasures(fit1.6IN, selected)

fitMeasures(fitSG, selected)
fitMeasures(fit1SG, selected)
fitMeasures(fit1.1SG, selected)
fitMeasures(fit1.2SG, selected)
fitMeasures(fit1.3SG, selected)
fitMeasures(fit1.4SG, selected)
fitMeasures(fit1.5SG, selected)
fitMeasures(fit1.6SG, selected)



########################                  3.2                ################################################
########################                                     ################################################
########################               BAYESIAN              ################################################



########################                  3.2.1              ################################################
########################                                     ################################################
########################               Estimation            ################################################

#USA
bfitUS <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', bcontrol=list(cores=11))
bfit1US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', bcontrol=list(cores=11),group.equal=c("loadings"))
bfit1.1US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', bcontrol=list(cores=11), group.equal=c("loadings","intercepts"))
bfit1.2US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals"))
bfit1.3US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

bfitGB <- bcfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T)
bfit1GB <- bcfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings"))
bfit1.1GB <- bcfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts"))
bfit1.2GB <- bcfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
bfit1.3GB <- bcfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4GB <- bcfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5GB <- bcfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6GB <- bcfa(models,data=GBgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

bfitCA <- bcfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T)
bfit1CA <- bcfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings"))
bfit1.1CA <- bcfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts"))
bfit1.2CA <- bcfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
bfit1.3CA <- bcfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4CA <- bcfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5CA <- bcfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6CA <- bcfa(models,data=CAgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

bfitAU <- bcfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11),  meanstructure=T)
bfit1AU <- bcfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings"))
bfit1.1AU <- bcfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts"))
bfit1.2AU <- bcfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
bfit1.3AU <- bcfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4AU <- bcfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5AU <- bcfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6AU <- bcfa(models,data=AUgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11), meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))


# HR
bfitHR <- bcfa(models,data=HRgendr, estimator = 'MLM', sample = 12000, burnin = 1000, group = 'gender', bcontrol=list(cores=11))
bfit1HR <- bcfa(models,data=HRgendr, estimator = 'MLM', sample = 12000, burnin = 1000, group = 'gender',bcontrol=list(cores=11), group.equal=c("loadings"))
bfit1.1HR <- bcfa(models,data=HRgendr, estimator = 'MLM', sample = 12000, burnin = 1000, group = 'gender',bcontrol=list(cores=11), group.equal=c("loadings","intercepts"))
bfit1.2HR <- bcfa(models,data=HRgendr, estimator = 'MLM', sample = 12000, burnin = 1000, group = 'gender',bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals"))
bfit1.3HR <- bcfa(models,data=HRgendr, estimator = 'MLM', sample = 12000, burnin = 1000, group = 'gender',bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4HR <- bcfa(models,data=HRgendr, estimator = 'MLM', sample = 12000, burnin = 1000, group = 'gender',bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5HR <- bcfa(models,data=HRgendr, estimator = 'MLM', sample = 12000, burnin = 1000, group = 'gender',bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6HR <- bcfa(models,data=HRgendr, estimator = 'MLM', sample = 12000, burnin = 1000, group = 'gender',bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# SG
bfitSG <- bcfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11))
bfit1SG <- bcfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings"))
bfit1.1SG <- bcfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts"))
bfit1.2SG <- bcfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals"))
bfit1.3SG <- bcfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4SG <- bcfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5SG <- bcfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6SG <- bcfa(models,data=SGgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# DE
bfitDE <- bcfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11))
bfit1DE <- bcfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings"))
bfit1.1DE <- bcfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts"))
bfit1.2DE <- bcfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals"))
bfit1.3DE <- bcfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4DE <- bcfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5DE <- bcfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6DE <- bcfa(models,data=DEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# IN
bfitIN <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000, bcontrol=list(cores=11))
bfit1IN <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings"))
bfit1.1IN <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts"))
bfit1.2IN <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals"))
bfit1.3IN <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4IN <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5IN <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6IN <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', sample = 12000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# BR
bfitBR <- bcfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', sample = 10000, burnin = 1000, bcontrol=list(cores=11))
bfit1BR <- bcfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', sample = 10000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings"))
bfit1.1BR <- bcfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', sample = 10000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts"))
bfit1.2BR <- bcfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', sample = 10000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals"))
bfit1.3BR <- bcfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', sample = 10000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4BR <- bcfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', sample = 10000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5BR <- bcfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', sample = 10000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6BR <- bcfa(models,data=BRgendr, estimator = 'MLM', group = 'gender', sample = 10000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# SE
bfitSE <- bcfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000, bcontrol=list(cores=11))
bfit1SE <- bcfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings"))
bfit1.1SE <- bcfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts"))
bfit1.2SE <- bcfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals"))
bfit1.3SE <- bcfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4SE <- bcfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5SE <- bcfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6SE <- bcfa(models,data=SEgendr, estimator = 'MLM', group = 'gender', sample = 8000, burnin = 1000,bcontrol=list(cores=11), group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitMeasures(bfitUS, selected)
fitMeasures(bfit1US, selected)
fitMeasures(bfit1.1US, selected)
fitMeasures(bfit1.2US, selected)
fitMeasures(bfit1.3US, selected)
fitMeasures(bfit1.4US, selected)
fitMeasures(bfit1.5US, selected)
fitMeasures(bfit1.6US, selected)

fitMeasures(bfitHR, selected)
fitMeasures(bfit1HR, selected)
fitMeasures(bfit1.1HR, selected)
fitMeasures(bfit1.2HR, selected)
fitMeasures(bfit1.3HR, selected)
fitMeasures(bfit1.4HR, selected)
fitMeasures(bfit1.5HR, selected)
fitMeasures(bfit1.6HR, selected)


summary(bfit, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.1, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.2, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.3, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.4, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.5, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.6, standardized=T, fit.measures=T, rsquare=T)

summary(bfitUS, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1US, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.1US, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.2US, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.3US, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.4US, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.5US, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.6US, standardized=T, fit.measures=T, rsquare=T)

summary(bfitHR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1HR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.1HR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.2HR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.3HR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.4HR, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.5HR, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.6HR, standardized=T, fit.measures=T, rsquare=T)

summary(bfitSG, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1SG, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.1SG, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.2SG, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.3SG, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.4SG, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.5SG, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.6SG, standardized=T, fit.measures=T, rsquare=T)

summary(bfitDE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1DE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.1DE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.2DE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.3DE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.4DE, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.5DE, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.6DE, standardized=T, fit.measures=T, rsquare=T)

summary(bfitIN, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1IN, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.1IN, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.2IN, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.3IN, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.4IN, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.5IN, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.6IN, standardized=T, fit.measures=T, rsquare=T)

summary(bfitBR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1BR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.1BR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.2BR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.3BR, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.4BR, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.5BR, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.6BR, standardized=T, fit.measures=T, rsquare=T)

summary(bfitSE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1SE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.1SE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.2SE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.3SE, standardized=T, fit.measures=T, rsquare=T) 
summary(bfit1.4SE, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.5SE, standardized=T, fit.measures=T, rsquare=T)
summary(bfit1.6SE, standardized=T, fit.measures=T, rsquare=T)


summary(bfit1US, psrf=TRUE, neff=TRUE)
summary(bfit1.1US, psrf=TRUE, neff=TRUE)
summary(bfit1.2US, psrf=TRUE, neff=TRUE)
summary(bfit1.3US, psrf=TRUE, neff=TRUE)
summary(bfit1.4US, psrf=TRUE, neff=TRUE)
summary(bfit1.5US, psrf=TRUE, neff=TRUE)
summary(bfit1.6US, psrf=TRUE, neff=TRUE)

summary(bfit1HR, psrf=TRUE, neff=TRUE)
summary(bfit1.1HR, psrf=TRUE, neff=TRUE)
summary(bfit1.2HR, psrf=TRUE, neff=TRUE)
summary(bfit1.3HR, psrf=TRUE, neff=TRUE)
summary(bfit1.4HR, psrf=TRUE, neff=TRUE)
summary(bfit1.5HR, psrf=TRUE, neff=TRUE)
summary(bfit1.6HR, psrf=TRUE, neff=TRUE)

summary(bfit1AU, psrf=TRUE, neff=TRUE)
summary(bfit1.1AU, psrf=TRUE, neff=TRUE)
summary(bfit1.2AU, psrf=TRUE, neff=TRUE)
summary(bfit1.3AU, psrf=TRUE, neff=TRUE)
summary(bfit1.4AU, psrf=TRUE, neff=TRUE)
summary(bfit1.5AU, psrf=TRUE, neff=TRUE)
summary(bfit1.6AU, psrf=TRUE, neff=TRUE)

summary(bfit1GB, psrf=TRUE, neff=TRUE)
summary(bfit1.1GB, psrf=TRUE, neff=TRUE)
summary(bfit1.2GB, psrf=TRUE, neff=TRUE)
summary(bfit1.3GB, psrf=TRUE, neff=TRUE)
summary(bfit1.4GB, psrf=TRUE, neff=TRUE)
summary(bfit1.5GB, psrf=TRUE, neff=TRUE)
summary(bfit1.6GB, psrf=TRUE, neff=TRUE)

summary(bfit1CA, psrf=TRUE, neff=TRUE)
summary(bfit1.1CA, psrf=TRUE, neff=TRUE)
summary(bfit1.2CA, psrf=TRUE, neff=TRUE)
summary(bfit1.3CA, psrf=TRUE, neff=TRUE)
summary(bfit1.4CA, psrf=TRUE, neff=TRUE)
summary(bfit1.5CA, psrf=TRUE, neff=TRUE)
summary(bfit1.6CA, psrf=TRUE, neff=TRUE)

summary(bfit1SG, psrf=TRUE, neff=TRUE)
summary(bfit1.1SG, psrf=TRUE, neff=TRUE)
summary(bfit1.2SG, psrf=TRUE, neff=TRUE)
summary(bfit1.3SG, psrf=TRUE, neff=TRUE)
summary(bfit1.4SG, psrf=TRUE, neff=TRUE)
summary(bfit1.5SG, psrf=TRUE, neff=TRUE)
summary(bfit1.6SG, psrf=TRUE, neff=TRUE)

summary(bfit1DE, psrf=TRUE, neff=TRUE)
summary(bfit1.1DE, psrf=TRUE, neff=TRUE)
summary(bfit1.2DE, psrf=TRUE, neff=TRUE)
summary(bfit1.3DE, psrf=TRUE, neff=TRUE)
summary(bfit1.4DE, psrf=TRUE, neff=TRUE)
summary(bfit1.5DE, psrf=TRUE, neff=TRUE)
summary(bfit1.6DE, psrf=TRUE, neff=TRUE)

summary(bfit1IN, psrf=TRUE, neff=TRUE)
summary(bfit1.1IN, psrf=TRUE, neff=TRUE)
summary(bfit1.2IN, psrf=TRUE, neff=TRUE)
summary(bfit1.3IN, psrf=TRUE, neff=TRUE)
summary(bfit1.4IN, psrf=TRUE, neff=TRUE)
summary(bfit1.5IN, psrf=TRUE, neff=TRUE)
summary(bfit1.6IN, psrf=TRUE, neff=TRUE)

summary(bfit1BR, psrf=TRUE, neff=TRUE)
summary(bfit1.1BR, psrf=TRUE, neff=TRUE)
summary(bfit1.2BR, psrf=TRUE, neff=TRUE)
summary(bfit1.3BR, psrf=TRUE, neff=TRUE)
summary(bfit1.4BR, psrf=TRUE, neff=TRUE)
summary(bfit1.5BR, psrf=TRUE, neff=TRUE)
summary(bfit1.6BR, psrf=TRUE, neff=TRUE)

summary(bfit1SE, psrf=TRUE, neff=TRUE)
summary(bfit1.1SE, psrf=TRUE, neff=TRUE)
summary(bfit1.2SE, psrf=TRUE, neff=TRUE)
summary(bfit1.3SE, psrf=TRUE, neff=TRUE)
summary(bfit1.4SE, psrf=TRUE, neff=TRUE)
summary(bfit1.5SE, psrf=TRUE, neff=TRUE)
summary(bfit1.6SE, psrf=TRUE, neff=TRUE)





########################                  3.2.3              ################################################
########################                                     ################################################
########################             Bayesian plots          ################################################



### US
plot(bfitUS, pars=1:40, plot.type="trace")
plot(bfitUS, pars=1:5, plot.type="acf")
plot(bfitUS, pars=1:25, plot.type="intervals")
plot(bfitUS, pars=1:20, plot.type="areas")
plot(bfitUS, pars=1:20, plot.type="areas_ridges")
plot(bfitUS, pars=1:12, plot.type="dens_overlay")

plot(bfit1US, pars=1:40, plot.type="trace")
plot(bfit1US, pars=1:5, plot.type="acf")
plot(bfit1US, pars=1:25, plot.type="intervals")
plot(bfit1US, pars=1:20, plot.type="areas")
plot(bfit1US, pars=1:20, plot.type="areas_ridges")
plot(bfit1US, pars=1:12, plot.type="dens_overlay")

plot(bfit1.1US, pars=1:40, plot.type="trace")
plot(bfit1.1US, pars=1:5, plot.type="acf")
plot(bfit1.1US, pars=1:25, plot.type="intervals")
plot(bfit1.1US, pars=1:20, plot.type="areas")
plot(bfit1.1US, pars=1:20, plot.type="areas_ridges")
plot(bfit1.1US, pars=1:12, plot.type="dens_overlay")

plot(bfit1.2US, pars=1:40, plot.type="trace")
plot(bfit1.2US, pars=1:5, plot.type="acf")
plot(bfit1.2US, pars=1:25, plot.type="intervals")
plot(bfit1.2US, pars=1:20, plot.type="areas")
plot(bfit1.2US, pars=1:20, plot.type="areas_ridges")
plot(bfit1.2US, pars=1:12, plot.type="dens_overlay")

plot(bfit1.3US, pars=1:40, plot.type="trace")
plot(bfit1.3US, pars=1:5, plot.type="acf")
plot(bfit1.3US, pars=1:25, plot.type="intervals")
plot(bfit1.3US, pars=1:20, plot.type="areas")
plot(bfit1.3US, pars=1:20, plot.type="areas_ridges")
plot(bfit1.3US, pars=1:12, plot.type="dens_overlay")

plot(bfit1.4US, pars=1:40, plot.type="trace")
plot(bfit1.4US, pars=1:5, plot.type="acf")
plot(bfit1.4US, pars=1:25, plot.type="intervals")
plot(bfit1.4US, pars=1:20, plot.type="areas")
plot(bfit1.4US, pars=1:20, plot.type="areas_ridges")
plot(bfit1.4US, pars=1:12, plot.type="dens_overlay")

plot(bfit1.5US, pars=1:40, plot.type="trace")
plot(bfit1.5US, pars=1:5, plot.type="acf")
plot(bfit1.5US, pars=1:25, plot.type="intervals")
plot(bfit1.5US, pars=1:20, plot.type="areas")
plot(bfit1.5US, pars=1:20, plot.type="areas_ridges")
plot(bfit1.5US, pars=1:12, plot.type="dens_overlay")

plot(bfit1.6US, pars=1:40, plot.type="trace")
plot(bfit1.6US, pars=1:5, plot.type="acf")
plot(bfit1.6US, pars=1:25, plot.type="intervals")
plot(bfit1.6US, pars=1:20, plot.type="areas")
plot(bfit1.6US, pars=1:20, plot.type="areas_ridges")
plot(bfit1.6US, pars=1:12, plot.type="dens_overlay")

### HR
plot(bfitHR, pars=1:40, plot.type="trace")
plot(bfitHR, pars=1:5, plot.type="acf")
plot(bfitHR, pars=1:25, plot.type="intervals")
plot(bfitHR, pars=1:20, plot.type="areas")
plot(bfitHR, pars=1:20, plot.type="areas_ridges")
plot(bfitHR, pars=1:12, plot.type="dens_overlay")

plot(bfit1HR, pars=1:40, plot.type="trace")
plot(bfit1HR, pars=1:5, plot.type="acf")
plot(bfit1HR, pars=1:25, plot.type="intervals")
plot(bfit1HR, pars=1:20, plot.type="areas")
plot(bfit1HR, pars=1:20, plot.type="areas_ridges")
plot(bfit1HR, pars=1:12, plot.type="dens_overlay")

plot(bfit1.1HR, pars=1:40, plot.type="trace")
plot(bfit1.1HR, pars=1:5, plot.type="acf")
plot(bfit1.1HR, pars=1:25, plot.type="intervals")
plot(bfit1.1HR, pars=1:20, plot.type="areas")
plot(bfit1.1HR, pars=1:20, plot.type="areas_ridges")
plot(bfit1.1HR, pars=1:12, plot.type="dens_overlay")

plot(bfit1.2HR, pars=1:40, plot.type="trace")
plot(bfit1.2HR, pars=1:5, plot.type="acf")
plot(bfit1.2HR, pars=1:25, plot.type="intervals")
plot(bfit1.2HR, pars=1:20, plot.type="areas")
plot(bfit1.2HR, pars=1:20, plot.type="areas_ridges")
plot(bfit1.2HR, pars=1:12, plot.type="dens_overlay")

plot(bfit1.3HR, pars=1:40, plot.type="trace")
plot(bfit1.3HR, pars=1:5, plot.type="acf")
plot(bfit1.3HR, pars=1:25, plot.type="intervals")
plot(bfit1.3HR, pars=1:20, plot.type="areas")
plot(bfit1.3HR, pars=1:20, plot.type="areas_ridges")
plot(bfit1.3HR, pars=1:12, plot.type="dens_overlay")

plot(bfit1.4HR, pars=1:40, plot.type="trace")
plot(bfit1.4HR, pars=1:5, plot.type="acf")
plot(bfit1.4HR, pars=1:25, plot.type="intervals")
plot(bfit1.4HR, pars=1:20, plot.type="areas")
plot(bfit1.4HR, pars=1:20, plot.type="areas_ridges")
plot(bfit1.4HR, pars=1:12, plot.type="dens_overlay")

plot(bfit1.5HR, pars=1:40, plot.type="trace")
plot(bfit1.5HR, pars=1:5, plot.type="acf")
plot(bfit1.5HR, pars=1:25, plot.type="intervals")
plot(bfit1.5HR, pars=1:20, plot.type="areas")
plot(bfit1.5HR, pars=1:20, plot.type="areas_ridges")
plot(bfit1.5HR, pars=1:12, plot.type="dens_overlay")

plot(bfit1.6HR, pars=1:40, plot.type="trace")
plot(bfit1.6HR, pars=1:5, plot.type="acf")
plot(bfit1.6HR, pars=1:25, plot.type="intervals")
plot(bfit1.6HR, pars=1:20, plot.type="areas")
plot(bfit1.6HR, pars=1:20, plot.type="areas_ridges")
plot(bfit1.6HR, pars=1:12, plot.type="dens_overlay")

## GB

plot(bfit1.5GB, pars=1:40, plot.type="trace")
plot(bfit1.5GB, pars=1:5, plot.type="acf")
plot(bfit1.5GB, pars=1:25, plot.type="intervals")
plot(bfit1.5GB, pars=1:20, plot.type="areas")
plot(bfit1.5GB, pars=1:20, plot.type="areas_ridges")
plot(bfit1.5GB, pars=1:12, plot.type="dens_overlay")


# plot(bfit, pars=1:40, plot.type="trace")
# plot(bfit, pars=1:5, plot.type="acf")
# plot(bfit, pars=1:25, plot.type="intervals")
# plot(bfit, pars=1:20, plot.type="areas")
# plot(bfit, pars=1:20, plot.type="areas_ridges")
# plot(bfit, pars=1:12, plot.type="dens_overlay")






########################                  3.2.4              ################################################
########################                                     ################################################
########################              Bayesian fit           ################################################


# compare fit indices
fit0US <- bcfa(null.modl, data = USgendr, cp = "fa", sample = 8000, burnin = 1000)
fit0HR <- bcfa(null.modl, data = HRgendr, cp = "fa", sample = 12000, burnin = 1000)

fit0GB <- bcfa(null.modl, data = GBgendr, cp = "fa", sample = 8000, burnin = 1000)
fit0CA <- bcfa(null.modl, data = CAgendr, cp = "fa", sample = 8000, burnin = 1000)
fit0AU <- bcfa(null.modl, data = AUgendr, cp = "fa", sample = 8000, burnin = 1000)

fit0SG <- bcfa(null.modl, data = SGgendr, cp = "fa", sample = 12000, burnin = 1000)
fit0DE <- bcfa(null.modl, data = DEgendr, cp = "fa", sample = 8000, burnin = 1000)
fit0IN <- bcfa(null.modl, data = INgendr, cp = "fa", sample = 12000, burnin = 1000)
fit0BR <- bcfa(null.modl, data = BRgendr, cp = "fa", sample = 1000, burnin = 1000)
fit0SE <- bcfa(null.modl, data = SEgendr, cp = "fa", sample = 8000, burnin = 1000)


MLUS <- blavFitIndices(bfitUS, baseline.model = fit0US)
ML1US <- blavFitIndices(bfit1US, baseline.model = fit0US)
ML1.1US <- blavFitIndices(bfit1.1US, baseline.model = fit0US)
ML1.2US <- blavFitIndices(bfit1.2US, baseline.model = fit0US)
ML1.3US <- blavFitIndices(bfit1.3US, baseline.model = fit0US)
ML1.4US <- blavFitIndices(bfit1.4US, baseline.model = fit0US)
ML1.5US <- blavFitIndices(bfit1.5US, baseline.model = fit0US)
ML1.6US <- blavFitIndices(bfit1.6US, baseline.model = fit0US)

MLHR <- blavFitIndices(bfitHR, baseline.model = fit0HR)
ML1HR <- blavFitIndices(bfit1HR, baseline.model = fit0HR)
ML1.1HR <- blavFitIndices(bfit1.1HR, baseline.model = fit0HR)
ML1.2HR <- blavFitIndices(bfit1.2HR, baseline.model = fit0HR)
ML1.3HR <- blavFitIndices(bfit1.3HR, baseline.model = fit0HR)
ML1.4HR <- blavFitIndices(bfit1.4HR, baseline.model = fit0HR)
ML1.5HR <- blavFitIndices(bfit1.5HR, baseline.model = fit0HR)
ML1.6HR <- blavFitIndices(bfit1.6HR, baseline.model = fit0HR)

MLGB <- blavFitIndices(bfitGB, baseline.model = fit0GB)
ML1GB <- blavFitIndices(bfit1GB, baseline.model = fit0GB)
ML1.1GB <- blavFitIndices(bfit1.1GB, baseline.model = fit0GB)
ML1.2GB <- blavFitIndices(bfit1.2GB, baseline.model = fit0GB)
ML1.3GB <- blavFitIndices(bfit1.3GB, baseline.model = fit0GB)
ML1.4GB <- blavFitIndices(bfit1.4GB, baseline.model = fit0GB)
ML1.5GB <- blavFitIndices(bfit1.5GB, baseline.model = fit0GB)
ML1.6GB <- blavFitIndices(bfit1.6GB, baseline.model = fit0GB)

MLCA <- blavFitIndices(bfitCA, baseline.model = fit0CA)
ML1CA <- blavFitIndices(bfit1CA, baseline.model = fit0CA)
ML1.1CA <- blavFitIndices(bfit1.1CA, baseline.model = fit0CA)
ML1.2CA <- blavFitIndices(bfit1.2CA, baseline.model = fit0CA)
ML1.3CA <- blavFitIndices(bfit1.3CA, baseline.model = fit0CA)
ML1.4CA <- blavFitIndices(bfit1.4CA, baseline.model = fit0CA)
ML1.5CA <- blavFitIndices(bfit1.5CA, baseline.model = fit0CA)
ML1.6CA <- blavFitIndices(bfit1.6CA, baseline.model = fit0CA)

MLAU <- blavFitIndices(bfitAU, baseline.model = fit0AU)
ML1AU <- blavFitIndices(bfit1AU, baseline.model = fit0AU)
ML1.1AU <- blavFitIndices(bfit1.1AU, baseline.model = fit0AU)
ML1.2AU <- blavFitIndices(bfit1.2AU, baseline.model = fit0AU)
ML1.3AU <- blavFitIndices(bfit1.3AU, baseline.model = fit0AU)
ML1.4AU <- blavFitIndices(bfit1.4AU, baseline.model = fit0AU)
ML1.5AU <- blavFitIndices(bfit1.5AU, baseline.model = fit0AU)
ML1.6AU <- blavFitIndices(bfit1.6AU, baseline.model = fit0AU)

MLSG <- blavFitIndices(bfitSG, baseline.model = fit0SG)
ML1SG <- blavFitIndices(bfit1SG, baseline.model = fit0SG)
ML1.1SG <- blavFitIndices(bfit1.1SG, baseline.model = fit0SG)
ML1.2SG <- blavFitIndices(bfit1.2SG, baseline.model = fit0SG)
ML1.3SG <- blavFitIndices(bfit1.3SG, baseline.model = fit0SG)
ML1.4SG <- blavFitIndices(bfit1.4SG, baseline.model = fit0SG)
ML1.5SG <- blavFitIndices(bfit1.5SG, baseline.model = fit0SG)
ML1.6SG <- blavFitIndices(bfit1.6SG, baseline.model = fit0SG)

MLDE <- blavFitIndices(bfitDE, baseline.model = fit0DE)
ML1DE <- blavFitIndices(bfit1DE, baseline.model = fit0DE)
ML1.1DE <- blavFitIndices(bfit1.1DE, baseline.model = fit0DE)
ML1.2DE <- blavFitIndices(bfit1.2DE, baseline.model = fit0DE)
ML1.3DE <- blavFitIndices(bfit1.3DE, baseline.model = fit0DE)
ML1.4DE <- blavFitIndices(bfit1.4DE, baseline.model = fit0DE)
ML1.5DE <- blavFitIndices(bfit1.5DE, baseline.model = fit0DE)
ML1.6DE <- blavFitIndices(bfit1.6DE, baseline.model = fit0DE)

MLIN <- blavFitIndices(bfitIN, baseline.model = fit0IN)
ML1IN <- blavFitIndices(bfit1IN, baseline.model = fit0IN)
ML1.1IN <- blavFitIndices(bfit1.1IN, baseline.model = fit0IN)
ML1.2IN <- blavFitIndices(bfit1.2IN, baseline.model = fit0IN)
ML1.3IN <- blavFitIndices(bfit1.3IN, baseline.model = fit0IN)
ML1.4IN <- blavFitIndices(bfit1.4IN, baseline.model = fit0IN)
ML1.5IN <- blavFitIndices(bfit1.5IN, baseline.model = fit0IN)
ML1.6IN <- blavFitIndices(bfit1.6IN, baseline.model = fit0IN)

MLBR <- blavFitIndices(bfitBR, baseline.model = fit0BR)
ML1BR <- blavFitIndices(bfit1BR, baseline.model = fit0BR)
ML1.1BR <- blavFitIndices(bfit1.1BR, baseline.model = fit0BR)
ML1.2BR <- blavFitIndices(bfit1.2BR, baseline.model = fit0BR)
ML1.3BR <- blavFitIndices(bfit1.3BR, baseline.model = fit0BR)
ML1.4BR <- blavFitIndices(bfit1.4BR, baseline.model = fit0BR)
ML1.5BR <- blavFitIndices(bfit1.5BR, baseline.model = fit0BR)
ML1.6BR <- blavFitIndices(bfit1.6BR, baseline.model = fit0BR)

MLSE <- blavFitIndices(bfitSE, baseline.model = fit0SE)
ML1SE <- blavFitIndices(bfit1SE, baseline.model = fit0SE)
ML1.1SE <- blavFitIndices(bfit1.1SE, baseline.model = fit0SE)
ML1.2SE <- blavFitIndices(bfit1.2SE, baseline.model = fit0SE)
ML1.3SE <- blavFitIndices(bfit1.3SE, baseline.model = fit0SE)
ML1.4SE <- blavFitIndices(bfit1.4SE, baseline.model = fit0SE)
ML1.5SE <- blavFitIndices(bfit1.5SE, baseline.model = fit0SE)
ML1.6SE <- blavFitIndices(bfit1.6SE, baseline.model = fit0SE)



# ML <- blavFitIndices(bfit, baseline.model = fit0)
# ML1 <- blavFitIndices(bfit1, baseline.model = fit0)
# ML1.1 <- blavFitIndices(bfit1.1, baseline.model = fit0)
# ML1.2 <- blavFitIndices(bfit1.2, baseline.model = fit0)
# ML1.3 <- blavFitIndices(bfit1.3, baseline.model = fit0)
# ML1.4 <- blavFitIndices(bfit1.4, baseline.model = fit0)
# ML1.5 <- blavFitIndices(bfit1.5, baseline.model = fit0)
# ML1.6 <- blavFitIndices(bfit1.6, baseline.model = fit0)

MLUS
ML1US
ML1.1US
ML1.2US
ML1.3US
ML1.4US
ML1.5US
ML1.6US

MLHR
ML1HR
ML1.1HR
ML1.2HR
ML1.3HR
ML1.4HR
ML1.5HR
ML1.6HR

MLGB
ML1GB 
ML1.1GB
ML1.2GB
ML1.3GB
ML1.4GB
ML1.5GB
ML1.6GB

MLCA 
ML1CA
ML1.1CA
ML1.2CA
ML1.3CA
ML1.4CA
ML1.5CA
ML1.6CA

MLAU 
ML1AU
ML1.1AU
ML1.2AU
ML1.3AU
ML1.4AU
ML1.5AU
ML1.6AU

MLSG 
ML1SG 
ML1.1SG
ML1.2SG
ML1.3SG
ML1.4SG
ML1.5SG
ML1.6SG

MLDE
ML1DE 
ML1.1DE
ML1.2DE
ML1.3DE
ML1.4DE
ML1.5DE
ML1.6DE

MLIN
ML1IN 
ML1.1IN 
ML1.2IN 
ML1.3IN 
ML1.4IN 
ML1.5IN 
ML1.6IN 

MLBR  
ML1BR
ML1.1BR
ML1.2BR
ML1.3BR
ML1.4BR
ML1.5BR
ML1.6BR

MLSE  
ML1SE 
ML1.1SE 
ML1.2SE 
ML1.3SE 
ML1.4SE 
ML1.5SE 
ML1.6SE 


summary(MLHR)  # gives CI


xbfitb <- bcfa(models,data=INgendr, estimator = 'MLM', group = 'gender', 
              cp = "fa", target = "jags", bcontrol = list(method = "rjparallel"), n.chains = 3, burnin = 1000, 
              jagcontrol=list(method="rjparallel"),  # it works here
              meanstructure=T)

summary(xbfitb, psrf=TRUE, neff=TRUE)
plot(xbfit, pars=1:40, plot.type="trace")
plot(xbfit, pars=40:80, plot.type="trace")
plot(xbfit, pars=1:40, plot.type="density")
plot(xbfit, pars=1:40, plot.type="autocorr")
plot(xbfit, plot.type="autocorr")
coef(fit)

AFIs <- ppmc(xbfitb, thin = 10, fit.measures = c("srmr","chisq","rmsea","cfi"))
summary(AFIs) # summarize the whole vector in a data.frame
hist(AFIs, element = "rmsea") # only plot one discrepancy function at a time
plot(AFIs, element = "srmr")





#### compare gender between countries  (not part of my paper)

sampl <- vuil[sample(vuil$country =="US"), 500)]
sampl <- sample(vuil$country =="US", 20, replace=TRUE)
sampl <- sample(1:nrow(vuil$country =="US"), 20)


mgendr <- filter(vuil, gender =="1", country =="US" | country =="SG")
fgendr <- filter(vuil, gender =="2", country =="US" | country =="SG")

mgendr <- filter(vuil, gender =="1", country =="US" | country =="BR")
fgendr <- filter(vuil, gender =="2", country =="US" | country =="BR")

mgendr <- filter(vuil, gender =="1", country =="US" | country =="IN")
fgendr <- filter(vuil, gender =="2", country =="US" | country =="IN")


fit <- cfa(models,data=mgendr, estimator = 'MLM', group = 'country', meanstructure=T)
fit1 <- cfa(models,data=mgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings"))
fit1.1 <- cfa(models,data=mgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts"))
fit1.2 <- cfa(models,data=mgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3 <- cfa(models,data=mgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4 <- cfa(models,data=mgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5 <- cfa(models,data=mgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6 <- cfa(models,data=mgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))


summary(fit, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.1, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.2, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.3, standardized=T, fit.measures=T, rsquare=T) 
summary(fit1.4, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.5, standardized=T, fit.measures=T, rsquare=T)
summary(fit1.6, standardized=T, fit.measures=T, rsquare=T)

fit <- cfa(models,data=fgendr, estimator = 'MLM', group = 'country', meanstructure=T)
fit1 <- cfa(models,data=fgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings"))
fit1.1 <- cfa(models,data=fgendr, estimator = 'MLM', group = 'country',meanstructure=T, group.equal=c("loadings","intercepts"))
fit1.2 <- cfa(models,data=fgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
fit1.3 <- cfa(models,data=fgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
fit1.4 <- cfa(models,data=fgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
fit1.5 <- cfa(models,data=fgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
fit1.6 <- cfa(models,data=fgendr, estimator = 'MLM', group = 'country',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

fitMeasures(fit, selected)
fitMeasures(fit1, selected)
fitMeasures(fit1.1, selected)
fitMeasures(fit1.2, selected)
fitMeasures(fit1.3, selected)
fitMeasures(fit1.4, selected)
fitMeasures(fit1.5, selected)
fitMeasures(fit1.6, selected)





save.image(file = 'SEMwork.RData')







#############################  Cloud document #########################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################



# run this by cloud:

library(stringr)
library(dplyr)
library(blavaan)
library(haven)


library(semPlot)
#library(lavaan)   # do not run lavaan, otherwise some code won't work! (its actually also loaded when loading blavaan)
library(ggcorrplot)
library(semTools)
library(runjags)
library(rjags)


modls <-'one =~ DDP1+DDP2+DDP3+DDP4
avoidence =~ DDN1+DDN2+DDN3+DDN4
two =~ DDM1+DDM2+DDM3+DDM4          
#DDN1 ~~ DDN2
#all =~ one+two+avoidence
'

models <-'P =~ DDP2+DDP1+DDP3+DDP4
N =~ DDN3+DDN2+DDN1+DDN4
M =~ DDM4+DDM2+DDM3+DDM1         
DDN1 ~~ DDN2
#all =~ P+M+N
'

US1gendr <- filter(vuil, gender =="1", country =="US") #16 817
US2gendr <- filter(vuil, gender =="2", country =="US") #10 657

USgendr <- filter(vuil, gender =="1"|gender =="2", country =="US")


fitUS1a <- cfa(modls,data=US1gendr, estimator = 'MLM', meanstructure=T) # need meanstructure to use parameters in bcfa
fitUS2a <- cfa(modls,data=US2gendr, estimator = 'MLM', meanstructure=T)

fitUS1 <- cfa(models,data=US1gendr, estimator = 'MLM', meanstructure=T) 
fitUS2 <- cfa(models,data=US2gendr, estimator = 'MLM', meanstructure=T)

# Bayes
bfitUS1a <- bcfa(parTable(fitUS1a),data=US1gendr, convergence = "manual", sample = 3000, burnin = 1000)
bfitUS2a <- bcfa(parTable(fitUS2a),data=US2gendr, convergence = "manual", sample = 3000, burnin = 1000)

bfitUS1 <- bcfa(parTable(fitUS1),data=US1gendr, convergence = "manual", sample = 3000, burnin = 1000)
bfitUS2 <- bcfa(parTable(fitUS2),data=US2gendr, convergence = "manual", sample = 3000, burnin = 1000)

summary(bfitUS1a, standardized=T, fit.measures=T) 
summary(bfitUS2a, standardized=T, fit.measures=T)

summary(bfitUS1, standardized=T, fit.measures=T) 
summary(bfitUS2, standardized=T, fit.measures=T) 

summary(bfitUS1a, psrf=TRUE, neff=TRUE)
summary(bfitUS2a, psrf=TRUE, neff=TRUE)

summary(bfitUS1, psrf=TRUE, neff=TRUE)
summary(bfitUS2, psrf=TRUE, neff=TRUE)

fitMeasUS1 <- fitMeasures(bfitUS1, "all")
fitMeasUS2 <- fitMeasures(bfitUS2, "all")

plottraceUS1 <- plot(bfitUS1, pars=1:40, plot.type="trace")
plottraceUS2 <- plot(bfitUS2, pars=1:40, plot.type="trace")
plottraceUS1b <- plot(bfitUS1, pars=41:80, plot.type="trace")
plottraceUS1b <- plot(bfitUS2, pars=41:80, plot.type="trace")

null.modl <- c(paste0("DDN", 1:4, " ~~ DDN", 1:4),paste0("DDP", 1:4, " ~~ DDP", 1:4),
               paste0("DDM", 1:4, " ~~ DDM", 1:4),paste0("DDN", 1:4, " ~ 1"), 
               paste0("DDP", 1:4, " ~ 1"), paste0("DDM", 1:4, " ~ 1"))

fit0US1a <- bcfa(null.modl, data = US1gendr, cp = "fa", sample = 8000, burnin = 1000)
fit0US2a <- bcfa(null.modl, data = US2gendr, cp = "fa", sample = 8000, burnin = 1000)

fit0US1 <- bcfa(null.modl, data = US1gendr, cp = "fa", sample = 8000, burnin = 1000)
fit0US2 <- bcfa(null.modl, data = US2gendr, cp = "fa", sample = 8000, burnin = 1000)

MLUS1a <- blavFitIndices(bfitUS1a, baseline.model = fit0US1a)
MLUS2a <- blavFitIndices(bfitUS2a, baseline.model = fit0US2a)

MLUS1 <- blavFitIndices(bfitUS1, baseline.model = fit0US1)
MLUS2 <- blavFitIndices(bfitUS2, baseline.model = fit0US2)



############################################################################
#######################                              #######################
#######################        Equivalence           #######################
#######################                              #######################
############################################################################


#USA
bfitUS <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender', meanstructure=T)
bfit1US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender',meanstructure=T,group.equal=c("loadings"))
bfit1.1US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts"))
bfit1.2US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals"))
bfit1.3US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances"))
bfit1.4US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender',meanstructure=T, group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances"))
bfit1.5US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.variances", "lv.covariances"))
bfit1.6US <- bcfa(models,data=USgendr, estimator = 'MLM', sample = 8000, burnin = 1000, group = 'gender',meanstructure=T,group.equal=c("loadings","intercepts", "residuals","residual.covariances", "lv.covariances","lv.variances","means"))

# compare fit indices
fit0US <- bcfa(null.modl, data = USgendr, cp = "fa", sample = 8000, burnin = 1000)

MLUS <- blavFitIndices(bfitUS, baseline.model = fit0US)
ML1US <- blavFitIndices(bfit1US, baseline.model = fit0US)
ML1.1US <- blavFitIndices(bfit1.1US, baseline.model = fit0US)
ML1.2US <- blavFitIndices(bfit1.2US, baseline.model = fit0US)
ML1.3US <- blavFitIndices(bfit1.3US, baseline.model = fit0US)
ML1.4US <- blavFitIndices(bfit1.4US, baseline.model = fit0US)
ML1.5US <- blavFitIndices(bfit1.5US, baseline.model = fit0US)
ML1.6US <- blavFitIndices(bfit1.6US, baseline.model = fit0US)

MLUS
ML1US
ML1.1US
ML1.2US
ML1.3US
ML1.4US
ML1.5US
ML1.6US

