# This file implements the following imputation method
# 1. impute all missing values in covariates (all variables except diab_bio)
#    using chain equation; repeat for 10 times to get 10 imputed sets
# 2. in each imputed dataset, built propensity score of having blood test and
#    created sub-groups based on propensity score
# 3. in each sub-group, used propensity score of having self-reported diabetes
#    as summaries for other covariates; imputed missing clnical diabetes status
#    using Bayesian logistic regression
# NOTE: step 2 and 3 are carried on one imputed set as an example

# 1. impute all missing values in covariates 

source('input_data.r')
imp.prop = mice(dat1[,-1],maxit=0, print = FALSE)
meth = imp.prop$meth
meth['bmi'] = '~I(weight/(height/100)^2)'
pred = imp.prop$pred
pred[,'height'] = 0
pred[c('weight','height'),'bmi'] = 0
pred[c('healthSR','weight'),'height'] = 1
pred['bmi',]=0
pred['bmi',c('weight','height')]=1

imp.prop = mice(dat1[,-1], m = 10, meth = meth, pred = pred, print=FALSE)
# get 1 of the 10 imputed set
cs1 = complete(imp.prop,1)

# 2. built propensity score of having blood test and
#    created sub-groups based on propensity score

cs1$hba1c = dat$hba1c
hba1c.prop = glm(hba1c~.-height,data=cs1,family='binomial')

library(survey)
score = hba1c.prop$fitted.values
strata = quantile(score,prob=seq(0,1,1/5))
s1 = which(score>=strata[1]&score<strata[2])
s2 = which(score>=strata[2]&score<strata[3])
s3 = which(score>=strata[3]&score<strata[4])
s4 = which(score>=strata[4]&score<strata[5])
s5 = which(score>=strata[5]&score<strata[6])
var.sub = c('age','female','bmi','hh_totalincome','yr_educ','diabSR','healthSR','rural','weight')
cs1.sub = cs1[,var.sub]
set = cbind(dat1$diab_bio, cs1.sub)
names(set)[1] = 'diab_bio'

S=list(s1,s2,s3,s4,s5)


# in each sub-group, used propensity score of having self-reported diabetes
# as summaries for other covariates; imputed missing clnical diabetes status
# using Bayesian logistic regression

glm.sr1=glm(diabSR~age + bmi + female + hh_totalincome + healthSR + yr_educ,data=cs1.sub,subset=s1,family='binomial')
summary(glm.sr1)

glm.sr2=glm(diabSR~.,data=cs1.sub,subset=s2,family='binomial')
summary(glm.sr2)

glm.sr3=glm(diabSR~age + bmi + hh_totalincome + healthSR +weight,data=cs1.sub,subset=s3,family='binomial')
summary(glm.sr3)

glm.sr4=glm(diabSR~age + bmi + hh_totalincome + weight + yr_educ + healthSR,data=cs1.sub,subset=s4,family='binomial')
summary(glm.sr4)

glm.sr5=glm(diabSR~age+bmi+female+hh_totalincome+healthSR+yr_educ+weight,data=cs1.sub,subset=s5,family='binomial')
summary(glm.sr5)

glm.sr = list(glm.sr1,glm.sr2,glm.sr3,glm.sr4,glm.sr5)

# calcuate covarates used in the final imputation model for clinical diabetes
# 1) propensity score of having self-reported diabetes, predictor
# 2) propensity score of taking blood test, score
# 3) self-reported diabetes status, diab_SR
# 4) interaction term of score and predictor

sr = lapply(glm.sr,function(x) x$fitted.values)

predictor = NULL
predictor[s1]=sr[[1]]
predictor[s2]=sr[[2]]
predictor[s3]=sr[[3]]
predictor[s4]=sr[[4]]
predictor[s5]=sr[[5]]

score.pred = score*predictor
dat.imp = as.data.frame(cbind(dat$diab_bio,dat$diabSR,score,predictor))
names(dat.imp)[1:2]=c('diab_bio','diabSR')

# impute diab_bio in each sub-group
imp.1 = mice(dat.imp[s1,],m=1,print=FALSE)
imp.2 = mice(dat.imp[s2,],m=1,print=FALSE)
imp.3 = mice(dat.imp[s3,],m=1,print=FALSE)
imp.4 = mice(dat.imp[s4,],m=1,print=FALSE)
imp.5 = mice(dat.imp[s5,],m=1,print=FALSE)

# combine clinical diabetes imputed in each sub-group
set$diab_bio[s1] = complete(imp.1,1)$diab_bio
set$diab_bio[s2] = complete(imp.2,1)$diab_bio
set$diab_bio[s3] = complete(imp.3,1)$diab_bio
set$diab_bio[s4] = complete(imp.4,1)$diab_bio
set$diab_bio[s5] = complete(imp.5,1)$diab_bio





