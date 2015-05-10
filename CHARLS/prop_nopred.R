# This file implements the following imputation method
# 1. impute all missing values in covariates (all variables except diab_bio)
#    using chain equation; repeat for 10 times to get 10 imputed sets
# 2. in each imputed dataset, built propensity score of having blood test and
#    created sub-groups based on propensity score
# 3. in each sub-group,imputed missing clnical diabetes status
#    using Bayesian logistic regression
# prop_nonpred(): implement step 2 and 3
# get_coef(): get coef of logistic regression in post-imputation analysis
# get_se(): get std.error of logistic regression in post-imputation analysis

source('input_data.r')
imp.nopred = mice(dat1[,-1],maxit=0, print = FALSE)
meth = imp.nopred$meth
meth['bmi'] = '~I(weight/(height/100)^2)'

pred = imp.nopred$pred

pred[,'height'] = 0
pred[c('weight','height'),'bmi'] = 0
pred[c('healthSR','weight'),'height'] = 1
pred['bmi',]=0
pred['bmi',c('weight','height')]=1

imp.nopred = mice(dat1[,-1], m = 10, meth = meth, pred = pred, print=FALSE)

# get the 10 imputed datasets and write into stata files
mult_set = do.call('list',lapply(1:10, function(x) prop_nonpred(imp.t5,x)))
foreach(x=1:10)%do%{ write.dta(mult_set[[x]],file=paste(x,'.dta'))}

# calculate prevalence of diabetes under categories defined by 1) gender; 
# 2) residence; 3) self-reported health condition; 4) years of education
avg_rate = rowMeans(sapply(mult_set,function(x) prevalence_bi_2(x,'female')))
avg_rate = rowMeans(sapply(mult_set,function(x) prevalence_bi_2(x,'rural')))
mean_prevalence = rowMeans(sapply(mult_set,function(x) prevalence_bi(x,'healthSR')))
avg_rate = rowMeans(sapply(mult_set,function(x) prevalence_cont_2(x,'yr_educ',9)))


# simple logistic regression analysis

avg_coef = rowMeans(sapply(mult_set,function(x) get_coef(x)))
avg_se = rowMeans(sapply(mult_set,function(x) get_se(x)))

# functions used in this file

prop_nonpred = function(imp,n){
    cs1 = complete(imp,n)
    cs1$hba1c = dat$hba1c
    hba1c.prop = glm(hba1c~.-height,data=cs1,family='binomial')
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

    set1 = set[s1,]
    set2 = set[s2,]
    set3 = set[s3,]
    set4 = set[s4,]
    set5 = set[s5,]

    # impute diab_bio
    imp.1 = mice(set1,m=1,print=FALSE)
    imp.2 = mice(set2,m=1,print=FALSE)
    imp.3 = mice(set3,m=1,print=FALSE)
    imp.4 = mice(set4,m=1,print=FALSE)
    imp.5 = mice(set5,m=1,print=FALSE)

    # completeset
    set$diab_bio[s1] = complete(imp.1,1)$diab_bio
    set$diab_bio[s2] = complete(imp.2,1)$diab_bio
    set$diab_bio[s3] = complete(imp.3,1)$diab_bio
    set$diab_bio[s4] = complete(imp.4,1)$diab_bio
    set$diab_bio[s5] = complete(imp.5,1)$diab_bio

    return(set)
}


get_coef = function(set){
	model1 = glm(diabSR~age+female+bmi+rural+yr_educ,data=set,family='binomial')
	# model2 = glm(diabSR~yr_educ, data=set,family='binomial')
	return(model1$coefficients[2:6])
}

get_se = function(set){
	model1 = glm(diab_bio~age+female+bmi,data=set,family='binomial')
	model2 = glm(diab_bio~yr_educ, data=set,family='binomial')
	return(c(summary(model1)$coef[2:4,'Std. Error'],summary(model2)$coef[2,'Std.Error']))
}
