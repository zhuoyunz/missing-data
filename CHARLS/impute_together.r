# this fill used method that imputes missing values in all variables
# together using chain equations, with predictive mean matching for
# continuous variables and bayesian logistic regression for binary
# variables.
# Note: imputations for missing 'bmi' is different since it's calcuated
# by 'weight' and 'height': bmi = weight/(height/100)^2. See the code
# for details of imputation method and covariates settings of 'bmi', 
# 'height' and 'weight'. 
# In experiment, impuation process is repeated for 10 times. 


# load incomplete dataset dat1
source('input_data.r')

# initialize the impution model
imp = mice(dat1,maxit=0, print = FALSE)

# set the imputation meth for 'bmi', and covariates used for 'height' and 
# 'weight'

meth = imp$meth
meth['bmi'] = '~I(weight/(height/100)^2)'
pred = imp$pred
pred[,'height'] = 0
pred[c('weight','height'),'bmi'] = 0
pred[c('healthSR','weight'),'height'] = 1
pred['bmi',]=0
pred['bmi',c('weight','height')]=1
imp.t = mice(dat1, m = 10, meth = meth, pred = pred, print=FALSE)

#  a simple check for imputation quality of continuous variables
densityplot(imp.t1,~bmi)
# get one filled-in dataset by function complete()
mult_set = do.call('list',lapply(1:10, function(x) complete(imp.t,x)))

# check imputation quality
check_imp(mult_set[[1]],'diab_bio')
check_imp(mult_set[[1]],'bmi')

# calculate prevalence of diabetes under categories defined by 1) gender; 
# 2) residence; 3) self-reported health condition; 4) years of education

mean_prevalence = rowMeans(sapply(mult_set,function(x) prevalence_bi(x,'female')))
mean_prevalence = rowMeans(sapply(mult_set,function(x) prevalence_bi(x,'rural')))
mean_prevalence = rowMeans(sapply(mult_set,function(x) prevalence_bi(x,'healthSR')))
mean_prevalence = rowMeans(sapply(mult_set,function(x) prevalence_cont(x,'yr_educ',9))) 


# write the 10 imputed datesets into files (here write to stata files)
mtd1= do.call('list',lapply(1:10,function(x) complete(imp.t,x)))
foreach(x=1:10)%do%{ write.dta(mtd1[[x]],file=paste(x,'.dta'))}
