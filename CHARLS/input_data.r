# input_data.r prepares dataset used for imputation model
# 'April21.csv' is merged from the raw dataset downloaded from http://charls.ccer.edu.cn/en

# dataframe 'dat1' is created by running this file, which contains,
#     continuous variables: age, bmi, height, weight, hh_totalincome and yr_educ
#     binary variables: diab_bio, diabSR, female, healthSR and rural

library(foreign)
library(mice)
library(foreach)
library(survey)

dat = read.csv(file='April21.csv',header=TRUE)
dat = dat[!is.na(dat$age),]
dat = dat[!is.na(dat$female),]
summary(dat)

# pre-process yr_educ
variable = c('diab_bio','diabSR','age','female','bmi','height','weight','hh_totalincome')
dat1 = dat[,variable]

dat1$yr_educ = NA
dat1$yr_educ[dat$bd001 == '1 No formal education illiterate'] = 0

dat1$yr_educ[dat$bd001 == '3 Sishu'] = 1
dat1$yr_educ[dat$bd001 == '2 Did not finish primary school but capable of reading or writing'] = 3
dat1$yr_educ[dat$bd001 == '4 Elementary school'] = 6
dat1$yr_educ[dat$bd001 == '5 Middle school'] = 9
dat1$yr_educ[dat$bd001 == '6 High school'] = 12
dat1$yr_educ[dat$bd001 == '7 Vocational school'|dat$bd001 == '8 Two/Three Year College / Associate degree'] = 14
dat1$yr_educ[dat$bd001 == '9 Four Year College / Bachelors degree'|dat$bd001 == '10 Post-graduate, Masters degree'] = 16
dat1$yr_educ = as.numeric(dat1$yr_educ)

dat1$healthSR = NA
dat1$healthSR[dat$health == 'not poor'] = 'not poor'
dat1$healthSR[dat$health == 'poor'] = 'poor'
dat1$healthSR = as.factor(dat1$healthSR)

dat1$rural = NA
dat1$rural[dat$rural == 'Rural'] = 'Rural'
dat1$rural[dat$rural == 'Urban'] = 'Urban'
dat1$rural = as.factor(dat1$rural)

categorical.vari = c('diab_bio','female','diabSR')
dat1[,categorical.vari] = foreach(x = categorical.vari) %do% {as.factor(dat1[,x])}

