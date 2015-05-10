# This file contains functions used for imputation quality checking
# and analysis after filling in the missing values.
# 1. check_imp(): check if the imputations are plausible. In each strata
#                 created by propensity score for response, distributions 
#                 of imputed values and observed values are ploted: if 
#                 the imputations are plausible, then the two distribution 
#                 should be similar. 
#                 argument: 1) imp: imputation object acquired by function mice()
#                           2) n: index of imputed dataset
#                           3) ctg: the variable needs to check
#                 returned values: plot distributions of imputed and observed values
#                                  in each of the five stratas


# 2. prevalence_bi(): calculate the prevalence rate of diabetes under different categories
#                     for binary variables
#                     argument: same as check_imp()
#                     returned values: a vector containing prevalence rates of clinical 
#                                      (1st & 2nd) and self-reported (3rd & 4th) diabetes
#                                      under different categories

# 3. prevalence_cont(): calculate the prevalence rate of diabetes under different categories
#                     created by quantiling values of continuous variables
#                     argument: key: thresheld for quantiling categries; the others are same
#                               as check_imp() and prevalence_bi()
#                     returned values: a vector containing prevalence rates of clinical 
#                                      (1st & 2nd) and self-reported (3rd & 4th) diabetes
#                                      under different categories

check_imp = function(set,ctg){
	
	var = variable[!(variable %in% c('height',ctg))]
	M = 1*(!is.na(dat1[,ctg]))
	set$M = M
	glm1 = glm(M~.,data=set[,var],family='binomial')
	score = glm1$fitted.values
    strata = quantile(score,prob=seq(0,1,1/5))
    s1 = which(score>=strata[1]&score<strata[2])
    s2 = which(score>=strata[2]&score<strata[3])
    s3 = which(score>=strata[3]&score<strata[4])
    s4 = which(score>=strata[4]&score<strata[5])
    s5 = which(score>=strata[5]&score<strata[6])
    s = list(s1,s2,s3,s4,s5)
    
    for(i in 1:5){
    	
    	s1.na = s[[i]][s[[i]] %in% which(M==0)]
        s1.ob = s[[i]][s[[i]] %in% which(M==1)]
        par(ask=TRUE)
        
        if(is.factor(set[,ctg])){
            par(mfrow=c(1,2))
            plot(set[,ctg][s1.na],main='imputed')
            plot(set[,ctg][s1.ob],main='observed')
        }
        
        else{
        par(mfrow=c(1,2))
        hist(set[,ctg][s1.na],main='imputed')
        hist(set[,ctg][s1.ob],main='observed')
        }
        
    }
    
}



prevalence_bi = function(set_complete,ctg){
	
	l1 = levels(set_complete[,ctg])[1]
	l2 = levels(set_complete[,ctg])[2]
	bio1 = sum(set_complete$diab_bio[set_complete[,ctg]==l1]=='1')/sum(set_complete[,ctg]==l1)
	bio2 = sum(set_complete$diab_bio[set_complete[,ctg]==l2]=='1')/sum(set_complete[,ctg]==l2)
	sr1 = sum(set_complete$diabSR[set_complete[,ctg]==l1]=='1')/sum(set_complete[,ctg]==l1)
	sr2 = sum(set_complete$diabSR[set_complete[,ctg]==l2]=='1')/sum(set_complete[,ctg]==l2)
	
	return(c(bio1,bio2,sr1,sr2))
	
}


prevalence_cont = function(set_complete,ctg,key){
	
	bio1 = sum(set_complete$diab_bio[set_complete[,ctg]<=key]=='1')/sum(set_complete[,ctg]<=key)
	bio2 = sum(set_complete$diab_bio[set_complete[,ctg]>key]=='1')/sum(set_complete[,ctg]>key)
	sr1 = sum(set_complete$diabSR[set_complete[,ctg]<=key]=='1')/sum(set_complete[,ctg]<=key)
	sr2 = sum(set_complete$diabSR[set_complete[,ctg]>key]=='1')/sum(set_complete[,ctg]>key)
	
	return(c(bio1,bio2,sr1,sr2))
	
}