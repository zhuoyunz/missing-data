# missing-data
Data used in this project is from China Health and Retirement Longitudinal Study (CHARLS):http://charls.ccer.edu.cn/en .

CHARLS study is a survey of the mid-age and elderly in China, based on a sample of households with members aged 45 years or above. The goal of this project is to impute missing clinical diabetes status based on self-reported diabetes status and other health-related information.

This project did a comparison between three imputation methods for missing clinical diabetes in terms of imputation quality and post-analysis results. All imputations are accomplished with the help of MICE package in R.  

impute_together.r, impute_prop.r and impute_nopred.r implement these three methods, respectively.

post_check_analysis.r contains functions used in the this three imputation methods.

imput_data.r prepares the dataframe used in other files.

  
