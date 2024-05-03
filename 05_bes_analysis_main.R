
#########################
### BES analysis pt 1 ###
#########################

### Install packages if necessary
# install.packages("foreign")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("MASS")
# install.packages("dplyr")
# install.packages("stargazer")
# install.packages("data.table")
# install.packages("ebal")
# install.packages("cobalt")

library(foreign)
library(ggplot2)
library(reshape2)
library(MASS)
library(dplyr)
library(stargazer)
library(data.table)
library(ebal)
library(cobalt)

# Set working directory
setwd("/Users/lillyamirjavadi/Desktop/POLI271")

### Load data
load("d_bes_w14_fin.RData")


### -------------------------------------------------------------------------------------------- ###
### ----------------------------- MODELS FOR HAS UNSECURED DEBT -------------------------------- ###
### -------------------------------------------------------------------------------------------- ###


###############
### Turnout ###
###############

data_ebal_v1 <- na.omit(d_bes_w14_fin[ , .(turnout,has_debt,educ,unemployed,male,retired,married,age,
                                           gross_pers_inc_log,homeowner,union_member,children_in_hh,
                                           student,profile_socgrade) ]) 

# Expand data
dummy.educ <- model.matrix( ~ educ - 1, data_ebal_v1)

dummy.profile_socgrade <- model.matrix( ~ profile_socgrade - 1, data_ebal_v1)

colnames(dummy.profile_socgrade) <- gsub(" ","_",colnames(dummy.profile_socgrade))

data_ebal_v1_fin <- as.data.table(cbind(data_ebal_v1, dummy.educ, dummy.profile_socgrade))

# Omitted baseline: routine_occupations
covar_ebal_v1_fin <- c("unemployed", "male", "retired", "married", "age", "gross_pers_inc_log", "homeowner", 
                       "educA_level","educBelow_GCSE","educGCSE","educPostgrad","educUndergraduate","union_member","children_in_hh",
                       "profile_socgradea", "profile_socgradeb", "profile_socgradeC1", "profile_socgradeC2","profile_socgraded")

d_covar_ebal_v1_fin <- data_ebal_v1_fin[ , covar_ebal_v1_fin, with=F]

# Treatment
tr_ebal_v1 <- as.numeric(as.character(data_ebal_v1_fin[ , has_debt ]))

# Get weights
out_ebal_v1 <- ebalance(Treatment=tr_ebal_v1, X=d_covar_ebal_v1_fin)

# Trim
out_ebal_v1_trim <- ebalance.trim(out_ebal_v1)

data_ebal_v1_fin[ has_debt == "0" , ebal.weights := out_ebal_v1_trim$w ]   # weights for control units
data_ebal_v1_fin[ has_debt == "1" , ebal.weights := 1 ]                    # no weights for treatment units, assign 1

# Model
mod1_has_debt.1 <- glm(turnout ~ has_debt + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + union_member + student + children_in_hh + profile_socgrade, data_ebal_v1_fin, 
                       family="quasibinomial", weights = data_ebal_v1_fin$ebal.weights)


### -----------
### Table 3 Panel A
### -----------

## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
install.packages("stargazer")
library(stargazer)
# Unload stargazer if loaded
detach("package:stargazer",unload=T)
# Delete it
remove.packages("stargazer")
# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
# Save back
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
install.packages("stargazer", repos = NULL, type="source")
library(stargazer)


#############################
### Turnout with controls ###
#############################

data_ebal_v2 <- na.omit(d_bes_w14_fin[ , .(turnout,has_debt,educ,unemployed,male,retired,married,age,
                                           gross_pers_inc_log,homeowner,union_member,children_in_hh,student, 
                                           cutsTooFarLocal_num, cutsTooFarNational_num, risk_unemp, 
                                           econ_retro_pers_decline, profile_socgrade) ]) 

# Expand data
dummy.educ <- model.matrix( ~ educ - 1, data_ebal_v2)

dummy.profile_socgrade <- model.matrix( ~ profile_socgrade - 1, data_ebal_v2)

colnames(dummy.profile_socgrade) <- gsub(" ","_",colnames(dummy.profile_socgrade))

data_ebal_v2_fin <- as.data.table(cbind(data_ebal_v2,dummy.educ, dummy.profile_socgrade))

covar_ebal_v2_fin <- c("unemployed", "male", "retired", "married", "age", "gross_pers_inc_log", "homeowner", 
                       "educA_level","educBelow_GCSE","educGCSE","educPostgrad","educUndergraduate","union_member","children_in_hh",
                       "profile_socgradea", "profile_socgradeb", "profile_socgradeC1", "profile_socgradeC2","profile_socgraded")

d_covar_ebal_v2_fin <- data_ebal_v2_fin[ , covar_ebal_v2_fin, with=F]

# Treatment
tr_ebal_v2 <- as.numeric(as.character(data_ebal_v2_fin[ , has_debt ]))

# Get weights
out_ebal_v2 <- ebalance(Treatment=tr_ebal_v2, X=d_covar_ebal_v2_fin)

# Trim
out_ebal_v2_trim <- ebalance.trim(out_ebal_v2)

data_ebal_v2_fin[ has_debt == "0" , ebal.weights := out_ebal_v2_trim$w ]   # weights for control units
data_ebal_v2_fin[ has_debt == "1" , ebal.weights := 1 ]                  # no weights for treatment units, assign 1

# Model
mod1_has_debt.2 <- glm(turnout ~ has_debt + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + male + 
                         married + unemployed + retired + gross_pers_inc_log + homeowner + union_member + student + 
                         children_in_hh + profile_socgrade, data_ebal_v2_fin, family="quasibinomial", weights = data_ebal_v2_fin$ebal.weights)

mod1_has_debt.3 <- glm(turnout ~ has_debt + risk_unemp + econ_retro_pers_decline + age + educ + male + married + 
                         unemployed + retired + gross_pers_inc_log + homeowner + union_member + student + children_in_hh + 
                         profile_socgrade, data_ebal_v2_fin, family="quasibinomial", weights = data_ebal_v2_fin$ebal.weights)

### Collect terms
d_out_has_debt_turnout <- as.data.table(rbind(
  c((cbind(coef(mod1_has_debt.1), confint(mod1_has_debt.1, level=0.95), confint(mod1_has_debt.1, level=0.9)))[2,], names(coef(mod1_has_debt.1)[2]), names(mod1_has_debt.1$model)[1], ctr="adjusted"), 
  c((cbind(coef(mod1_has_debt.2), confint(mod1_has_debt.2, level=0.95), confint(mod1_has_debt.2, level=0.9)))[2,], names(coef(mod1_has_debt.2)[2]), names(mod1_has_debt.2$model)[1], ctr="adjusted ctr welfare"), 
  c((cbind(coef(mod1_has_debt.3), confint(mod1_has_debt.3, level=0.95), confint(mod1_has_debt.3, level=0.9)))[2,], names(coef(mod1_has_debt.3)[2]), names(mod1_has_debt.3$model)[1], ctr="adjusted ctr risk")
))

d_out_has_debt_turnout[ , 1:5 := lapply(.SD, function(x) as.numeric(as.character(x)) ), .SDcols= 1:5 ]
names(d_out_has_debt_turnout) <- c("coef","lower95","upper95","lower90","upper90","type","dv","ctr")

d_out_has_debt_turnout[ , party := "Turnout" ]



###################
### Vote choice ###
###################

data_ebal_v3 <- na.omit(d_bes_w14_fin[ , .(vote_Conservative, has_debt, vote_LibDems,vote_Labour, vote_UKIP,educ,unemployed,male,retired,married,age,
                                            gross_pers_inc_log,homeowner,union_member,children_in_hh,student,profile_socgrade) ]) 

# Expand data
dummy.educ <- model.matrix( ~ educ - 1, data_ebal_v3)

dummy.profile_socgrade <- model.matrix( ~ profile_socgrade - 1, data_ebal_v3)

colnames(dummy.profile_socgrade) <- gsub(" ","_",colnames(dummy.profile_socgrade))

data_ebal_v3_fin <- cbind(data_ebal_v3,dummy.educ,dummy.profile_socgrade)

covar_ebal_v3_fin <- c("unemployed", "male", "retired", "married", "age", "gross_pers_inc_log", "homeowner", 
                       "educA_level","educBelow_GCSE","educGCSE","educPostgrad","educUndergraduate","union_member","children_in_hh",
                       "profile_socgradea", "profile_socgradeb", "profile_socgradeC1", "profile_socgradeC2","profile_socgraded")

d_covar_ebal_v3_fin <- data_ebal_v3_fin[ , covar_ebal_v3_fin, with=F]

# Treatment
tr_ebal_v3 <- as.numeric(as.character(data_ebal_v3_fin[ , has_debt ]))

# Get weights
out_ebal_v3 <- ebalance(Treatment=tr_ebal_v3, X=d_covar_ebal_v3_fin)

# Trim
out_ebal_v3_trim <- ebalance.trim(out_ebal_v3)

data_ebal_v3_fin[ has_debt == "0" , ebal.weights := out_ebal_v3_trim$w ]   # weights for control units
data_ebal_v3_fin[ has_debt == "1" , ebal.weights := 1 ]                  # no weights for treatment units, assign 1

### Models
mod3_has_debt.1 <- glm(vote_Conservative ~ has_debt + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v3_fin, 
                       family="quasibinomial", weights=data_ebal_v3_fin$ebal.weights)

mod3_has_debt.2 <- glm(vote_Labour ~ has_debt + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v3_fin, 
                       family="quasibinomial", weights=data_ebal_v3_fin$ebal.weights)

mod3_has_debt.3 <- glm(vote_LibDems ~ has_debt + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v3_fin, 
                       family="quasibinomial", weights=data_ebal_v3_fin$ebal.weights)

mod3_has_debt.4 <- glm(vote_UKIP ~ has_debt + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v3_fin, 
                       family="quasibinomial", weights=data_ebal_v3_fin$ebal.weights)


### For balance plot
d_bal_tab1 <- bal.tab(has_debt ~ age + educ + male + married + unemployed + retired + gross_pers_inc_log +
                        homeowner + union_member + student + children_in_hh + profile_socgrade,
                      data = data_ebal_v3_fin, weights = "ebal.weights", un = TRUE)

d_bal_tab1.1 <- as.data.table(d_bal_tab1$Balance, rownames(d_bal_tab1$Balance))
d_bal_tab1.1[ , dv := "has_debt" ]

names(d_bal_tab1.1) <- c("covars","type","Unadjusted","Adjusted","dv")

cvar_names <- c("Age","Education: no qualifications","Education: A-Level","Education: less GCSE","Education: GCSE","Education: Postgraduate","Education: Undergraduate",
                "Male","Married", "Unemployed","Retired","Income (log)","Homeowner","Union member","Student","Children in household",
                "Not working (class)", "Upper middle class","Middle middle class","Lower middle class","Skilled working class","Working class")

d_bal_tab1.1[ , covars_name := cvar_names ]



#################################
### Vote choice with controls ###
#################################

data_ebal_v4 <- na.omit(d_bes_w14_fin[ , .(vote_Conservative, vote_LibDems,vote_Labour, vote_UKIP,has_debt,educ,
                                           unemployed,male,retired,married,age,gross_pers_inc_log,homeowner,union_member,
                                           children_in_hh,student, cutsTooFarLocal_num, cutsTooFarNational_num, risk_unemp, 
                                           econ_retro_pers_decline, profile_socgrade) ]) 

# Expand data
dummy.educ <- model.matrix( ~ educ - 1, data_ebal_v4)

dummy.profile_socgrade <- model.matrix( ~ profile_socgrade - 1, data_ebal_v4)

colnames(dummy.profile_socgrade) <- gsub(" ","_",colnames(dummy.profile_socgrade))

data_ebal_v4_fin <- cbind(data_ebal_v4,dummy.educ,dummy.profile_socgrade)

covar_ebal_v4_fin <- c("unemployed", "male", "retired", "married", "age", "gross_pers_inc_log", "homeowner", 
                       "educA_level","educBelow_GCSE","educGCSE","educPostgrad","educUndergraduate","union_member","children_in_hh",
                       "profile_socgradea", "profile_socgradeb", "profile_socgradeC1", "profile_socgradeC2","profile_socgraded")

d_covar_ebal_v4_fin <- data_ebal_v4_fin[ , covar_ebal_v4_fin, with=F]

# Treatment
tr_ebal_v4 <- as.numeric(as.character(data_ebal_v4_fin[ , has_debt ]))

# Get weights
out_ebal_v4 <- ebalance(Treatment=tr_ebal_v4, X=d_covar_ebal_v4_fin)

# Trim
out_ebal_v4_trim <- ebalance.trim(out_ebal_v4)

data_ebal_v4_fin[ has_debt == "0" , ebal.weights := out_ebal_v4_trim$w ]   # weights for control units
data_ebal_v4_fin[ has_debt == "1" , ebal.weights := 1 ]                  # no weights for treatment units, assign 1


### Models
mod4_has_debt.1 <- glm(vote_Conservative ~ has_debt + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v4_fin, family="quasibinomial", weights=data_ebal_v4_fin$ebal.weights)

mod4_has_debt.2 <- glm(vote_Labour ~ has_debt + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v4_fin, family="quasibinomial", weights=data_ebal_v4_fin$ebal.weights)

mod4_has_debt.3 <- glm(vote_LibDems ~ has_debt + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v4_fin, family="quasibinomial", weights=data_ebal_v4_fin$ebal.weights)

mod4_has_debt.4 <- glm(vote_UKIP ~ has_debt + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v4_fin, family="quasibinomial", weights=data_ebal_v4_fin$ebal.weights)


mod4_has_debt.5 <- glm(vote_Conservative ~ has_debt + risk_unemp + econ_retro_pers_decline + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v4_fin, family="quasibinomial", weights=data_ebal_v4_fin$ebal.weights)

mod4_has_debt.6 <- glm(vote_Labour ~ has_debt + risk_unemp + econ_retro_pers_decline + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v4_fin, family="quasibinomial", weights=data_ebal_v4_fin$ebal.weights)

mod4_has_debt.7 <- glm(vote_LibDems ~ has_debt + risk_unemp + econ_retro_pers_decline + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v4_fin, family="quasibinomial", weights=data_ebal_v4_fin$ebal.weights)

mod4_has_debt.8 <- glm(vote_UKIP ~ has_debt + risk_unemp + econ_retro_pers_decline + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + student + union_member + children_in_hh + profile_socgrade, data_ebal_v4_fin, family="quasibinomial", weights=data_ebal_v4_fin$ebal.weights)

### Collect terms
d_out_has_debt_vote <- as.data.table(rbind(
  c((cbind(coef(mod3_has_debt.1), confint(mod3_has_debt.1, level=0.95), confint(mod3_has_debt.1, level=0.9)))[2,], names(coef(mod3_has_debt.1)[2]), names(mod3_has_debt.1$model)[1], ctr="adjusted"), 
  c((cbind(coef(mod3_has_debt.2), confint(mod3_has_debt.2, level=0.95), confint(mod3_has_debt.2, level=0.9)))[2,], names(coef(mod3_has_debt.2)[2]), names(mod3_has_debt.2$model)[1], ctr="adjusted"), 
  c((cbind(coef(mod3_has_debt.3), confint(mod3_has_debt.3, level=0.95), confint(mod3_has_debt.3, level=0.9)))[2,], names(coef(mod3_has_debt.3)[2]), names(mod3_has_debt.3$model)[1], ctr="adjusted"), 
  c((cbind(coef(mod3_has_debt.4), confint(mod3_has_debt.4, level=0.95), confint(mod3_has_debt.4, level=0.9)))[2,], names(coef(mod3_has_debt.4)[2]), names(mod3_has_debt.4$model)[1], ctr="adjusted"), 
  
  c((cbind(coef(mod4_has_debt.1), confint(mod4_has_debt.1, level=0.95), confint(mod4_has_debt.1, level=0.9)))[2,], names(coef(mod4_has_debt.1)[2]), names(mod4_has_debt.1$model)[1], ctr="adjusted ctr welfare"), 
  c((cbind(coef(mod4_has_debt.2), confint(mod4_has_debt.2, level=0.95), confint(mod4_has_debt.2, level=0.9)))[2,], names(coef(mod4_has_debt.2)[2]), names(mod4_has_debt.2$model)[1], ctr="adjusted ctr welfare"), 
  c((cbind(coef(mod4_has_debt.3), confint(mod4_has_debt.3, level=0.95), confint(mod4_has_debt.3, level=0.9)))[2,], names(coef(mod4_has_debt.3)[2]), names(mod4_has_debt.3$model)[1], ctr="adjusted ctr welfare"), 
  c((cbind(coef(mod4_has_debt.4), confint(mod4_has_debt.4, level=0.95), confint(mod4_has_debt.4, level=0.9)))[2,], names(coef(mod4_has_debt.4)[2]), names(mod4_has_debt.4$model)[1], ctr="adjusted ctr welfare"), 
  
  c((cbind(coef(mod4_has_debt.5), confint(mod4_has_debt.5, level=0.95), confint(mod4_has_debt.5, level=0.9)))[2,], names(coef(mod4_has_debt.5)[2]), names(mod4_has_debt.5$model)[1], ctr="adjusted ctr risk"), 
  c((cbind(coef(mod4_has_debt.6), confint(mod4_has_debt.6, level=0.95), confint(mod4_has_debt.6, level=0.9)))[2,], names(coef(mod4_has_debt.6)[2]), names(mod4_has_debt.6$model)[1], ctr="adjusted ctr risk"), 
  c((cbind(coef(mod4_has_debt.7), confint(mod4_has_debt.7, level=0.95), confint(mod4_has_debt.7, level=0.9)))[2,], names(coef(mod4_has_debt.7)[2]), names(mod4_has_debt.7$model)[1], ctr="adjusted ctr risk"), 
  c((cbind(coef(mod4_has_debt.8), confint(mod4_has_debt.8, level=0.95), confint(mod4_has_debt.8, level=0.9)))[2,], names(coef(mod4_has_debt.8)[2]), names(mod4_has_debt.8$model)[1], ctr="adjusted ctr risk")
))

gc()

d_out_has_debt_vote[ , 1:5 := lapply(.SD, function(x) as.numeric(as.character(x)) ), .SDcols= 1:5 ]
names(d_out_has_debt_vote) <- c("coef","lower95","upper95","lower90","upper90","type","dv","ctr")

d_out_has_debt_vote[ , party := gsub("vote_","",dv) ]
d_out_has_debt_vote[ , party := gsub("2","",party) ]
d_out_has_debt_vote[ party == "Conservative" , party := "Tories" ]

### Combine data
d_out_has_debt_all <- rbind(d_out_has_debt_vote, d_out_has_debt_turnout)

d_out_has_debt_all[ , party := factor(party, levels=c("Tories","Labour","LibDems","UKIP","Turnout"))]



### ----------------------------------------------------------------------------------------------- ###
### ----------------------------- MODELS FOR BORROW FOR ESSENTIALS -------------------------------- ###
### ----------------------------------------------------------------------------------------------- ###


###############
### Turnout ###
###############

data_ebal_v5 <- na.omit(d_bes_w14_fin[ , .(turnout,borrow_essentials,educ,unemployed,male,retired,married,age,
                                           gross_pers_inc_log,homeowner,union_member,children_in_hh,student,
                                           profile_socgrade) ]) 

# Expand data
dummy.educ <- model.matrix( ~ educ - 1, data_ebal_v5)

dummy.profile_socgrade <- model.matrix( ~ profile_socgrade - 1, data_ebal_v5)

colnames(dummy.profile_socgrade) <- gsub(" ","_",colnames(dummy.profile_socgrade))

data_ebal_v5_fin <- cbind(data_ebal_v5, dummy.educ, dummy.profile_socgrade)

covar_ebal_v5_fin <- c("unemployed", "male", "retired", "married", "age", "gross_pers_inc_log", "homeowner", 
                       "educA_level","educBelow_GCSE","educGCSE","educPostgrad","educUndergraduate","union_member","children_in_hh",
                       "profile_socgradea", "profile_socgradeb", "profile_socgradeC1", "profile_socgradeC2","profile_socgraded")
                       
d_covar_ebal_v5_fin <- data_ebal_v5_fin[ , covar_ebal_v5_fin, with=F]

# Treatment
tr_ebal_v5 <- as.numeric(as.character(data_ebal_v5_fin[ , borrow_essentials ]))

# Get weights
out_ebal_v5 <- ebalance(Treatment=tr_ebal_v5, X=d_covar_ebal_v5_fin)

# Trim
out_ebal_v5_trim <- ebalance.trim(out_ebal_v5)

data_ebal_v5_fin[ borrow_essentials == "0" , ebal.weights := out_ebal_v5_trim$w ]   # weights for control units
data_ebal_v5_fin[ borrow_essentials == "1" , ebal.weights := 1 ]                    # no weights for treatment units, assign 1

# Model
mod1_borrow_essentials.1 <- glm(turnout ~ borrow_essentials + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                                  homeowner + union_member + student + children_in_hh + profile_socgrade, data_ebal_v5_fin, 
                                family="quasibinomial", weights = data_ebal_v5_fin$ebal.weights)




#############################
### Turnout with controls ###
#############################

data_ebal_v6 <- na.omit(d_bes_w14_fin[ , .(turnout,borrow_essentials,educ,unemployed,male,retired,married,age,
                                           gross_pers_inc_log,homeowner,union_member,children_in_hh,student, 
                                           cutsTooFarLocal_num, cutsTooFarNational_num, risk_unemp, 
                                           econ_retro_pers_decline, profile_socgrade) ]) 

# Expand data
dummy.educ <- model.matrix( ~ educ - 1, data_ebal_v6)

dummy.profile_socgrade <- model.matrix( ~ profile_socgrade - 1, data_ebal_v6)

colnames(dummy.profile_socgrade) <- gsub(" ","_",colnames(dummy.profile_socgrade))

data_ebal_v6_fin <- cbind(data_ebal_v6,dummy.educ, dummy.profile_socgrade)


covar_ebal_v6_fin <- c("unemployed", "male", "retired", "married", "age", "gross_pers_inc_log", "homeowner", 
                       "educA_level","educBelow_GCSE","educGCSE","educPostgrad","educUndergraduate","union_member","children_in_hh",
                       "profile_socgradea", "profile_socgradeb", "profile_socgradeC1", "profile_socgradeC2","profile_socgraded")

d_covar_ebal_v6_fin <- data_ebal_v6_fin[ , covar_ebal_v6_fin, with=F]

# Treatment
tr_ebal_v6 <- as.numeric(as.character(data_ebal_v6_fin[ , borrow_essentials ]))

# Get weights
out_ebal_v6 <- ebalance(Treatment=tr_ebal_v6, X=d_covar_ebal_v6_fin)

# Trim
out_ebal_v6_trim <- ebalance.trim(out_ebal_v6)

data_ebal_v6_fin[ borrow_essentials == "0" , ebal.weights := out_ebal_v6_trim$w ]   # weights for control units
data_ebal_v6_fin[ borrow_essentials == "1" , ebal.weights := 1 ]                  # no weights for treatment units, assign 1

# Models
mod1_borrow_essentials.2 <- glm(turnout ~ borrow_essentials + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + male + 
                                  married + unemployed + retired + gross_pers_inc_log + homeowner + union_member + student + 
                                  children_in_hh + profile_socgrade, data_ebal_v6_fin, family="quasibinomial", weights = data_ebal_v6_fin$ebal.weights)

mod1_borrow_essentials.3 <- glm(turnout ~ borrow_essentials + risk_unemp + econ_retro_pers_decline + age + educ + male + married + 
                                  unemployed + retired + gross_pers_inc_log + homeowner + union_member + student + children_in_hh + 
                                  profile_socgrade, data_ebal_v6_fin, family="quasibinomial", weights = data_ebal_v6_fin$ebal.weights)

### Collect terms
d_out_borrow_ess_turnout <- as.data.table(rbind(
  c((cbind(coef(mod1_borrow_essentials.1), confint(mod1_borrow_essentials.1, level=0.95), confint(mod1_borrow_essentials.1, level=0.9)))[2,], names(coef(mod1_borrow_essentials.1)[2]), names(mod1_borrow_essentials.1$model)[1], ctr="adjusted"), 
  c((cbind(coef(mod1_borrow_essentials.2), confint(mod1_borrow_essentials.2, level=0.95), confint(mod1_borrow_essentials.2, level=0.9)))[2,], names(coef(mod1_borrow_essentials.2)[2]), names(mod1_borrow_essentials.2$model)[1], ctr="adjusted ctr welfare"), 
  c((cbind(coef(mod1_borrow_essentials.3), confint(mod1_borrow_essentials.3, level=0.95), confint(mod1_borrow_essentials.3, level=0.9)))[2,], names(coef(mod1_borrow_essentials.3)[2]), names(mod1_borrow_essentials.3$model)[1], ctr="adjusted ctr risk")
))

d_out_borrow_ess_turnout[ , 1:5 := lapply(.SD, function(x) as.numeric(as.character(x)) ), .SDcols= 1:5 ]
names(d_out_borrow_ess_turnout) <- c("coef","lower95","upper95","lower90","upper90","type","dv","ctr")

d_out_borrow_ess_turnout[ , party := "Turnout" ]



###################
### Vote choice ###
###################

data_ebal_v7 <- na.omit(d_bes_w14_fin[ , .(vote_Conservative, borrow_essentials, vote_LibDems,vote_Labour,
                                           vote_UKIP,educ,unemployed,male,retired,married,age,gross_pers_inc_log
                                           ,homeowner,union_member,children_in_hh,student,profile_socgrade) ]) 

# Expand data
dummy.educ <- model.matrix( ~ educ - 1, data_ebal_v7)

dummy.profile_socgrade <- model.matrix( ~ profile_socgrade - 1, data_ebal_v7)

colnames(dummy.profile_socgrade) <- gsub(" ","_",colnames(dummy.profile_socgrade))

data_ebal_v7_fin <- cbind(data_ebal_v7,dummy.educ,dummy.profile_socgrade)


covar_ebal_v7_fin <- c("unemployed", "male", "retired", "married", "age", "gross_pers_inc_log", "homeowner", 
                       "educA_level","educBelow_GCSE","educGCSE","educPostgrad","educUndergraduate","union_member","children_in_hh",
                       "profile_socgradea", "profile_socgradeb", "profile_socgradeC1", "profile_socgradeC2","profile_socgraded")

d_covar_ebal_v7_fin <- data_ebal_v7_fin[ , covar_ebal_v7_fin, with=F]


# Treatment
tr_ebal_v7 <- as.numeric(as.character(data_ebal_v7_fin[ , borrow_essentials ]))

# Get weights
out_ebal_v7 <- ebalance(Treatment=tr_ebal_v7, X=d_covar_ebal_v7_fin)

# Trim
out_ebal_v7_trim <- ebalance.trim(out_ebal_v7)

data_ebal_v7_fin[ borrow_essentials == "0" , ebal.weights := out_ebal_v7_trim$w ]   # weights for control units
data_ebal_v7_fin[ borrow_essentials == "1" , ebal.weights := 1 ]                  # no weights for treatment units, assign 1


### Models
mod3_borrow_essentials.1 <- glm(vote_Conservative ~ borrow_essentials + age + educ + male + married + unemployed + 
                                  retired + gross_pers_inc_log + homeowner + student + union_member + children_in_hh + 
                                  profile_socgrade, data_ebal_v7_fin, family="quasibinomial", weights=data_ebal_v7_fin$ebal.weights)

mod3_borrow_essentials.2 <- glm(vote_Labour ~ borrow_essentials + age + educ + male + married + unemployed + retired + 
                                  gross_pers_inc_log + homeowner + student + union_member + children_in_hh + 
                                  profile_socgrade, data_ebal_v7_fin, family="quasibinomial", weights=data_ebal_v7_fin$ebal.weights)

mod3_borrow_essentials.3 <- glm(vote_LibDems ~ borrow_essentials + age + educ + male + married + unemployed + retired + 
                                  gross_pers_inc_log + homeowner + student + union_member + children_in_hh + 
                                  profile_socgrade, data_ebal_v7_fin, family="quasibinomial", weights=data_ebal_v7_fin$ebal.weights)

mod3_borrow_essentials.4 <- glm(vote_UKIP ~ borrow_essentials + age + educ + male + married + unemployed + retired + 
                                  gross_pers_inc_log + homeowner + student + union_member + children_in_hh + 
                                  profile_socgrade, data_ebal_v7_fin, family="quasibinomial", weights=data_ebal_v7_fin$ebal.weights)


### For balance plot
d_bal_tab2 <- bal.tab(borrow_essentials ~ age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                        homeowner + union_member + student + children_in_hh + profile_socgrade, 
                      data = data_ebal_v7_fin, weights = "ebal.weights", un = TRUE)

d_bal_tab2.1 <- as.data.table(d_bal_tab2$Balance, rownames(d_bal_tab2$Balance))
d_bal_tab2.1[ , dv := "borrow_essentials" ]

names(d_bal_tab2.1) <- c("covars","type","Unadjusted","Adjusted","dv")

cvar_names <- c("Age","Education: no qualifications","Education: A-Level","Education: less GCSE","Education: GCSE","Education: Postgraduate","Education: Undergraduate",
                "Male","Married", "Unemployed","Retired","Income (log)","Homeowner","Union member","Student","Children in household",
                "Not working (class)", "Upper middle class","Middle middle class","Lower middle class","Skilled working class","Working class")

d_bal_tab2.1[ , covars_name := cvar_names ]



####SIMULATION FOR THE PROJECT - LILLY####

set.seed(1234)

n <- nrow(data_ebal_v7_fin)
# Calculate probabilities for binary variables
prob_male <- mean(data_ebal_v7_fin$male, na.rm = TRUE)
prob_married <- mean(data_ebal_v7_fin$married, na.rm = TRUE)
prob_unemployed <- mean(data_ebal_v7_fin$unemployed, na.rm = TRUE)
prob_retired <- mean(data_ebal_v7_fin$retired, na.rm = TRUE)
prob_homeowner <- mean(data_ebal_v7_fin$homeowner, na.rm = TRUE)
prob_union_member <- mean(data_ebal_v7_fin$union_member, na.rm = TRUE)
prob_student <- mean(data_ebal_v7_fin$student, na.rm = TRUE)
prob_children_in_hh <- mean(data_ebal_v7_fin$children_in_hh, na.rm = TRUE)
prob_borrow_essentials <- mean(as.numeric(as.character(data_ebal_v7_fin$borrow_essentials)) == 1, na.rm = TRUE) # Ensure correct calculation if stored as factor/character

# Continue for other variables as needed

sim_data <- data.frame(
  male = rbinom(n, 1, prob = prob_male),
  married = rbinom(n, 1, prob = prob_married),
  unemployed = rbinom(n, 1, prob = prob_unemployed),
  retired = rbinom(n, 1, prob = prob_retired),
  homeowner = rbinom(n, 1, prob = prob_homeowner),
  union_member = rbinom(n, 1, prob = prob_union_member),
  student = rbinom(n, 1, prob = prob_student),
  children_in_hh = rbinom(n, 1, prob = prob_children_in_hh),
  borrow_essentials = rbinom(n, 1, prob = prob_borrow_essentials)
)

sim_data$age <- rnorm(n, mean = mean(data_ebal_v7_fin$age, na.rm = TRUE), sd = sd(data_ebal_v7_fin$age, na.rm = TRUE))
sim_data$gross_pers_inc_log <- rnorm(n, mean = mean(data_ebal_v7_fin$gross_pers_inc_log, na.rm = TRUE), sd = sd(data_ebal_v7_fin$gross_pers_inc_log, na.rm = TRUE))

# Simulating categorical variables (adjust levels and probabilities as needed)
sim_data$educ <- factor(sample(c("Below_GCSE", "GCSE", "A_level", "Undergraduate"), size = n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))) # Example, adjust as needed
sim_data$profile_socgrade <- factor(sample(c("a", "b", "C1", "C2", "d"), size = n, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))) # Example, adjust as needed


#################################
### Vote choice with controls ###
#################################

data_ebal_v8 <- na.omit(d_bes_w14_fin[ , .(vote_Conservative, vote_LibDems,vote_Labour,vote_UKIP,borrow_essentials,
                                           educ,unemployed,male,retired,married,age,gross_pers_inc_log,homeowner,
                                           union_member,children_in_hh,student,cutsTooFarLocal_num,cutsTooFarNational_num,
                                           risk_unemp,econ_retro_pers_decline, profile_socgrade) ]) 

# Expand data
dummy.educ <- model.matrix( ~ educ - 1, data_ebal_v8)

dummy.profile_socgrade <- model.matrix( ~ profile_socgrade - 1, data_ebal_v8)

colnames(dummy.profile_socgrade) <- gsub(" ","_",colnames(dummy.profile_socgrade))

data_ebal_v8_fin <- cbind(data_ebal_v8,dummy.educ,dummy.profile_socgrade)

covar_ebal_v8_fin <- c("unemployed", "male", "retired", "married", "age", "gross_pers_inc_log", "homeowner", 
                       "educA_level","educBelow_GCSE","educGCSE","educPostgrad","educUndergraduate","union_member","children_in_hh",
                       "profile_socgradea", "profile_socgradeb", "profile_socgradeC1", "profile_socgradeC2","profile_socgraded")

d_covar_ebal_v8_fin <- data_ebal_v8_fin[ , covar_ebal_v8_fin, with=F]

# Treatment
tr_ebal_v8 <- as.numeric(as.character(data_ebal_v8_fin[ , borrow_essentials ]))

# Get weights
out_ebal_v8 <- ebalance(Treatment=tr_ebal_v8, X=d_covar_ebal_v8_fin)

# Trim
out_ebal_v8_trim <- ebalance.trim(out_ebal_v8)

data_ebal_v8_fin[ borrow_essentials == "0" , ebal.weights := out_ebal_v8_trim$w ]   # weights for control units
data_ebal_v8_fin[ borrow_essentials == "1" , ebal.weights := 1 ]                  # no weights for treatment units, assign 1


### Models
mod4_borrow_essentials.1 <- glm(vote_Conservative ~ borrow_essentials + cutsTooFarLocal_num + cutsTooFarNational_num + age + 
                                  educ + male + married + unemployed + retired + gross_pers_inc_log + homeowner + student + 
                                  union_member + children_in_hh + profile_socgrade, data_ebal_v8_fin, family="quasibinomial", 
                                weights=data_ebal_v8_fin$ebal.weights)

mod4_borrow_essentials.2 <- glm(vote_Labour ~ borrow_essentials + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + 
                                  male + married + unemployed + retired + gross_pers_inc_log + homeowner + student + union_member + 
                                  children_in_hh + profile_socgrade, data_ebal_v8_fin, family="quasibinomial", weights=data_ebal_v8_fin$ebal.weights)

mod4_borrow_essentials.3 <- glm(vote_LibDems ~ borrow_essentials + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + 
                                  male + married + unemployed + retired + gross_pers_inc_log + homeowner + student + union_member + 
                                  children_in_hh + profile_socgrade, data_ebal_v8_fin, family="quasibinomial", weights=data_ebal_v8_fin$ebal.weights)

mod4_borrow_essentials.4 <- glm(vote_UKIP ~ borrow_essentials + cutsTooFarLocal_num + cutsTooFarNational_num + age + educ + male + 
                                  married + unemployed + retired + gross_pers_inc_log + homeowner + student + union_member + 
                                  children_in_hh + profile_socgrade, data_ebal_v8_fin, family="quasibinomial", weights=data_ebal_v8_fin$ebal.weights)


mod4_borrow_essentials.5 <- glm(vote_Conservative ~ borrow_essentials + risk_unemp + econ_retro_pers_decline + age + educ + 
                                  male + married + unemployed + retired + gross_pers_inc_log + homeowner + student + union_member + 
                                  children_in_hh + profile_socgrade, data_ebal_v8_fin, family="quasibinomial", weights=data_ebal_v8_fin$ebal.weights)

mod4_borrow_essentials.6 <- glm(vote_Labour ~ borrow_essentials + risk_unemp + econ_retro_pers_decline + age + educ + male + 
                                  married + unemployed + retired + gross_pers_inc_log + homeowner + student + union_member + 
                                  children_in_hh + profile_socgrade, data_ebal_v8_fin, family="quasibinomial", weights=data_ebal_v8_fin$ebal.weights)

mod4_borrow_essentials.7 <- glm(vote_LibDems ~ borrow_essentials + risk_unemp + econ_retro_pers_decline + age + educ + male + 
                                  married + unemployed + retired + gross_pers_inc_log + homeowner + student + union_member +
                                  children_in_hh + profile_socgrade, data_ebal_v8_fin, family="quasibinomial", weights=data_ebal_v8_fin$ebal.weights)

mod4_borrow_essentials.8 <- glm(vote_UKIP ~ borrow_essentials + risk_unemp + econ_retro_pers_decline + age + educ + male + 
                                  married + unemployed + retired + gross_pers_inc_log + homeowner + student + union_member + 
                                  children_in_hh + profile_socgrade, data_ebal_v8_fin, family="quasibinomial", weights=data_ebal_v8_fin$ebal.weights)

### Collect terms
d_out_borrow_ess_vote <- as.data.table(rbind(
  c((cbind(coef(mod3_borrow_essentials.1), confint(mod3_borrow_essentials.1, level=0.95), confint(mod3_borrow_essentials.1, level=0.9)))[2,], names(coef(mod3_borrow_essentials.1)[2]), names(mod3_borrow_essentials.1$model)[1], ctr="adjusted"), 
  c((cbind(coef(mod3_borrow_essentials.2), confint(mod3_borrow_essentials.2, level=0.95), confint(mod3_borrow_essentials.2, level=0.9)))[2,], names(coef(mod3_borrow_essentials.2)[2]), names(mod3_borrow_essentials.2$model)[1], ctr="adjusted"), 
  c((cbind(coef(mod3_borrow_essentials.3), confint(mod3_borrow_essentials.3, level=0.95), confint(mod3_borrow_essentials.3, level=0.9)))[2,], names(coef(mod3_borrow_essentials.3)[2]), names(mod3_borrow_essentials.3$model)[1], ctr="adjusted"), 
  c((cbind(coef(mod3_borrow_essentials.4), confint(mod3_borrow_essentials.4, level=0.95), confint(mod3_borrow_essentials.4, level=0.9)))[2,], names(coef(mod3_borrow_essentials.4)[2]), names(mod3_borrow_essentials.4$model)[1], ctr="adjusted"), 
  
  c((cbind(coef(mod4_borrow_essentials.1), confint(mod4_borrow_essentials.1, level=0.95), confint(mod4_borrow_essentials.1, level=0.9)))[2,], names(coef(mod4_borrow_essentials.1)[2]), names(mod4_borrow_essentials.1$model)[1], ctr="adjusted ctr welfare"), 
  c((cbind(coef(mod4_borrow_essentials.2), confint(mod4_borrow_essentials.2, level=0.95), confint(mod4_borrow_essentials.2, level=0.9)))[2,], names(coef(mod4_borrow_essentials.2)[2]), names(mod4_borrow_essentials.2$model)[1], ctr="adjusted ctr welfare"), 
  c((cbind(coef(mod4_borrow_essentials.3), confint(mod4_borrow_essentials.3, level=0.95), confint(mod4_borrow_essentials.3, level=0.9)))[2,], names(coef(mod4_borrow_essentials.3)[2]), names(mod4_borrow_essentials.3$model)[1], ctr="adjusted ctr welfare"), 
  c((cbind(coef(mod4_borrow_essentials.4), confint(mod4_borrow_essentials.4, level=0.95), confint(mod4_borrow_essentials.4, level=0.9)))[2,], names(coef(mod4_borrow_essentials.4)[2]), names(mod4_borrow_essentials.4$model)[1], ctr="adjusted ctr welfare"), 
  
  c((cbind(coef(mod4_borrow_essentials.5), confint(mod4_borrow_essentials.5, level=0.95), confint(mod4_borrow_essentials.5, level=0.9)))[2,], names(coef(mod4_borrow_essentials.5)[2]), names(mod4_borrow_essentials.5$model)[1], ctr="adjusted ctr risk"), 
  c((cbind(coef(mod4_borrow_essentials.6), confint(mod4_borrow_essentials.6, level=0.95), confint(mod4_borrow_essentials.6, level=0.9)))[2,], names(coef(mod4_borrow_essentials.6)[2]), names(mod4_borrow_essentials.6$model)[1], ctr="adjusted ctr risk"), 
  c((cbind(coef(mod4_borrow_essentials.7), confint(mod4_borrow_essentials.7, level=0.95), confint(mod4_borrow_essentials.7, level=0.9)))[2,], names(coef(mod4_borrow_essentials.7)[2]), names(mod4_borrow_essentials.7$model)[1], ctr="adjusted ctr risk"), 
  c((cbind(coef(mod4_borrow_essentials.8), confint(mod4_borrow_essentials.8, level=0.95), confint(mod4_borrow_essentials.8, level=0.9)))[2,], names(coef(mod4_borrow_essentials.8)[2]), names(mod4_borrow_essentials.8$model)[1], ctr="adjusted ctr risk")
))

d_out_borrow_ess_vote[ , 1:5 := lapply(.SD, function(x) as.numeric(as.character(x)) ), .SDcols= 1:5 ]
names(d_out_borrow_ess_vote) <- c("coef","lower95","upper95","lower90","upper90","type","dv","ctr")

d_out_borrow_ess_vote[ , party := gsub("vote_","",dv) ]
d_out_borrow_ess_vote[ , party := gsub("2","",party) ]
d_out_borrow_ess_vote[ party == "Conservative" , party := "Tories" ]

### Combine
d_out_borrow_ess_all <- rbind(d_out_borrow_ess_vote, d_out_borrow_ess_turnout)

d_out_borrow_ess_all[ , party := factor(party, levels=c("Tories","Labour","LibDems","UKIP","Turnout"))]



### -----------------------------------------------------
### Combine has debt and borrowing for essentials output
### -----------------------------------------------------

comb_out <- rbind(d_out_has_debt_all, d_out_borrow_ess_all)

comb_out[ type == "has_debt" , type2 := "Has unsecured debt" ]
comb_out[ type == "borrow_essentials" , type2 := "Borrow for essentials" ]

comb_out[ party == "LibDems" , party := "Lib Dems"]
comb_out[ , party := factor(party, levels=c("UKIP","Lib Dems","Labour","Tories","Turnout"))]


### -----------
### Figure 4
### -----------

cairo_pdf("Output/plots/figure4.pdf", width=4.75, height=3)
(plot <- (ggplot(data=comb_out[ ctr == "adjusted" ], aes(y=coef, x=party, group=type2, colour=type2, shape=type2, fill=type2))
          + geom_hline(yintercept=0, linetype="dashed",colour="gray50")
          + geom_errorbar(aes(ymin=lower95, ymax=upper95), width=0, size=0.5, show.legend = F)  
          + geom_errorbar(aes(ymin=lower90, ymax=upper90), width=0, size=1, show.legend = F)  
          + geom_point(size=2.6, colour="black", show.legend = F)
          + theme_bw()
          + facet_wrap(~ type2)
          + ylab("Effect of debt on turnout and vote choice")
          + coord_flip()
          + theme(legend.position="bottom", panel.grid.minor=element_blank(), axis.title.y = element_blank(),
                  panel.border = element_rect(color="gray"))
          + scale_y_continuous(breaks=seq(-1,1,0.5), limits=c(-1,1))
          + scale_color_manual("Sample", values=c("#0066cc","#ff3333","#006600"))
          + scale_fill_manual("Sample", values=c("#1a8cff","#ff8080","#009900"))
          + scale_shape_manual("Sample", values=c(21,23,22))
))
dev.off()


### -----------
### SI Figure D.1
### -----------

d_bal_tab_comb <- rbind(d_bal_tab1.1,d_bal_tab2.1)

d_bal_tab_comb[ dv == "has_debt" , dv2 := "Has unsecured debt" ]
d_bal_tab_comb[ dv == "borrow_essentials" , dv2 := "Borrow for essentials" ]

d_bal_tab_comb_l <- melt(d_bal_tab_comb, id.vars = c("covars","covars_name","dv","dv2","type"))

cairo_pdf("Output/plots/si_figure_d1.pdf", width=6.5, height=4.5)
(plot <- (ggplot(d_bal_tab_comb_l, aes(y=value, x=reorder(covars_name, -value), shape=variable, fill=variable))
          + geom_hline(yintercept=0, linetype="dashed",colour="gray50")
          + geom_point(size=2)
          + theme_bw()
          + facet_wrap(~ dv2, scales = "free_x")
          + ylab("Mean difference")
          + xlab("")
          + theme(legend.position="bottom", panel.grid.minor=element_blank(), panel.grid.major=element_blank(), 
                  panel.border = element_rect(color="gray"))
          + coord_flip()
          + scale_color_manual("Sample", values=c("red","blue"))
          + scale_fill_manual("Sample", values=c("red","blue"))
          + scale_shape_manual("Sample", values=c(23,21))
))
dev.off()


#########################
### Regression tables ###
#########################

### -----------
### Table 3 Panel A
### -----------

stargazer(mod1_borrow_essentials.3, mod4_borrow_essentials.5, mod4_borrow_essentials.6,
          mod4_borrow_essentials.7, mod4_borrow_essentials.8,
          omit="factor", omit.stat=c("ser","F","Wald","lr","logrank"), digits.extra=0, digits=2,
          align=T, no.space=T, type="text", column.sep.width = "-10pt", 
          keep = c("borrow_essentials","risk_unemp","econ_retro_pers_decline"), 
          dep.var.labels = c("Turnout","Tories","Labour","Lib Dems","UKIP"),
         covariate.labels = c("Borrow for essentials","Risk of unemployment","Financial situation got worse"),
          add.lines = list(c("Mean DV",
                             round(mean(mod1_borrow_essentials.3$fitted.values),2),
                             round(mean(mod4_borrow_essentials.5$fitted.values),2),
                             round(mean(mod4_borrow_essentials.6$fitted.values),2),
                             round(mean(mod4_borrow_essentials.7$fitted.values),2),
                             round(mean(mod4_borrow_essentials.8$fitted.values),2)),
                           c("Controls","Y","Y","Y","Y","Y")),
          title = "Effect of Borrow for Essentials on Turnout and Vote Choice")



### -----------
### Table 3 Panel B
### -----------

stargazer(mod1_borrow_essentials.2, mod4_borrow_essentials.1, mod4_borrow_essentials.2,
          mod4_borrow_essentials.3, mod4_borrow_essentials.4,
          omit="factor", omit.stat=c("ser","F","Wald","lr","logrank"), digits.extra=0, digits=2,
          align=T, no.space=T, type="text", column.sep.width = "-10pt",  
          keep = c("borrow_essentials","cutsTooFarLocal_num","cutsTooFarNational_num"), 
          dep.var.labels = c("Turnout","Tories","Labour","Lib Dems","UKIP"),
          covariate.labels = c("Borrow for essentials","Cuts to local services gone too far","Cuts to public spending gone too far"),
          add.lines = list(c("Mean DV",
                             round(mean(mod1_borrow_essentials.2$fitted.values),2),
                             round(mean(mod4_borrow_essentials.1$fitted.values),2),
                             round(mean(mod4_borrow_essentials.2$fitted.values),2),
                             round(mean(mod4_borrow_essentials.3$fitted.values),2),
                             round(mean(mod4_borrow_essentials.4$fitted.values),2)),
                           c("Controls","Y","Y","Y","Y","Y")),
          title = "Effect of Borrow for Essentials on Turnout and Vote Choice")


exp(0.29)


### -----------
### SI Table D.2
### -----------

covars_list1 <- c("Borrow for essentials","Age","Education: A-Level","Education: less GCSE","Education: GCSE","Education: Postgraduate",
                  "Education: Undergraduate","Male","Married", "Unemployed","Retired","Income (log)","Homeowner","Union member",
                  "Student","Children in household","Upper middle class","Middle middle class","Lower middle class","Skilled working class",
                  "Working class")

stargazer(mod1_borrow_essentials.1, mod3_borrow_essentials.1, mod3_borrow_essentials.2, mod3_borrow_essentials.3, mod3_borrow_essentials.4,
          omit="factor", omit.stat=c("ser","F","Wald","lr","logrank"), digits.extra=0, digits=2, 
          align=T, no.space=T, type="text", column.sep.width = "-10pt", 
          dep.var.labels = c("Turnout","Tories","Labour","Lib Dems","UKIP"),
          covariate.labels = covars_list1,
          add.lines = list(c("Mean DV",
                             round(mean(mod1_borrow_essentials.1$fitted.values),2), 
                             round(mean(mod3_borrow_essentials.1$fitted.values),2), 
                             round(mean(mod3_borrow_essentials.2$fitted.values),2), 
                             round(mean(mod3_borrow_essentials.3$fitted.values),2), 
                             round(mean(mod3_borrow_essentials.4$fitted.values),2))),
          title = "Effect of Borrow for Essentials on Turnout and Vote Choice")


### -----------
### SI Table D.3
### -----------

covars_list2 <- c("Has unsecured debt","Age","Education: A-Level","Education: less GCSE","Education: GCSE",
                  "Education: Postgraduate","Education: Undergraduate","Male","Married", "Unemployed","Retired",
                  "Income (log)","Homeowner","Union member","Student","Children in household","Upper middle class",
                  "Middle middle class","Lower middle class","Skilled working class","Working class")

stargazer(mod1_has_debt.1, mod3_has_debt.1, mod3_has_debt.2, mod3_has_debt.3, mod3_has_debt.4,
          omit="factor", omit.stat=c("ser","F","Wald","lr","logrank"), digits.extra=0, digits=2, 
          align=T, no.space=T, type="text", column.sep.width = "-10pt",
          dep.var.labels = c("Turnout","Tories","Labour","Lib Dems","UKIP"),
          covariate.labels = covars_list2,
          add.lines = list(c("Mean DV",
                             round(mean(mod1_has_debt.1$fitted.values),2), 
                             round(mean(mod3_has_debt.1$fitted.values),2), 
                             round(mean(mod3_has_debt.2$fitted.values),2), 
                             round(mean(mod3_has_debt.3$fitted.values),2), 
                             round(mean(mod3_has_debt.4$fitted.values),2))),
          title = "Effect of Borrow for Essentials on Turnout and Vote Choice")


### -----------
### SI Table D.5
### -----------

covars_list3 <- c("Borrow for essentials","Risk of unemployment","Financial sit. got worse",
                  "Age","Education: A-Level","Education: less GCSE","Education: GCSE","Education: Postgraduate","Education: Undergraduate",
                  "Male","Married", "Unemployed","Retired","Income (log)","Homeowner","Union member","Student","Children in household",
                  "Upper middle class","Middle middle class","Lower middle class","Skilled working class","Working class")

stargazer(mod1_borrow_essentials.3, mod4_borrow_essentials.5, mod4_borrow_essentials.6,
          mod4_borrow_essentials.7, mod4_borrow_essentials.8,
          omit="factor", omit.stat=c("ser","F","Wald","lr","logrank"), digits.extra=0, digits=2,
          align=T, no.space=T, type="text", column.sep.width = "-10pt", 
          dep.var.labels = c("Turnout","Tories","Labour","Lib Dems","UKIP"),
          covariate.labels = covars_list3,
          add.lines = list(c("Mean DV",
                             round(mean(mod1_borrow_essentials.3$fitted.values),2),
                             round(mean(mod4_borrow_essentials.5$fitted.values),2),
                             round(mean(mod4_borrow_essentials.6$fitted.values),2),
                             round(mean(mod4_borrow_essentials.7$fitted.values),2),
                             round(mean(mod4_borrow_essentials.8$fitted.values),2))),
          title = "Effect of Borrow for Essentials on Turnout and Vote Choice")


### -----------
### SI Table D.6
### -----------

covars_list4 <- c("Borrow for essentials","Cuts to local services gone too far","Cuts to public spending gone too far",
                  "Age","Education: A-Level","Education: less GCSE","Education: GCSE","Education: Postgraduate","Education: Undergraduate",
                  "Male","Married", "Unemployed","Retired","Income (log)","Homeowner","Union member","Student","Children in household",
                  "Upper middle class","Middle middle class","Lower middle class","Skilled working class","Working class")

stargazer(mod1_borrow_essentials.2, mod4_borrow_essentials.1, mod4_borrow_essentials.2,
          mod4_borrow_essentials.3, mod4_borrow_essentials.4,
          omit="factor", omit.stat=c("ser","F","Wald","lr","logrank"), digits.extra=0, digits=2,
          align=T, no.space=T, type="text", column.sep.width = "-10pt", 
          dep.var.labels = c("Turnout","Tories","Labour","Lib Dems","UKIP"),
          covariate.labels = covars_list4, 
          add.lines = list(c("Mean DV",
                             round(mean(mod1_borrow_essentials.2$fitted.values),2),
                             round(mean(mod4_borrow_essentials.1$fitted.values),2),
                             round(mean(mod4_borrow_essentials.2$fitted.values),2),
                             round(mean(mod4_borrow_essentials.3$fitted.values),2),
                             round(mean(mod4_borrow_essentials.4$fitted.values),2))))




### -----------------------------------------
### Robustness check: models with debt amount 
### -----------------------------------------

d_bes_w14_fin[ , debt_amt_ihs := log(debt_amt + sqrt(1+debt_amt^2)) ]

mod1_debt_amt.1 <- glm(turnout ~ debt_amt_ihs + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + union_member + student + children_in_hh + profile_socgrade, d_bes_w14_fin, family="quasibinomial")

mod1_debt_amt.2 <- glm(vote_Conservative ~ debt_amt_ihs + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + union_member + student + children_in_hh + profile_socgrade, d_bes_w14_fin, family="quasibinomial")

mod1_debt_amt.3 <- glm(vote_Labour ~ debt_amt_ihs + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + union_member + student + children_in_hh + profile_socgrade, d_bes_w14_fin, family="quasibinomial")

mod1_debt_amt.4 <- glm(vote_LibDems ~ debt_amt_ihs + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + union_member + student + children_in_hh + profile_socgrade, d_bes_w14_fin, family="quasibinomial")

mod1_debt_amt.5 <- glm(vote_UKIP ~ debt_amt_ihs + age + educ + male + married + unemployed + retired + gross_pers_inc_log + 
                         homeowner + union_member + student + children_in_hh + profile_socgrade, d_bes_w14_fin, family="quasibinomial")

mod1_debt_amt_covars <- c("Unsecured debt (log)","Age","Education: A-Level","Education: less GCSE","Education: GCSE","Education: Postgraduate",
                           "Education: Undergraduate","Male","Married", "Unemployed","Retired","Income (log)","Homeowner","Union member",
                           "Student","Children in household","Upper middle class","Middle middle class","Lower middle class","Skilled working class",
                           "Working class")

### ---------------
### SI Table D.4
### ---------------

stargazer(mod1_debt_amt.1, mod1_debt_amt.2, mod1_debt_amt.3,
          mod1_debt_amt.4, mod1_debt_amt.5,
          omit="factor", omit.stat=c("ser","F","Wald","lr","logrank"), digits.extra=0, digits=2, 
          align=T, no.space=T, type="text", column.sep.width = "-10pt",  
          dep.var.labels = c("Turnout","Tories","Labour","Lib Dems","UKIP"),
          covariate.labels = mod1_debt_amt_covars,
          add.lines = list(c("Mean DV",
                             round(mean(mod1_debt_amt.1$fitted.values),2), 
                             round(mean(mod1_debt_amt.2$fitted.values),2), 
                             round(mean(mod1_debt_amt.3$fitted.values),2), 
                             round(mean(mod1_debt_amt.4$fitted.values),2), 
                             round(mean(mod1_debt_amt.5$fitted.values),2))),
          title = "Effect of Debt Amount on Turnout and Vote Choice")
