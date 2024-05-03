####################################
###ANALYZING MONTE CARLO DATASETS###
####################################

#Clearing Workspace
rm(list=ls(all=TRUE))

#Working Directory
setwd("/Users/lillyamirjavadi/Desktop/capstone/Replications")
#Packages
library(lme4)
library(boot)

###############
###FUNCTIONS###
###############

#Warning Indicator from Multilevel Model
catch.warning <- function(expr) { 
  W <- NULL 
  w.handler <- function(w){ # warning handler 
    W <<- w 
    invokeRestart("muffleWarning") 
  } 
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler), warning = W) 
} 
                 
################
###SIMULATION###
################
                    
#Date Analysis Starts
date.d<-Sys.Date()
    
#Looping Through All Conditions
for (n.condition in c(1:4)) {
  
  #Data
  rfilename<-paste("data_", "condition", "_", n.condition, ".RData", sep="")
  load(rfilename)

  #########################
  ###SETUP FOR SIMUATION###
  #########################
  
  #Number of Iterations
  n.sims<-2
  
  #Sample Size
  n.values<-c(1500)
  
  #Instance number so can run multiple versions simultaneously
  instance.number<-1
  
  #Random number generator
  set.seed(instance.number)
  
  #Bootstrap number for confidence interval
  n.bootstraps<-10
  
  #File with Results
  filename<-paste("results", instance.number, "_", n.condition, "_", date.d, ".csv", sep="")
  names<-cbind("state", "truth", "mrp_estimate", "mrp_se", "mrp_low_norm", "mrp_high_norm", "mrp_low_basic", "mrp_high_basic", "mrp_low_perc", "mrp_high_perc","dm_estimate","dm_se","dm_low_norm", "dm_high_norm", "dm_low_basic", "dm_high_basic", "dm_low_perc", "dm_high_perc","n", "condition", "iteration",  "model_warning", "dm_missing", "icc", "mcfaddens", "state_r2") 
  write.table(names, file=filename, append=F, sep=",", col.names=F) 
  
  #File with Each Estimate from bootstrap
  boot.filename<-paste("bootresults", instance.number, "_", n.condition, "_", date.d, ".csv", sep="")
  boot.names<-cbind("condition", "iteration","state", rbind(paste("bs", seq(1:n.bootstraps), sep=""))) 
  write.table(boot.names, file=boot.filename, append=F, sep=",", col.names=F) 
  
  #Looping over number of iterations
  for (n.sim in c(1:n.sims)) {

  #Sample Data Set
  sample.data<-voter.data[sample(nrow(voter.data), n.values, replace=T),]
  
  #MRP Function
  mrp <- function(data, indices) {
    d <- sample.data[indices,] 
    
    #MRP Model
    temp<-catch.warning(glmer(y ~
      prox1 +
      (1|x1) +
      (1|x2) +
      (1|x3) +
      (1|state),
      family=binomial(link=logit),
      data=d
    ))
    model<-temp$value
    model.warn<<-ifelse(is.null(temp$warning)==T, 0, 1)
    
    #Fixed Effects
    b.intercept<-fixef(model)[1]
    b.prox1<-fixef(model)[2]
        
    #Random Effects
    a.x1<-ranef(model)$x1[,1]
    a.x2<-ranef(model)$x2[,1]
    a.x3<-ranef(model)$x3[,1]
    a.state<-cbind(rownames(ranef(model)$state), ranef(model)$state[,1])
        
    #Predicted Value for Each Profile
    L<-nrow(census.data)
    p<-rep(NA, L)
    for (l in 1:L) {
      p[l]<-b.intercept+
        b.prox1*census.data$prox1[l]+
        a.x1[census.data$x1[l]]+
        a.x2[census.data$x2[l]]+
        a.x3[census.data$x3[l]]+
        ifelse(identical(a.state[,2][a.state[,1]==census.data$state[l]], character(0))==T, 0, as.numeric(a.state[,2][a.state[,1]==census.data$state[l]])) 
    }
    p<-exp(p)/(1+exp(p))
    
    #MRP State Ideology
    mrp.state.y<-NA
    mrp.data<-as.data.frame(cbind(census.data,p))
    for (j in 1:50){
      mrp.state.y[j]<-sum(mrp.data$n[mrp.data$state==j]*mrp.data$p[mrp.data$state==j])/sum(mrp.data$n[mrp.data$state==j])
    }          
    
    return(mrp.state.y)
  } 
  
  #Bootstrapped MRP Esimates
  mrp.results <- boot(data=sample.data, statistic=mrp, R=n.bootstraps)
  mrp.state.y<-mrp.results$t0
  mrp.se<-apply(mrp.results$t, 2, sd)
  mrp.cis<-matrix(NA, 50, 6)
  for(i in 1:50) {
    temp<-boot.ci(mrp.results, type = c("norm", "basic", "perc"), index=i, conf=.90)
    ifelse(is.null(temp)==T, mrp.cis[i,]<-cbind(NA, NA,NA, NA,NA, NA), mrp.cis[i,]<-cbind(temp$normal[,2], temp$normal[,3], temp$basic[,4], temp$basic[,5], temp$perc[,4], temp$perc[,5]))
  }
  
  #Saving Bootstrapps Samples
  bootstrapped.results<-cbind(condition, n.sim, state, data.frame(t(mrp.results$t)) )
  write.table(bootstrapped.results, file=boot.filename, append=T, sep=",", col.names=F) 
  
  #Disaggregated Mean - National Mean
  dm.warn<-50-length(unique(sample.data$state))
  dm <- function(data, indices) {
    d <- sample.data[indices,] # allows boot to select sample 
    dm.state.y<-rep(NA, 50)
    for (j in 1:50) {
      dm.state.y[j]<-ifelse(length(d$y[d$state==j])>0, mean(d$y[d$state==j]), mean(d$y))
    }
    
    return(dm.state.y)
  } 
  dm.results <- boot(data=sample.data, statistic=dm, R=n.bootstraps)
  dm.state.y<-dm.results$t0
  dm.se<-apply(dm.results$t, 2, sd)
  dm.cis<-matrix(NA, 50, 6)
  for(i in 1:50) {
    temp<-boot.ci(dm.results, type = c("norm", "basic", "perc"), index=i, conf=.90)
    ifelse(is.null(temp)==T, dm.cis[i,]<-cbind(NA, NA,NA, NA,NA, NA), dm.cis[i,]<-cbind(temp$normal[,2], temp$normal[,3], temp$basic[,4], temp$basic[,5], temp$perc[,4], temp$perc[,5]))
  }
  
  
  #####################
  ###STORING RESULTS###
  #####################
  
  #Results
  results<<-cbind(state, state.y, mrp.state.y, mrp.se, mrp.cis, dm.state.y, dm.se, dm.cis, n.values, condition, n.sim, model.warn, dm.warn, icc.temp, mcfaddens.temp, state.r2.temp)
  
  #Saving Dataset for Each Condition
  write.table(results, file=filename, append=T, sep=",", col.names=F) 

}
}

print(icc.temp)

