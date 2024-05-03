##################################
###MRP FUNCTION FOR SURVEY DATA###
##################################

#clearing Workspace
rm(list=ls(all=TRUE))

#Working Directory
setwd("/Users/lillyamirjavadi/Desktop/capstone/Replications")
#Packages
#install.packages("lme4")
library(lme4)
#install.packages("boot")
library(boot)
#install.packages("foreign")
library(foreign)

###############
###FUNCTIONS###
###############

#ICC
icc.function<-function(x,y){
  temp<-VarCorr(lmer(x~(1|y)))
  attributes(temp$y)$stddev^2/(attributes(temp)$sc^2+attributes(temp$y)$stddev^2)
}

#Warning Indicator from Multilevel Model
catch.warning <- function(expr) { 
  W <- NULL 
  w.handler <- function(w){ # warning handler 
    W <<- w 
    invokeRestart("muffleWarning") 
  } 
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler), warning = W) 
} 

#########
###MRP###
#########

#Function
mrping<-function(x, y){
  
  #Creating Object for Storage
  simsXvalues<-n.sims*length(n.values)
  correlations<-cbind(rep(NA, simsXvalues), rep(NA, simsXvalues), rep(NA, simsXvalues), rep(NA, simsXvalues), rep(NA, simsXvalues),rep(NA, simsXvalues) )
  maes<-cbind(rep(NA, simsXvalues), rep(NA, simsXvalues), rep(NA, simsXvalues))
  
  #Dataset
  new.data<-x
  
  #True state value
  truth<<-tapply(new.data$y, new.data$state, function(x) mean(x,na.rm=T))
  
  #Starting Counter
  counter<-0
  
  #Loops Over N and Iterations
  for (k in 1:n.sims) {
    for (i in n.values) {
      
      #Updating Counter
      counter<-counter+1
      
      #Sampling a Given Number of Respondents
      sample.data<-new.data[sample(nrow(new.data), i, replace=T),]
      
      #Multilevel Regression
      # temp<-catch.warning(lmer(y ~ 
      #   pvote +                                                                   
      #   religcon+
      #   (1|age) +
      #   (1|educ) +
      #   (1|gXr) +
      #   (1|stateid)+
      #   (1|region), 
      #   family=binomial(link="logit"),
      #   data=sample.data
      # ))
      
      model_fit_result <- tryCatch({
        list(success = TRUE, model = glmer(y ~ pvote + religcon + (1|age) + (1|educ) + (1|gXr) + (1|stateid) + (1|region), 
                                           family = binomial(link = "logit"), 
                                           data = sample.data))
      }, error = function(e) {
        list(success = FALSE, message = e$message)
      })
      
      # After model fitting attempt
      if (model_fit_result$success) {
        # Proceed with extracting fixed and random effects from model_fit_result$model
        b.intercept <- fixef(model_fit_result$model)[1]
        b.pres <- fixef(model_fit_result$model)[2]
        b.religcon <- fixef(model_fit_result$model)[3]
        # Rest of your code for successful model fitting
      } else {
        # Handle the error case
        cat("Error in model fitting: ", model_fit_result$message, "\n")
        # Skipping the iteration or handling error
        next
      }
      
      model<-model_fit_result$value
      model.warn<-ifelse(is.null(model_fit_result$warning)==T, 0, 1)
      
      #Fixed Effects
      b.intercept<-fixef(model)[1]
      b.pres<-fixef(model)[2]
      b.religcon<-fixef(model)[3]
          
      #Random Effects
      a.age<-ranef(model)$age[,1]
      a.educ<-ranef(model)$educ[,1]
      a.gXr<-ranef(model)$gXr[,1]
      a.region<-ranef(model)$region[,1]
      a.stateid<-cbind(rownames(ranef(model)$stateid), ranef(model)$stateid[,1])
      
      #Predicted Value for Each Profile
      L<-nrow(census.data)
      p<-rep(NA, L)
      for (l in 1:L) {
        p[l]<-b.intercept+
          b.pres*census.data$pvote[l]+
          b.religcon*census.data$religcon[l]+  
          a.age[census.data$age[l]]+
          a.educ[census.data$educ[l]]+
          a.gXr[census.data$gXr[l]]+
          a.region[census.data$region[l]]+
          ifelse(identical(a.stateid[,2][a.stateid[,1]==census.data$stateid[l]], character(0))==T, 0, as.numeric(a.stateid[,2][a.stateid[,1]==census.data$stateid[l]])) 
      }
      p<-exp(p)/(1+exp(p))
      
      #MRP State Ideology
      mrp.state.lc<<-NA
      mrp.data<-as.data.frame(cbind(census.data,p))
      for (j in 1:length(unique(newdata$stateid))){
        mrp.state.lc[j]<<-sum(mrp.data$n[mrp.data$stateid==j]*mrp.data$p[mrp.data$stateid==j])/sum(mrp.data$n[mrp.data$stateid==j])
      }          
      
      #Disaggregated Mean - National Mean
      dm.state.lc<-rep(NA, length(unique(newdata$stateid)))
      dm.warn<-0
      for (j in 1:length(unique(newdata$stateid))) {
        dm.state.lc[j]<-ifelse(length(sample.data$y[sample.data$stateid==j])>0, mean(sample.data$y[sample.data$stateid==j]), mean(sample.data$y))
        dm.warn<-ifelse(length(sample.data$y[sample.data$stateid==j])==0, dm.warn+1, dm.warn)
      }
      
      #Saving Results
      results<<-cbind(names(truth), truth, mrp.state.lc, dm.state.lc, z, k, i, model.warn, dm.warn, icc.value, mcfaddens, state.r2)
      write.table(results, file=filename, append=T, sep=",", col.names=F) 
    }
  }
}




