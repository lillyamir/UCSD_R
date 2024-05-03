library(glmnet)
dat <- read.csv("/Users/lillyamirjavadi/Desktop/recidivism_data_sample.csv")
dat$race <- factor(dat$race)
dat$sex <- factor(dat$sex)



#########################################
#c(-1,-2) gets rid of id var and y variable
x <- data.matrix(dat[,-c(1,2)])
y <- as.vector(dat$recidivate)
n.total <- length(y)
prop.train <- 0.7
set.seed(1)
r <- sample(1:n.total,round(prop.train*n.total), replace = FALSE)
x.train <- x[r,]
x.test <- x[-r,]
y.train <- y[r]
y.test <- y[-r]
#####################################################

mod <- cv.glmnet(x = x.train, y = y.train, nfold= 5, family = 'binomial',alpha = 1)

coef(mod)




x.train <- as.data.frame(x.train)

linear <- lm(y ~ sex + age + juv_fel_count + priors_count + charge_name, dat = x.train)
linear

##Model Evaluation
mse.linear <- mean((linear$model$y - predict(linear))^2)
mse.linear


newdat <- data.frame(outcome = linear$model$y, predprob = predict(linear, type = "response"))
newdat

t <- 0.6
newdat$class <- as.numeric(newdat$predprob >= t)

mean(newdat$class == newdat$outcome)

table(newdat$class)

FP <- sum(newdat$outcome == 0 & newdat$class==1)
FP

FN <- sum(newdat$outcome == 1 & newdat$class==0)

TN <- sum(newdat$outcome == 0 & newdat$class==0)

TP <- sum(newdat$outcome == 1 & newdat$class==1)


Precision <- TP/(TP +FP)
Precision

Recall <- TP / (TP + FN)
Recall

Accuracy <- (TP + TN) / (TP + TN + FP + FN)
Accuracy



