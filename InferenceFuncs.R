## Inference methods for Latent-Logit ##

## packages ##
library(doParallel)

## Method 1: replace Z with P(Z|Y)
inferB_probs = function(dat,pZ_1,pZ_0){
  dat$Z = ifelse(dat$Y==1,pZ_1,pZ_0)
  dat$Y = NULL
  fit = glm(Z~.,data=dat,family=binomial(link='logit'),weights=rep(100,nrow(dat)))
  list(Bhat=coef(fit),seB=summary(fit)$coefficients[,2])
}
summary(fit)
## Method 1b: replicate sample 100x with approximate P(Z|Y) accordingly
inferB_repl = function(dat,pZ_1,pZ_0){
  repl = foreach(n=1:nrow(dat),.combine='rbind') %do% {
    out = dat[n,]
  }
  dat$Z = ifelse(dat$Y==1,pZ_1,pZ_0)
  dat$Y = NULL
  fit = glm(Z~.,data=dat,family=binomial(link='logit'))
  list(Bhat=coef(fit),seB=summary(fit)$coefficients[,2])
}

## Method 2: Impute Z from P(Z|Y) and use empirical variance
## Method 3: Impute Z from P(Z|Y) and use imputation standard errors
# share simulations, and parse se differently 
imputeZ = function(Y,pZ_1,pZ_0){
  rbinom(length(Y),1,ifelse(Y==1,pZ_1,pZ_0))
}

inferB_imputeZ = function(dat,pZ_1,pZ_0){
    dat$Z = imputeZ(dat$Y,pZ_1,pZ_0)
    dat$Y = NULL
    fit = glm(Z~.,data=dat,family=binomial(link='logit'))
    list(Bhat=coef(fit),seB=summary(fit)$coefficients[,2])
}

