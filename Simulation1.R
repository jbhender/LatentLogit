## Simulations for the latent logit model:
#
#   {X, (B)} -> (Z) -> Y; assume P(Z|Y=1) known
#   
#   A - observed, (A) - unobserved
#  
#  Interested in inference for B

# P(Y=1|X) = P(Y=1|Z=1)*P(Z=1|X) + P(Y=1|Z=0)*P(Z=0|X)
# P(Y=0|X) = P(Y=0|Z=1)*P(Z=1|X) + P(Y=0|Z=0)*P(Z=0|X)

## Bayes' Rule ##
# P(Y|Z,X) = P(Z|Y,X)P(Y|X) / P(Z|X)
# P(Z|Y,X) = P(Y|Z,X)P(Z|X) / P(Y|X)
# P(Z|Y) = P(Y|Z)P(Z) / P(Y)
# P(Y|Z) = P(Z|Y)P(Y) / P(Z)
#
# P(Y) = P(Y|Z=0)P(Z=0) + P(Y|Z=1)P(Z=1)

# Inverse logit 
invLogit = function(x) 1/{1+exp(-x)}
# Simulate Y and Z given 
simYZ = function(X,B,pY_1,pY_0){
  Z = rbinom(nrow(X),1,invLogit(X%*%B))
  Y = rbinom(nrow(X),1,ifelse(Z==1,pY_1,pY_0))
  return(data.frame(Y=Y,Z=Z))
}

## Model 1: No covariates, case/control design

# simulation parameters
n = 1e3
pY_1 = .4
pY_0 = .005
# B[1,] - controls the baseline rate
# B[2,] - controls the treatment effect
B = matrix(c(-2,2),ncol=1)

# Design matrix
X = cbind(1,rep(0:1,each=n/2))

# Conditonals probabilities
invLogit(X[c(1,n/2+1),]%*%B)

simData = simYZ(X,B,pY_1,pY_0)

