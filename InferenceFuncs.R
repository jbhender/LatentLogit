## Inference methods for Latent-Logit ##


## Method 1: replace Z with P(Z|Y)

## Method 2: Impute Z from P(Z|Y) and use empirical variance
imputeZ = function(Y,pZ_1,pZ_0){
  rbinom(length(Y),1,ifelse(Y==1,pZ_1,pZ_0))
}

## Method 3: Impute Z from P(Z|Y) and use imputation standard errors
