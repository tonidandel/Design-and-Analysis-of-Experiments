# Functions to calculate sample size for several situations

source('calcN_tost.R')


# Calculate number of samples for pilot study based on d value 

#TODO: fix infinite loop for betas too high
calcN_pilotD = function(p_alpha = 0.05,
                        p_beta = 0.2,
                        p_type = 'two-sample',
                        p_alternative = 'two-sided',
                        p_d)
{
  if(p_alternative == 'two-sided'){
    v_alphaDiv = 2
  }
  else{
    v_alphaDiv = 1
  }
  
  if(p_type == 'two-sample'){
    v_multiplier = 2
  }
  else if(p_type == 'paired' || p_type == 'one-sample'){
    v_multiplier = 1
  }
  
  v_tAlpha = qnorm(1 - p_alpha/v_alphaDiv)
  v_tBeta = qnorm(1 - p_beta)
  v_n = (v_multiplier * p_d^2 * (v_tAlpha + v_tBeta)^2)
  
  v_iterationWithoutChange = 0
  
  while(v_iterationWithoutChange < 50){
    # TODO: check if df is correct (2n?)
    v_df = v_n - 1
    v_tAlpha = qt(1 - p_alpha/v_alphaDiv, v_df)
    v_tBeta = qt(1 - p_beta, v_df)
    v_nNew = (v_multiplier * p_d^2 * (v_tAlpha + v_tBeta)^2)
    
    if(v_nNew == v_n){
      v_iterationWithoutChange = v_iterationWithoutChange+1
    }
    else{
      v_iterationWithoutChange = 0
    }
    v_n = v_nNew
  }
  
  return(ceiling(v_n))
}

calcN = function(p_alpha = 0.05,
                  p_beta = 0.2,
                  p_type = 'two-sample',
                  p_alternative = 'two-sided',
                  p_sd,
                  p_delta
                  )
{
  if(p_alternative == 'two-sided'){
    v_alphaDiv = 2
  }
  else{
    v_alphaDiv = 1
  }
  
  if(p_type == 'two-sample'){
    v_multiplier = 2
  }
  else if(p_type == 'paired' || p_type == 'one-sample'){
    v_multiplier = 1
  }
  v_d = p_delta/p_sd
  
  v_tAlpha = qnorm(1 - p_alpha/v_alphaDiv)
  v_tBeta = qnorm(1 - p_beta)
  v_n = (v_multiplier * v_d^2 * (v_tAlpha + v_tBeta)^2)
  
  v_iterationWithoutChange = 0
  
  while(v_iterationWithoutChange < 50){
    # TODO: check if df is correct (2n?)
    v_df = v_n - 1
    v_tAlpha = qt(1 - p_alpha/v_alphaDiv, v_df)
    v_tBeta = qt(1 - p_beta, v_df)
    v_nNew = (v_multiplier * v_d^2 * (v_tAlpha + v_tBeta)^2)
    
    if(v_nNew == v_n){
      v_iterationWithoutChange = v_iterationWithoutChange+1
    }
    else{
      v_iterationWithoutChange = 0
    }
    v_n = v_nNew
  }
  
  return(ceiling(v_n))
}

calcN_oneVsAll = function(p_alpha,
                          p_beta,
                          p_alternative = 'one-sided',
                          p_k,
                          p_sd,
                          p_delta
)
{
  if(p_alternative == 'two-sided'){
    v_alphaDiv = 2
  }
  else{
    v_alphaDiv = 1
  }
  
  v_tAlpha = qnorm(1 - p_alpha/v_alphaDiv)
  v_tBeta = qnorm(1 - p_beta)
  v_n = ( (1 + 1/sqrt(p_k)) * ((v_tAlpha + v_tBeta)*p_sd/p_delta)^2)
  
  v_iterationWithoutChange = 0
  
  while(v_iterationWithoutChange < 20){
    # TODO: check if df is correct (2n?)
    v_df = (p_k+1)*(v_n - 1)
    v_tAlpha = qt(1 - p_alpha/v_alphaDiv, v_df)
    v_tBeta = qt(1 - p_beta, v_df)
    v_nNew = ( (1 + 1/sqrt(p_k)) * ((v_tAlpha + v_tBeta)*p_sd/p_delta)^2)
    
    if(v_nNew == v_n){
      v_iterationWithoutChange = v_iterationWithoutChange+1
    }
    else{
      v_iterationWithoutChange = 0
    }
    v_n = v_nNew
  }
  
  return(ceiling(v_n))
}