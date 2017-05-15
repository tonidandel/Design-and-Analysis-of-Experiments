# Functions to calculate sample size for several situations

source('calcN_tost.R')


# Calculate number of samples for pilot study based on d value 

#TODO: fix infinite loop for betas too high
calcN_pilotD = function(p_alpha = 0.05,
                        p_beta = 0.2,
                        p_type = 'one-sample',
                        p_alternative = 'two-sided',
                        p_d)
{
  if(p_alternative == 'two-sided'){
    v_alphaDiv = 2
  }
  else{
    v_alphaDiv = 1
  }
  
  if(p_type == 'one-sample'){
    v_multiplier = 2
  }
  else if(p_type == 'paired'){
    v_multiplier = 1
  }
  
  v_tAlpha = qnorm(1 - p_alpha/v_alphaDiv)
  v_tBeta = qnorm(1 - p_beta)
  v_n = ceiling(v_multiplier * p_d^2 * (v_tAlpha + v_tBeta)^2)
  
  v_iterationWithoutChange = 0
  
  while(v_iterationWithoutChange < 5){
    # TODO: check if df is correct (2n?)
    v_df = v_n - 1
    v_tAlpha = qt(1 - p_alpha/v_alphaDiv, v_df)
    v_tBeta = qt(1 - p_beta, v_df)
    v_nNew = ceiling(v_multiplier * p_d^2 * (v_tAlpha + v_tBeta)^2)
    
    if(v_nNew == v_n){
      v_iterationWithoutChange = v_iterationWithoutChange+1
    }
    else{
      v_iterationWithoutChange = 0
    }
    v_n = v_nNew
  }
  
  return(v_n)
}