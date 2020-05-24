implement_fit_easylinear <- function(value)
{
  
  ## Return NA if all zero vector ##
  if(sum(value) == 0)
  {
    return(list(NA, NA))
  }
  
  ## Cut-off data after count has stabilized. ##
  
  max_vals <- which(value == max(value))
  
  value <- value[1:max_vals[1]]
  
  ## Return NA if not more than 1 value greater than zero. ##
  
  if(!sum(value == 0) > 1)
  {
    value <- value[value > 0]
        fit <- fit_easylinear(0:(length(value) - 1), value)
        return(list(round((fit@fit$coefficients[[2]] * 100), 2), 
                    round(fit@rsquared[[1]], 2)))
  }else
  {
    return(list(NA, NA))
  }
}