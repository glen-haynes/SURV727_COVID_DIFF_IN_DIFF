cmpnd_avg_growth <- function(series, start_val, n)
{
  
  from_start <- series[series >= start_val]
  
  if(missing(n))
  {
    n <- (length(from_start) - 1)
  }
  
  if(length(from_start))
  {
    round(
      ((from_start[length(from_start)]/
       start_val)^(1/n)-1) * 100
       , 2)
  }else
  {
    NA
  }
}