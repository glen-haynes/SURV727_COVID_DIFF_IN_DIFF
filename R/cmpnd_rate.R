cmpnd_rate <- function(series, start_val)
{
  after_start <- after_start[after_start > start_val]
  
  if(length(after_start))
  {
    (after_start[length(after_start)]/
       start_val)^(1/(length(after_start) - 1))-1
  }else
  {
    NA
  }
}