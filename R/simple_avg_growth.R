simple_avg_growth <- function(series, start_val = 1, n)
{
  if(any(series >= start_val))
  {
    from_start <- series[series >= start_val]
  }else
  {
    return(NA)
  }
 
 if(missing(n))
 {
   n <- (length(from_start) - 1)
 }
 
 rtrn <- round(
   (sum((from_start[2:length(from_start)]/
   from_start[1:(length(from_start) - 1)]) - 1) /
   n) * 100
   , 2)
 
 return(rtrn)
}