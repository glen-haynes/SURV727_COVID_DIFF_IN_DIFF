csse_merge <- function(dat, 
                      fltr_ptrn,
                      wide_field = "Series", 
                      dat_form = "wide")
{
  require(dplyr)
  require(purrr)
  
  dat_form<- match.arg(dat_form, c("wide", "long"))
  
  ## Select tables to join by filtering data list ##
  
  ## Case when data elements are long form ##
  
  if(dat_form == 'long')
  {  
    ## Find data list elements with fltr_ptrn in column names ##
    
    ptrn_in_col_nms <- unlist(map(dat, function(x)
    {
      any(grepl(fltr_ptrn, {colnames(x)}))
    }))
    
    ## Collect desired elements from data list ## 
    
    dat <- dat[ptrn_in_col_nms]
    
    ## Use map and reduce from purrr package to join element lists by common ##
    ## column names. ##
    
    dat <- dat %>%
      {join_vars <- intersect(colnames(.[[1]]), colnames(.[[2]]));
      map(., function(x){x}) %>% 
        reduce(full_join, by = join_vars)}
    
    return(dat)
  }
  
  ## Case when data elements are wide form ##
  
  if(dat_form == 'wide')
  {
    ptrn_in_col <- unlist(map(dat, function(x)
    {
      grepl(fltr_ptrn, x[1,wide_field])
    }))
    
    ## Collect desired elements from data list ## 
    
    dat <- dat[ptrn_in_col]
    
    dat <- bind_rows(dat)
  }
  
}