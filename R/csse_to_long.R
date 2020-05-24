csse_to_long <- function(csse_dat,
                         series,
                         piv_ptrn = "\\d+/\\d+/\\d+",
                         date_format = "%m/%d/%y")
{
  csse_dat %>% 
    filter(grepl(series, Series)) %>% 
    pivot_longer(cols = matches(piv_ptrn),
                 names_to = "Date") %>% 
    
    ## Convert dates to R date type. ##
      mutate(Date = as.Date(Date, format = date_format))
}

tmpChck <- csse_to_long(csse_global_merge, 
                        "*")
