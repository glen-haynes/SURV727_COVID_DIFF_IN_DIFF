csse_covid_dat <- function(file_name = "time_series_covid19_confirmed_US.csv",
                           base_url = paste0("https://raw.githubusercontent.",
                                             "com/CSSEGISandData/COVID-19/",
                                             "master/csse_covid_19_data/",
                                             "csse_covid_19_time_series/"),
                           long_form = FALSE,
                           date_format = "%m/%d/%y",
                           use_saved_file = FALSE,
                           save_file = FALSE,
                           save_dir = 'data',
                           clean_nm_ptrn = "time_series_covid19_|\\.csv|\\.rds")
{
  require(readr)
  
  ## Save file, if specified ##
  
  if(save_file)
  {
    download.file(paste0(base_url, file_name), paste0(save_dir, "/",
                                                      file_name))
  }
  
  ## Read from local file option ##
  
  if(use_saved_file)
  {
    
    ## Read from file ##
    
    dat <- read_csv(paste0(save_dir, "/", file_name))
    
  }else
  {
    ## Read from web ##
    
    dat <- read_csv(paste0(base_url, file_name))
    
  }
  
  ## Convert to long form data, if specified ##
  
  if(long_form){
    
    
    
    rtrn_dat <- dat %>% pivot_longer(., cols = matches('\\d+/\\d+/\\d+'),
                                     names_to = 'Date') %>%
      rename_at(vars(contains('value')), funs(gsub(clean_nm_ptrn, "", 
                                                   tolower(file_name)))) %>%
      
      ## Convert dates to R date type. ##
      
      mutate(Date = as.Date(Date, format = date_format))
    
  }else
  {
    rtrn_dat <- dat  %>% mutate(., Series = gsub(clean_nm_ptrn, "", 
                                                 tolower(file_name)))
  }
  
  return(rtrn_dat) 
  
}
