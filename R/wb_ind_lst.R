wb_ind_lst <- function(dat_loc = paste0("http://datatopics.worldbank.org/",
                                         "universal-health-coverage/",
                                         "coronavirus/covid-indicators.json"),
                        save_file = FALSE,
                        save_dir = 'data'
)
{
  require(jsonlite)
  require(readr)
  require(tibble)
  
  ## Read from local file option ##
  
  if(grepl("*\\.csv|rds", tolower(dat_loc)) & !grepl('http', tolower(dat_loc)))
  {
    # Return and stop #
    
    return(read_csv(dat_loc))
    
  }else
  {
    if(grepl("*\\.json", tolower(dat_loc)))
    {
      ## Save file, if specified ##
      
      if(save_file)
      {
        j_raw <- download.file(dat_loc, paste0(save_dir, 
                                               gsub(".*/", "/", dat_loc)))
        
        j_dat <- read_json(paste0(save_dir, 
                                  gsub(".*/", "/", dat_loc)))
      }else
      {
        j_dat <- read_json(dat_loc)
      }
      
      ## Clean up data ##
      
      dat <- j_dat %>% 
        map(~ .x %>% 
              .$indicators %>% 
              do.call(rbind, .) %>% 
              data.frame() %>% 
              rownames_to_column(., "id") %>% 
              rename(description = 2) %>% 
              data.frame(topic = .x$topic, .) %>% 
              as_tibble() %>% 
              mutate_all(as.character)) %>% 
        bind_rows() %>% 
        mutate(topic = as.factor(topic))
      
      ## Save CSV, if specified ##
      
      if(save_file)
      {
        
        write_csv(dat, paste0(save_dir, 
                              gsub(".*/", "/", gsub("\\.json", ".csv", 
                                                    dat_loc))))
      }
      
    }
    return(dat)
  }
}
                        