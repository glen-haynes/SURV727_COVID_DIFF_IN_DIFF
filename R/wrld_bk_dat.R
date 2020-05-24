wrld_bk_dat <- function(dat_addrs = paste0("https://api.worldbank.org/v2/",
                                           "country?format=json&per_page=200"),
                        save_file = FALSE,
                        save_dir = 'data',
                        use_local_json = FALSE,
                        indicator,
                        date
                        
                        
)
{
  require(jsonlite)
  require(dplyr)
  require(readr)
  require(tibble)
  require(purrr)

  ## Pull data from full address string by default, allowing construct of ##
  ## JSON query string from indicator and date alternatively ##
  
  if(!missing(indicator))
  {
    if(!missing(dat_addrs) && !is.null(dat_addrs))
    {
      warning(paste0("Setting indicator variable overrides setting of html ", 
                     "string with dat_addrs."))
    }
    dat_addrs = paste0("https://api.worldbank.org/v2/country/all/indicator/",
                       indicator, "?format=json&per_page=200")
  }
  
  if(!missing(date))
  {
    if(missing(indicator))
    {
      warning(paste0("Only use date parameter with indicator parameter. ",
                     "Defaulting to html string in dat_addrs."))
    }else
    {
      dat_addrs <- paste0(dat_addrs, "&date=", date)
    }
  }

  ## Read from local file option ##
  
  if(grepl("*\\.csv|rds", tolower(dat_addrs)) & !grepl('http', tolower(dat_addrs)))
  {
    # Return and stop #
    
    return(read_csv(dat_addrs))
    
  }
  ## Save file, if specified ##
  
  if(save_file)
  {
    ## Download specified file ##
    
    j_raw <- download.file(dat_addrs, paste0(save_dir, 
                                           gsub("&per_page=200.*", "", 
                                             gsub("\\?format=", 
                                                paste0("-page-1", "."),
                                                gsub(".*/", "/", dat_addrs)))
    ))
    
    ## Load file ##
    
    j_dat_1 <- fromJSON(paste0(save_dir, 
                               gsub("&per_page=200.*", "", 
                                    gsub("\\?format=", 
                                         paste0("-page-1", "."),
                                         gsub(".*/", "/", dat_addrs)))
    ))
    
    ## Check if JSON contains multiple pages ##
    #  If so, loop through pages and collect data  #
    
    if(j_dat_1[[1]]$pages > 1)
    {
      
      ## Download specified file ##
      
      walk(2:j_dat_1[[1]]$pages,
           function(x)
           {
             download.file(paste0(dat_addrs, "&page=", x), 
                           paste0(save_dir, 
                                  gsub("&per_page=200.*", "", 
                                       gsub("\\?format=", 
                                            paste0("-page-", x, "."),
                                            gsub(".*/", "/", dat_addrs)))
                           ))
           })
      
      ## Read in data ##
      
      j_dat_lst <- map(2:j_dat_1[[1]]$pages,
                       function(x)
                       {
                         fromJSON(paste0(save_dir, 
                                         gsub("&per_page=200.*", "", 
                                              gsub("\\?format=", 
                                                   paste0("-page-", x, "."),
                                                   gsub(".*/", "/", dat_addrs)))
                         ))
                       })
      
      ## Combine pulled data ## 
      
      j_dat_lst <- c(list(j_dat_1), j_dat_lst)
    }else
    {
      ## Keep same list structure for single page files ##
      
      j_dat_lst <- list(j_dat_1)
      
    }
  }else
    ## If not saving downloads ##
    
  {
    
    ## Read in specified file ##
    
    j_dat_1 <- fromJSON(dat_addrs)

    ## Check if JSON contains multiple pages ##
    #  If so, loop through pages and collect data  #
    
    if(j_dat_1[[1]]$pages > 1)
    {
      
      ## If local files selected find all page files in directory ##
      
      if(use_local_json)
      {
        ## Get list of files in directory ##
        
        dir_files <- list.files(save_dir)
        
        # Get file name from dat_addrs parameter #
        
        file_name <- gsub("-page-\\d+|&page=\\d+|&per_page=\\d+|&date=.*", "", 
                          gsub(".*/(.*)(\\?format=|\\.)json", "\\1", dat_addrs))
        
        # Stop, if files not in folder #

        if(!paste0(file_name, ".json") %in% gsub("-page-\\d+",
                                                 "", dir_files))
        {
          stop(paste0("Argument 'use_local_json' set to true, but files are",
                      " not in ", save_dir, "."))
        }
        
        # Find files in directory #
        
        file_loc <- grep(paste0(file_name, ".json", 
                                "|", file_name, "-page-\\d+",
                                ".json"), dir_files)

        # Read in files from directory #
        
        j_dat_lst <- map(file_loc,
                         function(x)
                         {
                           fromJSON(paste0(save_dir, "/", dir_files[x]))
                         })
        
      }else
      {
        ## If not reading files from folder ##
        
        if(!grepl('http', tolower(dat_addrs)))
        {
          
          stop(paste("You selected use_local_json = FALSE,"
                     , "but have no 'http' in the,
                                          connection string?"))
        }
        
        
        j_dat_lst <- map(2:j_dat_1[[1]]$pages,
                         function(x)
                         {
                           fromJSON(
                             gsub("*$|\\&page=\\d+", 
                                  paste0("&page=", x), 
                                  dat_addrs))
                         })
        
      ## Combine pulled data ## 
      
      j_dat_lst <- c(list(j_dat_1), j_dat_lst)
      
      }
    }else
    {
      ## Keep same list structure for single page files ##
      
      j_dat_lst <- list(j_dat_1)
      
    }
  }
  
  tbl <- lapply(1:length(j_dat_lst), function(x)
  {
    j_dat_lst[[x]][[2]]  %>%
      apply(2, function(x){x})
  }) %>%
    do.call(rbind, .) %>%
    as_tibble()

  return(tbl)

}

