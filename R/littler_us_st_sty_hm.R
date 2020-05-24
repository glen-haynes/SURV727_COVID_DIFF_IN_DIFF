littler_us_st_sty_hm <- function(url = paste0("https://www.littler.com/",
                                              "publication-press/publication/",
                                              "stay-top-stay-home-list",
                                              "-statewide"),
                                 save_file = FALSE,
                                 save_dir = 'data',
                                 use_local_file = FALSE,
                                 add_na_states = TRUE
)
{
  if(use_local_file)
  {
    if(
      !"state_lock.rds" %in% list.files(save_dir)
    )
    {
      stop(paste0("Parameter use_local_file set to true, but file",
                  " 'state_lock.rds' is not in ", save_dir, "/."))
    }

    return(readRDS(paste0(save_dir, "/state_lock.rds")))
  }
  
  require(dplyr)
  require(purrr)
  require(robotstxt)
  require(xml2)
  require(rvest)
  require(datasets)
  
  ## Check site is allowing bots ##

  dmn <- gsub("/.*", "", gsub("^.*//", "", url))

  path <- gsub(".*\\.[[:alnum:]]+/", "", url)

  rbt_status <- robotstxt(domain = dmn)

  # Check access permission (returns true/false)

  scrape_allowed <- rbt_status$check(
    paths = path,
    bot = "*")
  
  ## Get US states list from datasets package and append with ##
  ## territories and DC. ##
  
  state_terr_list <- c(datasets::state.name, 
                  "District of Columbia", 
                  "Puerto Rico",
                  "American Samoa",
                  "Guam",
                  "Northern Mariana Islands",
                  "Virgin Islands"
  )
                  
  
  if(scrape_allowed)
  {
    html <- read_html(url)
    state_lock <- html_table(html, header = TRUE)
    
    months <- format(ISOdatetime(2000,1:12,1,0,0,0),"%B")
    state_lock <- state_lock[[1]] %>%
      mutate(start_date = {
        sapply(state_lock[[1]]$`Effective Date`, function(i)
        {
          month <- months %>% map(~grepl(.x, i)) %>% unlist() %>% which() %>% 
            {if(length(.) > 1){warning(paste0("Multiple dates in cell: '",i,
                                              "' only the first date ",
                                              "will be used."))}; .[1]}
          
          if(nchar(month)==1){month <- paste0("0", month)}
          day <- gsub(".*\\s(\\d{1,2}),.*", "\\1", i)
          if(nchar(day)==1){day <- paste0("0", day)}
          year <- gsub(".*(\\d{4}).*", "\\1", i)
          return(paste0(year, "-", month, "-", day))
        }) %>% unlist()}) %>%
      mutate(end_date = {
        sapply(state_lock[[1]][,3], function(i)
        {
          if(any(grepl("[[:alpha:]]+\\s+\\d+,\\s+\\d{4}", i)))
          {
            month <- months %>% map(~grepl(.x, i)) %>% unlist() %>% which() %>% 
              .[1]
            if(nchar(month)==1){month <- paste0("0", month)}
            day <- gsub(".*\\s(\\d{1,2}),.*", "\\1", i)
            if(nchar(day)==1){day <- paste0("0", day)}
            year <- gsub(".*(\\d{4}).*", "\\1", i)
            return(paste0(year, "-", month, "-", day))
          }else
          {
            return(NA)
          }
        }) %>% unlist()}) %>% 
      mutate(lock_caveat = {.$State %>% gsub(paste(state_terr_list, 
                                      collapse = "|"), "", .)}) %>% 
      select(State, lock_caveat, start_date, end_date, everything()) %>% 
      {if(add_na_states){
      bind_rows(., {state_list <- .$State; state_terr_list %>% 
        map(~if(FALSE) ## To be fixed
      {
        tmpX <- state_lock[1, , drop = FALSE]
        tmpX <- tmpX %>% mutate_all(function(x){NA})
        tmpX <- tmpX %>% mutate(State = .x)
        return(tmpX)
      }else(NULL)) %>% 
          bind_rows()})}else{.}} %>% 
      ## Convert start and end dates to R date type ##
      mutate(start_date = as.Date(start_date, format = "%Y-%m-%d")) %>%
      mutate(end_date = as.Date(end_date, format = "%Y-%m-%d"))
  }else
  {
    stop(paste("Bots not allowed for", url))
  }
  if(save_file)
  {
    saveRDS(state_lock, paste0(save_dir, "/state_lock.rds"))
  }
  return(state_lock)
}