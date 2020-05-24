wiki_crs_wlk <- function(csse_wb_dat = csse_wb_crs, 
                            wiki_dat = df_lock,
                            return_match_to_tidy = FALSE,
                            cse_tidy_index = c(91),
                            wb_tidy_index = c(3)
)
{
  require(dplyr)
  

  
  miss_match_csse_wb <- csse_wb_dat$wb_to_csse[!(tolower(
    csse_wb_dat$wb_to_csse) %in% tolower(wiki_dat$`Countries and territories`)
    |
      tolower(
        csse_wb_dat$name) %in% 
      tolower(wiki_dat$`Countries and territories`)
    )
     ] %>%
    unique() %>%
    tolower()
  
  miss_match_wiki <- wiki_dat$
    `Countries and territories`[!(tolower(wiki_dat$`Countries and territories`)
                                        %in% tolower(
                                          csse_wb_dat$wb_to_csse)
                                  |
                                    tolower(wiki_dat$
                                              `Countries and territories`)
                                  %in% tolower(
                                    csse_wb_dat$name))] %>%
    unique() %>%
    tolower() %>%
    sort()

  cross_match <- sapply(tolower(miss_match_csse_wb), function(x){
    miss_match_wiki[grep(x, tolower(miss_match_wiki))]
  })

  if(return_match_to_tidy)
  {
    return(list(cross_match, miss_match_wiki))
  }

  cross_match[cse_tidy_index] <- miss_match_wiki[wb_tidy_index]

  cross_match_plc <- sapply(Filter(function(x){length(x)}, cross_match),
                            function(x){
                              grep(x, 
                                   tolower(wiki_dat$`Countries and territories`))
                            }) %>%
    unlist()

  wiki_dat_meta <- wiki_dat %>%
    mutate(wiki_to_csse = tolower(wiki_dat$`Countries and territories`))

  wiki_dat_meta$wiki_to_csse[cross_match_plc] <- gsub("\\d+$", '',
                                                      names(cross_match_plc)
  )
  
  wiki_dat<- wiki_dat %>% gsub("^\\s+|\\s+$", "", .)

  return(wiki_dat_meta)
}