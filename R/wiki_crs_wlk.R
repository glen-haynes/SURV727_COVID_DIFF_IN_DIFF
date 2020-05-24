wiki_crs_wlk <- function(csse_wb_dat = csse_wb_crs, 
                         wiki_dat = wiki_lock,
                         return_match_to_tidy = FALSE,
                         cse_tidy_index = c(42),
                         wb_tidy_index = c(3),
                         clean_up = list(c("Nigeria;South Africa", ""),
                                         c("Oman;Romania", "Romania"))
)
{
  require(dplyr)
  
  miss_match_csse_wb <- csse_wb_dat$wb_to_csse[!(tolower(
    csse_wb_dat$wb_to_csse) %in% tolower(wiki_dat$`Countries and territories`)
    |
      tolower(
        csse_wb_dat$wb_name) %in% 
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
                                    csse_wb_dat$wb_name))] %>%
    unique() %>%
    tolower() %>%
    sort()
  
  if(return_match_to_tidy)
  {
    return(list(miss_match_csse_wb, miss_match_wiki))
  }
  
  wiki_dat_adj <- wiki_dat$`Countries and territories`
  
  for(i in 1:length(wb_tidy_index))
  {
    wiki_dat_adj[grep(
      miss_match_wiki[wb_tidy_index[i]], tolower(
        wiki_dat$`Countries and territories`)
    )] <- paste(wiki_dat$`Countries and territories`[grep(
      miss_match_wiki[wb_tidy_index[i]], tolower(
        wiki_dat$`Countries and territories`)
    )], miss_match_csse_wb[cse_tidy_index[i]], sep = "|") %>% 
      gsub("\\(|\\)", ".", .)
  }
  
  cross_walk <- tolower(wiki_dat_adj) %>%
    map(~{
      c(grep(.x, tolower(csse_wb_dat$csse_cntry)),
        grep(.x, tolower(csse_wb_dat$wb_name)),
        grep(.x, tolower(csse_wb_dat$wb_to_csse))) %>% 
        unique() %>% 
        {if(length(.))
        {
          .
        }else
        {
          NA
        }}
    }) %>% 
    {
      csse_wb_dat$wiki_to_csse_wb <- NA
      names(.) <- wiki_dat$`Countries and territories`
      for(i in 1:length(unlist(.)))
      {
        x <- unlist(.)[i]
        names(x) <- gsub("\\d+$", "", names(x))
        
        if(is.na(csse_wb_dat$wiki_to_csse_wb[x]) |
           csse_wb_dat$wiki_to_csse_wb[x] != paste("NA", names(x), sep = ";"))
        {
          csse_wb_dat$wiki_to_csse_wb[x] <- paste(
            csse_wb_dat$wiki_to_csse_wb[x], names(x),
            sep = ";")
        }
      }
      
      csse_wb_dat$wiki_to_csse_wb <- gsub("NA;", "", 
                                          csse_wb_dat$wiki_to_csse_wb)
      
      return(csse_wb_dat)
    } %>% 
    select(csse_cntry, wb_name, wiki_to_csse_wb) %>% 
    rename(wiki_country = wiki_to_csse_wb) %>% 
    unique()
  
  for(i in 1:nrow(cross_walk))
  {
    if(grepl('exclud', tolower(cross_walk$wb_name[i])))
    {
      cross_walk$csse_cntry[i] <- cross_walk$wiki_country[i] <- NA
    }
  }
  
  for(i in clean_up)
  {
    cross_walk$wiki_country <- gsub(i[1], i[2], cross_walk$wiki_country)
  }
  
  return(cross_walk)
}