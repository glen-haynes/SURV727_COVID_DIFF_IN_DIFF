csse_wb_crs_wlk <- function(csse_dat,
                            wb_country_meta,
                            return_match_to_tidy = FALSE,
                            cse_tidy_index = c(
                              3, 4, 6, 11:12, 14:16, 18, 21:23),
                            wb_tidy_index = c(16, 15, 18, 63, 64, 119, 121,
                            110, 136, 65, 118, 89)
)
{
  require(dplyr)
  require(jsonlite)
  require(readxl)
  
  miss_match_csse <- csse_dat$`Country/Region`[!tolower(
    csse_dat$`Country/Region`) %in% tolower(wb_country_meta$name)] %>%
    unique() %>%
    tolower()
  
  miss_match_wb <- wb_country_meta$name[!tolower(wb_country_meta$name)
                                        %in% tolower(
                                          csse_dat$`Country/Region`)] %>%
    unique() %>%
    tolower() %>% 
    sort()
  
  cross_match <- sapply(tolower(miss_match_csse), function(x){
    miss_match_wb[grep(x, tolower(miss_match_wb))]
  })
  
  if(return_match_to_tidy)
  {
    return(list(cross_match, miss_match_wb))
  }

  cross_match[cse_tidy_index] <- miss_match_wb[wb_tidy_index]

  cross_match_plc <- sapply(Filter(function(x){length(x)}, cross_match),
                            function(x){
                              grep(x, tolower(wb_country_meta$name))
                            }) %>%
    unlist()

  wb_country_meta <- wb_country_meta %>%
    mutate(wb_to_csse = tolower(wb_country_meta$name))

  wb_country_meta$wb_to_csse[cross_match_plc] <- gsub("\\d+$", '',
                                                      names(cross_match_plc)
  )

  wb_country_meta <- wb_country_meta %>%
    unique()

  wb_country_meta <- wb_country_meta %>%
    full_join({csse_dat %>% mutate(lower_name = tolower(`Country/Region`))},
              by = c("wb_to_csse" = "lower_name")) %>% 
    select(`Country/Region`, name, wb_to_csse) %>% 
    rename(wb_name = name, csse_cntry = `Country/Region`) %>% 
    unique()

  return(wb_country_meta)
}