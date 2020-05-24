csse_global_wide %>% 
  group_by(`Province/State`, `Country/Region`,   Lat,  Long) %>% 
  summarise(
    value = {
      non_zero_conf <- confirmed_global[confirmed_global > 0]
      
      if(length(non_zero_conf))
      {
        sum(((non_zero_conf[2:length(non_zero_conf)]-
                non_zero_conf[1:(length(non_zero_conf) - 1)])/
               non_zero_conf[1:(length(non_zero_conf) - 1)]))/
          (length(non_zero_conf) - 1) 
      }else
      {
        NA
      }
    }
  ) %>% 
  mutate(series = 'conf_grow') %>% 
  bind_rows(csse_global_wide %>% 
              group_by(`Province/State`, `Country/Region`,   Lat,  Long) %>% 
              summarise(
                value = {
                  non_zero_mort <- deaths_global[deaths_global > 0]
                  
                  if(length(non_zero_mort))
                  {
                    sum(((non_zero_mort[2:length(non_zero_mort)]-
                            non_zero_mort[1:(length(non_zero_mort) - 1)])/
                           non_zero_mort[1:(length(non_zero_mort) - 1)]))/
                      (length(non_zero_mort) - 1) 
                  }else
                  {
                    NA
                  }
                }
              ) %>% 
              mutate(series = 'mort_grow')
  ) %>% 
  bind_rows(
    csse_global_wide %>% 
  group_by(`Province/State`, `Country/Region`,   Lat,  Long) %>% 
  summarise(
    value = {
      non_zero_recov <- recovered_global[recovered_global > 0]
      
      if(length(non_zero_recov))
      {
        sum(((non_zero_recov[2:length(non_zero_recov)]-
                               non_zero_recov[1:(length(non_zero_recov) - 1)])/
                              non_zero_recov[1:(length(non_zero_recov) - 1)]))/
          (length(non_zero_recov) - 1) 
      }else
      {
        NA
      }
    }
  ) %>% 
  mutate(series = 'recov_global')
  )





