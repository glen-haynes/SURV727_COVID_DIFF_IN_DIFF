## Get totals ##

csse_totals <- csse_global_merge %>%
  csse_to_long("*") %>% 
  group_by_at(vars(-one_of("Date", "value"))) %>% 
  summarise(
    total = max(value))

## Filter out Non-Occurances across Confirmed Cases, Deaths and Recovered ##

csse_events <- csse_global_merge %>% 
  filter(., {select(., matches("\\d+/\\d+/\\d+")) %>% 
      apply(1, function(x){!all(x == 0)})})

## Calculate Growth Rates (Simple Average, Compound Average, and Maximum)

csse_grwth <- map(list("confirmed", "deaths", "recovered"), function(srs)
{
  grw1 <- csse_events %>%
    csse_to_long(srs) %>% 
    group_by_at(vars(-one_of("Series", "Date", "value"))) %>% 
    summarise(
      smpl_growth = simple_avg_growth(value, start_val = 10),
      cmpnd_growth = cmpnd_avg_growth(value, start_val = 10),
      max = max(value),
      t = sum(value > 0)
    ) 
  
  grw2 <- csse_events %>%
    csse_to_long(srs) %>% 
    group_by_at(vars(-one_of("Series", "Date", "value"))) %>%
    summarise(max_grw = implement_fit_easylinear(value)[[1]],
              max_grw_r2 = implement_fit_easylinear(value)[[2]])
  
  grw_rtrn <- full_join(grw1, grw2, by = intersect(colnames(grw1), 
                                                   colnames(grw2))) %>% 
    mutate(Series = srs) %>% 
 select(-max, -t, -series, everything())
}) %>% 
  bind_rows()




fit_easylinear()

csse_global_merge %>% head()
