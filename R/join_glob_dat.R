join_glob_dat <- function(csse_dat = csse_global_merge, 
                          lockdown_dat = wiki_lock, 
                          wb_dat = wb_rel_ind_List, 
                          wb_met = NA, 
                          crss_walk = global_csse_crs)
{
  join_dat <- csse_dat %>%
    rename(csse_cntry = `Country/Region`) %>% 
    left_join(crss_walk,
              by = intersect(colnames(.), colnames(crss_walk))) %>%
    left_join(
      {
        left_join(crss_walk, 
                  {
                    if(!is.na(wb_met))
                    {
                      next_join <- "name"
                     full_join(wb_met, 
                              wb_dat,           
                              by = c("name" = "country.value"))
                    }else
                    {
                      next_join <- "country.value"
                      wb_dat
                    } 
                  }, by = c("wb_name" = next_join))
      }, by = intersect(colnames(.), colnames(crss_walk))) %>%
    left_join(
      {
        full_join(
          lockdown_dat,
          crss_walk,
          by = c(
            "Countries and territories" = "wiki_country"))
      }, by = c("csse_cntry", "wb_name")
      )
}