fltr_crss_wb_cat <- function(wb_field, wb_filter, crsswlk = global_csse_crs)
{
  wb_field = enquo(wb_field)
  
  crsswlk %>% 
    filter(wb_name %in% 
             {
               unlist(wb_meta %>% 
                        filter(grepl(tolower(wb_filter), 
                                     tolower(!!wb_field))) %>% 
                        select(name))
             }
    )
}

