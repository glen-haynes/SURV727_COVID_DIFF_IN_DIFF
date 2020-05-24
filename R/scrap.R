# # wb_poptot %>% 
# #   group_by(country) %>%
# #   group_map(~ .x %>% 
# #               filter(!is.na(SP.POP.TOTL)) %>%
# #               filter(year == max(year))) %>% 
# #   bind_rows() %>% 
# #   left_join(., as_tibble(WDI_data$country),
# #             by = intersect(colnames(.), colnames(WDI_data$country)))
# 
# miss_match_global <- global_dat$`Country/Region`[!global_dat$`Country/Region` 
#                                                  %in% wb_poptot$country] %>% 
#   unique() %>% 
#   tolower()
# 
# miss_match_wb <- wb_poptot$country[!wb_poptot$country 
#                                    %in% global_dat$`Country/Region`] %>% 
#   unique() %>% 
#   tolower()
# 
# cbind(miss_match_wb[grep(paste(miss_match_global, collapse = "|"), 
#                          miss_match_wb)], miss_match_global)
# 
# cross_match <- sapply(miss_match_global, function(x){
#   miss_match_wb[grep(x, miss_match_wb)]
# })
# 
# cross_match[c(3, 4, 6, 11:13, 15:17, 19, 22:24)] <- miss_match_wb[c(11, 10, 13, 
#                                                                     26, 27, 36, 
#                                                                     46, 48, 43, 
#                                                                     54, 28, 45,
#                                                                     33)]
# 
# cross_match_plc <- sapply(Filter(function(x){length(x)}, cross_match), 
#                           function(x){
#   grep(x, tolower(wb_poptot$country))
# })
# 
# wb_poptot$match <- tolower(wb_poptot$country)
# wb_poptot$match[cross_match_plc] <- names(cross_match_plc)
# 
# 
# wb_poptot %>% 
#   filter('country' == 'Afghanistan')
#   
#   
#   summarise(max_yr = max(year)) %>% 
#   select(max_yr) %>% 
#   unique()
#   
#   
  ## Setup
# 
#   library(xml2)
#   library(rvest)
#   library(httr)
# 
#   url <- read_html("https://www.littler.com/publication-press/publication/",
#                    "stay-top-stay-home-list-statewide")
#   
#   # Now extract the tables from this object (using the `rvest` package) and save the result as a new object. Follow the instructions if there is an error.
# 
#   tbls <- html_table(url, header = TRUE)
# 
#   
#   # Use `str()` on this new object -- it should be a list. Try to find the position of the "Historical population" in this list, since we need it in the next step.
# 
#   str(tbls)
#   
#   tbls[[1]]


## Conditionally update World Bank (WB) Country Classification file in ##
## working directory. ## 

# if(update_wb_class)
# {
#   if(grepl('xls', tolower(wb_file_nm))) # Change mode for xls(x) #
#   {
#     md = 'wb'
#   }else
#   {
#     md = 'w'
#   }
#   
#   download.file(paste0(wb_web_loc, "/", wb_file_nm), 
#                 paste0(wb_metadata_dir, "/", wb_file_nm),
#                 mode = md)
# }
# 
# ## Read in WB data. (Allow to be in xlsx, xls, or csv format). ##
# 
# if(grepl('xls', tolower(wb_file_nm)))
# {
#   wb_in <- read_excel(paste0(wb_metadata_dir, "/", wb_file_nm),
#                       wb_sheet_num)
# }else
# {
#   if(grepl('csv', tolower(wb_file_nm)))
#   {
#     wb_in <- read_csv(paste0(wb_metadata_dir, "/", wb_file_nm))
#   }
# }
# 
# 
# # library(xml2)
# library(rvest)
# 
# url <- read_html("https://www.littler.com/publication-press/publication/stay-top-stay-home-list-statewide")
# tbls <- html_table(url, header = TRUE)
# 
# View(tbls[[1]])
# 
# 
# 


