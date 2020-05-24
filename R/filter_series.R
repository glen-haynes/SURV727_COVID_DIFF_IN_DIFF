filter_series <- function(fltr_dat,
                          fltr_ptrn,
                          fltr_col = "series")
{
  fltr_dat[
    unlist(map(fltr_dat, function(x)
    {
      any(grepl(fltr_ptrn, {colnames(x)}))
    }))
    ]
}
