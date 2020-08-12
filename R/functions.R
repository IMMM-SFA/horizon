# filter_monthplus7days
#
# This function fixes column names where o_cfs is entered instead of r_cfs.

filter_monthplus7days <- function(x, mth){

  if(mth %in% 1:11){
    nxt_mth <- mth + 1
  }else{
    nxt_mth <- 1
  }

  x %>% arrange(date) %>%
    filter(month(date) %in% c(mth, nxt_mth)) %>%
    mutate(diff = c(1, diff(.[["date"]])),
           diff_ = if_else(diff == 1, 0, 1),
           instance = 1 + cumsum(diff_)) %>%
    select(-diff, -diff_) %>% split(.$instance) %>%
    map_dfr(function(inst){
      inst %>% filter(month(date) == mth) -> inst_mth
      if(nrow(inst_mth) == 0) return(inst_mth)
      inst %>% filter(month(date) == nxt_mth) %>%
        .[1:7,] -> inst_nxtmth
      bind_rows(inst_mth, inst_nxtmth)
    })

}
