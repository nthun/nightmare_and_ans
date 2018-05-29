# Trim data with a padding before and after the first and last markers
# INPUT: df: tibble
#        stim_var: variable name that contains the stimuli <chr>
#        time_var: variable name that contains time <chr>
#        padding_before: padding in seconds <num> before the first stimulus
#        padding_after: padding in seconds <num> after the last stimulus
# OUTPUT: a trimmed tibble
# EXAMPLE: trim_data(df = temp, stim_var = "stimulus", time_var = "time", padding_before = 30, padding_after = 40)

library(dplyr)
library(tidyr)
trim_data <- function(df, stim_var = "stimulus", time_var = "time", padding_before = NULL, padding_after = NULL){
    df %>% 
        mutate(first_stim = first(drop_na(., !!stim_var) %>% pull(!!time_var)),
               last_stim =  last(drop_na(., !!stim_var) %>% pull(!!time_var))) %>% 
        filter_at(vars(matches(time_var)),  
                  any_vars(. >= (first(first_stim) - padding_before) &
                           . <= (first(last_stim) + padding_after))) %>% 
        select(-first_stim, -last_stim)
}
