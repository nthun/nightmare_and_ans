# Downsampling function

downsample <- function(df, variable, from, to){
    
    temp <-
        df %>% 
        mutate( sample = seq(0L, nrow(.) - 1, 1L) %/% (from/to),
                time = sample/to)

        if (from/to == 1) return(select(temp, time, value = !!variable))    
    
    temp %>% 
        group_by(time) %>%
        summarise(value = mean(!!rlang::sym(variable), na.rm = TRUE))
}