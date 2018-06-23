# Function to calculate interpolated HR from ibi
# 

# EXAMPLE: calculate_hr(temp, "ibi_time", "ibi", 6000)


library(rlang)
calculate_hr <- function(df, time_col, ibi_col, window = 1000, ...){
    df %>%
        tidyr::complete(!!sym(time_col) := tidyr::full_seq(!!sym(time_col), 1)) %>%
        dplyr::mutate(ibi = stats::approx(
                            x = !!sym(time_col),
                            y = !!sym(ibi_col),
                            xout = !!sym(time_col)
                        )$y) %>%
        dplyr::mutate(hr = 60000 / zoo::rollmean(
                            x = !!sym(ibi_col),
                            k = window,
                            na.pad = TRUE,
                            na.rm = TRUE,
                            ...
                        )) %>%
        dplyr::filter(!!sym(time_col) %% 1000 == 0) %>%
        dplyr::transmute(time = ibi_time/1000, 
                         hr)
}





