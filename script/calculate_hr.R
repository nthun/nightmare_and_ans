# Function to calculate regular time series interpolated HR from irregular time series IBI
# INPUT: df: a data frame with ibi data, 
#        time_col <chr> name of the column that contains time (will be created by the function)
#        ibi_col <chr> the name of the ibi column (obligatory)
#        window <int> the widht of the window in miliseconds
#        ... parameters passed to rollmean
# OUTPUT: A df with time <int> and interpolated hr <dbl>
# EXAMPLE: calculate_hr(temp, "ibi_time", "ibi", 6000)
# TODO: let the user choose between cubic spline and linear interpolation (stats::approx) purrr::invoke()

library(rlang)
calculate_hr <- function(df, time_col, ibi_col, window = 1000L, ...){
    df %>%
        # Complete the data frame to contain all secods
        tidyr::complete(!!sym(time_col) := tidyr::full_seq(!!sym(time_col), 1)) %>%
        # Use a cubic spline to interpolate data (is linear better?)
        dplyr::mutate(ibi = stats::spline(
                            x = !!sym(time_col),
                            y = !!sym(ibi_col),
                            xout = !!sym(time_col)
                            )$y) %>%
        # Calculate a rolling mean of bpm using the window parameter
        dplyr::mutate(hr = 60000 / zoo::rollmean(
                            x = !!sym(ibi_col),
                            k = window,
                            na.pad = TRUE,
                            na.rm = TRUE,
                            ...)) %>%
        # Keep only every full second
        dplyr::filter(!!sym(time_col) %% window == 0) %>%
        # Keep the time and hr cols only
        dplyr::transmute(time = ibi_time/1000, 
                         hr)
}





