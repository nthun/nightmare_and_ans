# DESCRIPTION: Calculates two types of AUC (area under the curve), common for calculating hormonal changes over time. The function works by calculating the area of the rectangles and triangles that make up the measurement points, with respect to time. The funcion can calculate AUCg (AUC in respect to ground) or AUCi (AUC in respect to baseline).
# For further info, check: Fekedulegn, D. B., Andrew, M. E., Burchfiel, C. M., Violanti, J. M., Hartley, T. A., Charles, L. E., & Miller, D. B. (2007). Area Under the Curve and Other Summary Indicators of Repeated Waking Cortisol Measurements. Psychosomatic Medicine, 69(7), 651–659. https://doi.org/10.1097/PSY.0b013e31814c405c
# INPUT:        time: time points of the measurements
#               measurement: measurements
#               type: either "g" (default) , or  "i". See description. 
# OUTPUT: A single value that represent the selected AUC type
# EXAMPLE: calculate_auc(time = c(0, 15, 45, 90), measurement = c(5, 10.2, 12.3, 11.4), type = "g")

if (!require("tidyverse")) install.packages("tidyverse")
library(rlang)
library(tidyverse)

calculate_auc <- function(df, 
                          time,
                          measurement,
                          type = c("g","i","both")) {

    type <- type[1]
                              
    output <-
        df %>%
        mutate(
            # length of time between successive timepoints
            length = !!sym(time) - lag(!!sym(time)),
            # change in measurement
            increase = !!sym(measurement) - lag(!!sym(measurement)),
            rectangle = if_else(increase >= 0,
                                length * lag(!!sym(measurement)),
                                length * !!sym(measurement)),
            triangle = if_else(increase >= 0,
                               length * increase / 2,
                               length * (lag(!!sym(measurement)) - !!sym(measurement)) / 2),
            auc = triangle + rectangle,
            # A basline rectngle is calculated based on first measurement. This will
            # be subtracted to make a relative metric.
            baseline = max(!!sym(time), na.rm = T) * first(!!sym(measurement))) %>%
        summarise(auc_g = sum(auc, na.rm = T),
                  # Baseline is the same for all measurement points, so pick just one
                  # auc_i can be negative if the metric decreased at all data points
                  auc_i = auc_g - max(baseline, na.rm = T))
    
    if (type == "g") return(select(output, auc_g))
    if (type == "i") return(select(output, auc_i))
    if (type == "both") return(select(output, auc_g, auc_i))
    if (!type %in% c("g", "i")) return(tibble())
}