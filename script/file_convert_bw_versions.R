# Make new files compatible with the old files
# 1. Read files, cut header and footer (OK)
# 2. Select only relevant columns (OK)
# 3. Downsample (OK)
# 4. Save to separate files by metric

library(tidyverse)
library(fs)
source("script/downsample.R")

# File properties
dir = "all_data/"
header = 11L
footer = 3L
pattern = ".txt"
req_columns = c("ecg" = "Sensor-C:ECG/EKG", "scr" = "Sensor-E:SC/GSR")

# Sampling rates
scr_sampling_rate = 32L
ecg_sampling_rate = 1024L


df <- 
    tibble(file = dir_ls(dir, regexp = pattern)) %>% 
    mutate(name = str_replace(file, str_glue(".*/(\\d+.*){pattern}$"),"\\1")) %>% 
    separate(name, into = c("id","session"), extra = "merge") %>% 
    filter(id >=40 & !str_detect(session, "proba")) %>% 
    mutate( file_length = map_int(file, ~read_lines(.x) %>% length()),
            file_sampling = map(file, ~read_lines(.x, skip = 8, n_max = 1)) %>% 
                                           str_extract("\\d+") %>% 
                                           as.integer(),
            data = map2(file, file_length, ~read_tsv( .x, 
                                              skip = header, 
                                              n_max = .y - header - footer
                                              ) %>% 
                                             select(!!!req_columns)
                            ))


# Playground --------------------------------------------------------------

temp <-
df %>% 
        mutate(
            scr_ds = map2(data, file_sampling, ~downsample(.x, variable = "scr", from = .y, to = 32)),
            ecg_ds = map2(data, file_sampling, ~downsample(.x, variable = "ecg", from = .y, to = 1024))
        )

temp  %>% 
    select(data, scr_ds, ecg_ds)



