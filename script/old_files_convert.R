# Merge read the old and new ECG files, bind them together, and save them in the same df
# TODO:
# 1. Old data time has to be converted from sample to time (DONE)
# 2. Save old data to the converted folder (DONE)
# 3. Read all data to one df and save it

library(tidyverse)
library(fs)

dir = "all_data/"
header_length = 11L
footer_length = 3L
ecg_columns = c(sample = "TIME", "ecg" = "Sensor-C:ECG/EKG")
scr_columns = c(sample = "TIME",  "scr" = "Sensor-E:SC/GSR")
req_columns = c("ecg" = "Sensor-C:ECG/EKG", "scr" = "Sensor-E:SC/GSR")
new_dir = "all_data/converted/"

# Process old ECG data
pattern = "_ECG.txt"
req_columns = c("ecg" = "Sensor-C:ECG/EKG", "scr" = "Sensor-E:SC/GSR")
old_ecg <- 
    tibble(file = fs::dir_ls(dir, regexp = pattern)) %>% 
    mutate(name = str_replace(file, str_glue(".*/(\\d+.*){pattern}$"),"\\1")) %>% 
    separate(name, into = c("id","session"), extra = "merge") %>% 
    # Read only the older files that has phys data in separate files
    # Extract absolute starting time, number of rows, sampling freq, and data (time, value)
    filter(id < 40 & !str_detect(session, "proba")) %>% 
    mutate( 
        # Getting the starting time is only important if we want to sync using absolute time
            time_start =    map_int(file,  ~read_lines(.x, skip = 6, n_max = 1) %>% 
                                    str_extract("\\d+:\\d+:\\d+") %>% 
                                    hms::as.hms() %>% 
                                    as.integer()
                                    ),
            file_length =   map_int(file,  ~read_lines(.x) %>% length()),
            file_sampling = map_int(file,  ~read_lines(.x, skip = 8, n_max = 1) %>% 
                                        str_extract("\\d+") %>% 
                                        as.integer()
                                    ),
            data = pmap(list(file, file_length, file_sampling), 
                        ~read_tsv( 
                                  file = ..1, 
                                  skip = header_length, 
                                  n_max = ..2 - header_length - footer_length) %>% 
                        select(!!!ecg_columns) %>% 
                        transmute(time = sample/..3, ecg)))

# Write the corrected files
walk2(old_ecg$data, str_replace(old_ecg$file, "all_data", new_dir),
      ~write_tsv(.x, path = .y, col_names = FALSE))

# Process old SCR data
pattern = "_SCR.txt"
old_scr <- 
    tibble(file = fs::dir_ls(dir, regexp = pattern)) %>% 
    mutate(name = str_replace(file, str_glue(".*/(\\d+.*){pattern}$"),"\\1")) %>% 
    separate(name, into = c("id","session"), extra = "merge") %>% 
    # Read only the older files that has phys data in separate files
    # Extract absolute starting time, number of rows, sampling freq, and data (time, value)
    filter(id < 40 & !str_detect(session, "proba")) %>% 
    mutate( 
        # Getting the starting time is only important if we want to sync using absolute time
        time_start =    map_int(file,  ~read_lines(.x, skip = 6, n_max = 1) %>% 
                                        str_extract("\\d+:\\d+:\\d+") %>% 
                                        hms::as.hms() %>% 
                                        as.integer()
                        ),
        file_length =   map_int(file,  ~read_lines(.x) %>% length()),
        file_sampling = map_int(file,  ~read_lines(.x, skip = 8, n_max = 1) %>% 
                                str_extract("\\d+") %>% 
                                as.integer()
                        ),
    data = pmap(list(file, file_length, file_sampling), 
                ~read_tsv( 
                    file = ..1, 
                    skip = header_length, 
                    n_max = ..2 - header_length - footer_length) %>% 
                    select(!!!scr_columns) %>% 
                    transmute(time = sample/..3, scr)))

# Write the corrected files
walk2(old_scr$data, str_replace(old_scr$file, "all_data", new_dir),
      ~write_tsv(.x, path = .y, col_names = FALSE))



