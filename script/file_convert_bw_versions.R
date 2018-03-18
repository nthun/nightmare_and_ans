# Make new files compatible with the old files
# 1. Read files, cut header and footer (OK)
# 2. Select only relevant columns (OK)
# 3. Downsample (OK)
# 4. Save to separate files by metric (OK)

# 5. Join by approximate numeric match using fuzzyjoin::difference_left_join()
library(fuzzyjoin)

library(tidyverse)
library(fs)
source("script/downsample.R")

# File properties
dir = "all_data/"
header_length = 11L
footer_length = 3L
pattern = ".txt"
req_columns = c("ecg" = "Sensor-C:ECG/EKG", "scr" = "Sensor-E:SC/GSR")

# Sampling rates
scr_sampling_rate = 32L
ecg_sampling_rate = 1024L


# Read new files
df <- 
    tibble(file = dir_ls(dir, regexp = pattern)) %>% 
    mutate(name = str_replace(file, str_glue(".*/(\\d+.*){pattern}$"),"\\1")) %>% 
    separate(name, into = c("id","session"), extra = "merge") %>% 
    # Read only the nweer files that has all phys data in one file
    filter(id >= 40 & !str_detect(session, "proba")) %>% 
    # Get info from header: absoulte time, file length, and sampling freq
    # The read all files into a nested df and clean those files
    mutate( time_start =    map_int(file,  ~read_lines(.x, skip = 6, n_max = 1) %>% 
                                            str_extract("\\d+:\\d+:\\d+") %>% 
                                            hms::as.hms() %>% 
                                            as.integer()),
            file_length =   map_int(file,  ~read_lines(.x) %>% length()),
            file_sampling = map_int(file,  ~read_lines(.x, skip = 8, n_max = 1) %>% 
                                            str_extract("\\d+") %>% 
                                            as.integer()),
            data = map2(file, file_length, ~read_tsv( file = .x, 
                                                      skip = header_length, 
                                                      n_max = .y - header_length - footer_length) %>% 
# This also renames variables, but all variables must be in the file (doesn't work with earlier versions)
                                            select(!!!req_columns))
            )

# Downsampling  --------------------------------------------------------------

downsampled_df <-
    df %>% 
        mutate(
            scr_ds = pmap(list(data, file_sampling, time_start), ~downsample(..1, variable = "scr", from = ..2, to = 32) %>%
                                                                  mutate(time = time + ..3) %>% 
                                                                  drop_na()),
            ecg_ds = pmap(list(data, file_sampling, time_start), ~downsample(..1, variable = "ecg", from = ..2, to = 1024)%>% 
                                                                  mutate(time = time + ..3) %>%
                                                                  drop_na()),
            file = str_replace(file, "_SCR","") %>% 
                   str_replace(., "all_data/", "all_data/converted/"))

# Save the files separately to a new library
walk2(downsampled_df$scr_ds, downsampled_df$file, ~write_tsv(x = .x, path = str_replace(.y, ".txt","_SCR.txt"), na = "", col_names = FALSE))

walk2(downsampled_df$ecg_ds, downsampled_df$file, ~write_tsv(x = .x, path = str_replace(.y, ".txt","_ECG.txt"), na = "", col_names = FALSE))



map(downsampled_df$scr_ds, ~tail(.x))


