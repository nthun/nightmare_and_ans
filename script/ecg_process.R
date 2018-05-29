library(tidyverse)
library(fs)
source("script/process_phys.R")

header = 13L
footer = 3L

ecg <- 
    process_phys(dir = "all_data/",
             pattern = "_ECG.txt", 
             sampling_rate = 1024,
             header = 13, 
             footer = 3)

eda <- 
    process_phys(dir = "all_data/",
                 pattern = "_SCR.txt", 
                 sampling_rate = 32,
                 header = 13, 
                 footer = 3)

length_33 <- read_lines("all_data/33_B_emot_SCR.txt") %>% length()

eda_33 <- read_tsv("all_data/33_B_emot_SCR.txt", 
                   skip = header, 
                   n_max = length_33 - header - footer,
                   col_types = "id__",
                   col_names = c("sample","value"))


quiet_read_tsv <- quietly(read_tsv)

dir = "all_data/"
pattern = "_SCR.txt"
sampling_rate = 1024L
header = 13L
footer = 3L

df <- 
    tibble( file = dir_ls(dir, regexp = pattern),
            # Have to read the lines first to know to skip the last 3 lines
            file_length = map_int(file, ~read_lines(.x) %>% length()),
            name = str_replace(file, str_glue(".*/(\\d+.*){pattern}$"),"\\1")) %>%
    # Read all files, skip header and footer, 
    mutate(data = map2(file, file_length, ~quiet_read_tsv( .x, 
                                                     skip = header, 
                                                     n_max = .y - header - footer,
                                                     col_types = "id__", 
                                                     col_names = c("sample","value")))) %>% 
    separate(name, into = c("id","session"), extra = "merge") # %>% 
    unnest() %>%
    select(id, session, file, time, value) %>% 
    mutate(time = sample/sampling_rate)))


safe_read_tsv("all_data/33_B_emot_SCR.txt") %>% str()
    

df %>% 
    mutate(warn = map(data, "warnings")) %>% 
    print(n = 100)

df %>% 
    slice(45) %>% 
    pull(data)





    
    