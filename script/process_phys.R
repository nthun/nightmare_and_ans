# Read any physchophys data
library(tidyverse)
library(fs)

process_phys <- function(dir, pattern, sampling_rate = 1024L, header = 13L, footer = 3L){
        tibble(file = dir_ls(dir, regexp = pattern),
               # Have to read the lines first to know to skip the last 3 lines
               file_length = map_int(file, ~read_lines(.x) %>% length())) %>%
        mutate(name = str_replace(file, str_glue(".*/(\\d+.*){pattern}$"),"\\1"),
               # Read all files, skip header and footer, 
               data = map2(file, file_length, ~read_tsv( .x, 
                                                         skip = header, 
                                                         n_max = .y - header - footer,
                                                         col_types = "id__", 
                                                         col_names = c("sample","value")) %>% 
                               mutate(time = sample/sampling_rate))) %>% 
        separate(name, into = c("id","session"), extra = "merge") %>% 
        unnest() %>%
        select(id, session, file, time, value)
}


    
    