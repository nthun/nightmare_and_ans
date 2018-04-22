# TODO: investigate marker errors
# TODO: Merge markers to physiological data
# TODO: Make sure to verify if all markers are merged properly


library(tidyverse)
# Create two versions of the markers, rounded to sampling frequency boundaries. This makes it possible to join by time. The change in marker times is extremely small, and should not affect any of the physiological measures (especially gsr and ecg)
markers_scr <- 
    read_csv("all_data/marker/all_triggers.csv", col_types = "cidiiiiiic") %>%
    mutate(id = if_else(str_length(id) == 1, paste0("0",id), id),
           # Round time to the nearest 1/32
           time = plyr::round_any(time,1/32)) %>% 
    drop_na(block)
   
markers_ecg <- 
    read_csv("all_data/marker/all_triggers.csv", col_types = "cidiiiiiic") %>%
    mutate(id = if_else(str_length(id) == 1, paste0("0",id), id),
           # Round time to the nearest 1/1024
           time = plyr::round_any(time,1/1024)) %>% 
    arrange(id, session, time)

# Read the physiological data
all_ecg <- 
    read_csv("all_data/clean/all_ecg.csv") %>% 
    mutate(session = str_replace(session, "._(\\w+)$", "\\1"))

all_scr <- 
    read_csv("all_data/clean/all_scr.csv") %>% 
    mutate(session = str_replace(session, "._(\\w+)$", "\\1"))
    



nrow(markers)

# Marker troubles
# Negative times
markers_ecg %>% 
    filter(time <0)



# temp <-
all_scr %>% 
    inner_join(markers_scr, by = c("id", "session", "time"))
    #filter(is.na(scr))

all_ecg %>% 
    left_join(markers_ecg, by = c("id", "session", "time")) %>% 
    drop_na(stimulus)

temp %>% distinct(id) %>% pull()
    
