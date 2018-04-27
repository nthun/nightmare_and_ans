# TODO: investigate marker errors
# TODO: Merge markers to physiological data
# TODO: Make sure to verify if all markers are merged properly


library(tidyverse)
# Create two versions of the markers, rounded to sampling frequency boundaries. This makes it possible to join by time. The change in marker times is extremely small, and should not affect any of the physiological measures (especially gsr and ecg)

markers <- 
    read_csv("all_data/marker/all_triggers.csv", col_types = "cidiiiiiic") %>%
    mutate(id = if_else(str_length(id) == 1, paste0("0",id), id))

markers_scr <- 
    markers %>% 
    # Round time to the nearest 1/32
    mutate(time = plyr::round_any(time,1/32)) %>% 
    drop_na(block) %>% 
    arrange(id, session, time)

markers_ecg <- 
    markers %>% 
    # Round time to the nearest 1/1024
    mutate(time = plyr::round_any(time,1/1024)) %>% 
    # drop_na(block) %>% 
    arrange(id, session, time)

# Read the physiological data
all_ecg <- 
    read_csv("all_data/clean/all_ecg.csv") %>% 
    mutate(session = str_replace(session, "._(\\w+)$", "\\1"))

all_scr <- 
    read_csv("all_data/clean/all_scr.csv") %>% 
    mutate(session = str_replace(session, "._(\\w+)$", "\\1"))
    
# Number of all markers
nrow(markers)

## Marker troubles
all_scr %>% 
    inner_join(markers_scr, by = c("id", "session", "time")) %>% 
    group_by(id, session) %>% 
    count()

# Out of 5000 markers, only 2800 markers have matching times in the physiological recordings

# There are two markers with negative times
markers %>% 
    filter(time < 0)

## Which participants have markers?
# First compare data
marker_participants <-
    markers %>% 
    distinct(id, session) %>% 
    arrange(id, session)

scr_participants <-
    all_scr %>% 
    distinct(id, session) %>% 
    arrange(id, session) %>% 
    mutate(exist = TRUE)

ecg_participants <-
    all_ecg %>% 
    distinct(id, session) %>% 
    arrange(id, session) %>% 
    mutate(exist = TRUE)

# Plot the number of successfully matched markers
all_scr %>% 
    inner_join(markers_scr) %>% 
    full_join(marker_participants) %>% 
    arrange(id, session, time) %>% 
    mutate(id = as_factor(id), session = as_factor(session)) %>% 
    drop_na(id) %>% 
    group_by(id, session) %>% 
    count() %>% 
    ggplot() +
    aes(x = id, y = n, fill = session) +
    geom_col(position = "dodge")

all_ecg %>% 
    inner_join(markers_ecg) %>% 
    full_join(marker_participants) %>% 
    arrange(id, session, time) %>% 
    mutate(id = as_factor(id), session = as_factor(session)) %>% 
    drop_na(id) %>% 
    group_by(id, session) %>% 
    count() %>% 
    ggplot() +
    aes(x = id, y = n, fill = session) +
    geom_col(position = "dodge")

