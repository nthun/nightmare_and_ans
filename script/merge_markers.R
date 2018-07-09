# TODO: Unite the two recording part for id54

library(tidyverse)
source("script/trim_data.R")
# Create two versions of the markers, rounded to sampling frequency boundaries. This makes it possible to join by time. The change in marker times is extremely small, and should not affect any of the physiological measures (especially gsr and ecg)

memory.limit(12000000)

sampling_rate <- 1024L

markers <- 
    read_csv("all_data/marker/all_triggers.csv", col_types = "cidiiiiiic") %>%
    mutate(id = if_else(str_length(id) == 1, paste0("0",id), id))

# Round markers to the closest sampling point
# That should be done for SCR and ECG separately because of the different sampling rate
markers_scr <- 
    markers %>% 
    mutate(time = plyr::round_any(time,1/32)) %>% 
    drop_na(block) %>% 
    arrange(id, session, time)

markers_ecg <- 
    markers %>% 
    mutate(time = plyr::round_any(time,1/1024)) %>% 
    drop_na(block) %>%
    arrange(id, session, time)

# Read the physiological data
# The physiological signals are already on a regular sampling rate, so they does not have to be adjusted
all_ecg <- 
    read_csv("all_data/clean/all_ecg.csv", col_types = "ccdd") %>% 
    mutate(session = str_replace(session, "._(\\w+)$", "\\1"))

all_scr <- 
    read_csv("all_data/clean/all_scr.csv", col_types = "ccdd") %>% 
    mutate(session = str_replace(session, "._(\\w+)$", "\\1"))
    
# Number of all markers
nrow(markers)

# Number of valid markers (within blocks)
markers_ecg %>% 
    drop_na(block) %>% 
    nrow()

# There are two markers with negative times
markers %>% 
    filter(time < 0)

## Which participants have markers?
# First compare data
# There are markers for 89 participants*sessions
marker_participants <-
    markers %>% 
    distinct(id, session) %>% 
    arrange(id, session)

# SCR data is available for 89 participants*sessions (ALL)
scr_participants <-
    all_scr %>% 
    distinct(id, session) %>% 
    arrange(id, session) %>% 
    mutate(exist = TRUE)

# ECG data is available for 86 participants*sessions
ecg_participants <-
    all_ecg %>% 
    distinct(id, session) %>% 
    arrange(id, session) %>% 
    mutate(exist = TRUE)

# Plot the number of successfully matched markers
# For markers
markers %>% 
    group_by(id, session) %>%
    count() %>% 
    ggplot() +
        aes(x = id, y = n, fill = session) +
        geom_col(position = "dodge")

all_scr %>% 
    inner_join(markers_scr) %>% 
    full_join(marker_participants) %>% 
    arrange(id, session, time) %>% 
    mutate(id = as_factor(id), session = as_factor(session)) %>% 
    drop_na(id) %>% 
    group_by(id, session) %>% 
    count() %>% 
    ggplot() +
        aes(x = id, y = n, fill = session, label = n) +
        geom_col(position = "dodge") +
        geom_text() +
        coord_flip()


all_ecg %>% 
    full_join(markers_ecg, by = c("id", "session")) %>% 
    mutate(id = as_factor(id), session = as_factor(session)) %>% 
    group_by(id, session) %>% 
    count() %>% 
    ggplot() +
        aes(x = id, y = n, fill = session, label = n) +
        geom_col(position = "dodge") +
        geom_text() +
        coord_flip()

# Missing markers
# (4 emot, 26 emot+session, 54 emot+session)



# Cutting all data before point of interest ---------------
# 30s before first, and 40s after

trimmed_ecg <- 
    all_ecg %>%
    left_join(markers_ecg, by = c("id", "session", "time")) %>%
    arrange(id, session, time) %>%
    drop_na(id) %>%
    group_by(id, session) %>%
    nest() %>%
    mutate(trimmed_data = map(
        data,
        ~ trim_data(
            df = .x,
            stim_var = "stimulus",
            time_var = "time",
            padding_before = 30,
            padding_after = 40)
    ))

trimmed_ecg2 <-
    trimmed_ecg %>% 
    mutate(trimmed_data = map(trimmed_data, 
                              ~ pull(.x, ecg) %>% 
                                paste(collapse = ",")))

pwalk(list(trimmed_ecg2$id, trimmed_ecg2$session, trimmed_ecg2$trimmed_data),
      ~write_lines(x = ..3,
                   path = str_glue("all_data/ecg_trimmed/{..1}_{..2}_ECG.txt")))
