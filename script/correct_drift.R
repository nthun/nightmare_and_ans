# Process ECG data (downsample, correct drift, put it inrequired format)

library(tidyverse)
source("script/downsample.R")

start_rate <- 1024
end_rate <- 128

all_ecg <- 
    read_csv("all_data/clean/all_ecg.csv", col_types = "ccdd") %>% 
    mutate(session = str_replace(session, "._(\\w+)$", "\\1"))


# Correct drift using moving average --------------------------------------
# First verify if the method works
temp <-
    all_ecg %>% 
    filter(id == "10" & session == "emot") %>% 
    downsample("ecg", start_rate, end_rate)
    

temp %>% 
    mutate(rolmean = caTools::runmean(value, end_rate, align = "center"),
           corrected = value - rolmean) %>%     
    ggplot() +
    aes(x = time) +
    geom_line(aes(y = corrected)) +
    geom_line(aes(y = rolmean), color = "blue") +
    geom_line(aes(y = value), alpha = .5)

# Do the correction by using a 10 second rolling mean
ecg_ds <- 
    all_ecg %>% 
    group_by(id, session) %>% 
    nest %>% 
    mutate(ecg_ds = map(data, ~downsample(.x, "ecg", start_rate, end_rate))) %>% 
    select(-data) %>% 
    unnest(ecg_ds) %>% 
    group_by(id, session) %>% 
    mutate(rolmean = caTools::runmean(value, end_rate, align = "center"),
           corrected = value - rolmean)

# Prepare for saving
ecg_save <- 
    ecg_ds %>% 
    select(-time, -value, -rolmean) %>% 
    nest() %>% 
    mutate(to_save = map_chr(data, 
                             ~pull(.x, corrected) %>% 
                              paste(collapse = ",")))

# Save all data
pwalk(list(ecg_save$id, ecg_save$session, ecg_save$to_save), ~write_lines(x = ..3, path = str_glue("all_data/ecg_full/{..1}_{..2}_ECG.txt")))


