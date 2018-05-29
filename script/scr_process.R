# Create ledalab input
library(tidyverse)
all_scr <- 
    read_csv("all_data/clean/all_scr.csv", col_types = "ccdd") %>% 
    mutate(session = str_replace(session, "._(\\w+)$", "\\1"))

# Round markers to the closest sampling point
scr_markers <- 
    read_csv("all_data/marker/all_triggers.csv", col_types = "cidiiiiiic") %>%
    mutate(id = if_else(str_length(id) == 1, paste0("0",id), id)) %>% 
    mutate(time = plyr::round_any(time,1/32)) %>% 
    drop_na(block) %>% 
    arrange(id, session, time)

scr_save <-
    all_scr %>% 
    left_join(scr_markers, by = c("id", "session", "time")) %>% 
    select(id, session, time, scr, marker = stimulus) %>% 
    mutate(marker = if_else(is.na(marker), 0L, marker)) %>% 
    drop_na() %>% 
    group_by(id, session) %>% 
    nest()

# Save files in the format ledalab can read (text 1)
fs::dir_create("all_data/scr_to_process/")

pwalk(list(scr_save$id, scr_save$session, scr_save$data),
      ~write_tsv(x = ..3, path = str_glue("all_data/scr_to_process/{..1}_{..2}_SCR.txt"), col_names = FALSE))

