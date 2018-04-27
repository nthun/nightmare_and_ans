# Create one single clean file for the aggregated ECG and SCR data

dir = "all_data/converted/"
ecg_pattern = "_ECG.txt"
scr_pattern = "_SCR.txt"

# Aggregate all ECG data and write it in a single file
all_ecg <-
    tibble(file = fs::dir_ls(dir, regexp = ecg_pattern)) %>% 
    mutate(name = str_replace(file, str_glue("{dir}(\\d+.*){ecg_pattern}$"),"\\1")) %>% 
    separate(name, into = c("id","session"), extra = "merge") %>% 
    mutate( data = map(file, ~read_csv(file = .x, 
                                       col_names = c("time", "ecg"), 
                                       col_types = "dd")))
all_ecg %>% 
    unnest() %>% 
    select(-file) %>% 
    write_csv("all_data/clean/all_ecg.csv")

# Aggregate all SCR data and write it in
all_scr <- 
    tibble(file = fs::dir_ls(dir, regexp = scr_pattern)) %>% 
    mutate(name = str_replace(file, str_glue("{dir}(\\d+.*){scr_pattern}$"),"\\1")) %>% 
    separate(name, into = c("id","session"), extra = "merge") %>% 
    mutate( data = map(file, ~read_csv(file = .x, 
                                       col_names = c("time", "scr"), 
                                       col_types = "dd")))

all_scr %>% 
    unnest() %>% 
    select(-file) %>% 
    write_csv("all_data/clean/all_scr.csv")




