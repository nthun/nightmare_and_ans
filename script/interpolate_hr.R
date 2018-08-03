# Intrapolate IBI (using both original and corrected IBI)

library(tidyverse)
library(multidplyr)
library(fs)
source("script/calculate_hr.R")

ibi <-
    tibble(artifact_file = dir_ls(path = "all_data/artifact_corrected_ibi_berntson/", regexp = "IBI_artifacts_IBIs_from_"),
           corrected_file = dir_ls(path = "all_data/artifact_corrected_ibi_berntson/", regexp = "IBI_artifactCorrected_IBIs_from_")
    ) %>% 
    transmute(id = str_extract(artifact_file, "\\d+"),
              session = str_replace(artifact_file, ".*_\\d+_(\\w+)_ECG.txt.csv$", "\\1"),
              artifact_data = map(artifact_file, ~read_csv(.x, col_types = "di")),
              corrected_data = map(corrected_file, ~read_csv(.x, col_types = "d")),
              data = map2(artifact_data,
                          corrected_data,
                          ~bind_cols(.x, .y))
    ) %>%
    unnest(data) %>% 
    rename(ibi = IBI, ibi_corrected = IBI_artefactCorrected)


memory.limit(16000000)

hrd <-
    ibi %>%
    group_by(id, session) %>%
    mutate(ibi_time = cumsum(ibi)) %>%
    select(-ibi,-artifact) %>%
    nest() %>%
    mutate(hr_data = purrr::map(
                        data,
                        ~calculate_hr(
                            df = .x,
                            time_col = "ibi_time",
                            ibi_col = "ibi_corrected",
                            window = 500,
                            align = "center"
                        )
                    )
    )

beepr::beep(sound = 1)

hrd %>%
    unnest(hr_data) %>%
    write_excel_csv("hrd_cubic.csv")

