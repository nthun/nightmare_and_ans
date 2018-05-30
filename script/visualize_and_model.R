# Aggregate SCR data
# devtools::install_github("thomasp85/patchwork")
library(tidyverse)
library(fs)
library(lme4)
library(lmerTest)
library(patchwork)

scr_era_dir <- "all_data/scr01/"


# Data reading and verification -------------------------------------------
# Participants data
participants <-
    readxl::read_excel("all_data/participants/2016_2017_összegzés_25_05_2018.xlsx") %>%
    select(
        id = ID,
        group = Group,
        gender = Gender,
        pic_set = pics.,
        scr_emot_data = SCR,
        emg_emot_data = EMG,
        ecg_emot_data = HR,
        scr_recall_data = SCR_r,
        emg_recall_data = EMG_r,
        ecg_recall_data = HR_r,
        comment_emot = megj.,
        edf_00 = EDF_00,
        edf_01 = EDF_01,
        comment_recall = megj.__1,
        cortisol_data = Cortisol,
        diary_data = Naplo,
        full_data = `Teljes adat(1)`,
        cortisol_without = 18,
        exclude = KIZARNI,
        reason = INDOK) %>%
    mutate(
        id = if_else(
                        str_length(id) == 1,
                        paste0("0", as.character(id)),
                        as.character(id)),
        gender = case_when(gender == 1 ~ "Male",
                           gender == 2 ~ "Female",
                           TRUE ~ NA_character_),
        group = case_when(
                            group == "CTR" ~ "Control",
                            group == "NM" ~ "Nightmare",
                            TRUE ~ NA_character_)) %>%
    filter(exclude != 1) %>% 
    select(id, group, gender, pic_set) %>% 
    arrange(id)

# Getting markers
markers <- 
    read_csv("all_data/marker/all_triggers.csv", col_types = "cidiiiiiic") %>%
    mutate(id = if_else(str_length(id) == 1, paste0("0",id), id),
           pic_valence = case_when(category == 0 ~ "Neutral",
                                   category == 1 ~ "Negative",
                                   TRUE ~ NA_character_) %>% factor(levels = c("Neutral", "Negative"))) %>% 
    select(-time, -category)

# Extraction picture sets
pic_sets <- 
    markers %>% 
    drop_na(block) %>% 
    filter(session == "emot") %>%
    select(id, stimulus) %>% 
    inner_join(participants %>% select(id, pic_set), by = "id") %>% 
    distinct(stimulus, pic_set) %>% 
    arrange(pic_set)

set_a <- pic_sets %>% filter(pic_set == "A") %>% pull(stimulus)
set_b <- pic_sets %>% filter(pic_set == "B") %>% pull(stimulus)

# Merging all data
scr_era <-
    tibble(file = dir_ls(scr_era_dir, regexp = ".*_era.txt$")) %>% 
    mutate(id = str_replace(file, paste0(scr_era_dir, "(\\d+)_.+_SCR_era.txt$"), "\\1"),
           session = str_replace(file, paste0(scr_era_dir, "\\d+_(.+)_SCR_era.txt$"), "\\1"),
           data = map(file, 
                      ~read_tsv(.x) %>% 
                          select(order = Event.Nr, stimulus = Event.NID, scr = CDA.SCR) %>% 
                          mutate(block = case_when(order <= 20 ~ 1L,
                                                   order >20 & order <= 40 ~ 2L,
                                                   order > 40 ~ 3L)) %>% 
                          select(-order)
                      )) %>% 
    unnest() %>% 
    group_by(id) %>% 
    mutate(scr_std = scale(scr) %>% as.numeric(),
           scr_sqrt = sqrt(scr) %>% scale() %>% as.numeric()) %>% 
    ungroup() %>% 
    # Remove outliers
    filter(abs(scr_std) < 3.5) %>% 
    inner_join(markers, by = c("id", "session", "stimulus", "block")) %>% 
    inner_join(participants, by = c("id")) %>% 
    # Add whether a picture in the recall session is new or not
    mutate(novelty = case_when(
                           session == "recall" & pic_set == "A" & stimulus %in% set_a ~ "old",
                           session == "recall" & pic_set == "A" & !(stimulus %in% set_a) ~ "new",
                           session == "recall" & pic_set == "B" & stimulus %in% set_b ~ "old",
                           session == "recall" & pic_set == "B" & !(stimulus %in% set_b) ~ "new",
                           TRUE ~ NA_character_) %>% 
               factor(., levels = c("old", "new")))


# Use square root transformation to correct data
scr_era %>% 
    ggplot() +
        aes(x = scr_sqrt) +
        geom_histogram()

# Should we omit participants with too many non-responses?
scr_era %>% 
    filter(scr == 0) %>% 
    group_by(id, session) %>% 
    count() %>% 
    print(n = 100)


# Plotting ----------------------------------------------------------------
# Plotting SCR
plot_emot_scr <-
    scr_era %>%
    filter(session == "emot") %>% 
    group_by(session, block, group, pic_valence) %>% 
    summarise(Mean = mean(scr_sqrt, na.rm = TRUE),
              Se = sd(scr_sqrt, na.rm = TRUE)/sqrt(n())) %>% 
    ggplot() +
        aes(x = block, y = Mean, group = pic_valence, ymin = Mean - Se, ymax = Mean + Se) +
        geom_point() +
        geom_errorbar(width = .2) +
        geom_line(aes(linetype = pic_valence), size = 1) +
        coord_cartesian(ylim = c(-0.4, .75)) +
        scale_x_continuous("Block", breaks = 1:3) +
        scale_y_continuous("Mean (SEM) of skin counductance response") +
        guides(linetype = "none") +
        facet_grid(group ~ session) +
        theme(strip.text.y = element_blank())

plot_recall_scr <-
    scr_era %>%
    filter(session == "recall") %>% 
    group_by(session, group, pic_valence, novelty) %>% 
    summarise(Mean = mean(scr_sqrt, na.rm = TRUE),
           Se = sd(scr_sqrt, na.rm = TRUE)/sqrt(n())) %>% 
    ggplot() +
        aes(x = novelty, y = Mean, group = pic_valence, ymin = Mean - Se, ymax = Mean + Se) +
        geom_point() +
        geom_errorbar(width = .2) +
        geom_line(aes(linetype = pic_valence), size = 1) +
        coord_cartesian(ylim = c(-0.4, .75)) +
        scale_x_discrete("Novelty") +
        scale_y_continuous(NULL) +
        facet_grid(group ~ session) +
        theme(axis.text.y = element_blank(),
              axis.ticks = element_blank())

plot_emot_scr + 
    plot_recall_scr +
    plot_layout(widths = c(3,2))

# Subjective arousal
plot_emot_arousal <-
    scr_era %>%
    filter(session == "emot") %>% 
    group_by(session, block, group, pic_valence) %>% 
    summarise(Mean = mean(arousal, na.rm = TRUE),
              Se = sd(arousal, na.rm = TRUE)/sqrt(n())) %>% 
    ggplot() +
        aes(x = block, y = Mean, group = pic_valence, ymin = Mean - Se, ymax = Mean + Se) +
        geom_point() +
        geom_errorbar(width = .2) +
        geom_line(aes(linetype = pic_valence), size = 1) +
        coord_cartesian(ylim = c(1, 9)) +
        scale_x_continuous("Block", breaks = 1:3) +
        scale_y_continuous("Mean (SEM) of subjective arousal") +
        guides(linetype = "none") +
        facet_grid(group ~ session) +
        theme(strip.text.y = element_blank())

plot_recall_arousal <-
    scr_era %>%
    filter(session == "recall") %>% 
    group_by(session, group, pic_valence, novelty) %>% 
    summarise(Mean = mean(arousal, na.rm = TRUE),
              Se = sd(arousal, na.rm = TRUE)/sqrt(n())) %>% 
    ggplot() +
        aes(x = novelty, y = Mean, group = pic_valence, ymin = Mean - Se, ymax = Mean + Se) +
        geom_point() +
        geom_errorbar(width = .2) +
        geom_line(aes(linetype = pic_valence), size = 1) +
        coord_cartesian(ylim = c(1, 9)) +
        scale_x_discrete("Novelty") +
        scale_y_continuous(NULL) +
        facet_grid(group ~ session) +
        theme(axis.text.y = element_blank(),
              axis.ticks = element_blank())

plot_emot_arousal + 
    plot_recall_arousal +
    plot_layout(widths = c(3,2))
    

# Subjective valence
plot_emot_valence <-
    scr_era %>%
    filter(session == "emot") %>% 
    group_by(session, block, group, pic_valence) %>% 
    summarise(Mean = mean(valence, na.rm = TRUE),
              Se = sd(valence, na.rm = TRUE)/sqrt(n())) %>% 
    ggplot() +
    aes(x = block, y = Mean, group = pic_valence, ymin = Mean - Se, ymax = Mean + Se) +
    geom_point() +
    geom_errorbar(width = .2) +
    geom_line(aes(linetype = pic_valence), size = 1) +
    coord_cartesian(ylim = c(1, 9)) +
    scale_x_continuous("Block", breaks = 1:3) +
    scale_y_continuous("Mean (SEM) of subjective arousal") +
    guides(linetype = "none") +
    facet_grid(group ~ session) +
    theme(strip.text.y = element_blank())

plot_recall_valence <-
    scr_era %>%
    filter(session == "recall") %>% 
    group_by(session, group, pic_valence, novelty) %>% 
    summarise(Mean = mean(valence, na.rm = TRUE),
              Se = sd(valence, na.rm = TRUE)/sqrt(n())) %>% 
    ggplot() +
    aes(x = novelty, y = Mean, group = pic_valence, ymin = Mean - Se, ymax = Mean + Se) +
    geom_point() +
    geom_errorbar(width = .2) +
    geom_line(aes(linetype = pic_valence), size = 1) +
    coord_cartesian(ylim = c(1, 9)) +
    scale_x_discrete("Novelty") +
    scale_y_continuous(NULL) +
    facet_grid(group ~ session) +
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank())

plot_emot_valence + 
    plot_recall_valence +
    plot_layout(widths = c(3,2))



# Model building ----------------------------------------------------------
scr_sum <-
    scr_era %>% 
    group_by(id, session, block, group, pic_valence) %>% 
    summarise(scr = mean(scr_sqrt, na.rm = TRUE)) %>% 
    filter(session == "emot")


model_lm <- lm(scr ~ group * block * pic_valence, data = scr_sum)

model_i <- lmer(scr ~ group * block * pic_valence + (1 | id), data = scr_sum)
model_is <- lmer(scr ~ group * block * pic_valence + (block : group : pic_valence | id), data = scr_sum)

summary(model_i)
anova(model_i, model_is)

# Remove 3 way interaction
model_best <- model_i
model_3 <- update(model_best, . ~ . - group : block : pic_valence)

anova(model_best, model_3) # Drop 3-way
summary(model_3)

model_best <- model_3

# Remove 2-way interactions
model_2_1 <- update(model_best, . ~ . - group : block) # Drop
model_2_2 <- update(model_best, . ~ . - group : pic_valence) # Keep
model_2_3 <- update(model_best, . ~ . - block : pic_valence) # Drop

anova(model_best, model_2_3)
model_best <- model_3 <- update(model_best, . ~ . - group:block - block:pic_valence)
summary(model_best)

lmer(scr ~ group + block + pic_valence + group:pic_valence + (1 | id), data = scr_sum) %>% summary()
lmer(scr ~ block*group*pic_valence + (1 | id), data = scr_sum) %>% summary()
    
# time x group x pic_valence interaction
# time x group x pic_valence x session
    

# bugfix area ------------------------------------------------------------------


