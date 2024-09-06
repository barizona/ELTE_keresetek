library(tidyverse)

#xxxxxxxxxxxxxxxxxxxx
# AIM: plot salary --------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxx

Tab <- read_tsv("input/KSH_atlag_ELTE_garantalt_ber.tsv") %>% 
  select(-`Forrás`) %>% 
  # add a column "Kar"
  mutate(Kar = NA)

# Format átalg & median fata
Tab <- read_tsv("input/Kozalk_tanacs_atlagberek.tsv") %>% 
  pivot_longer(cols = -Kar, names_to = "Ágazat", values_to = "Bruttó kereset 2024") %>% 
  # split Ágazat to Ágazat & Típus
  separate(`Ágazat`, into = c("Ágazat", "Típus"), sep = " ") %>% 
  # mutate Típus to match Tab
  mutate(`Típus` = case_match(`Típus`, 
                              "ÁTLAG" ~ "ELTE, becsült átlagbér",
                              "MEDIÁN" ~ "ELTE, becsült mediánbér")) %>% 
  # mutate Egyetemi_tanár to Egyetemi tanár
  mutate(`Ágazat` = case_match(`Ágazat`, 
                               "Egyetemi_tanár" ~ "Egyetemi tanár",
                               .default = `Ágazat`)) %>%
  # order columns like in Tab
  select(colnames(Tab)) %>% 
  # merge with Tab
  bind_rows(Tab, .)


         
