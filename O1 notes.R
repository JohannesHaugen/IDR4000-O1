# Last inn pakker
library(readxl)
library(tidyverse)


# Forbered datasettet
data <- read_excel("./data/dataset.xlsx", na = "na") %>% # Last inn datasettet og lagrer det i et nytt objekt "data"
  select(subject, timepoint, vo2.max) %>% # Velger ut hvilke variabler vi er interessert i
  pivot_wider(names_from = timepoint,
              values_from = vo2.max) %>% # Formatterer datasettet slik at det blir til et "wide" datasett
  filter(!is.na(t2),
         !is.na(t1)) # Filtrerer vekk forsøkspersoner uten gyldige resultater


# Estimat av technical error
tec.err <- data %>% # Lager ett nytt objekt som skal inneholde estimatet av technical error
  mutate(change = t2 - t1) %>% # Lager en ny variabel "change" som viser forskjellen mellom t2 og t1
  group_by() %>%
  summarise(sd.change = sd(change), 
            mean.test = mean(c(t1, t2)), 
            te.abs = (sd.change / sqrt(2)), 
            te.relative = (te.abs / mean.test) * 100) %>%
  print() # Printer resulatet av utregningen i "summarise"


# Estimat av smallest worthwhile change
swcvo2.max <- data %>% # Lager ett nytt objekt som skal inneholde estimatet av smallest worthwhile change
  rowwise() %>%
  mutate(m = mean(c(t1, t2))) %>%
  ungroup() %>%
  summarise(sd = sd(m), 
            swc = 0.2 * sd) %>%
  print() # Printer resultatet av utregningen i "summarise"




