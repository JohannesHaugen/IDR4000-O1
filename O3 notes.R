# Last inn datapakker
library(tidyverse) # Laster inn de ulike pakkene som blir brukt i prosjektet
library(readr)
library(rstatix)
library(flextable)
library(grid)
library(gridExtra)


# Last ned datafilen
download.file(url = "https://ndownloader.figstatic.com/files/14702420", 
              destfile = "./data/hypertrophy.csv") # Laster ned datafilen.

hypertrophy <- read_csv("./data/hypertrophy.csv")  # Laster inn datafilen og kobler den til objektet hypertrophy.


# Velger ut interessante variabler
var_interest <- c("SUB_ID", "GROUP", "AGE", "T1_BODY_MASS", "PERCENT_TYPE_II_T1", 
                  "Squat_3RM_kg", "DXA_LBM_1", "DXA_FM_T1", "SQUAT_VOLUME") # Plukker ut hvilke variabler vi er interesserte i å ha med og lagrer de i var_interest.

tabell1 <- hypertrophy %>% # Kobler datasettet hypetrophy til objektet hyptable slik at vi kan lage en tabell uten å påvirke hypertrophy datasettet.
  
  select(all_of(var_interest)) %>% # Selekterer variablene fra var_interest.
  
  
  # Denne delen spesifiserer hvilke verdier vi vil ha med og komprimerer datasettet.
  # Navnene kommer inn i "variable" og verdier inn i "value".
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  group_by(variable) %>%
  summarise (m = mean(value),
             s = sd(value)) %>%  #Regner ut gjennomsnittet og standardavviket.
  
  mutate(Gjennomsnitt = paste(round(m, 1), 
                    " (",
                    round(s, 1),
                    ")", sep = ""), # Denne delen gjør at standardavviket havner i en parantes
         # med en desimal.
         variable = factor(variable, 
                           levels = c("AGE", # Bestemmer rekkefølgen i tabellen
                                      "T1_BODY_MASS", 
                                      "DXA_LBM_1", 
                                      "DXA_FM_T1", 
                                      "PERCENT_TYPE_II_T1", 
                                      "Squat_3RM_kg", 
                                      "SQUAT_VOLUME"), 
                           labels = c("Alder (år)", # Bestemmer navnene på variablene
                                      "Kroppsvekt (kg)", 
                                      "DXA LST (kg)", 
                                      "DXA FM (kg)", 
                                      "Type II Fiber (%)", 
                                      "3RM knebøy (kg)", 
                                      "Totalt treningsvolum (kg) fra uke 1 til 6"))) %>%
  select(-m, -s) %>%  # Selekterer vekk gjennomsnittet og standardavviket
  arrange(variable)   # Sorterer tabellen med utgangspunkt i variablene


tabell1 %>% # Bruker objektet hyptable til å lage tabellen
  
  flextable() %>% #Lag tabell med Flextable
  
  set_header_labels(variable = "Variabel") %>% 
  
  add_header_row(values = "Tabell 1", colwidths = 2) %>% # Angir tittel på tabellen
  
  add_footer_row(values = "Verdier er oppgitt i gjennomsnitt og (Standardavvik)", colwidths = 2) %>%  #Angir en fotnote med beskrivelse av tabellen.
  
  autofit() %>% #Gjør tabellen penere
  fontsize(part = "header", size = 12)


##### Regresjonsmodell ######
hypertrophy %>%
  ungroup() # Fjerner grupperingen av datasettet.

# Lager modellen
model1 <- lm(Squat_3RM_kg ~ AVG_CSA_T1 + DXA_LBM_1, data = hypertrophy) # Lager en regresjonsmodell hvor vi tester sammenhengen i Squat_3RM_kg med både kroppsvekt og tverrsnittsareal.
summary(model1)

tidymodel1 <- tidy(model1) # Gjør tallene fra modellen penere og lagrer det i et nytt objekt.

# Korrelasjonstest
cor <- cor.test(hypertrophy$Squat_3RM_kg,  hypertrophy$DXA_LBM_1)

# Konfidensintervallene til modellen
cfmodel1 <- confint(model1)

# Lager tabell med regresjonsmodellen og konfidensintervallene
tabell2 <- cbind(tidymodel1, cfmodel1) %>% # Setter sammen tidymodel2 og cfmodel2
  mutate(term = factor(term, levels = c("(Intercept)",
                                        "AVG_CSA_T1",
                                        "DXA_LBM_1"),
                       labels = c("Intercept", 
                                  "AVG CSA", 
                                  "DXA LBM"))) %>% # Endrer navn på kolonnene under "term"
  flextable() %>% # Binder sammen konfidensintervallene og regresjonsmodellen til en tabell
  colformat_num(col_keys = c("estimate", 
                             "std.error",
                             "statistic", 
                             "p.value",
                             "2.5 %",
                             "97.5 %"), 
                digits = 3) %>% # Endrer antall desimaler på bestemte kolonner.
  set_header_labels(estimate = "Estimat (r)", # Endrer navn 
                    std.error = "Standard Error", 
                    statistic = "Statistic (t)",
                    p.value = "P-verdi",
                    term = "Term",
                    "2.5 %" = "CI 2.5 %",
                    "97.5 %" = "CI 97.5 %") %>%
  autofit() %>% # Gjør tabellen penere.
  add_header_row(values = "Tabell 2: Resultater regresjonsmodell med konfidensintervaller", colwidths = 7) %>% # Legger til en overskrift.
  fontsize(part = "header", size = 12) # Endrer størrelsen på overskriftene.


# Lager figurer som sammenligner de to variablene
figur1 <- ggplot(hypertrophy, aes(AVG_CSA_T1, Squat_3RM_kg)) + 
  geom_point() +
  geom_smooth(method = lm, se=FALSE) +
  labs(title="Figur 1: Muskelstyrke og muskelstørrelse",
       x = expression(paste("Tverrsnittsareal (", mu, "m)")) , y = "Muskelstyrke (kg)")

figur2 <- ggplot(hypertrophy, aes(DXA_LBM_1, Squat_3RM_kg)) + 
  geom_point() +
  geom_smooth(method = lm, se=FALSE) +
  labs(title="Figur 2: Muskelstyrke og kroppsmasse",
       x="Kroppsmasse (kg)", y = "Muskelstyrke (kg)")

grid.arrange(figur1, figur2, nrow=1)

