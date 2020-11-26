# Last inn datapakker
library(readr)
library(tidyverse)
library(flextable)
library(ggpubr)
library(rstatix)

# Last ned og klargjør datafilen
download.file(url = "https://ndownloader.figstatic.com/files/14702420", 
              destfile = "./data/hypertrophy.csv")  # Laster ned datafilen.

hypertrophy <- read_csv("./data/hypertrophy.csv")  # Laster inn datafilen og kobler den til objektet hypertrophy.

var_interest <- c("SUB_ID", "GROUP", "CLUSTER", "AGE", "T1_BODY_MASS", "PERCENT_TYPE_II_T1", 
                  "Squat_3RM_kg", "DXA_LBM_1", "DXA_FM_T1", "SQUAT_VOLUME") # Plukker ut hvilke variabler vi er interesserte i å ha med og lagrer de i var_interest.


##### TABELL #####
# Lager en oppsummeringstabell (tabell 1)
hyptable <- hypertrophy %>% # Kobler datasettet hypetrophy til objektet hyptable slik at vi kan lage en tabell uten å påvirke hypertrophy datasettet.
  
  select(all_of(var_interest)) %>% # Selekterer variablene fra var_interest.
  
  
  # Denne delen spesifiserer hvilke verdier vi vil ha med og komprimerer datasettet.
  # Navnene kommer inn i "variable" og verdier inn i "value".
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = AGE:SQUAT_VOLUME) %>%
  
  group_by(CLUSTER, variable) %>%
  filter(!is.na(CLUSTER)) %>%  #Filtrerer vekk forsøkspersoner som ikke ble regnet som 
  # HIGH eller LOW responders.
  
  summarise (m = mean(value),
             s = sd(value)) %>%  #Regner ut gjennomsnittet og standardavviket.
  
  mutate(ms = paste(round(m, 1), 
                    " (",
                    round(s, 1),
                    ")", sep = ""), # Denne delen gjør at standardavviket havner i en parantes
         # med en desimal.
         
         CLUSTER = factor(CLUSTER, levels = c("LOW", "HIGH"), #Justerer navnene på variablene
                          labels = c("LOW (n = 10)",
                                     "HIGH (n = 10)")),
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
                                      "Total treningsvolum (kg) fra uke 1 til 6"))) %>%
  select(-m, -s) %>%   # Selekterer vekk gjennomsnittet og standardavviket
  
  pivot_wider(names_from = CLUSTER,
              values_from = ms) %>% 
  
  arrange(variable) %>%   # Sorterer tabellen med utgangspunkt i variablene
  select(variable, `LOW (n = 10)`, `HIGH (n = 10)`) # Sorterer rekkefølgen på high og low


# Print tabellen
hyptable %>% # Bruker objektet hyptable til å lage tabellen
  
  flextable() %>% #Lag tabell med Flextable
  
  set_header_labels(variable = "Variabel") %>% 
  
  add_header_row(values = "Tabell 1", colwidths = 3) %>% # Angir tittel på tabellen
  
  add_footer_row(values = "Verdier er oppgitt i gjennomsnitt og (Standardavvik)", colwidths = 3) %>%  #Angir en fotnote med beskrivelse av tabellen.
  
  autofit() %>% #Gjør tabellen penere
  fontsize(part = "header", size = 12) # Endrer størrelsen på headerne



##### Dataanalyse, type II fiber #####
# Oppsummering av valgt variabel
df <- read_csv("./data/hypertrophy.csv") %>% # Les datasettet og angi det i objektet df
  filter(!is.na(CLUSTER)) %>% # Filtrerer vekk fp uten en definert gruppe
  select(CLUSTER, SUB_ID, T3T1_PERCENT_CHANGE_FAST_CSA) %>% # Velg hvilke variabler vi vil ha med.
  group_by(CLUSTER) %>% # Grupper etter CLUSTER-variabelen
  get_summary_stats(T3T1_PERCENT_CHANGE_FAST_CSA, type = "mean_sd") # Få en oppsummering av tallene.

df %>%
  mutate(variable = factor(variable, levels = c("T3T1_PERCENT_CHANGE_FAST_CSA", 
                                                "T3T1_PERCENT_CHANGE_FAST_CSA"),
                           labels = c("Type II fiber change (%)",
                                      "Type II fiber change (%)"))) %>% # Endrer variabelteksten.
  flextable() %>% #Lag tabell med Flextable
  
  set_header_labels(CLUSTER = "Cluster", # Endrer overskriftene i tabellen.
                    variable = "Variabel", 
                    n = "n",
                    mean = "Gjennomsnitt",
                    sd = "SD") %>%
  add_header_row(values = "Tabell 2", colwidths = 5) %>% #Angir tittel på tabellen
  
  
  autofit() %>% #Gjør tabellen penere
  fontsize(part = "header", size = 12) # Endrer størrelsen på headerne


# Figur 1
hyp <- read_csv("./data/hypertrophy.csv") %>% 
  filter(!is.na(CLUSTER)) # Filtrerer vekk forsøkspersoner uten definert gruppe

bxp <- ggboxplot(hyp, x = "CLUSTER", y = "T3T1_PERCENT_CHANGE_FAST_CSA", 
                 ylab = "Endringer i type II fiber (%)", xlab = "CLUSTER", add = "jitter", SIZE = 0.5, fill = "CLUSTER",
                 title = "Figur 1") + # Denne funskjonen lager et boxplot med utgangspunkt i variabelen hypertrophy.
  font("title", size = 10, face = "bold") + # Endrer størrelsen på etikettene
  font("xlab", size = 9) +
  font("ylab", size = 9) 

bxp + theme(legend.position = "none") # Fjerner legenden


# Normalfordeling
# Tester om dataene er normaltfordelt med en shapiro-test.
shapiro <- hyp %>%
  group_by(CLUSTER) %>%
  shapiro_test(T3T1_PERCENT_CHANGE_FAST_CSA)

# Plott som viser normalfordelingen
ggqqplot(hyp, x = "T3T1_PERCENT_CHANGE_FAST_CSA", facet.by = "CLUSTER") + # Lager plottet med utgangspunkt i variablene
  labs(caption = "QQ-plottet viser at dataene er tilnærmet normalfordelt.", title = "Figur 2", cex.title=0.5) +
  font("title", size = 10, face = "bold") +
  font("xlab", size = 9) +
  font("ylab", size = 9) +
  font("caption", size = 8)


# Varians
levene <- hyp %>% # Variansen i gruppene testes ved hjelp av en levene-test.
  levene_test(T3T1_PERCENT_CHANGE_FAST_CSA ~ CLUSTER)


# t-test
stat.test <- hyp %>% # Lager et nytt objekt
  t_test(T3T1_PERCENT_CHANGE_FAST_CSA ~ CLUSTER, var.equal = TRUE) %>% # Gjennomfører t-testen med valgte variabler med en varians som er registrert som lik.
  add_significance()
stat.test # Viser resultatet av testen

# tabell t-test
stat.test %>%
  select(group1, group2, n1, n2, p) %>% # Velger hvilke variabler vi vil ha med i tabellen
  flextable() %>% #Lag tabell med Flextable
  
  set_header_labels(group1 = "Gruppe 1",
                    group2 = "Gruppe 2",
                    n1 = "Antall i gruppe 1",
                    n2 = "Antall i gruppe 2",
                    p = "P-verdi") %>% # Angir overskriftene i de ulike kolonnene
  
  add_header_row(values = "Tabell 3", colwidths = 5) %>% # Angir tittel på tabellen
  
  autofit() %>% # Gjør tabellen penere
  fontsize(part = "header", size = 12) # Endrer størrelsen på headerne


# Effect-size regnet ut ved hjelp av Cohens D-formel.
cohen <- hyp %>%  cohens_d(T3T1_PERCENT_CHANGE_FAST_CSA ~ CLUSTER, var.equal = TRUE) 


# Figur 2: Oppsummering av resultatene
stat.test <- stat.test %>% add_xy_position(x = "CLUSTER")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE),
       title = "Figur 3") +
  theme(legend.position = "none") +
  font("subtitle", size = 9) +
  font("xlab", size = 9) +
  font("ylab", size = 9) + 
  font("title", size = 10, face = "bold")

