# Laste inn pakker
library(lme4)
library(readxl)
library(tidyverse)
library(broom)
library(lme4)
library(emmeans)
library(cowplot)
library(kableExtra)

# Laste inn data
df <- read_excel("./data/ten_vs_thirty.xlsx", na = "NA")

# Klargjør data for legpress
legpress <- df %>%
  filter(!is.na(load)) %>% # Fjerner forsøkspersoner uten verdier i load
  filter(exercise == "legpress", # Velger ut øvelsen legpress
         timepoint %in% c("pre", "post")) %>% # Velger ut timepoints
  mutate(timepoint = factor(timepoint,
                            levels = c("pre", "post")), # Endre rekkefølgen på timepoints
         group = factor(group,
                        levels = c("RM30", "RM10"))) # Endre rekkefølgen på gruppene. Ønsker å kontrollere for RM30.

legpress # Printer resultatene

# Lager en figur som viser endringene i styrke for hver forsøksperson i de to gruppene
leg.figur <- legpress %>%
  mutate(timepoint = factor(timepoint, levels = c("pre", "post"))) %>%
  ggplot(aes(timepoint, load, group = subject, color)) + # Velger akser og bestemmer farge etter grupper
  geom_line() + # Hver forsøksperson får en linje mellom timepoints
  labs(x = "Timepoint", y = "1RM (kg)") + # Endre akseetiketter
  theme_minimal() # Velger layout på figuren

leg.figur # Printer figuren
# Figuren viser endringer i styrke hos forsøkspersonene uavhengig av gruppe


# Lager en mixed model
# Interessert i å sammenligne endringer de to gruppene fra pre-test til post-test
# En mixed model kan ta høyde for repeterte målinger ved å bruke forsøkspersonen som en "tilfeldig effekt"
# Hver forsøksperson blir sammenlignet mot deres egen "pretest" (intercept)
lmer1 <- lmer(load ~ timepoint * group + (1|subject), data = legpress)

summary(lmer1) # Printer en oppsummering av resultatene til modellen

## Beskrivelse av output fra modellen:
# (Intercept) er gjennomsnittet ved pre-test (10RM)
# timepointpost er forskjellen fra pre-test til post-test
# groupRM10 er forskjellen mellom gruppene (10RM og 30RM) ved pre-test
# timepointpost:groupRM10 er forskjellen mellom gruppene ved post-test

## Variablene vi er interesserte i
# Forskjellen mellom gruppene når vi har kontrollert for pre-test etter en treningsintervensjon
# 39kg forskjell mellom 10RM og 30RM etter intevernsjonen (post-test)



# Plot av residualene
plot(lmer1)
# Variansen i residualene er tilnærmet like stor som "rangen" fitted values, så dette er ok.



## Estimater fra modellen
# Estimert gjennomsnitt ved hvert timepoint i hver gruppe
est <- emmeans(lmer1, specs = ~ timepoint|group) # Spesifiserer hvilke gjennomsnitt som skal kalkuleres

est # Printer resultatene


## Plotter esimatene fra modellen (Estimert gjennomsnitt)
# Objectet "est" må konverteres til en data frame for å kunne plottes
est.figur <- est %>% # Lager et nytt objekt med utgangspunkt i gjennomsnittene fra est
  data.frame() %>%
  mutate(timepoint = factor(timepoint, levels = c("pre", "post"))) %>%
  ggplot(aes(timepoint, emmean, group = group, color = group)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), # Representerer konfidensintervallene (95% CI)
                position = position_dodge(width = 0.2), # Flytter errorbarene fra hverandre
                width = 0.1) + # Endrer bredden på errorbarene. Det samme med de to linjene under.
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  labs(x = "Timepoint", y = "1RM (kg)") + # Endrer akseetiketter
  theme_minimal() # Endrer temaet til figuren
# Figur: Viser estimerte gjennomsnitt for hver gruppe ved timepoint pre og post

est.figur # Printer figuren


raw.est.figur <- est %>%
  # Estimatene fra modellen
  data.frame() %>%
  mutate(timepoint = factor(timepoint, levels = c("pre", "post"))) %>%
  ggplot(aes(timepoint, emmean, group = group, color = group)) +
  #legg til rå-dataene
  geom_line(data = legpress, aes(timepoint, load, group = subject, color = group),
            alpha = 0.4) + # Legg til gjennomsiktighet til linjene
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.2),
                width = 0.1) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  labs(x = "", y = "1RM (kg)",
       color = "Intensity") + # Nye akseetiketter
  # Fjerner tekst og indikatorer på x-aksen fordi vi skal kombinere dette plottet med et annet, og bruke teksten og indikatorene fra det plottet
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  theme_minimal()
# Figur: Endringer i styrke for hver forsøksperson og estimert gjennomsnitt i hver gruppe ved timepoint pre og post

raw.est.figur # Printer figuren



# Lagrer konfidensintervallene
# Henter 95% konfindensintervall fra hver av koeffisientene
konfidensintervall <- confint(lmer1)
konfidensintervall
# Konfidensintervallet ved post inneholder ikke 0. Det vil si at vi har bevis mot nullhypotesen som sier at det ikke er noen forskjell mellom de to gruppene. Det er en forskjell.


# Lagrer regresjonskoeffisientene
# Henter estimatene fra interaksjonskoeffisientene fra modellen
koef <- summary(lmer1)$coef
koef


# Bruker cbind for å kombinere konfidensintervallene med koeffisientene.
sum1 <- cbind(koef, data.frame(konfidensintervall)[3:6, ])
sum1


# Lager en tabell ved hjelp av kable
koeftabell <- sum1 %>%
  kable(col.names = c("Estimat", "Std. Error", "t-verdi", "CI 2,5%", "CI 97,5%"), # Navn på overskrifter
        digits = c(1,1,2,1,1), # Bestemmer antall desimaler
        caption = "Tabell 1: Estimater fra koeffisientene, standard error, t-statistikk, nedre og øvre
        konfidensintervall ved pre- og post-test i begge gruppene") %>%
  kable_classic()

koeftabell


# Plott for å vise anbefalt intensitet for utvikling av styrke. 
# Estimatene for "timepointpost" og "timepointpost:groupRM10"

# Redusere dataene og plotte dem slik at de viser forskjellene på gruppene ved post-test.
est.diff.figur <- sum1 %>%
  mutate(koef = rownames (.)) %>%
  # Filtrerer variablene
  filter(koef %in% c("timepointpost:groupRM10")) %>%
  # Lager en "timepointvariabel" for å presentere legpress datasettet
  mutate(timepoint = gsub("timepoint", "", koef),
         timepoint = gsub(":groupRM10", "", timepoint)) %>%
  # Legger til en rad slik at pre-test er representert
  add_row(timepoint = "pre", koef = "pre") %>%
  # Fikser rekkefølgen til timepoint variabelen
  mutate(timepoint)



