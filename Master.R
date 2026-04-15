#FORSØK PÅ Å FINNE NOEN SAKER, masterrelatert

sivile_saker %>% dplyr::select(utfall_riktig, relasjon, referanse) %>% filter (relasjon == 3 ) %>% filter (utfall_riktig == 1) %>% tbl_summary()


sivile_saker %>% dplyr::select(utfall_riktig) %>% filter(utfall_riktig==1) %>% tbl_summary()

sivile_saker %>% dplyr::select(relasjon) %>% filter(relasjon==3) %>% tbl_summary()


sivile_saker_erstatning %>% dplyr::select (relasjon, deltakelse_sikkerhetsbrudd) %>% filter (relasjon == 3) %>% tbl_cross()


sivile_saker_erstatning <- sivile_saker_erstatning %>% mutate(nærstående = case_when(relasjon == 1 | relasjon == 2 | relasjon == 3 | relasjon == 4 | relasjon == 5 ~ 1,
                                                                                    TRUE ~ 0))


sivile_saker_erstatning %>% dplyr::select(nærstående, ektefelle, utfall_riktig) %>% tbl_summary(by=utfall_riktig, type=list(ektefelle ~ "categorical", 
                                                                                                                            nærstående ~ "categorical"))
sivile_saker%>% dplyr::select(nærstående, ektefelle, utfall_riktig) %>% tbl_summary(by=utfall_riktig, type=list(ektefelle ~ "categorical", 
                                                                                                                            nærstående ~ "categorical"))

sivile_saker_erstatning %>% dplyr::select (nærstående, deltakelse_sikkerhetsbrudd) %>% tbl_cross()

sivile_saker %>% dplyr::select (ektefelle, erstat_lemp) %>% tbl_cross()

sivile_saker %>% dplyr::select (ektefelle, erstat_lemp) %>% tbl_cross()


sivile_saker %>% dplyr::select (kvinne_offer, erstat_lemp) %>% tbl_cross()

sivile_saker <- sivile_saker %>% 
  dplyr::mutate(
    utfall_finans_endret = dplyr::case_when(
      utfall_fil1999 == 5 ~ 0,
      TRUE ~ utfall_riktig     # ellers: behold verdien fra utfall_riktig
    )
  )

sivile_saker %>% dplyr::select (utfall_finans_endret, utfall_riktig) %>% tbl_summary()


sivile_saker %>% dplyr::select (kvinne_offer, erstat_lemp, ektefelle, deltakelse_sikkerhetsbrudd, hvordan_passord, referanse) %>% filter (kvinne_offer == 1) %>% filter(erstat_lemp == 3) %>% tbl_summary(by= erstat_lemp)



sivile_saker %>% dplyr::select (kvinne_offer, hvordan_passord, hvorfor_passord, erstat_lemp, utfall_finans_endret) %>% filter (kvinne_offer == 0) %>% tbl_summary(by=utfall_finans_endret)

sivile_saker %>% dplyr::select (kvinne_offer, hvordan_passord, hvorfor_passord, erstat_lemp) %>% filter (kvinne_offer == 0) %>% tbl_summary(by=erstat_lemp)

sivile_saker %>% 

sivile_saker %>% dplyr::select (kvinne_offer, ektefelle) %>% filter (kvinne_offer == 1) %>% tbl_summary()


sivile_saker %>% dplyr::select (kvinne_offer, ektefelle, erstat_lemp) %>% filter (kvinne_offer == 1) %>% tbl_summary()


sivile_saker %>% dplyr::select (kvinne_offer, erstat_lemp, referanse, nærstående, hvordan_passord, hvorfor_passord) %>% filter (erstat_lemp == 3) %>% filter (kvinne_offer == 0) %>% tbl_summary()



sivile_saker <- sivile_saker%>% mutate(nærstående = case_when(relasjon == 1 | relasjon == 2 | relasjon == 3 | relasjon == 4 | relasjon == 5 ~ 1,
                                                                                     TRUE ~ 0))
sivile_saker %>% dplyr::select (nærstående, utfall_riktig) %>% tbl_summary()

sivile_saker <- sivile_saker%>% mutate(ektefelle = case_when(relasjon == 3 ~ 1,
                                                              TRUE ~ 0))


sivile_saker_avtale <- sivile_saker_avtale %>% mutate(ektefelle = case_when(relasjon == 3 ~ 1,
                                                             TRUE ~ 0))


sivile_saker %>% dplyr::select (ektefelle) %>% tbl_summary()


sivile_saker %>% dplyr::select (kvinne_offer, låneopptak) %>% tbl_summary(by=låneopptak)


glm_kvinne <- glm (nærstående ~ kvinne_offer, data=sivile_saker, family=binomial(link="logit"))

summary(glm_kvinne)


sivile_saker %>% dplyr::select (kvinne_offer, nærstående) %>% tbl_cross()





sivile_saker %>% dplyr::select (nærstående, låneopptak) %>% tbl_summary(by=nærstående)

sivile_saker %>% dplyr::select (nærstående, låneopptak) %>% tbl_summary()

sivile_saker %>% dplyr::select (nærstående, låneopptak) %>% tbl_cross()


#variabel for år mellom svindel og dom
sivile_saker$år_mellom <- sivile_saker$dato_avsagt1 - sivile_saker$dato_svindel2




sivile_saker %>% dplyr::select(ektefelle, nærstående, utfall_riktig) %>% tbl_summary(type= list (nærstående ~ "categorical"), by=utfall_riktig)

sivile_saker %>% dplyr::select (relasjon) %>% tbl_summary()

sivile_saker %>% dplyr::select (deltakelse_sikkerhetsbrudd, nærstående) %>% tbl_cross()

sivile_saker %>% dplyr::select (deltakelse_sikkerhetsbrudd, ektefelle) %>% tbl_cross()

sivile_saker %>% dplyr::select (hvorfor_passord, ektefelle) %>% tbl_cross()


Modell_relasjon <- glm(utfall_riktig ~ opplysning1 + språk_utfordring + 
                         prof_svindel1 + dømt_svindler + 
                         as.factor(deltakelse_sikkerhetsbrudd) +
                         dato_avsagt1 + selvprosederende + lagmannsrett + #anført_avtale +
                         log_sum_svindel + mannlig_dommer + #ukjent_svindler +
                         nærstående*kvinne_offer + domstol_omvendt*dommerfullmektig,
                       data=sivile_saker, family=binomial(link="logit"))

vif(Modell_relasjon)

step_Modell_relasjon <- stepAIC(Modell_relasjon, direction = "both")

summary(step_Modell_relasjon)


summary(Modell_relasjon)

Modell_relasjon <- glm(utfall_riktig ~ språk_utfordring + selvprosederende +
                         dato_avsagt1 + kvinne_offer*nærstående + as.factor(deltakelse_sikkerhetsbrudd), data = sivile_saker,
                       family=binomial(link="logit"))

summary(Modell_relasjon)


Modell_relasjon <- glm(utfall_riktig ~ opplysning1 + språk_utfordring + 
                         as.factor(deltakelse_sikkerhetsbrudd) +
                         dato_avsagt1 + selvprosederende + lagmannsrett + #anført_avtale +
                         log_sum_svindel + mannlig_dommer + nærstående + prof_svindel1 +
                       kvinne_offer + domstol_omvendt*dommerfullmektig,
                       data=sivile_saker_erstatning, family=binomial(link="logit"))

summary(Modell_relasjon)






sivile_saker %>% dplyr::select (ektefelle, utfall_riktig) %>% tbl_summary(by=utfall_riktig)

sivile_saker %>% dplyr::select (ektefelle, anmeld) %>% tbl_summary()

sivile_saker <- sivile_saker %>% mutate(dømt_svindler = case_when (anmeld== 7 ~ 1,
                                                                   TRUE ~ 0))

sivile_saker <- sivile_saker %>% mutate(henlagt_sak = case_when (anmeld== 3 ~ 1,
                                                                   TRUE ~ 0))


sivile_saker_erstatning <- sivile_saker_erstatning %>% mutate(dømt_svindler = case_when (anmeld== 7 ~ 1,
                                                                   TRUE ~ 0))


sivile_saker_erstatning <- sivile_saker_erstatning %>% mutate(henlagt_sak = case_when (anmeld== 3 ~ 1,
                                                                 TRUE ~ 0))


sivile_saker %>% dplyr::select(ukjent_svindler, ektefelle, prof_svindel1, dømt_svindler, dato_svindel2, kvinne_offer,
                               språk_utfordring, log_sum_svindel, utfall_riktig, deltakelse_sikkerhetsbrudd) %>% tbl_summary(by=ektefelle)


sivile_saker %>% dplyr::select(part, dømt_svindler) %>% tbl_cross()


sivile_saker %>% dplyr::select(ektefelle, hvorfor_passord) %>% tbl_cross()

sivile_saker %>% dplyr::select(ektefelle, dato_svindel2) %>% tbl_cross()

sivile_saker %>% dplyr::select (dato_svindel2, type_lan) %>% tbl_cross()

sivile_saker %>% count(dato_svindel2, type_lan, ektefelle) %>% print(n=Inf)

library(dplyr)
library(ggplot2)

sivile_saker %>% 
  count(dato_svindel2, ektefelle) %>% filter(ektefelle== 1) %>%
  ggplot(aes(x = factor(dato_svindel2), y = n, fill = factor(ektefelle))) +
  geom_col(position = "dodge") +
  labs(
    x = "År for svindel",
    y = "Antall saker",
    fill = "Ektefelle",
    title = "Svindelsaker per år (ektefelle = 0 eller 1)"
  ) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"),
                    labels = c("0" = "Ingen ektefelle", "1" = "Ektefelle")) +
  theme_minimal()




#HER NÅ-----
#endre prof-svindel variabel


sivile_saker <- sivile_saker %>% mutate(indikasjon_prof = case_when(prof == 1 ~ 0,
                                                                    TRUE~ 1))


sivile_saker %>% dplyr::select(indikasjon_prof, ektefelle) %>% tbl_cross()


#lage variabel for type_lan: 

sivile_saker %>% dplyr::select(type_lan) %>% tbl_summary()

#lage variabel for nærstående - ektefelle

sivile_saker <- sivile_saker %>% mutate(nærstående_minus = case_when (
                                                                      relasjon == 1 |
                                                                        relasjon == 2 |
                                                                        relasjon == 4 | 
                                                                        relasjon == 5 ~ 1,
                                                                      TRUE ~ 0))


sivile_saker %>% dplyr::select(nærstående_minus, nærstående) %>% tbl_summary()

sivile_saker$nærstående_minus <- as.numeric(sivile_saker$nærstående_minus)


#lage straffeprosess variabler

sivile_saker <- sivile_saker %>% mutate (ikke_anmeldt = case_when (anmeld == 2 ~ 1,
                                                                   TRUE ~ 0))



#modeller RELASJON- sivile saker--------

#Her ser jeg at det er mer sannsynlig at svindler er anmeldt og dømt når det er en ektefelle/ samboer som har svindlet
#dette kan være med å forklare hvorfor det er lavere sannsynlighet for at banken vinner i disse sakene?
#dette må jeg sjekke ut, de virker per nå å være uavhengige

Modell_relasjon <- glm(ektefelle~  prof_svindel1 + dømt_svindler + dato_svindel2 + kvinne_offer + språk_utfordring + log_sum_svindel +
                        opplysning1 + as.factor(type_lan)  + år_mellom + varsel.2 + år_mellom + as.factor(erstat_lemp),
                       data=sivile_saker, family=binomial(link="logit"))



Modell_relasjon <- glm(nærstående  ~ prof_svindel1 + dømt_svindler + dato_svindel2 + kvinne_offer + språk_utfordring + log_sum_svindel +
                         opplysning1 + as.factor(type_lan)  + år_mellom + varsel.2 + år_mellom + as.factor(erstat_lemp),
                       data=sivile_saker, family=binomial(link="logit"))

avg_predictions(Modell_relasjon, variables = "dømt_svindler")# p(0) og p(1)


summary(Modell_relasjon)
vif(Modell_relasjon)

pR2(Modell_relasjon)

AME_Modell_relasjon_siv <- margins(Modell_relasjon)

summary(AME_Modell_relasjon_siv)

sivile_saker %>% dplyr::select (henlagt_sak, ukjent_svindler) %>% tbl_cross()
#nærstående, bare endre variabel


#obs- husk bruk ukjent istedenfor ukjent_svindler

Modell_ukjent <- glm(ukjent~  prof_svindel1 + henlagt_sak + dato_svindel2 + kvinne_offer + språk_utfordring + log_sum_svindel +
                       utfall_riktig + opplysning1 + as.factor(type_lan) + varsel.2 + år_mellom,
                     data=sivile_saker, family=binomial(link="logit"))


summary(Modell_ukjent)
pR2(Modell_ukjent)

sivile_saker %>% dplyr::select(ukjent, type_lan) %>% tbl_cross()

Modell_uklar <- glm(relasjon_uklar ~  prof_svindel1 + henlagt_sak + dato_svindel2 + kvinne_offer + språk_utfordring + log_sum_svindel +
                       utfall_riktig + opplysning1 + as.factor(type_lan) + varsel.2 + år_mellom,
                     data=sivile_saker, family=binomial(link="logit"))


summary(Modell_uklar)




#lage en variabel for gjemt unna post?

sivile_saker$type_hva
#veldig interessant dette med om svindleren er dømt, da er det større sannsynlighet for ektefelle som svindler
#sier noe om bevisene


AME_Modell_relasjon <- margins(Modell_relasjon)

summary(AME_Modell_relasjon)


sivile_saker %>% dplyr::select (utfall_riktig, anmeld) %>% tbl_cross()


sivile_saker %>% dplyr::select (ektefelle, anmeld) %>% tbl_cross()


step_relasjon <- stepAIC(Modell_relasjon, direction = "both")

summary(step_relasjon)








#lage samme modell med straffesaker?





Modell_sum <- lm(sum_svindel ~ ektefelle + kvinne_offer + opplysning1 + språk_utfordring + 
                    prof_svindel1 + dato_svindel2 + dømt_svindler + as.factor(deltakelse_sikkerhetsbrudd),
                  data=sivile_saker)

summary(Modell_sum)


Modell_sum_sivil <- lm(sum_svindel ~ eID_lån + år_mellom + ukjent_svindler, data= sivile_saker)


sivile_saker %>% dplyr::select (sum_svindel, relasjon) %>% tbl_cross()


Modell_sum_sivil <- lm(log_sum_svindel~ukjent_svindler,data= sivile_saker)


summary(Modell_sum_sivil)


sivile_saker %>% dplyr::select (relasjon) %>% tbl_summary()



vif(Modell_sum)

sivile_saker %>% dplyr::select(log_sum_svindel) %>% tbl_summary(type=list(log_sum_svindel ~ "categorical"))

sivile_saker %>% dplyr::select (ektefelle, kvinne_offer) %>% tbl_cross()

sivile_saker %>% dplyr::select (nærstående, kvinne_offer) %>% tbl_cross()

sivile_saker %>% dplyr::select(prof_svindel1, ektefelle) %>% tbl_cross()

sivile_saker %>% dplyr::select(prof_svindel1, nærstående) %>% tbl_cross()

sivile_saker %>% dplyr::select(prof_svindel1, ektefelle) %>% tbl_summary()


sivile_saker %>% dplyr::select(hvorfor_passord, ektefelle, nærstående, relasjon) %>% tbl_summary(by=hvorfor_passord) 

sivile_saker %>% dplyr::select(hvordan_oppdage, ektefelle, nærstående, relasjon) %>% tbl_summary(by=hvordan_oppdage)


sivile_saker_erstatning %>% dplyr::select(prof_svindel1, ektefelle) %>% tbl_cross()

sivile_saker %>% dplyr::select (dato_avsagt, nærstående) %>% tbl_cross()

sivile_saker %>% dplyr::select (dato_avsagt, utfall_riktig) %>% tbl_cross()

sivile_saker %>% dplyr::select (dato_avsagt, hvorfor_passord) %>% tbl_cross()

sivile_saker %>% dplyr::select (dato_avsagt, ektefelle) %>% tbl_cross()
sivile_saker %>% dplyr::select (utfall_riktig, relasjon) %>% tbl_cross()
sivile_saker %>% dplyr::select(varsel.1, varsel.2, varsel.3, varsel.4, varsel.5, utfall_riktig) %>% tbl_summary(by=utfall_riktig)

sivile_saker %>% dplyr::select (dato_avsagt2) %>% tbl_summary()

#utforske varsel mer
sivile_saker$varsel













sivile_saker %>% dplyr::select (svindelhandling, type_lan) %>% tbl_summary()
sivile_saker %>% dplyr::select (hvorfor_passord, utfall_riktig, referanse) %>% filter(hvorfor_passord == 7) %>% filter(utfall_riktig== 0) %>% tbl_summary()

sivile_saker %>% dplyr::select(dato_svindel2, dato_avsagt1, referanse) %>% print(n=Inf)
#lånesvindel, kvinnelig offer



#SIVILE SAKER-----                                                        
#fordeling av saker per år

ggplot(sivile_saker, aes(x = dato_svindel2)) +
  geom_bar() +
  labs(title = "Fordeling utfall etter når svindelen skjedde",
       x = "Dato/år", y = "Antall saker") +
  theme_minimal()

ggplot(sivile_saker, aes(x = dato_svindel2)) +
  geom_bar(fill = "green", colour = "white") +
  labs(title = "Når svindelen skjedde- sivile saker",
       x = "År", y = "Antall saker") +
  theme_classic()

ggplot(straffe_saker, aes(x = år_svindel)) +
  geom_bar(fill = "steelblue", colour = "white") +
  labs(title = "Når svindelen skjedde - straffe saker",
       x = "År", y = "Antall saker") +
  theme_classic()



#når saken kom opp i sivilretten


ggplot(sivile_saker, aes(x = dato_avsagt1)) +
  geom_histogram(binwidth =  1, fill = "#69b3a2", colour = "white") +
  labs(title = "Når saken ble avgjort i retten - Sivile saker",
       x = "År", y = "Antall saker") +
  theme_classic()


#når svindelen skjedde- sivile saker

ggplot(sivile_saker, aes(x = dato_svindel2)) +
  geom_histogram(binwidth =  1, fill = "#BFE8B2", colour = "white") +
  labs(title = "Tidspunkt for svindel - Sivile saker",
       x = "År", y = "Antall saker") +
  theme_classic()



library(ggplot2)
library(lubridate)








#ulike farger jeg kan bruke
geom_col(fill = "#69b3a2", colour = "white")   # dempet turkis
# andre fine: "#3178C6" (blå), "#E389B9" (rosa), "#9FBF3B" (grønn)


library(ggplot2)



xmin <- floor(min(sivile_saker$dato_avsagt1, na.rm = TRUE))
xmax <- ceiling(max(sivile_saker$dato_avsagt1, na.rm = TRUE))

ggplot(sivile_saker, aes(x = dato_avsagt1)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left",
                 fill = "#69b3a2", colour = "white", linewidth = 0.6) +
  scale_x_continuous(breaks = seq(xmin, xmax, by = 1),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(title = "Når saken kom opp i retten",
       x = "År", y = "Antall saker") +
  theme_classic() #+                    # ingen rutenett, tydelige akser
  #theme(plot.title = element_text(hjust = 0.5))


library(ggplot2)

xmin <- floor(min(sivile_saker$dato_avsagt1, na.rm = TRUE))
xmax_data <- ceiling(max(sivile_saker$dato_avsagt1, na.rm = TRUE))
xmax <- max(xmax_data, 2023)  # sørg for at 2023 vises

ggplot(sivile_saker, aes(x = dato_avsagt1)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left",
                 fill = "#69b3a2", colour = "white", linewidth = 0.6) +
  scale_x_continuous(
    breaks = seq(xmin, xmax, by = 1),
    limits = c(xmin, xmax),
    expand = expansion(mult = c(0.01, 0.05))  # litt luft høyre
  ) +
  labs(title = "Når saken kom opp i retten",
       x = "År", y = "Antall saker") +
  theme_classic()



sivile_saker %>% dplyr::select(dato_svindel2, dato_avsagt1) %>% tbl_cross()

ggplot(sivile_saker, aes(x = dato_svindel2, y = dato_avsagt1)) +
  geom_count() +
  scale_size_area(max_size = 5) +
  labs(x = "Svindelår", y = "Domsår", size = "Antall") +
  theme_minimal()

library(ggplot2)

xmin <- floor(min(sivile_saker$dato_svindel2, na.rm = TRUE))
xmax <- ceiling(max(sivile_saker$dato_svindel2, na.rm = TRUE))

ggplot(sivile_saker, aes(x = dato_svindel2)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left",
                 fill = "steelblue", colour = "white") +
  scale_x_continuous(breaks = seq(xmin, xmax, by = 1),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(title = "Når svindelen skjedde", x = "År", y = "Antall saker") +
  theme_minimal()

library(ggplot2)

xmin <- floor(min(sivile_saker$dato_svindel2, na.rm = TRUE))
xmax <- ceiling(max(sivile_saker$dato_svindel2, na.rm = TRUE))
ymin <- floor(min(sivile_saker$dato_avsagt1, na.rm = TRUE))
ymax <- ceiling(max(sivile_saker$dato_avsagt1, na.rm = TRUE))

ggplot(sivile_saker, aes(x = dato_svindel2, y = dato_avsagt1)) +
  geom_bin2d(binwidth = c(1, 1)) +
  scale_x_continuous(breaks = seq(xmin, xmax, 1)) +
  scale_y_continuous(breaks = seq(ymin, ymax, 1)) +
  scale_fill_viridis_c(name = "Antall") +
  coord_fixed() +
  labs(title = "Saker etter svindelår og domsår",
       x = "Svindelår", y = "Domsår") +
  theme_minimal()


#denne koden er riktig
library(dplyr); library(ggplot2)

saker2 <- sivile_saker %>%
  mutate(lag_år = dato_avsagt1 - dato_svindel2) %>%
  filter(!is.na(lag_år), lag_år >= 0)


min_lag <- floor(min(saker2$lag_år, na.rm = TRUE))
max_lag <- ceiling(max(saker2$lag_år, na.rm = TRUE))

ggplot(saker2, aes(x = lag_år)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left",
                 fill = "#3178C6", colour = "white") +
  scale_x_continuous(breaks = seq(min_lag, max_lag, by = 1),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(title = "Tid fra svindel til sivil rettssak (år)",
       x = "År", y = "Antall saker") +
   theme_classic()  # ingen rutenett, beholder akser


sivile_saker %>% dplyr::select(dato_avsagt1, dato_svindel2) %>% tbl_cross()
sivile_saker %>% dplyr::select(dato_avsagt1, dato_svindel2) %>% tbl_summary(by=dato_svindel2)


library(dplyr)
library(tidyr)

sivile_saker$dato

#prosentandeler forskjell mellom rettssak og når svindelen ble utført
saker_pct <- sivile_saker %>%
  mutate(lag_år = dato_avsagt1 - dato_svindel2) %>%   # begge som år (numeric)
  filter(!is.na(lag_år), lag_år >= 0) %>%
  transmute(lag_år = floor(lag_år)) %>%              # 0-<1, 1-<2, ...
  count(lag_år, name = "antall") %>%
  complete(lag_år = seq(min(lag_år), max(lag_år), 1), fill = list(antall = 0)) %>%
  mutate(prosent = antall / sum(antall))

saker_pct

library(gt)


library(gt)
library(scales)

saker_pct %>%
  arrange(lag_år) %>%
  mutate(prosent = percent(prosent, accuracy = 0.10)) %>%
  rename("År mellom svindel og dom" = lag_år,
         "Antall saker" = antall,
         "Andel" = prosent) %>%
  gt() %>%
  tab_header(
    title = "Sivile saker")


sivile_saker %>% count(dato_avsagt1, dato_svindel2, referanse) %>% print(n=Inf)



#lage et kakediagram av hvem som står bak svindelen i de sivile sakene

str(sivile_saker$relasjon)



library(dplyr)

#RELASJONSVARIABLER-----
sivile_saker <- sivile_saker %>%
  mutate(
    forelder = as.integer(relasjon == "1"),
    barn = as.integer(relasjon == "2"),
    ektefelle_samboer_kjæreste = as.integer(relasjon == "3"),
    annen_familie = as.integer(relasjon == "4"),
    nær_venn = as.integer(relasjon == "5"),
    noen_arbeidsplass = as.integer(relasjon == "6"),
    annen_kjent = as.integer(relasjon == "7"),
    offentlig_privattjenesteyter = as.integer(relasjon == "8"),
    ukjent = as.integer(relasjon %in% c("9","10")),
    relasjon_uklar = as.integer(relasjon == "11")
  )
sivile_saker_erstatning <- sivile_saker_erstatning %>%
  mutate(ukjent = case_when(
    relasjon %in% c("9","10") ~ 1,
    TRUE ~ 0
  ))

#med bedre labels for visualisering
sivile_saker <- sivile_saker %>%
  mutate(
    "Forelder" = as.integer(relasjon == "1"),
    "Barn" = as.integer(relasjon == "2"),
    "Ektefelle/samboer/kjæreste" = as.integer(relasjon == "3"),
    "Annen familie" = as.integer(relasjon == "4"),
    "Nær venn" = as.integer(relasjon == "5"),
    "Noen på arbeidsplass" = as.integer(relasjon == "6"),
    "Annen kjent" = as.integer(relasjon == "7"),
    "Offentlig/privat tjenesteyter" = as.integer(relasjon == "8"),
    "Ukjent svindler" = as.integer(relasjon %in% c("9","10")),
    "Uklar relasjon" = as.integer(relasjon == "11")
  )








sivile_saker_erstatning %>% dplyr::select(ukjent) %>% tbl_summary()                                                                               
                                                                                  


sivile_saker %>% dplyr::select (forelder, barn, ektefelle_samboer_kjæreste, annen_familie,
                                nær_venn, noen_arbeidsplass, annen_kjent, offentlig_privattjenesteyter, ukjent, relasjon_uklar) %>% tbl_summary()


sivile_saker %>% dplyr::select(relasjon)

#KAKEDIAGRAM SIVIL----

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

cols <- c(
  "forelder","barn","ektefelle_samboer_kjæreste","annen_familie",
  "nær_venn","noen_arbeidsplass","annen_kjent",
  "offentlig_privattjenesteyter","ukjent", "relasjon_uklar"
)

pie_df <- sivile_saker %>%
  summarise(across(all_of(cols), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "relasjon", values_to = "antall") %>%
  filter(antall > 0) %>%
  mutate(andeler = antall / sum(antall),
         etikett = percent(andeler))

ggplot(pie_df, aes(x = "", y = antall, fill = relasjon)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  labs(title = "Fordeling etter relasjon", fill = "Relasjon") +
  theme_void() +
  geom_text(aes(label = etikett), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set3")

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# 1) Definer nye visningsnavn
labels_map <- c(
  forelder = "Forelder",
  barn = "Barn",
  ektefelle_samboer_kjæreste = "Ektefelle / samboer / kjæreste (inkl.tidligere)",
  annen_familie = "Annen familie",
  nær_venn = "Nær venn",
  noen_arbeidsplass = "Noen på arbeidsplassen",
  annen_kjent = "Annen kjent person",
  offentlig_privattjenesteyter = "Offentlig- eller privat tjenesteyter",
  ukjent = "Ukjent person",
  relasjon_uklar = "Uklart hva som er relasjonen"
)

# 2) Recode til andre etiketter + lag tekst som skal stå i skivene
pie_df2 <- pie_df %>%
  mutate(
    relasjon = factor(relasjon,
                      levels = names(labels_map),
                      labels = unname(labels_map)),
    label_inn = paste0(relasjon, " (", percent(andeler), ")")
  )

# 3) Kakediagram med nye navn
ggplot(pie_df2, aes(x = "", y = antall, fill = relasjon)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Relasjon mellom offer og svindler - Sivile saker", fill = "Relasjon") +
  geom_text(aes(label = str_wrap(label_inn, 10)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer (palette = "Set3")

library(dplyr)
library(ggrepel)

#install.packages("ggrepel")

#trenger mindre tekst, endring i visualisering

pie_labs <- pie_df2 %>%
  arrange(desc(relasjon)) %>%
  mutate(ypos = cumsum(antall) - antall/2)  # midten av hvert kakestykke

ggplot(pie_labs, aes(x = 1, y = antall, fill = relasjon)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y", clip = "off") +
  labs(title = "Relasjon mellom offer og svindler - Sivile saker", fill = "Relasjon") +
  theme_void() +
  geom_text_repel(
    aes(y = ypos, label = stringr::str_wrap(label_inn, 16)),
    size = 3, direction = "y", nudge_x = 0.6,
    segment.size = 0, min.segment.length = 0,
    show.legend = FALSE
  ) +
  xlim(0.5, 1.8) +                      # lager plass til etiketter
  scale_fill_brewer(palette = "Set3")


#fordeling utfall SIVIL------

sivile_saker$utfall_riktig_figur <- factor(sivile_saker$utfall_riktig)

ggplot(sivile_saker, aes(x = utfall_riktig_figur, 
                         fill = utfall_riktig_figur)) +
  geom_bar(position= "stack") +
  scale_fill_manual(values = c("0" = "#1F5591", 
                               "1" = "#D73529")) +
  scale_x_discrete(labels = c("0" = "Offer vant", "1" = "Finansinstitusjon vant")) +
  
  labs(title = "Fordeling av utfall - Sivile saker",
       x = "Utfall",
       y = "Antall saker",
       fill = "Utfall") +
  theme_classic()


sivile_saker %>% dplyr::select (utfall_riktig) %>% tbl_summary()

ggplot(sivile_saker, aes(x = utfall_riktig_figur, fill = utfall_riktig_figur)) + 
  geom_bar() +
  scale_x_discrete(labels = c("0" = "Nei", "1" = "Ja")) +
  scale_fill_discrete(labels = c("0" = "Nei", "1" = "Ja")) +
  labs(title = "Fordeling utfall",
       x = "utfall",
       y = "Antall saker",
       fill = "Utfall") +
  theme_minimal()



#lånesvindel


sivile_saker <- sivile_saker %>% mutate(identitetsmisbruk_kategori = case_when(svindelhandling == 1 ~ "Involverer ikke penger",
                                                                               svindelhandling == 2 ~ "Inngåelse av/forsøk på låne- eller kredittavtaler",
                                                                               svindelhandling == 3 ~ "Misbruk av eksisterende betalingstjeneste (betalingskort eller betalingskonto)",
                                                                               svindelhandling == 4 ~ "Både inngåelse av/forsøk på lån/kreditt OG misbruk av betalingstjeneste",
                                                                               svindelhandling == 5 ~ "Annet (eks. transaksjon fra verdipapirkonto)"))


sivile_saker %>% dplyr::select(identitetsmisbruk_kategori) %>% tbl_summary()

sivile_saker$identitetsmisbruk_kategori <- as.factor(sivile_saker$identitetsmisbruk_kategori)


ggplot(sivile_saker, aes(x = identitetsmisbruk_kategori,  
                         fill = identitetsmisbruk_kategori)) +
  geom_bar(position= "stack") + labs(title = "Type identitetsmisbruk")
  
  

library(ggplot2)
library(scales)



ggplot(sivile_saker,
       aes(x = identitetsmisbruk_kategori,
           y = after_stat(prop), group = 1,
           fill = identitetsmisbruk_kategori)) +
  geom_bar() +
  geom_text(stat = "count",
            aes(label = percent(after_stat(prop), accuracy = 1),
                y = after_stat(prop)),
            vjust = -0.4) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.06))) +
  labs(title = "Type identitetsmisbruk", x = NULL, y = "Andel") +
  theme_minimal() + coord_flip()


library(dplyr)
library(forcats)
library(gtsummary)
library(gt)

# (valgfritt) gjør manglende eksplisitt og sorter etter hyppighet
tab <- sivile_saker %>%
  mutate(identitetsmisbruk_kategori =
           identitetsmisbruk_kategori %>%
           fct_explicit_na("(mangler)") %>%
           fct_infreq() %>% fct_rev()) %>%
  dplyr::select(identitetsmisbruk_kategori) %>%
  tbl_summary(
    label     = list(identitetsmisbruk_kategori ~ "Type identitetsmisbruk"),
    type      = all_categorical() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",  # f.eks. 23 (35%)
    missing   = "ifany",
    sort      = all_categorical() ~ "frequency"
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Kategori**", stat_0 ~ "**Antall (andel)**") %>%
  as_gt() %>%
  gt::tab_header(title = md("**Type identitetsmisbruk - Sivile saker**")) %>%
 gt::opt_row_striping()

tab




#straffesaker tabell over misbrukstype

straffe_saker <- straffe_saker %>% mutate(identitetsmisbruk_kategori = case_when(svindelhandling == 1 ~ "Involverer ikke penger",
                                                                               svindelhandling == 2 ~ "Inngåelse av/forsøk på låne- eller kredittavtaler",
                                                                               svindelhandling == 3 ~ "Misbruk av eksisterende betalingstjeneste (betalingskort eller betalingskonto)",
                                                                               svindelhandling == 4 ~ "Både inngåelse av/forsøk på lån/kreditt OG misbruk av betalingstjeneste",
                                                                               svindelhandling == 5 ~ "Annet (eks. transaksjon fra verdipapirkonto)"))


straffe_saker %>% dplyr::select (identitetsmisbruk_kategori) %>% tbl_summary()
tab <- straffe_saker %>%
  mutate(identitetsmisbruk_kategori =
           identitetsmisbruk_kategori %>%
           fct_explicit_na("(mangler)") %>%
           fct_infreq() %>% fct_rev()) %>%
  dplyr::select(identitetsmisbruk_kategori) %>%
  tbl_summary(
    label     = list(identitetsmisbruk_kategori ~ "Type identitetsmisbruk"),
    type      = all_categorical() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",  # f.eks. 23 (35%)
    missing   = "ifany",
    sort      = all_categorical() ~ "frequency"
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Kategori**", stat_0 ~ "**Antall (andel)**") %>%
  as_gt() %>%
  gt::tab_header(title = md("**Type identitetsmisbruk - Straffesaker**")) %>%
  gt::opt_row_striping()

tab


#STRAFFESAKER------
  
straffe_saker <- read_excel("data/straffesaker.xlsx")

view(straffe_saker$relasjon.3)

view


straffe_saker %>% dplyr:: select (relasjon.1, relasjon.2, relasjon.3, relasjon.4, relasjon.5, relasjon.6, relasjon.7, relasjon.8, relasjon.9, relasjon.10, relasjon.11, var24, num_K, siv_beh, gender_s, var21, alder_s, gender_dommer, sum_svindel_gen, var28, annen_svindel, svindelhandling, type_lan.1, type_lan.2, type_lan.3, type_lan.4, type_lan.5,type_lan.6, hvordan_passord, hvorfor_passord, sivile_krav,
                                  var48.1, var48.2, var48.3, var49, utfall_strl, anket, type_lan.1,siv_beh, var18) %>% tbl_summary()



straffe_saker %>% dplyr:: select (relasjon.1, relasjon.2, relasjon.3, relasjon.4, relasjon.5, relasjon.6, relasjon.7, relasjon.8, relasjon.9, relasjon.10, relasjon.11) %>% print (n=Inf)



#finne ut om overlapp


library(dplyr)
library(tidyr)
library(stringr)

rel_cols <- paste0("relasjon.", 1:11)

overlapp_per_sak <- straffe_saker %>% 
  dplyr::select(referanse, all_of(rel_cols)) %>%
  pivot_longer(cols = all_of(rel_cols), names_to = "relasjon_kol", values_to = "relasjon") %>%
  mutate(relasjon = str_squish(as.character(relasjon))) %>%
  filter(!is.na(relasjon), relasjon != "") %>%
  distinct(referanse, relasjon) %>%          # unike relasjonstyper per sak
  count(referanse, name = "antall_relasjonstyper")

# Fordeling (hvor mange saker med 1, 2, 3 ... relasjonstyper)
overlapp_per_sak %>%
  count(antall_relasjonstyper) %>%
  arrange(antall_relasjonstyper)



# Hvor mange saker har overlapp (>= 2 relasjonstyper)?
overlapp_per_sak %>%
  summarise(
    antall_saker = n(),
    saker_med_overlapp = sum(antall_relasjonstyper >= 2),
    andel_med_overlapp = mean(antall_relasjonstyper >= 2)
  )

library(dplyr)
library(stringr)


#hvor mange overlapp i nærstående + ikke nærstående? ----

near_cols <- paste0("relasjon.", 1:5)    # nærstående
non_cols  <- paste0("relasjon.", 6:11)   # ikke-nærstående

overlapp_saker <- straffe_saker %>%
  mutate(
    har_naerstaaende = if_any(all_of(near_cols), ~ !is.na(.) & str_squish(as.character(.)) != ""),
    har_ikke         = if_any(all_of(non_cols),  ~ !is.na(.) & str_squish(as.character(.)) != ""),
    overlapp         = har_naerstaaende & har_ikke
  )



straffe_saker %>% dplyr::select (nærstående, ukjen)

# 1) Hvor mange saker i hver kategori?
overlapp_saker %>%
  summarise(
    antall_saker = n(),
    kun_naerstaaende = sum(har_naerstaaende & !har_ikke),
    kun_ikke         = sum(!har_naerstaaende & har_ikke),
    overlapp         = sum(overlapp),
    ingen_registrert = sum(!har_naerstaaende & !har_ikke),
    andel_overlapp   = mean(overlapp)
  )






head(straffe_saker)

straffe_saker %>% dplyr::select (relasjon.3, referanse) %>% filter (relasjon.3 == 1) %>% tbl_summary()

straffe_saker$ektefelle <- straffe_saker$relasjon.3


str(straffe_saker$ektefelle)

straffe_saker$ektefelle <- as.numeric(straffe_saker$ektefelle)
  


straffe_saker %>% dplyr::select (dato_avsagt, dom_år, år_svindel, referanse, år_mellom_straff) %>% print (n=Inf)


straffe_saker %>% dplyr::select (dato_avsagt, dom_år, dato_svindel, år_mellom_straff) %>% tbl_summary()

summary(straffe_saker$år_mellom_straff)

summary(sivile_saker$år_mellom)

summary(straffe_saker$dom_år)

summary(sivile_saker$dato_avsagt1)






straffe_saker <- straffe_saker %>% mutate(år_svindel = case_when(dato_svindel == "1" ~ 2009,
                                                                 dato_svindel == "2" ~ 2010,
                                                                 dato_svindel == "15" ~ 2011,
                                                                 dato_svindel == "3" ~ 2012,
                                                                 dato_svindel == "4" ~ 2013,
                                                                 dato_svindel == "5" ~ 2014, 
                                                                 dato_svindel == "6" ~ 2015,
                                                                 dato_svindel == "7" ~ 2016,
                                                                 dato_svindel == "8" ~ 2017,
                                                                 dato_svindel == "9" ~ 2018,
                                                                 dato_svindel == "10" ~ 2019,
                                                                 dato_svindel == "11" ~ 2020,
                                                                 dato_svindel == "12" ~ 2021,
                                                                 dato_svindel == "13" ~ 2022,
                                                                 dato_svindel == "14" ~ 2023))



library(dplyr)
library(lubridate)

#19-105096ENE-OTIR/01 
#feilskrevet år for dom



straffe_saker %>% dplyr::select (dato_avsagt, år_svindel) %>% tbl_cross()



#dato avsagt til år
straffe_saker <- straffe_saker %>%
  mutate(
    dom_dato = ymd(dato_avsagt),   # konverter fra tekst til Date
    dom_år  = year(dato_avsagt)   # nytt heltall: f.eks. 2016
  )


library(dplyr)

straffe_saker <- straffe_saker %>%
  mutate(dom_år = if_else(referanse == "19-105096ENE-OTIR/01", 2019, dom_år))



#var28 mutert til 1= over den veiledende grense 0= under, eller ikke økonomisk utbytte


straffe_saker <- straffe_saker %>% mutate (over_grovt = case_when(var28 == 2 | var28 == 3 ~ 0,
                                                                  TRUE ~ 1))


straffe_saker %>% dplyr::select (over_grovt) %>% tbl_summary()


#en dato for svindel som må endres manuelt:


#MODELL RELASJON STRAFF-----

#19-105096ENE-OTIR/01 


str(straffe_saker$sum_svindel_gen)

straffe_saker %>% dplyr::select (dom_år, år_svindel, år_mellom_straff, type_lan.4, ektefelle) %>% tbl_summary(type= list(år_mellom_straff ~ "categorical"))


straffe_saker$år_mellom_straff <- straffe_saker$dom_år - straffe_saker$år_svindel






straffe_saker %>% dplyr::select(år_mellom_straff) %>% tbl_summary(type=list(år_mellom_straff ~ "categorical"))

Modell_relasjon_straff <- glm(nærstående ~ år_svindel + type_lan.3 + as.factor(gender_s) + annet_tiltale + var28 + år_mellom_straff + annet_tiltale,
                              data=straffe_saker, family=binomial(link="logit"))


summary(Modell_relasjon_straff)
pR2(Modell_relasjon_straff)
vif(Modell_relasjon_straff)



AME_relasjon_straff <- margins(Modell_relasjon_straff)

summary(AME_relasjon_straff)

#Lånesvindel- viktig modell---------straffesaker
straffe_saker <- straffe_saker %>% mutate(låne_svindel = case_when(svindelhandling == 2 |
                                                                     svindelhandling == 4 ~ 1,
                                                                   TRUE ~ 0))


straffe_saker %>% dplyr::select (type)


straffe_saker %>% dplyr::select (svindelhandling) %>% tbl_summary()


straffe_saker %>% dplyr::select (låne_svindel, ektefelle, ukjent_svindler, nærstående ) %>% tbl_summary(by=låne_svindel)

Modell_lånesvindel <- glm(låne_svindel ~ ektefelle + etter_2020 + tiltalt_ + prof + år_svindel + annet_tiltale + sum_svindel_num + år_mellom_straff + annet_tiltale, #var28
                              data=straffe_saker, family=binomial(link="logit"))


straffe_saker$eID_lån_straff <- as.factor(straffe_saker$type_lan.3)


#LÅNESVINDEL, STRAFF, I OPPGAVE-----


Modell_lånesvindel <- glm(låne_svindel ~ as.factor(nærstående) + as.factor(etter_2020) + år_svindel + as.numeric(sum_svindel_num)+ år_mellom_straff, #var28
                          data=straffe_saker, family=binomial(link="logit"))


Modell_lånesvindel <- glm(eID_lån_straff ~ as.factor(nærstående) + as.factor(etter_2020) + år_svindel + as.numeric(sum_svindel_num)+ år_mellom_straff, #var28
                          data=straffe_saker, family=binomial(link="logit"))


str(straffe_saker$sum_svindel_num)



summary(Modell_lånesvindel)
pR2(Modell_lånesvindel)
vif(Modell_lånesvindel)


AME_Modell_lånesvindel <- margins(Modell_lånesvindel)
summary(AME_Modell_lånesvindel)


library(margins)

m <- margins(Modell_lånesvindel, variables = "sum_svindel_num")
s <- summary(m)

options(digits = 12)
print(s)

format(s$AME, scientific = TRUE)


summary(straffe_saker$sum_svindel_num)


straffe_saker$sum_svindel_100k <- straffe_saker$sum_svindel_num / 100000

mod2 <- update(Modell_Lånesvindel, . ~ . - sum_svindel_num + sum_svindel_100k, data = df)

summary(margins(mod2, variables = "sum_svindel_100k"))
#Modell lånesvindel straff----RIKTIG---

Modell_lånesvindel <- glm(låne_svindel ~ as.factor(nærstående) + as.factor(etter_2020) + år_svindel + as.numeric(sum_svindel_100k)+ år_mellom_straff, #var28
                          data=straffe_saker, family=binomial(link="logit"))


Modell_lånesvindel <- glm(eID_lån_straff ~ as.factor(nærstående) + as.factor(etter_2020) + år_svindel + as.numeric(sum_svindel_100k)+ år_mellom_straff, #var28
                          data=straffe_saker, family=binomial(link="logit"))

summary(Modell_lånesvindel)
summary(Modell_lånesvindel)
pR2(Modell_lånesvindel)
vif(Modell_lånesvindel)





AME_Modell_lånesvindel <- margins(Modell_lånesvindel)
summary(AME_Modell_lånesvindel)




straffe_saker %>% dplyr::select (ektefelle, gender_s) %>% tbl_cross()

straffe_saker %>% dplyr::select (ektefelle, tiltalt_) %>% tbl_cross()



#annet misbruk--straffesaker-----

Modell_annet_misbruk<- glm(annet_misbruk ~ ektefelle + etter_2020 + tiltalt_ + prof + år_svindel + annet_tiltale + sum_svindel_num + år_mellom_straff + annet_tiltale, #var28
                          data=straffe_saker, family=binomial(link="logit"))


summary(Modell_annet_misbruk)
pR2(Modell_lånesvindel)
vif(Modell_lånesvindel)



#bruk av eID i svindel, viktig modell----


straffe_saker$eID_lån <- as.numeric(straffe_saker$type_lan.3)

straffe_saker %>% dplyr::select(eID_lån) %>% tbl_summary()

eID_brukt <- glm(eID_lån ~ ektefelle + tiltalt_ + år_svindel + as.factor(etter_2020) + år_mellom_straff + prof + sum_svindel_num, data=straffe_saker, family=binomial(link="logit"))


#ingen straffesaker med eID lån etter 2020-----derfor går ikke modellen-----
straffe_saker %>% dplyr::select(eID_lån, etter_2020) %>% tbl_cross()
summary(eID_brukt)

pR2(eID_brukt)
vif(eID_brukt)

straffe_saker <- straffe_saker%>% 
  mutate(etter_easybank = case_when(ymd(dato_avsagt > "2020-10-22" ~ 1, 
                                  .default = 0))
         
         
str(straffe_saker$dato_svindel)
         

straffe_saker %>% dplyr::select(låne_svindel, eID_lån, etter_2020) %>% tbl_summary(by=etter_2020)
sivile_saker %>% dplyr::select(låneopptak, eID_lån, etter_reform) %>% tbl_summary(by=etter_reform)

sivile_saker %>% dplyr::select(låneopptak, etter_reform) %>% tbl_summary(by=etter_reform)


straffe_saker <- straffe_saker %>% mutate (etter_2020 = case_when(dato_svindel == "12" |
                                                                    dato_svindel == "13" |
                                                                    dato_svindel == "14" ~ 1,
                                                                  TRUE ~ 0))

straffe_saker %>% dplyr::select (etter_2020, ektefelle, nærstående, ukjent_svindler) %>% tbl_summary(by=etter_2020)





eID_brukt_siv <- glm(eID_lån ~ kvinne_offer + språk_utfordring + as.factor(deltakelse_sikkerhetsbrudd) + ektefelle + år_mellom  + etter_reform + dato_svindel2 + log_sum_svindel, data=sivile_saker, family=binomial(link="logit"))

summary(eID_brukt_siv)
pR2(eID_brukt_siv)
vif(eID_brukt_siv)

#svindelsum, og relasjon- straffesaker, viktig modell-------
straffe_saker %>% dplyr::select(sum_svindel) %>% tbl_summary()

str(straffe_saker$sum_svindel)

straffe_saker$sum_svindel_num <- as.numeric(straffe_saker$sum_svindel)


library(dplyr)

straffe_saker <- straffe_saker %>%
  mutate(log_sum = log(sum_svindel_num))

str(straffe_saker$log_sum)

str(straffe_saker$sum)

straffe_saker %>% dplyr::select(log_sum) %>% tbl_summary(type=list(log_sum~ "categorical"))


Modell_sum_straff <- lm(sum_svindel_num ~ nærstående + prof + låne_svindel, data= straffe_saker) #tiltalt_


Modell_sum_straff <- lm(sum_svindel_num ~ ukjent_svindler , data= straffe_saker)


summary(Modell_sum_straff)

pR2(Modell_sum_straff)



Modell_sum_straff <- lm(log_sum ~ nærstående, data= straffe_saker)


Modell_sum_siv <- lm(sum_svindel1 ~ ektefelle, data= sivile_saker)

summary(Modell_sum_siv)




#større sannsynlighet for kvinnelig tiltalt for svindel av ektefeller i strafferett/ nærstående

straffe_saker <- straffe_saker %>% mutate ("Ukjent svindler" = case_when(relasjon.9 == "1" |
                                                                         relasjon.10 == "1"  ~ 1,
                                                                       TRUE ~ 0))



straffe_saker <- straffe_saker %>% mutate ("Uklar relasjon" = case_when(relasjon.11== "1"  ~ 1,
                                                                       TRUE ~ 0))


straffe_saker <- straffe_saker %>% mutate (nærstående = case_when (relasjon.1 == "1" |
                                                                     relasjon.2 == "1" |
                                                                     relasjon.3 == "1"|
                                                                     relasjon.4 == "1" |
                                                                     relasjon.5 == "1" ~ 1,
                                                                   TRUE ~ 0))


straffe_saker <- straffe_saker %>% mutate (ikke_nærstående = case_when (relasjon.6 == "1" |
                                                                     relasjon.7 == "1" |
                                                                     relasjon.8 == "1"|
                                                                     relasjon.9 == "1" |
                                                                     relasjon.10 == "1" |
                                                                       relasjon.11 == "1" ~ 1,
                                                                   TRUE ~ 0))


straffe_saker %>% dplyr::select (nærstående, ikke_nærstående) %>% tbl_cross()

straffe_saker %>% dplyr::select (nærstående) %>% tbl_summary()


straffe_saker





straffe_saker %>% dplyr::select (ukjent_svindler, sum_svindel) %>% tbl_summary()


straffe_saker <- straffe_saker %>% mutate (tiltalt_ = case_when (gender_s == "1" ~ "mann",
                                                                     gender_s == "2" ~ "kvinne",
                                                                 gender_s == "3" ~ "flere_"))

straffe_saker$tiltalt_ <- relevel(factor(straffe_saker$tiltalt_), ref = "kvinne")


straffe_saker %>% dplyr::select (tiltalt_, ektefelle) %>% tbl_cross()

straffe_saker %>% dplyr::select (tiltalt_, nærstående) %>% tbl_cross()


straffe_saker %>% dplyr::select (tiltalt_, ukjent_svindler) %>% tbl_cross()





Modell_ukjent_straff <- glm(ukjent_svindler ~ år_svindel + type_lan.1 + tiltalt_ + as.factor(org_krim) + as.factor(var28) + var48.2,
                              data=straffe_saker, family=binomial(link="logit"))




summary(Modell_ukjent_straff)
pR2(Modell_ukjent_straff)


straffe_saker %>% dplyr::select(ukjent_svindler, uklar_relasjon) %>% tbl_summary()

straffe_saker %>% dplyr::select(var21) %>% tbl_summary()


straffe_saker %>% dplyr::select (referanse, nærstående) %>% filter(nærstående==1) %>% tbl_summary()

straffe_saker %>% dplyr::select (annet_tiltale, gender_s) %>% tbl_cross()


straffe_saker %>% dplyr::select (svindelhandling, nærstående) %>% tbl_cross()

straffe_saker %>% dplyr::select (svindelhandling, ektefelle) %>% tbl_cross()

straffe_saker %>% dplyr::select (svindelhandling, ukjent_svindler) %>% tbl_cross()

#større sannsynlighet for ikke inngåelse av lån


  
#uklar relasjon
  
Modell_uklar <- glm(uklar_relasjon ~ år_svindel + type_lan.1 + tiltalt_ + as.factor(org_krim) + as.factor(var28),
                    data=straffe_saker, family=binomial(link="logit"))


summary(Modell_uklar)
#figurer

  
ggplot(straffe_saker, aes(x = dom_år)) +
  geom_histogram(binwidth =  1, fill = "steelblue", colour = "white") +
  labs(title = "Når saken ble avgjort i retten - Straffesaker",
       x = "År", y = "Antall saker") +
  theme_classic()


ggplot(straffe_saker, aes(x = år_svindel)) +
  geom_histogram(binwidth =  1, fill = "#81B1D6", colour = "white") +
  labs(title = "Tidspunkt for svindel - Straffesaker",
       x = "År", y = "Antall saker") +
  theme_classic()




ggplot(straffe_saker, aes(x = factor(`dom_år`), fill = sivile_krav)) +
  geom_bar(position = "dodge", colour = "white") +
  labs(title = "Når saken kom opp i strafferetten",
       x = "År", y = "Antall saker", fill = "Sivile krav") +
  theme_classic()


ggplot(straffe_saker, aes(x = factor(`dom_år`), fill = var48.2)) +
  geom_bar(position = "dodge", colour = "white") +
  labs(title = "Når saken kom opp i strafferetten",
       x = "År", y = "Antall saker", fill = "Sivile krav av finans") +
  theme_classic()

ggplot(straffe_saker, aes(x = factor(`dom_år`), fill = var48.3)) +
  geom_bar(position = "dodge", colour = "white") +
  labs(title = "Når saken kom opp i strafferetten",
       x = "År", y = "Antall saker", fill = "Sivile krav av offer") +
  theme_classic()

ggplot(straffe_saker, aes(x = factor(`dom_år`), fill = var48.1)) +
  geom_bar(position = "dodge", colour = "white") +
  labs(title = "Når saken kom opp i strafferetten",
       x = "År", y = "Antall saker", fill = "ingen sivile krav") +
  theme_classic()




ggplot(straffe_saker, aes(x = år_svindel)) +
  geom_histogram(binwidth =  1, fill = "#69b3a2", colour = "white") +
  labs(title = "Når saken kom opp i retten",
       x = "År", y = "Antall saker") +
  theme_classic()

#prosentandeler forskjell mellom rettssak og når svindelen ble utført
saker_straff<- straffe_saker %>%
  mutate(lag_år = dom_år - år_svindel) %>%   # begge som år (numeric)
  filter(!is.na(lag_år), lag_år >= 0) %>%
  transmute(lag_år = floor(lag_år)) %>%              # 0-<1, 1-<2, ...
  count(lag_år, name = "antall") %>%
  complete(lag_år = seq(min(lag_år), max(lag_år), 1), fill = list(antall = 0)) %>%
  mutate(prosent = antall / sum(antall))



library(gt)
library(scales)

#år mellom svindel og straff tabell----

saker_straff %>%
  arrange(lag_år) %>%
  mutate(prosent = percent(prosent, accuracy = 0.10)) %>%
  rename("År mellom svindel og dom" = lag_år,
         "Antall saker" = antall,
         "Andel" = prosent) %>%
  gt() %>%
  tab_header(
    title = "Straffesaker")



straffe_saker %>% dplyr::select (år_mellom_straff) %>% tbl_summary(type=list(år_mellom_straff ~ "categorical"))

sivile_saker %>% dplyr::select (år_mellom) %>% tbl_summary(type=list(år_mellom ~ "categorical"))




#relasjonsvariabler STRAFF-----

straffe_saker$forelder<- straffe_saker$relasjon.1
straffe_saker$barn<- straffe_saker$relasjon.2
straffe_saker$ektefelle_samboer_kjæreste<- straffe_saker$relasjon.3
straffe_saker$annen_familie<- straffe_saker$relasjon.4
straffe_saker$nær_venn<- straffe_saker$relasjon.5
straffe_saker$noen_arbeidsplass<- straffe_saker$relasjon.6
straffe_saker$annen_kjent<- straffe_saker$relasjon.7
straffe_saker$offentlig_privattjenesteyter<- straffe_saker$relasjon.8


#for bedre etiketter i visualisering- straff----

#kan ikke ha kakediagram pga. overlapp

straffe_saker$"Forelder"<- straffe_saker$relasjon.1
straffe_saker$"Barn"<- straffe_saker$relasjon.2
straffe_saker$"Ektefelle/samboer/kjæreste"<- straffe_saker$relasjon.3
straffe_saker$"Annen familie"<- straffe_saker$relasjon.4
straffe_saker$"Nær venn"<- straffe_saker$relasjon.5
straffe_saker$"Noen på arbeidsplass"<- straffe_saker$relasjon.6
straffe_saker$"Annen kjent"<- straffe_saker$relasjon.7
straffe_saker$"Offentlig/privat tjenesteyter"<- straffe_saker$relasjon.8


straffe_saker %>% dplyr::select (forelder, barn, ektefelle_samboer_kjæreste, annen_familie, nær_venn, noen_arbeidsplass, annen_kjent, offentlig_privat_tjenesteyter, ukjent_svindler, uklar_relasjon) %>% tbl_summary()


straffe_saker %>% count (forelder, barn, ektefelle_samboer_kjæreste, annen_familie, nær_venn, noen_arbeidsplass, annen_kjent, offentlig_privat_tjenesteyter, ukjent_svindler, uklar_relasjon) %>% print(n=Inf)


sivile_saker %>% count (forelder, barn, ektefelle_samboer_kjæreste, annen_familie, nær_venn, noen_arbeidsplass, annen_kjent, offentlig_privattjenesteyter, ukjent, relasjon_uklar) %>% print(n=Inf)
#kakediagram med straffesaker


#riktig tabell for relasjon i straffesaker-----

cols_straffesaker <- c(
  "Forelder","Barn","Ektefelle/samboer/kjæreste","Annen familie",
  "Nær venn","Noen på arbeidsplass","Annen kjent",
  "Offentlig/privat tjenesteyter","Ukjent svindler", "Uklar relasjon"
)

library(dplyr)
library(tidyr)


straffe_saker <- straffe_saker %>%
  mutate(across(all_of(cols_straffesaker),
                ~ as.integer(. %in% c(1, "1", TRUE, "TRUE"))))#, #"Ja", "ja"))))

long <- straffe_saker %>%
  pivot_longer(all_of(cols_straffesaker),
               names_to = "relasjon", values_to = "flag") %>%
  filter(flag == 1)


andeler <- long %>%
  count(relasjon) %>%
  mutate(p = n / nrow(straffe_saker))

summary(andeler)




view(andeler)


library(dplyr)
library(flextable)
library(officer)
library(scales)

andeler_straff <- andeler %>%
  arrange(desc(p)) %>%
  transmute(Relasjon = relasjon,
            Antall = n,
            "Andel saker" = percent(p, accuracy = 0.1)) %>%
  flextable() %>%
  set_caption(caption = "Relasjon mellom offer og svindler – Straffesaker") %>% 
  autofit() %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  align(j = c("Antall", "Andel saker"), align = "right") %>%
  add_footer_lines("Merk: Summer kan overstige 100 % fordi en sak kan ha flere relasjoner.")


andeler_straff








#fordeling relasjon sivile saker---tabell-----

#riktig tabell for relasjon i straffesaker-----

cols_sivile_saker <- c(
  "Forelder","Barn","Ektefelle/samboer/kjæreste","Annen familie",
  "Nær venn","Noen på arbeidsplass","Annen kjent",
  "Offentlig/privat tjenesteyter","Ukjent svindler", "Uklar relasjon"
)

library(dplyr)
library(tidyr)


sivile_saker <- sivile_saker %>%
  mutate(across(all_of(cols_sivile_saker,
                ~ as.integer() #. %in% c(1, "1", TRUE, "TRUE")))))


#, #"Ja", "ja"))))

long <- sivile_saker %>%
  pivot_longer(all_of(cols_sivile_saker),
               names_to = "Relasjon", values_to = "flag") %>%filter(flag == 1)


andeler <- long %>%
  count(Relasjon) %>%
  mutate(p = n / nrow(sivile_saker))

summary(andeler)

andeler


view(andeler)


library(dplyr)
library(flextable)
library(officer)
library(scales)

andeler_sivile <- andeler %>%
  arrange(desc(p)) %>%
  transmute(Relasjon = Relasjon,
            Antall = n,
            "Andel saker" = percent(p, accuracy = 0.1)) %>%
  flextable() %>%set_caption(caption = "Relasjon mellom offer og svindler – Sivile saker") %>% 
  autofit() %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  align(j = c("Antall", "Andel saker"), align = "right")  
  
  
  andeler_sivile








library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(scales)
library(forcats)

cols_straffesaker <- c(
  "forelder","barn","ektefelle_samboer_kjæreste","annen_familie",
  "nær_venn","noen_arbeidsplass","annen_kjent",
  "offentlig_privat_tjenesteyter","ukjent_svindler","uklar_relasjon"
)

labels_map_straff <- c(
  forelder = "Forelder",
  barn = "Barn",
  ektefelle_samboer_kjæreste = "Ektefelle / samboer / kjæreste (inkl. tidligere)",
  annen_familie = "Annen familie",
  nær_venn = "Nær venn",
  noen_arbeidsplass = "Noen på arbeidsplassen",
  annen_kjent = "Annen kjent person",
  offentlig_privat_tjenesteyter = "Offentlig- eller privat tjenesteyter",
  ukjent_svindler = "Ukjent person",
  uklar_relasjon = "Uklart hva som er relasjonen"
)

# 1) Rydd 0/1
straffe_saker <- straffe_saker %>%
  mutate(across(all_of(cols_straffesaker),
                ~ as.integer(. %in% c(1, "1", TRUE, "TRUE"))))

# 2) Long-format
long <- straffe_saker %>%
  pivot_longer(all_of(cols_straffesaker),
               names_to = "relasjon", values_to = "flag") %>%
  filter(flag == 1)

# 3) Andeler (andel saker per relasjon)
tot_saker <- nrow(straffe_saker)  # evt. n_distinct(sak_id) hvis du har unik ID
andeler <- long %>%
  count(relasjon, name = "n") %>%
  mutate(p = n / tot_saker)


andeler

# 4) Recode til pene etiketter
andeler <- andeler %>%
  mutate(
    relasjon_label = recode(relasjon, !!!labels_map_straff),
    # sorter etter andel, og bruk faktornivå for konsistent rekkefølge
    relasjon_label = fct_reorder(relasjon_label, p, .desc = TRUE)
  )

# 5) Publiserbar flextable
andeler_fx <- andeler %>%
  arrange(desc(p)) %>%
  transmute(
    Relasjon = relasjon_label,
    Antall = n,
    `Andel saker` = percent(p, accuracy = 0.1)
  ) %>%
  flextable() %>%
  autofit() %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  align(j = c("Antall", "Andel saker"), align = "right") %>%
  add_footer_lines("Merk: Summer kan overstige 100 % fordi én sak kan ha flere relasjoner.")

andeler_fx



#annet forsøk 

# andeler per relasjon = unike saker med flagget / totalt antall saker
tot_saker <- n_distinct(straffe_saker$referanse)

andeler_id <- long %>%
  summarise(n_saker = n_distinct(referanse), .by = relasjon) %>%
  mutate(p = n_saker / tot_saker,
         relasjon = fct_reorder(relasjon, p, .desc = TRUE))



ggplot(andeler_id, aes(relasjon, p)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Andel saker",
       title = "Relasjon mellom offer og svindler - Straffesaker") +
  theme_minimal(base_size = 12)


ggplot(andeler_id, aes(relasjon, p, fill = relasjon)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = NULL, y = "Andel saker",
       title = "Relasjon mellom offer og svindler - Straffesaker") +
  theme_minimal(base_size = 12)

#med bedre navn--andeler for relasjon i straffesaker-----

ggplot(andeler_id, aes(relasjon, p, fill = relasjon)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) labels_map_straff[x]) +  # <— her
  scale_fill_brewer (palette = "Set3") +
  labs(x = NULL, y = "Andel",
       title = "Relasjon mellom offer og svindler - Straffesaker") +
  theme_minimal(base_size = 12)









#MÅ GJØRE NOE ANNET ENN KAKEDIAGRAM!!---


diagram_straff <- straffe_saker %>%
  summarise(across(all_of(cols_straffesaker), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "relasjon", values_to = "antall") %>%
  filter(antall > 0) %>%
  mutate(andeler = antall / sum(antall),
         etikett = percent(andeler))

ggplot(diagram_straff, aes(x = "", y = antall, fill = relasjon)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  labs(title = "Fordeling etter relasjon - Straffesaker", fill = "Relasjon") +
  theme_void() +
  geom_text(aes(label = etikett), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set3")

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# 1) Definer nye visningsnavn
labels_map_straff <- c(
  forelder = "Forelder",
  barn = "Barn",
  ektefelle_samboer_kjæreste = "Ektefelle / samboer / kjæreste (inkl.tidligere)",
  annen_familie = "Annen familie",
  nær_venn = "Nær venn",
  noen_arbeidsplass = "Noen på arbeidsplassen",
  annen_kjent = "Annen kjent person",
  offentlig_privat_tjenesteyter = "Offentlig- eller privat tjenesteyter",
  ukjent_svindler = "Ukjent person",
  uklar_relasjon= "Uklart hva som er relasjonen"
)

# 2) Recode til andre etiketter + lag tekst som skal stå i skivene
kake_2 <- diagram_straff %>%
  mutate(
    relasjon = factor(relasjon,
                      levels = names(labels_map_straff),
                      labels = unname(labels_map_straff)),
    label_inn = paste0(relasjon, " (", percent(andeler), ")")
  )

# 3) Kakediagram med nye navn
ggplot(kake_2, aes(x = "", y = antall, fill = relasjon)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Fordeling etter relasjon til svindler - Straffesaker", fill = "Relasjon") +
  geom_text(aes(label = str_wrap(label_inn, 10)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set3")

library(dplyr)
library(ggrepel)

#install.packages("ggrepel")

#trenger mindre tekst, endring i visualisering

pie_labs <- kake_2 %>%
  arrange(desc(relasjon)) %>%
  mutate(ypos = cumsum(antall) - antall/2)  # midten av hvert kakestykke

ggplot(pie_labs, aes(x = 1, y = antall, fill = relasjon)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y", clip = "off") +
  labs(title = "Fordeling etter relasjon til svindler - Straffesaker", fill = "Relasjon") +
  theme_void() +
  geom_text_repel(
    aes(y = ypos, label = stringr::str_wrap(label_inn, 16)),
    size = 3, direction = "y", nudge_x = 0.6,
    segment.size = 0, min.segment.length = 0,
    show.legend = FALSE
  ) +
  xlim(0.5, 1.8) +                      # lager plass til etiketter
  scale_fill_brewer(palette = "Set3")




#figur for resultat av straffesakene----

library(dplyr)
library(ggplot2)
library(scales)

# 1) Recode til faktor med ønskede navn og rekkefølge
straffe_saker <- straffe_saker %>%
  mutate(
    utfall_str_faktor = factor(
      utfall_strl,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Dømt for grovt bedrageri",
        "Dømt for simpelt bedrageri",
        "Dømt for grovt og simpelt bedrageri",
        "Frifinnelse av alle tiltalepunktene"
      )
    )
  )


ggplot(straffe_saker, aes(x = utfall_str_faktor)) +
  geom_bar(
    aes(y = after_stat(prop), fill = after_stat(x), group = 1),
    width = 0.8, show.legend = FALSE
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Utfall i straffesakene", x = NULL, y = "Andel") +
  theme_minimal()








#år mellom dom som avhengig variabel-------/ LÅNESVINDEL----

sivile_saker <- sivile_saker %>% mutate(låneopptak = case_when(svindelhandling == 2 |
                                                                 svindelhandling == 4 ~ 1,
                                                               TRUE ~ 0))








Modell_lånesvindel_siv <- glm(låneopptak ~ as.factor(nærstående) + as.factor(etter_reform) + dato_svindel2 + sum_svindel_100k + år_mellom,
                          data=sivile_saker, family=binomial(link="logit"))



sivile_saker$sum_svindel_100k <- sivile_saker$sum_svindel1/ 100000


library(margins)

m_near <- margins(Modell_lånesvindel_siv, variables = "nærstående")
summary(m_near)

# Predikert nivå ved 0 og 1 (gjennomsnitt over data)
p <- prediction(Modell_lånesvindel_siv, at = list(nærstående = c(0, 1)))
p




summary(Modell_lånesvindel_siv)
vif(Modell_lånesvindel_siv)
pR2(Modell_lånesvindel_siv)


AME_Modell_lånesvindel_siv <- margins(Modell_lånesvindel_siv)
summary(AME_Modell_lånesvindel_siv)

#Modell tid sivile saker-----

Modell_tid <- lm(år_mellom ~ ektefelle + sum_svindel1 + dato_svindel2, data=sivile_saker)

Modell_tid_nærstående <- glm(nærstående~ år_mellom + låneopptak + etter_reform + ,
                             data=sivile_saker, family=binomial(link="logit"))



summary(Modell_tid_nærstående)

summary(Modell_tid)
vif(Modell_tid)

summary(sivile_saker$år_mellom)



sivile_saker <- sivile_saker %>% mutate(annet_misbruk = case_when (låneopptak == 1 ~ 0,
                                                                   låneopptak == 0 ~ 1))



straffe_saker <- straffe_saker %>% mutate (annet_misbruk = case_when (låne_svindel == 1 ~ 0,
                                                                      låne_svindel == 0 ~ 1))



sivile_saker %>% dplyr::select (annet_misbruk) %>% tbl_summary()
straffe_saker %>% dplyr::select (annet_misbruk) %>% tbl_summary()
sivile_saker %>% dplyr::select (svindelhandling) %>% tbl_summary()

#ÅR MELLOM DOM SIVIL--------


sivile_saker %>% dplyr:: select (dato_avsagt2, dato_svindel2) %>% tbl_cross()

sivile_saker %>% dplyr::select (år_mellom) %>% tbl_summary()

str(sivile_saker$år_mellom)
straffe_saker %>% dplyr::select (år_mellom_straff) %>% tbl_summary(type = list(år_mellom_straff ~ "categorical"))

str(sivile_saker$dato_avsagt2)

str(sivile_saker$dato_svindel2)


library(dplyr)

#forbedringer---

## 1) Parse domsdato (dato_avsagt2) til Date -------------------------------

# Hvis datoene dine er "dd.mm.yyyy"
sivile_saker <- sivile_saker %>%
  mutate(
    dato_avsagt_date = as.Date(dato_avsagt2))
  

## Sjekk parsing:
head(sivile_saker %>% dplyr::select(dato_avsagt2, dato_avsagt_date), 10)
sum(is.na(sivile_saker$dato_avsagt_date))

## 2) Lag misbruk-dato fra årstall (dato_svindel2) --------------------------

sivile_saker <- sivile_saker %>%
  mutate(
    svindel_år = as.integer(dato_svindel2),
    
    # HOVEDVALG: midtpunkt i året (nøytral antakelse)
    dato_svindel_mid = as.Date(paste0(svindel_år, "-07-01")),
    
    # Robusthet (valgfritt): tidlig/sen antakelse
    dato_svindel_min = as.Date(paste0(svindel_år, "-01-01")), # lengst tid til dom
    dato_svindel_max = as.Date(paste0(svindel_år, "-12-31"))  # kortest tid til dom
  )

## 3) Funksjon: Date -> desimalår (tar hensyn til skuddår) ------------------

date_to_decimal_year <- function(d) {
  stopifnot(inherits(d, "Date"))
  y <- as.integer(format(d, "%Y"))
  start <- as.Date(paste0(y, "-01-01"))
  end   <- as.Date(paste0(y + 1L, "-01-01"))
  y + as.numeric(d - start) / as.numeric(end - start)
}

sivile_saker <- sivile_saker %>%
  mutate(
    avsagt_aar_desimal = date_to_decimal_year(dato_avsagt_date),
    svindel_mid_desimal = date_to_decimal_year(dato_svindel_mid),
    svindel_min_desimal = date_to_decimal_year(dato_svindel_min),
    svindel_max_desimal = date_to_decimal_year(dato_svindel_max),
    
    # Tid til dom (år)
    tid_til_dom_aar_mid = avsagt_aar_desimal - svindel_mid_desimal,
    tid_til_dom_aar_min = avsagt_aar_desimal - svindel_min_desimal,
    tid_til_dom_aar_max = avsagt_aar_desimal - svindel_max_desimal,
    
    # (valgfritt) i måneder
    tid_til_dom_mnd_mid = tid_til_dom_aar_mid * 12
  )


sivile_saker %>% dplyr::select (avsagt_aar_desimal, svindel_mid_desimal,
                                svindel_min_desimal, svindel_max_desimal,
                                tid_til_dom_aar_mid, tid_til_dom_aar_min, tid_til_dom_aar_max,
                                tid_til_dom_mnd_mid) %>% tbl_summary()


library(dplyr)
library(gtsummary)

sivile_saker %>%
  dplyr::select(
    avsagt_aar_desimal, svindel_mid_desimal, svindel_min_desimal, svindel_max_desimal,
    tid_til_dom_aar_mid, tid_til_dom_aar_min, tid_til_dom_aar_max,
    tid_til_dom_mnd_mid
  ) %>%
  tbl_summary(
    type = everything() ~ "categorical",
    statistic = everything() ~ "{n} ({p}%)"
  )



## 4) Kvalitetssjekk ---------------------------------------------------------

summary(sivile_saker$tid_til_dom_aar_mid)
sum(sivile_saker$tid_til_dom_aar_mid < 0, na.rm = TRUE)  # bør normalt være 0

# Se noen rader for å verifisere:
head(sivile_saker %>% 
       select(dato_avsagt2, dato_avsagt_date, dato_svindel2, dato_svindel_mid, tid_til_dom_aar_mid), 10)






#forbedret modell

år_mellom_dom <- lm(tid_til_dom_aar_mid~  as.factor(indikasjon_prof) + as.factor(nærstående) + as.factor(bank_varslet) + 
                      as.factor(låneopptak) + as.factor(lagmannsrett) + sum_svindel_100k + as.factor(opplysning1) +
                      as.factor(ikke_utbetalt_konto), data= sivile_saker) #as.factor(type_lan) 




summary(år_mellom_dom)

#selve modell, før forbedring ----





år_mellom_dom <- lm(år_mellom ~  as.factor(indikasjon_prof) + as.factor(nærstående) + as.factor(bank_varslet) + 
                      as.factor(låneopptak) + as.factor(lagmannsrett) + sum_svindel_100k + as.factor(opplysning1) +
                    as.factor(ikke_utbetalt_konto), data= sivile_saker) #as.factor(type_lan) 



summary(sivile_saker$vars)







library(dplyr)

sivile_saker <- sivile_saker %>%
  mutate(
    bank_varslet = case_when(
      varsel.3 == 1 | varsel.4 == 1 | varsel.5 == 1 ~ 1,
      varsel.1 == 1 | varsel.2 == 1                 ~ 0,
      TRUE                                          ~ NA_real_
    )
  )






sivile_saker %>% dplyr::select (bank_varslet) %>% tbl_summary()

str(sivile_saker$utbetalt_lan)
str(sivile_saker$lag)

sivile_saker <- sivile_saker %>% mutate(ikke_utbetalt_konto = case_when(utbetalt_lan == 5 |
                                                                     utbetalt_lan == 4 ~ 1,
                                                                   TRUE~ 0))

sivile_saker %>% dplyr::select(ikke_utbetalt_konto, utbetalt_lan) %>% tbl_summary()




summary(år_mellom_dom)
vif(år_mellom_dom)

str(sivile_saker$år_mellom)
str(straffe_saker$år_mellom_straff)

pR2(år_mellom_dom)
vif(år_mellom_dom)




AME_år_mellom_dom_siv <- margins(år_mellom_dom)

summary(AME_år_mellom_dom_siv)



sivile_saker <- sivile_saker %>% mutate(eID_lån = case_when (type_lan == 3 ~ 1,
                                                             TRUE ~ 0))

sivile_saker %>% dplyr::select (eID_lån) %>% tbl_summary()
#åR MELLOM DOM, STRAFF----



#denne burde forbedres-----






##

år_mellom_dom_straff <- lm(år_mellom_straff ~ as.factor(nærstående) +  as.factor(låne_svindel) + as.factor(indikasjon_prof)  + sum_svindel_100k + as.factor(annet_tiltale) + as.numeric(num_S) + as.numeric(num_K) , data= straffe_saker) #type_lan.1 + annet_tiltale + var28


straffe_saker <- straffe_saker %>% mutate (indikasjon_prof = case_when (prof == 2 |
                                                                          prof== 3 ~ 1,
                                                                        TRUE ~ 0))

#interaksjon mellom kun tiltale for id-tyveri og lånesvindel er signifikant----burde legges ved---

år_mellom_dom_straff_interaksjon <- lm(år_mellom_straff ~ as.factor(nærstående) +  as.factor(låne_svindel)*as.factor(annet_tiltale) + 
                             as.factor(indikasjon_prof)  + sum_svindel_100k + as.factor(annet_tiltale) + as.numeric(num_S) + as.numeric(num_K) , data= straffe_saker) #type_lan.1 + annet_tiltale + var28


head(straffe_saker)

view(straffe_saker)


sapply(straffe_saker[, c("nærstående","indikasjon_prof","instans","annet_tiltale")],
       function(x) length(unique(na.omit(x))))

straffe_saker %>% dplyr::select(dato_anmeld, num_S) %>% tbl_summary()



str(straffe_saker$instans)

straffe_saker %>% dplyr::select(indikasjon_prof, prof, sum_svindel_100k) %>% tbl_summary()

summary(år_mellom_dom_straff)

pR2(år_mellom_dom_straff)
vif(år_mellom_dom_straff)


sivile_saker %>% dplyr::select (år_mellom, nærstående) %>% tbl_summary(by=nærstående, type=list(år_mellom ~ "continuous2"))

library(dplyr)
library(gtsummary)

sivile_saker %>%
 dplyr:: select(år_mellom, nærstående) %>%
  tbl_summary(
    by = nærstående,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    type = list(år_mellom ~ "continuous2"))
    
  


straffe_saker %>% dplyr::select (år_mellom_straff, nærstående) %>% tbl_summary(by=nærstående)

straffe_saker %>%
  dplyr:: select(år_mellom_straff, nærstående) %>%
  tbl_summary(
    by = nærstående,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    type = list(år_mellom_straff~ "continuous2"))



#sivile krav ved ulik relasjon-----
straffe_saker$sivilekrav_finans <- as.numeric(straffe_saker$var48.2)

straffe_saker$sivilekrav_offer <- as.numeric(straffe_saker$var48.3)

straffe_saker %>% dplyr::select (sum_svindel_num) %>% tbl_summary()




sivile_krav <- glm(sivilekrav_finans ~ nærstående + type_lan.3 + org_krim + var28 , data=straffe_saker, family=binomial(link= "logit"))


#virker som sivile krav fremmet av banken har en sammenheng med hvor stort beløpet er (over/under beløpsgrensen for grovt bedragei)--
summary(sivile_krav)

pR2(sivile_krav)

str(straffe_saker$sum_svindel_num)


sivile_krav_offer <- glm(sivilekrav_offer ~ ektefelle + var28 + org_krim + type_lan.3 , data=straffe_saker, family=binomial(link= "logit"))

summary(sivile_krav_offer)

pR2(sivile_krav)
vif(sivile_krav)

str(straffe_saker$sum_svindel_num)
str(straffe_saker$relasjon.3)


#varsel---denne må forbedres, kanskje legge til en variabel om gjemt unna post----

sivile_saker$varsel_finans <- as.numeric(sivile_saker$varsel.2)

sivile_saker %>% dplyr::select (varsel_finans) %>% tbl_summary()


varsel <- glm(varsel_finans ~nærstående + opplysning1 + log_sum_svindel + dato_avsagt1, data= sivile_saker, family= binomial(link= "logit"))

summary(varsel)

#år mellom svindel og straff øker når det er flere tiltalte punkter
#kan dette tyde på at det kanskje ikke er svindelen som er hovedgreia som de ble tiltalt for?


straffe_saker %>% dplyr::select(ektefelle, var24) %>% tbl_cross()


straffe_saker %>% dplyr::select (siv_beh) %>% tbl_summary()

sivile_saker %>% dplyr::select (siv_beh) %>% tbl_summary()


straffe_saker %>% dplyr::select (sum_svindel_gen) %>% tbl_summary(type=list(sum_svindel_gen ~ "categorical"))

str(straffe_saker$sum_svindel_gen)


straffe_saker %>% dplyr::select (sivile_krav, nærstående) %>% tbl_cross()


straffe_saker %>% dplyr::select (nærstående, var48.2) %>% tbl_cross()


sivile_saker %>% dplyr::select (language_k) %>% tbl_summary() 


#diagram---må forbedres etterhvert-----


DiagrammeR::grViz("
digraph {

graph [layout = dot, rankdir = TB]
  
  node [shape = rectangle]

  rec1  [label = 'Oppdage misbruk']


  rec2  [label = 'Anmelde til politiet']
  rec3  [label = 'Etterforskning']
  rec4  [label = 'Henleggelse']
  rec5  [label = 'Tiltale']
  rec6  [label = 'Straffesak i retten']
  


  rec7 [label = 'Kontakte bank']
  rec8 [label = 'Finansklagenemnda']
  rec9 [label = 'Forliksrådet']
rec10 [label = 'Sivil sak i retten']


#REKKEFØLGE
  rec1 -> rec2
  rec1 -> rec7


  rec2 -> rec3
  rec3 -> rec4
  rec3 -> rec5
  rec5 -> rec6
  


  rec7 -> rec8
  rec8 -> rec9
  rec9 -> rec10


}
", height = 500)



#NYTT FORSØK PÅ DIAGRAM-----

DiagrammeR::grViz("
digraph {

  # GRAF-OPPSETT
  graph [layout = dot, rankdir = LR]   # LR = Left–Right (liggende)
  node  [shape = rectangle]

  # NODER: STRAFFERETTSLIG SPOR
  rec1  [label = 'Oppdage\nmisbruk']

  rec2  [label = 'Anmelde\ntil politiet']
  rec3  [label = 'Etterforskning']
  rec4  [label = 'Henleggelse']
  rec5  [label = 'Tiltale']
  rec6  [label = 'Straffesak\ni retten']

  # NODER: SIVIL-/FORBRUKERSPOR
  rec7  [label = 'Kontakte bank']
  rec8  [label = 'Finans-\nklagenemnda']
  rec9  [label = 'Forliksrådet']
  rec10 [label = 'Sivil sak\ni retten']

  # NODER: FRAFALL / STOPP
  drop1 [label = 'Ingen\nvidere oppfølging', shape = ellipse, style = dashed]
  drop2 [label = 'Ingen\nvidere straffesak', shape = ellipse, style = dashed]
  drop3 [label = 'Ingen\nvidere klage/krav', shape = ellipse, style = dashed]
  drop4 [label = 'Sak\navsluttes', shape = ellipse, style = dashed]

  # HOVEDFLYT

  # Start -> to spor
  rec1 -> rec2
  rec1 -> rec7

  # STRAFFERETTSLIG SPOR
  rec2 -> rec3
  rec3 -> rec4
  rec3 -> rec5
  rec5 -> rec6

  # SIVIL-/FORBRUKERSPOR
  rec7  -> rec8
  rec8  -> rec9
  rec9  -> rec10

  # FRAFALL / DROP-OUT-PILER

  # Strafferettslig spor
  rec2 -> drop1 [style = dashed, label = '  ingen anmeldelse\n  / ingen videre kontakt', fontsize = 9]
  rec3 -> drop2 [style = dashed, label = '  henleggelse\n  uten videre steg', fontsize = 9]

  # Sivil-/forbrukerspor
  rec7  -> drop3 [style = dashed, label = '  ikke klage\n  videre', fontsize = 9]
  rec8  -> drop3 [style = dashed]
  rec9  -> drop4 [style = dashed, label = '  ingen sak\n  i retten', fontsize = 9]

}
", height = 500)


ggsave("figur1.pdf", width = 7, height = 5)


#FINNE SAKER: SIVILE SAKER og straffe saker----
#prosess for å finne saker
#dette skal være en del av metode/ utvalgsprosessen

#finne nærstående saker

sivile_saker %>% dplyr::select (nærstående, referanse, ektefelle, utfall_riktig) %>% filter(nærstående == 1) %>% filter(ektefelle== 0) %>% tbl_summary(by=utfall_riktig)

#etter 2020

sivile_saker %>% dplyr::select (nærstående, referanse, ektefelle, utfall_riktig, etter_reform) %>% filter(nærstående == 1) %>% filter(ektefelle== 0) %>% filter(etter_reform==1) %>% tbl_summary(by=utfall_riktig)



sivile_saker %>% dplyr::select (nærstående, referanse, utfall_riktig, opplysning1, prof, dato_avsagt, bank_anført_grunnlag) %>% filter(referanse == "22-190320TVI-TOSL/02") %>% tbl_summary()


sivile_saker %>% dplyr::select (dato_avsagt) %>% tbl_summary()

sivile_saker %>% dplyr::select (opplysning1, nærstående) %>% tbl_cross()

sivile_saker %>% dplyr::select (deltakelse_sikkerhetsbrudd, språk_utfordring) %>% tbl_cross()

sivile_saker %>% dplyr::select (hvordan_passord, språk_utfordring) %>% tbl_cross()

sivile_saker %>% dplyr::select (hvorfor_passord, språk_utfordring) %>% tbl_cross()

#finne ektefelle saker


sivile_saker %>% dplyr::select (nærstående, referanse, ektefelle, utfall_riktig) %>% filter(ektefelle== 1) %>% tbl_summary(by=utfall_riktig)


sivile_saker %>% dplyr::select(ektefelle, svindelhandling) %>% tbl_cross()
sivile_saker %>% dplyr::select(ektefelle, bank_anført_grunnlag) %>% tbl_cross()

sivile_saker %>% dplyr::select(ukjent, svindelhandling) %>% tbl_cross()
sivile_saker %>% dplyr::select(ukjent, bank_anført_grunnlag) %>% tbl_cross()
sivile_saker %>% dplyr::select (nærstående, referanse, ektefelle, utfall_riktig) %>% filter(ektefelle== 1) %>% tbl_summary(by=utfall_riktig)


sivile_saker %>% dplyr::select (opplysning, referanse, prof, deltakelse_sikkerhetsbrudd) %>% filter(referanse== "17-197796TVI-HAUG/") %>% tbl_summary()


#ektefelle saker, sivile, etter reform
sivile_saker %>% dplyr::select (nærstående, referanse, ektefelle, utfall_riktig, etter_reform) %>% filter(ektefelle== 1) %>% filter(etter_reform==1) %>% tbl_summary(by=utfall_riktig)


sivile_saker %>% count (ektefelle, etter_reform, utfall_riktig)

sivile_saker %>% dplyr::select (nærstående, referanse, ektefelle, ukjent, utfall_riktig, easybank, dato_avsagt) %>% filter(ukjent== 1) %>% filter(easybank==1) %>% tbl_summary(by=utfall_riktig)

sivile_saker %>% dplyr::select (Barn, nærstående, referanse, ektefelle, utfall_riktig) %>% filter(nærstående == 1) %>% filter(Barn== 1) %>% tbl_summary(by=utfall_riktig)
sivile_saker %>% dplyr::select (Barn, nærstående, referanse, ektefelle, utfall_riktig, sum_svindel1, dato_svindel2, dato_avsagt1, prof_svindel1, prof, anmeld,bank_anført_grunnlag, dommerfullmektig, arbeidssted_dommer_binær, deltakelse_sikkerhetsbrudd ) %>% filter(referanse== "TSOFT-2017-175325")%>% tbl_summary(by=utfall_riktig)

sivile_saker %>% dplyr::select (Barn, nærstående, referanse, ektefelle, utfall_riktig, sum_svindel1, dato_svindel2, dato_avsagt1, prof_svindel1, prof, anmeld, bank_anført_grunnlag, dommerfullmektig, arbeidssted_dommer_binær, deltakelse_sikkerhetsbrudd) %>% filter(referanse== "16-050194TVI-NOHO/")%>% tbl_summary(by=utfall_riktig)


#delt passord, men offer vunnet allikevel- finne en utypisk sak

sivile_saker %>% dplyr::select(referanse, deltakelse_sikkerhetsbrudd, utfall_riktig) %>% filter (deltakelse_sikkerhetsbrudd == 2) %>% filter (utfall_riktig == 0) %>% tbl_summary()


sivile_saker %>% dplyr::select (ukjent, referanse, utfall_riktig)  %>%  filter(ukjent == 1) %>% tbl_summary(by=utfall_riktig)
sivile_saker %>% dplyr::select (nærstående, referanse, ektefelle, utfall_riktig) %>% filter(nærstående == 1) %>% tbl_summary(by=utfall_riktig)

sivile_saker %>% dplyr::select (nærstående, referanse, ektefelle, utfall_riktig, bank_anført_grunnlag) %>% filter(nærstående == 1) %>% filter(bank_anført_grunnlag == 1 ) %>% tbl_summary(by=utfall_riktig)


sivile_saker %>% dplyr::select(bank_anført_grunnlag, ukjent, relasjon_uklar) %>% tbl_summary(by=bank_anført_grunnlag)


#finne nærstående saker, nærstående

straffe_saker %>% dplyr::select (nærstående, referanse) %>% filter(nærstående == 1) %>% tbl_summary()


#ekskludere frifinnelser
straffe_saker %>% dplyr::select (nærstående, referanse, utfall_strl, ektefelle) %>% filter (utfall_strl != 4) %>% filter(nærstående == 1) %>% filter (ektefelle !=1) %>% tbl_summary()


straffe_saker %>% dplyr::select (nærstående, referanse, utfall_strl, ektefelle, år_svindel, etter_2020) %>% filter (utfall_strl != 4) %>% filter (ektefelle !=1) %>% filter(år_svindel == 2020) %>% filter(nærstående == 1) %>% tbl_summary()

straffe_saker %>% dplyr::select (nærstående, referanse, utfall_strl, ektefelle, år_svindel, etter_2020) %>% filter (utfall_strl != 4) %>% filter (ektefelle !=1) %>% filter(etter_2020 == 1) %>% filter(nærstående == 1) %>% tbl_summary() #denne går ikke, fordi det er ingen etter 2020


straffe_saker %>% dplyr::select ( referanse, utfall_strl, år_svindel, etter_2020, svindelhandling, nærstående) %>% filter (utfall_strl != 4) %>% filter(etter_2020 == 1) %>% tbl_summary() 


straffe_saker %>% dplyr::select (nærstående, referanse, utfall_strl, ektefelle, år_svindel, etter_2020) %>% filter (utfall_strl != 4) %>% filter (ektefelle !=1) %>% filter(år_svindel == 2020) %>% tbl_summary()
straffe_saker %>% dplyr::select (år_svindel) %>% tbl_summary(type=list(år_svindel ~ "categorical"))

straffe_saker %>% dplyr::select (år_svindel, referanse, etter_2020) %>% filter(referanse == "21-148951MED-TOSL/03") %>% tbl_summary()




straffe_saker %>% dplyr::select (nærstående, referanse, utfall_strl, ektefelle) %>% filter (utfall_strl == 4) %>% filter (nærstående == 1) %>% tbl_summary()

straffe_saker %>% dplyr::select (nærstående, referanse, utfall_strl, ektefelle) %>% filter (utfall_strl != 4) %>% filter (ektefelle ==1) %>% tbl_summary()


straffe_saker %>% dplyr::select (nærstående, referanse, utfall_strl, ektefelle, år_svindel) %>% filter (utfall_strl != 4) %>% filter (ektefelle ==1) %>% filter(år_svindel == 2020) %>% tbl_summary()


straffe_saker %>% dplyr::select (referanse, dom_år) %>% filter(dom_år == 2015 ) %>% tbl_summary()

straffe_saker %>% dplyr::select (tiltalt_) %>% tbl_summary()


straffe_saker %>% dplyr::select (svindelhandling, sum_svindel_gen) %>%tbl_summary(by=svindelhandling)


straffe_saker %>% dplyr::select (svindelhandling, var28) %>%tbl_summary(by=svindelhandling)

straffe_saker %>% dplyr::select (nærstående,ukjent_svindler) %>% tbl_cross()

straffe_saker %>% count(relasjon.1, relasjon.2, relasjon.3, relasjon.4, relasjon.5, relasjon.6, relasjon.7, relasjon.8, relasjon.9, relasjon.10, relasjon.11) %>% #, relasjon.6, relasjon.7, relasjon.8, relasjon.9, relasjon.10, relasjon.11) %>%
  print(n=Inf)



#språkutfordringer og dele passord--- funn til master

sivile_saker %>% dplyr::select (språk_utfordring, hvorfor_passord) %>% tbl_cross()


sivile_saker %>% dplyr::select (nærstående, hvorfor_passord) %>% filter (nærstående==1) %>% tbl_summary()

sivile_saker %>% dplyr::select ( hvorfor_passord, relasjon )%>% tbl_summary(by=relasjon)


sivile_saker %>% dplyr::select (språk_utfordring, utfall_riktig) %>% tbl_cross()

sivile_saker %>% dplyr::select (språk_utfordring, bank_anført_grunnlag) %>% tbl_cross()

sivile_saker %>% dplyr::select (språk_utfordring, referanse, ektefelle, deltakelse_sikkerhetsbrudd, utfall_riktig) %>% filter (språk_utfordring == 1) %>% filter(ektefelle==1)%>% filter(deltakelse_sikkerhetsbrudd == 2) %>% tbl_summary()

sivile_saker %>% dplyr::select (språk_utfordring, referanse, ektefelle, hvorfor_passord, utfall_riktig) %>% filter (språk_utfordring == 1) %>% filter(ektefelle==1)%>% filter(hvorfor_passord == 4) %>% tbl_summary()


sivile_saker %>% dplyr::select (språk_utfordring, referanse, nærstående, deltakelse_sikkerhetsbrudd) %>% filter (språk_utfordring == 1) %>% filter(nærstående==1) %>% filter(deltakelse_sikkerhetsbrudd == 2) %>% tbl_summary()

sivile_saker %>% dplyr::select (språk_utfordring, referanse, nærstående, deltakelse_sikkerhetsbrudd, relasjon) %>% filter (språk_utfordring == 1) %>% filter(deltakelse_sikkerhetsbrudd == 2) %>% tbl_summary(by=relasjon)


sivile_saker %>% dplyr::select (språk_utfordring, referanse, nærstående, hvorfor_passord, utfall_riktig) %>% filter (språk_utfordring == 1) %>% filter(nærstående==1)%>% tbl_summary()

sivile_saker %>% dplyr::select(hvorfor_passord, nærstående) %>% tbl_cross()

sivile_saker%>% dplyr::select(hvorfor_passord, utfall_riktig) %>% tbl_cross()

sivile_saker %>% dplyr::select(hvorfor_passord, utfall_finans_endret) %>% tbl_cross()



sivile_saker %>% dplyr::select (nærstående, hvorfor_passord, utfall_finans_endret) %>% filter (nærstående == 1) %>% tbl_summary(by=utfall_finans_endret)


sivile_saker <- sivile_saker %>% 
  dplyr::mutate(
    utfall_finans_endret = dplyr::case_when(
      utfall_fil1999 == 5 ~ 0,
      TRUE ~ utfall_riktig     # ellers: behold verdien fra utfall_riktig
    )
  )




sivile_saker %>% dplyr::select(språk_utfordring, language_k) %>% tbl_cross()


sivile_saker %>% dplyr::select(nærstående) %>% tbl_summary()
straffe_saker %>% dplyr::select (nærstående, utfall_strl) %>% filter(utfall_strl !=4) %>% tbl_summary()



#sivile saker, anmeldelser


sivile_saker %>% dplyr::select (nærstående, anmeld) %>% filter (nærstående == 1) %>% tbl_cross()

sivile_saker %>% dplyr::select (nærstående, anmeld) %>% filter %>% tbl_cross()




#STRAFFESAKER, EKTEFELLE

straffe_saker <- read_excel("data/straffesaker.xlsx")


#ekskludere frifinnelser
straffe_saker %>% dplyr::select ( referanse, utfall_strl, ektefelle, svindelhandling) %>% filter (utfall_strl != 4) %>% filter (ektefelle ==1) %>% filter(svindelhandling == 2) %>% tbl_summary()

straffe_saker %>% dplyr::select ( referanse, utfall_strl, nærstående, svindelhandling) %>% filter (utfall_strl != 4) %>% filter(nærstående==1) %>% filter (svindelhandling == 2) %>% tbl_summary()
#bare sjekke noe

sivile_saker_erstatning %>% dplyr::select (rettsgrunnlag_, utfall_riktig) %>% tbl_summary(by=utfall_riktig)

sivile_saker_erstatning




sivile_saker %>% dplyr::select(ukjent_svindler, bank_anført_grunnlag) %>% tbl_cross()


sivile_saker %>% dplyr::select (utfall_finans_endret, erstat_lemp) %>% tbl_cross()

sivile_saker %>% dplyr::select (nærstående, erstat_lemp) %>% tbl_summary (by=nærstående)



sivile_saker %>% dplyr::select (erstat_lemp, referanse, kvinne_offer, nærstående, ektefelle) %>% filter (erstat_lemp == 3 ) %>% tbl_summary( by= kvinne_offer)

sivile_saker %>% dplyr::select (erstat_lemp, referanse, kvinne_offer, nærstående, ektefelle) %>% filter (erstat_lemp == 3 ) %>% tbl_summary( by= nærstående)


sivile_saker %>% dplyr::select (utfall_finans_endret, nærstående) %>% tbl_cross()


straffe_saker %>% dplyr::select (utfall_strl, nærstående) %>% tbl_summary(by = nærstående)

straffe_saker %>% dplyr :: select (utfall_strl, nærstående, referanse) %>% filter(utfall_strl == 4) %>% tbl_summary (by= nærstående)

sivile_saker %>% dplyr::select (hvorfor_passord, utfall_finans_endret, referanse, nærstående, deltakelse_sikkerhetsbrudd) %>% filter (deltakelse_sikkerhetsbrudd == 2) %>% filter(utfall_finans_endret == 0) %>% filter (nærstående == 1) %>% tbl_summary()


summary (sivile_saker$sum_svindel1)

sivile_saker %>% dplyr::select (nærstående, sum_svindel1) %>% filter (nærstående == 1) %>% tbl_summary()

sivile_saker %>% 
  dplyr::select(nærstående, sum_svindel1) %>% 
  dplyr::filter(nærstående == 1) %>% 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({min}, {max})"
    )
  )


sivile_saker %>% 
  dplyr::select(ukjent_svindler, sum_svindel1) %>% 
  dplyr::filter(ukjent_svindler == 1) %>% 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({min}, {max})"
    )
  )

sivile_saker %>% dplyr::select (utfall_finans_endret, deltakelse_sikkerhetsbrudd, nærstående) %>% filter (nærstående == 1) %>% tbl_summary(by=utfall_finans_endret)


sivile_saker %>% dplyr::select (anmeld, nærstående) %>% tbl_cross()


sivile_saker %>% dplyr::select (utfall_finans_endret) %>% tbl_summary()


straffe_saker %>% dplyr::select (år_svindel, dom_år, annet_tiltale) %>% tbl_summary (by = annet_tiltale, type= list (år_svindel ~ "categorical"))

straffe_saker %>% dplyr::select (tiltalt_, nærstående, ikke_nærstående) %>% tbl_summary (by=tiltalt_)


straffe_saker %>% dplyr::select (nærstående, sum_svindel_num) %>% filter (nærstående ==1) %>% tbl_summary ()

straffe_saker %>% dplyr::select (nærstående, sum_svindel_num) %>% filter (nærstående ==0) %>% tbl_summary ()

straffe_saker %>%
  dplyr::filter(nærstående == 1) %>%
  dplyr::select(sum_svindel_num) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean}",
      "{min}",
      "{max}"
    )
  )

sivile_saker %>% dplyr::select (sum_svindel1, nærstående, eID_lån)

sivile_saker %>%
  dplyr::filter(nærstående == 1) %>%
  dplyr::select(sum_svindel1) %>%
  dplyr::filter (eID_lån == 1) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean}",
      "{min}",
      "{max}"
    )
  )

sivile_saker$eID


#deskriptivt, økonomisk tap for låneopptak, nærstående ----
sivile_saker %>%
  dplyr::filter(nærstående == 1) %>%
    dplyr::filter (låneopptak == 1) %>%
  dplyr::select(sum_svindel1) %>%

  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean}",
      "{min}",
      "{max}"
    )
  )


#JURK DATASETT-------GATEJURISTEN, JUSSBUSS, JURK, PERIODE 2015-2021------
install.packages("haven")       # en gang
library(haven)

Jurk <- read_dta("data/Dataset_rapport.dta")   # leser Stata- .dta til en tibble
glimpse(Jurk)
view(Jurk)


Jurk %>% dplyr::select(var54, var27, var24) %>% tbl_summary()
Jurk %>% dplyr::select(var54, var27) %>% tbl_cross()



