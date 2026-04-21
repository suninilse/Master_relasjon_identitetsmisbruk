
#####SCRIPT FOR DEN KVANTITATIVE ANALYSEN MASTEROPPGAVE OM IDENTITETSMISBRUK BEGÅTT AV NÆRSTÅENDE#####"


######SIVILE SAKER, KVANTITATIV ANALYSE#######


#pakker brukt

library(modelsummary)
library(gtsummary)
library(ggplot2)
library(tidyverse)
library(marginaleffects)
library (readxl)
library(margins)
library(dplyr)
library("lubridate")
library(performance)


#Lese inn datasett----

sivile_saker <- read_excel("data/sivile_saker.xlsx")



#OPERASJONALISERING AV VARIABLER TIL REGRESJONSMODELL 1 
#(Presentert i Tabell 3 i oppgaven)-----

# 1) Avhengig variabel til regresjonsmodell 1:låne- og kredittopptak == 1, annet misbruk == 0-----

sivile_saker <- sivile_saker %>% mutate(låneopptak = case_when(svindelhandling == 2 |
                                                                 svindelhandling == 4 ~ 1,
                                                               TRUE ~ 0))




# 2) Forklaringsvariabel av hovedinteresse: relasjonsvariabel, nærstående gjerningsperson == 1, ukjent/perifer == 0------

sivile_saker <- sivile_saker%>% mutate(nærstående = case_when(relasjon == 1 | relasjon == 2 | relasjon == 3 | 
                                                                relasjon == 4 | relasjon == 5 ~ 1,
                                                              TRUE ~ 0))



#3) Tidsvariabel for HR-2020-2021-A, før HR-2020-2021-A == 0, etter == 1----


sivile_saker <- sivile_saker%>% 
  mutate(etter_reform = case_when(ymd(dato_avsagt2) > "2020-10-22" ~ 1, 
                                  .default = 0))


#4) Tidsvariabel for når svindelen ble utført----



sivile_saker<- sivile_saker %>% mutate(dato_svindel2 = case_when(
  dato_svindel==12 ~ 2020-0,
  dato_svindel==11 ~ 2020-1,
  dato_svindel==10 ~ 2020-2,
  dato_svindel==9~ 2020-3,
  dato_svindel==8~ 2020-4,
  dato_svindel==7~ 2020-5,
  dato_svindel==6 ~2020-6,
  dato_svindel==5 ~ 2020-7,
  dato_svindel==4 ~ 2020-8,
  dato_svindel==3 ~ 2020-9,
  dato_svindel==2 ~ 2020-10,
  dato_svindel==1~ 2020-11))



#5) Variabel for svindelsum, per 100 tusen---



sivile_saker$sum_svindel1 <- as.numeric(sivile_saker$sum_svindel)

sivile_saker$sum_svindel_100k <- sivile_saker$sum_svindel1/ 100000


#6) Variabel for år mellom identitetsmisbruk til endelig dom i retten--------

##1) først- lage variabel for datoen dommen ble avsagt

sivile_saker %>% dplyr::select (dato_avsagt) %>% tbl_summary (type=list(dato_avsagt~ "categorical"))

#observerte at en dato var tastet inn feil, måtte rette opp i det

#Forsøk på å finne referansen til den feiltastede datoen i datasettet

feiltastet_dato <- "2917-12-06"

referanser_feiltastet_dato <- sivile_saker%>%
  filter(dato_avsagt == feiltastet_dato) %>%
  dplyr::select(referanse)


#rette opp i feiltastet dato

sivile_saker<- sivile_saker %>%
  mutate(dato_avsagt2 = ifelse(dato_avsagt == feiltastet_dato, "2017-12-06", dato_avsagt))

#begynne å lage variabel for dato avsagt

sivile_saker<- sivile_saker %>%
  mutate(dato_avsagt1 = ymd(dato_avsagt))

view(sivile_saker$dato_avsagt1)


#konverter til faktor med nivåer 2016-2023
sivile_saker<- sivile_saker %>%
  mutate(dato_avsagt1= factor(year(dato_avsagt2), levels = 2016:2023))

#deretter gjøre om til numerisk

sivile_saker$dato_avsagt1 <- as.numeric(sivile_saker$dato_avsagt1)

str(sivile_saker$dato_avsagt1)

#deretter ta dato avsagt/år avsagt - dato/år identitetsmisbruket ble utført
#dette er variabelen for år mellom identitetsmisbruk til endelig dom
sivile_saker$år_mellom <- sivile_saker$dato_avsagt1 - sivile_saker$dato_svindel2


#REGRESJONSMODELL 1--------
#Presentert i tabell 3, Modell 1, Sivile saker

#låne-eller kredittopptak som avhengig variabel (presentert i Tabell 3 i oppgaven)




Modell_lånesvindel_siv <- glm(låneopptak ~ as.factor(nærstående) + as.factor(etter_reform) + dato_svindel2 + sum_svindel_100k + år_mellom,
                              data=sivile_saker, family=binomial(link="logit"))



#sjekke resultater (log-odds, st.feil, p-verdi)-----
summary(Modell_lånesvindel_siv)

#sjekke  multikollinearitet-----

vif(Modell_lånesvindel_siv)

#sjekke modelltilpasning/ McFaddens R2-----
pR2(Modell_lånesvindel_siv)

#Sjekke gjennomsnittlige marginaleffekter (AME)-----
AME_Modell_lånesvindel_siv <- margins(Modell_lånesvindel_siv)
summary(AME_Modell_lånesvindel_siv)

#OPERASJONALISERING AV VARIABLER TIL REGRESJONSMODELL 2 (Presentert i Tabell 4 i oppgaven)-----

#1)Avhengig variabel: år mellom identitetsmisbruk og dom
##denne er allerede laget i forbindelse med regresjonsmodell 1


#2) Forklaringsvariabel av interesse: nærstående gjerningsperson
##denne er allerede laget i forbindelse med regresjonsmodell 1

#3) variabel for om det var svindel i form av låne- eller kredittopptak, eller annet misbruk
##denne er allerede laget i forbindelse med regresjonsnodell 1

#4) variabel for svindelsum per 100 tusen
##denne er allerede laget i forbindelse med regresjonsnodell 1

#5) variabel for om saken er behandlet i lagmannsretten----


sivile_saker <-  sivile_saker %>% mutate(lagmannsrett = case_when(instans==1 ~ 0,
                                                                  instans==2 ~ 1))


#6) variabel for om det er opgitt feil opplysninger i lånesøknad-----

sivile_saker <-  sivile_saker %>% mutate(opplysning1= case_when(opplysning==1 | 
                                                                  opplysning==3 ~ 0,
                                                                opplysning==4 ~ 1,
                                                                opplysning==2~ 0))


#7) variabel for om lånet er utbetalt ti en annen konto enn offerets----
sivile_saker <- sivile_saker %>% mutate(ikke_utbetalt_konto = case_when(utbetalt_lan == 5 |
                                                                          utbetalt_lan == 4 ~ 1,
                                                                        TRUE~ 0))
#8) variabel for om identitetsmisbruket/svindelen er profesjonell-----

sivile_saker <- sivile_saker %>% mutate(indikasjon_prof = case_when(prof == 1 ~ 0,
                                                                    TRUE~ 1))


#REGRESJONSMODELL 3-----
#presentert i Tabell 4, Modell 3, Sivile saker

år_mellom_dom <- lm(år_mellom ~  as.factor(indikasjon_prof) + as.factor(nærstående) + 
                      as.factor(låneopptak) + as.factor(lagmannsrett) + sum_svindel_100k + as.factor(opplysning1) +
                      as.factor(ikke_utbetalt_konto), data= sivile_saker)

#sjekke resultater (B-koeffisient, st.feil, p-verdi, Adjusted R squared-----
summary(år_mellom_dom)

#sjekke multikollinearitet-----
vif(år_mellom_dom)




#DESKRIPTIV STATISTIKK FOR SIVILE SAKER-----


#fordeling av ulike relasjonskategorier (presentert i tabell 1)-----


sivile_saker %>% dplyr::select (relasjon) %>% tbl_summary()

#kjønnsfordeling, ofre for identitetsmisbruk begåt av nærstående----
#deskriptiv statistikk presentert i del 4.5.7
#dette er en krysstabell, jeg regnet ut prosentandeler manuelt

sivile_saker %>% dplyr::select (nærstående, kvinne_offer) %>% tbl_cross()

#fordeling av nærstående gjerningspersoner vs. ukjente/perifere-----
#deskriptiv statistikk presentert i del 5.1 i oppgaven

sivile_saker %>% dplyr::select (nærstående) %>% tbl_summary()

#sjekke svindelsummer/ økonomiske tap i saker med nærstående gjerningsperson-----
#deskriptiv statistikk presentert i del 5.1 i oppgaven
sivile_saker %>% 
  dplyr::select(nærstående, sum_svindel1) %>% 
  dplyr::filter(nærstående == 1) %>% 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({min}, {max})"
    )
  )


#oppgivelse av passord i tillit-----
#deskriptiv statistikk presentert i delkapittel 5.4 i oppgaven

sivile_saker %>% dplyr::select (hvorfor_passord) %>% tbl_summary()

#oppgivelse av passord i tilit, til nærstående-----
#deskriptiv statistikk presentert i delkapitel 5.4 i oppgaven
#dette er en krysstabell, jeg regnet ut prosent manuelt

sivile_saker %>% dplyr::select (hvorfor_passord, nærstående) %>% tbl_cross()

#språkutfordring og oppgivelse av passord av tillit-----
#deskriptiv statistikk presentert i delkapittel 5.5 i oppgaven
#dette er en krysstabell, jeg regnet ut prosent manuelt

sivile_saker %>% dplyr::select (språk_utfordring, hvorfor_passord) %>% tbl_cross()


#deling av passord, og utfall i saken-----
#deskriptiv statistikk presentert i delkapittel 6.3 i oppgaven

sivile_saker %>% dplyr::select (utfall_finans_endret, hvorfor_passord, nærstående) %>% filter (nærstående == 1) %>% tbl_summary (by=utfall_finans_endret)


#prosentandel av saker som ender med lemping-----
#deskriptiv statistikk presentert i dekapittel 6.5 i oppgaven


sivile_saker %>% dplyr::select (erstat_lemp) %>% tbl_summary()


#lemping fordelt etter kjønn og relasjon-----
#deskriptiv statistikk presentert i delkapittel 6.5 i oppgaven

sivile_saker %>% dplyr::select (erstat_lemp, kvinne_offer, relasjon) %>% tbl_summary(by=erstat_lemp)


#fordeling av lemping i saker med nærstående gjerningsperson------
#deskriptiv statistikk presentert i delkapittel 6.5 i oppgaven
#krysstabell, men prosentandeler regnet ut manuelt

sivile_saker %>% dplyr::select (erstat_lemp, nærstående) %>% tbl_cross()


#utfall i saker med en nærstående gjerningsperson-----
#deskriptiv statistikk presentert i delkapittel 6.6 i oppgaven
#dette er en krysstabell, men prosenten er regnet manuelt

sivile_saker %>% dplyr::select (nærstående, utfall_finans_endret) %>% tbl_cross()



#strafferettslig behandling, er identitetsmisbrukeren dømt for forholdet?-----
#deskriptiv statistikk presentert i delkapittel 6.6 i oppgaven

sivile_saker %>% dplyr::select (anmeld) %>% tbl_summary()



####STRAFFESAKER KVANTITATIV ANALYSE###########



#Lese inn datasett---

straffe_saker <- read_excel("data/straffesaker.xlsx")



#OPERASJONALISERING AV VARIABLER TIL REGRESJONSMODELL 2 (presentert i tabell 3 i oppgaven)-----


#1) Avhengig variabel: Låne- eller kredittopptak == 1, annet misbruk == 0-----

straffe_saker <- straffe_saker %>% mutate(låne_svindel = case_when(svindelhandling == 2 |
                                                                     svindelhandling == 4 ~ 1,
                                                                   TRUE ~ 0))

#2) Forklaringsvariabel av interesse: nærstående gjerningsperson == 1, ukjent/perifer == 0-----



straffe_saker <- straffe_saker %>% mutate (nærstående = case_when (relasjon.1 == "1" |
                                                                     relasjon.2 == "1" |
                                                                     relasjon.3 == "1"|
                                                                     relasjon.4 == "1" |
                                                                     relasjon.5 == "1" ~ 1,
                                                                   TRUE ~ 0))



#3) Tidsvariabel for når identitetsmisbruket skjedde (år) -----

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


#4) variabel for svindelsum/økonomisk tap per 100 tusen -----


straffe_saker$sum_svindel_100k <- straffe_saker$sum_svindel_num / 100000

straffe_saker$sum_svindel_num <- as.numeric(straffe_saker$sum_svindel)

#5) År mellom identitetsmisbruk til endelig dom-----

#må først lage variabel for år dommen ble avsagt


#dato avsagt til år
straffe_saker <- straffe_saker %>%
  mutate(
    dom_dato = ymd(dato_avsagt),   # konverter fra tekst til Date
    dom_år  = year(dato_avsagt)   # nytt heltall: f.eks. 2016
  )


#fant en sak med feiltastet år, så måtte fikse den

straffe_saker <- straffe_saker %>%
  mutate(dom_år = if_else(referanse == "19-105096ENE-OTIR/01", 2019, dom_år))

#så lage variabel for år mellom identitetsmisbruk til endeling dom: ved å ta år dommen ble avsagt minus år for identitetsmisbruk

straffe_saker$år_mellom_straff <- straffe_saker$dom_år - straffe_saker$år_svindel

#6) tidsvariabel for HR-2020-2021-A, før == 0, etter == 1-----
#kaller den etter_easybank, fordi den kalles "easybank-dommen" muntlig

straffe_saker <- straffe_saker%>% 
  mutate(etter_easybank = case_when(ymd(dato_avsagt > "2020-10-22" ~ 1, 
                                        .default = 0))
         


#REGRESJONSMODELL 2-----
#presentert i Tabell 3 i oppgaven, Modell 2, straffesaker


Modell_lånesvindel_straff <- glm(låne_svindel ~ as.factor(nærstående) + as.factor(etter_2020) + år_svindel + as.numeric(sum_svindel_100k)+ år_mellom_straff,
                          data=straffe_saker, family=binomial(link="logit"))


#sjekke resultater-----
summary(Modell_lånesvindel_straff)

#sjekke multikollinearitet-----
vif(Modell_lånesvindel_straff)

#sjekke modelltilpasning/McFaddens R2-----
pR2(Modell_lånesvindel_straff)


#sjekke gjennomsnittlige marginaleffekter (AME) -----

AME_Modell_lånesvindel_straff <- margins (Modell_lånesvindel_straff)
summary(AME_Modell_lånesvindel_straff)



#OPERASJONALISERING AV VARIABLER TIL REGRESJONSMODELL 2 (presentert i tabell 4 i oppgaven)-----

#1)Avhengig variabel: år mellom identitetsmisbruk til dom-----
#allerede laget i forbindelse med regresjonsmodell 2

#2) Forklaringsvariabel av interesse: nærstående gjerningsperson-----
#allerede laget i forbindelse med regresjonsmodell 2

#3) Variabel for låne- eller kredittopptak-----
#allerede laget i forbindelse med regresjonsmodell 2


#4) Variabel for svindelsum/økonomisk tap per 100-----
#allerede laget i forbindelse med regresjonsmodell 2

#5) Variabel for profesjonell svindel eller ikke-----


straffe_saker <- straffe_saker %>% mutate (indikasjon_prof = case_when (prof == 2 |
                                                                          prof== 3 ~ 1,
                                                                        TRUE ~ 0))


#6) Variabel for om tiltalen gjaldt flere tiltaler enn for ID-tyveri/identitetskrenkelse-----
#denne var allerede laget i Nettskjema av SODI-prosjektet, jeg trengte ikke omkode
#jeg beholdt bare opprinnelig navn på variabel

straffe_saker %>% dplyr::select (annet_tiltale) %>% tbl_summary()

#7) Variabel for antall tiltalte-----
#denne var allerede laget i Nettskjema av SODI-prosjektet, jeg trengte ikke omkode
#jeg beholdt opprinnelig navn på variabel

straffe_saker %>% dplyr::select (num_S) %>% tbl_summary()

#8) Variabel for antall fornærmede -----
#allerede laget i Nettskjema av SODI-prosjektet
#beholdt opprinnelig navn på variabel

straffe_saker %>% dplyr::select (num_K) %>% tbl_summary()

#REGRESJONSMODELL 4-----
#presentert i tabell 4 i oppgaven, Modell 4, straffesaker

år_mellom_dom_straff <- lm(år_mellom_straff ~ as.factor(nærstående) +  
                             as.factor(låne_svindel) + as.factor(indikasjon_prof)  +
                             sum_svindel_100k + as.factor(annet_tiltale) + as.numeric(num_S) +
                             as.numeric(num_K) , data= straffe_saker)


#sjekke resultater------

summary (år_mellom_dom_straff)

#sjekke multikollinearitet-----

vif(år_mellom_dom_straff)

#DESKRIPTIV STATISTIKK FOR STRAFFESAKER----

#fordeling av relasjon i straffesaker-----
#presentert i Tabell 2 i oppgaven, del 5.1

straffe_saker$Forelder<- straffe_saker$relasjon.1
straffe_saker$Barn<- straffe_saker$relasjon.2
straffe_saker$Ektefelle_samboer_kjæreste<- straffe_saker$relasjon.3
straffe_saker$Annen_familie<- straffe_saker$relasjon.4
straffe_saker$Nær_venn<- straffe_saker$relasjon.5
straffe_saker$Noen_på_arbeidsplass<- straffe_saker$relasjon.6
straffe_saker$Annen_kjent<- straffe_saker$relasjon.7
straffe_saker$Offentlig_privat_tjenesteyter<- straffe_saker$relasjon.8



straffe_saker %>% dplyr::select (Forelder, Barn, Ektefelle_samboer_kjæreste,
                                 Annen_familie, Nær_venn, Noen_på_arbeidsplass, Annen_kjent, 
                                 Offentlig_privat_tjenesteyter) %>% tbl_summary()





#overlapp i saker med både nærstående og ikke nærstående gjerningsperson i samme sak-----
#tabell presentert i Tabell 1, i delkapittel 4.5.7 i oppgaven
#først lage variabel for saker med ikke-nærstående

straffe_saker <- straffe_saker %>% mutate (ikke_nærstående = case_when (relasjon.6 == "1" |
                                                                          relasjon.7 == "1" |
                                                                          relasjon.8 == "1"|
                                                                          relasjon.9 == "1" |
                                                                          relasjon.10 == "1" |
                                                                          relasjon.11 == "1" ~ 1,
                                                                        TRUE ~ 0))

#så krysstabell mellom nærstående og ikke-nærstående variabel
#prosentandel for saker som inkluserer minst en nærstående, og saker som inkluderer kun ikke-nærstående gjerningspersoner-----
#tabell presentert i tabell 2, del 5.1 i oppgaven
#dette er krysstabell, men prosent er regnet ut manuelt


straffe_saker %>% dplyr::select (ikke_nærstående, nærstående) %>% tbl_cross()





                                