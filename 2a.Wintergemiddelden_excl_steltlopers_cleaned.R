library(tidyverse)
library(INBOtheme)
library(zoo)
# theme_set(theme_inbo(base_size = 12, transparent = FALSE))
conflicted::conflict_prefer("filter", "dplyr")

### functies opvragen
source("./GitScripts/Functies_WV_grafieken.R")

##################
# 1 DATA INLEZEN
##################

WV_DB <- read_csv("./Data/WV_DB_2025-01-23.csv")
WV_DB <- WV_DB %>% rename(LocationWVNaam = Gebied) 

Gebieden_ZS <- read_csv("./Data/Gebieden_ZS_2025-03-27.csv")

### Gebiedsklasse benoemen:
Gebieden_ZS <- Gebieden_ZS %>% 
  mutate(Gebiedsklasse = ifelse(LocationGroupCode %in% c("SEST","WETL"), "Sigmagebied", 
                                ifelse(LocationGroupCode == "ZSVAL", "Valleigebied",
                                       if_else(LocationGroupCode %in% c("ZS","ZR"), "Estuarium",
                                               if_else(LocationGroupCode == "NOH", "Haven", NA)))))
Gebieden_ZS %>% distinct(LocationGroupCode, Gebiedsklasse)

### check:
Gebieden_ZS %>% anti_join(WV_DB, by = c("LocationWVNaam")) %>% select(LocationWVNaam) %>% pull()
### geen mismatches: alle gebieden zitten ook in de WV_DB

### selecteren van de tellingen in de relevante telgebieden
Tellingen_ZS <- WV_DB %>% right_join(Gebieden_ZS, by = c("LocationWVNaam"))

### selecteren periode 1991/92 -> 2023/24; meeuwen worden niet betrokken in de analyse
Tellingen_ZS <- Tellingen_ZS %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4)),
         Telseizoen_chr = str_sub(Telseizoen, 1, 4)) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1990 & Telseizoen_num < 2024)

### zelfde actie voor volledige WV_DB:
WV_DB <- WV_DB %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4)),
         Telseizoen_chr = str_sub(Telseizoen, 1, 4)) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1990 & Telseizoen_num < 2024)

# Tellingen_ZS %>%
#   group_by(ProjectCode) %>% summarise(n())
# 
# WV_DB %>%
#   group_by(ProjectCode) %>% summarise(n())
# 
# WV_DB %>% filter(LocationWVNaam == "Poldercomplex OOSTKERKE") %>%
#   distinct(NedNaam) %>%
#   filter(str_detect(NedNaam, "gans")|str_detect(NedNaam, "Gans"))
# ### Geen inheemse ganzen! Dit betekent dat de Analysesetkey selectie "F.Analysesetkey in (2, 3, 4)" in 1 beweging
# ### de juiste soorten voor de juiste gebieden selecteert; de onderstaande filter op ProjectCode lijkt mij daarom verkeerd!!
# ### -> nagevraagd bij Frederic: de filtering op projectcode is idd foutief (in deze context) en wordt bij deze gedeactiveerd:
# WV_DB <- WV_DB %>% filter(ProjectCode %in% c("MIDMA","ZSCH"))

### check:
nrow(WV_DB) == 875963
nrow(Tellingen_ZS) == 234277

#############################
# 2 Meest getelde soorten
#############################

Aantallen_per_soort <- Tellingen_ZS %>% 
  group_by(NedNaam, Telseizoen) %>% 
  summarise(n = n(), total = sum(Aantal), mean = total/n) 

Aantallen_per_soort %>% arrange(desc(mean)) %>% head(20)
Aantallen_per_soort %>% arrange(desc(mean)) %>% head(20) %>% distinct(NedNaam) 
### ganzen en smienten zitten in de grootste concentraties
Aantallen_per_soort %>% arrange(desc(total)) %>% head(20) 
Aantallen_per_soort %>% arrange(desc(total)) %>% head(20) %>% distinct(NedNaam) 
### wilde eend en wintertaling komen voor in de grootste totale aantallen

Aantallen_per_soort %>%
  group_by(NedNaam) %>% 
  summarise(total = sum(total)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  select(NedNaam, total)
### dit zijn de 5 meest getelde soorten in de periode 1991-2024

### Ter controle:
# 1 Wilde Eend   2106409
# 2 Wintertaling 1857232
# 3 Smient       1138447
# 4 Meerkoet      950949
# 5 Kievit        886092

##############################
# 3 Aantal getelde gebieden
##############################

Telseizoen_gebied <- Tellingen_ZS %>% 
  distinct(Telseizoen_chr, LocationWVNaam, Gebiedsklasse) 

a1.N_tellingen <- ggplot(Telseizoen_gebied) + geom_bar(aes(x = Telseizoen_chr)) + 
  scale_x_discrete(breaks = seq(from = 1991, to = 2023, by = 2)) +
  labs(title = "Zeeschelde-vallei", x = "Telseizoen", y = "Aantal getelde gebieden") +
  theme_bw() +
  f_graph_theme()

a1.N_tellingen
f_save_graph(a1.N_tellingen)

a2.N_tellingen_gebied <- a1.N_tellingen + facet_wrap(~ Gebiedsklasse, nrow = 2, scales = "free_y") +
  theme(strip.text = element_text(size = 9, hjust = 0),
        strip.background = element_rect(fill = "white", linetype = "blank"),
        axis.text = element_text(size = 7))

a2.N_tellingen_gebied
f_save_graph(a2.N_tellingen_gebied)

#########################################################
# 4 Aantallen in Vlaanderen, Zeeschelde & Sigmagebieden
#########################################################
Tot_Telling_Vl <- WV_DB %>% 
  filter(Groepscode != "S") %>% 
  group_by(Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal))

Tot_Telling_ZS <- Tellingen_ZS %>% 
  filter(Groepscode != "S") %>%   
  group_by(Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal))

Tot_Telling_Sigma <- Tellingen_ZS %>% 
  filter(Gebiedsklasse == "Sigmagebied", 
         Groepscode != "S") %>%  
  group_by(Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal))

Tot_Telling_summary_temp <- Tot_Telling_Sigma %>% 
  full_join(Tot_Telling_ZS, by = c("Telseizoen_num", "Telling")) %>% 
  full_join(Tot_Telling_Vl, by = c("Telseizoen_num", "Telling")) %>% 
  rename(aantal_Sigma = aantal.x,
         aantal_ZS = aantal.y,
         aantal_Vl = aantal) %>% 
  mutate(Aandeel_Sigma_ZS = aantal_Sigma / aantal_ZS,
         Aandeel_Sigma_Vl = aantal_Sigma / aantal_Vl,
         Aandeel_ZS_Vl = aantal_ZS / aantal_Vl)
Tot_Telling_summary_temp

Tot_Telling_summary <- Tot_Telling_summary_temp %>%
  group_by(Telseizoen_num) %>% 
  summarise(max_Sigma = max(aantal_Sigma), 
            mean_Sigma = mean(aantal_Sigma),
            max_ZS = max(aantal_ZS),
            mean_ZS = mean(aantal_ZS),
            max_Vl = max(aantal_Vl),
            mean_Vl = mean(aantal_Vl),
            mean_Aandeel_Sigma_ZS = mean(Aandeel_Sigma_ZS),
            mean_Aandeel_Sigma_Vl = mean(Aandeel_Sigma_Vl),
            mean_Aandeel_ZS_Vl = mean(Aandeel_ZS_Vl)) 
Tot_Telling_summary

#####################################
# 5a Grafieken: wintergemiddelden  
#####################################
ggplot(Tot_Telling_summary_temp, aes(x = Telseizoen_num, y = aantal_Sigma))  +
  geom_point() + geom_smooth() + 
  f_labs_totaal("Sigmagebieden (getelde aantallen)") 

ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = max_Sigma)) +
  geom_point() + geom_smooth() +
  f_labs_totaal("Sigmagebieden (wintermaxima)") 

b1a.Totaal_Sigma <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Sigma)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 20000)) + 
  f_labs_totaal("Sigmagebieden: wintergemiddelden") + 
  theme_bw() +
  f_graph_theme()

b1a.Totaal_Sigma
f_save_graph(b1a.Totaal_Sigma)

b2a.Totaal_ZS <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_ZS)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 125000)) + 
  f_labs_totaal("Zeeschelde-vallei: wintergemiddelden") + 
  theme_bw() +
  f_graph_theme()

b2a.Totaal_ZS
f_save_graph(b2a.Totaal_ZS)

b3a.Totaal_VL <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 320000)) + 
  f_labs_totaal("Vlaanderen: wintergemiddelden") + 
  theme_bw() +
  f_graph_theme()

b3a.Totaal_VL
f_save_graph(b3a.Totaal_VL)


Tot_Telling_Sigma_ZS <- Tot_Telling_summary %>% pivot_longer(cols = c(2:10)) %>% filter(name %in% c("mean_Sigma","mean_ZS"))

b4a.Totaal_Sigma_ZS <- ggplot() + 
  geom_point(data = Tot_Telling_Sigma_ZS, aes(x = Telseizoen_num, y = value, colour = name), size = 0.5) + 
  geom_smooth(data = Tot_Telling_Sigma_ZS, aes(x = Telseizoen_num, y = value, colour = name, fill = name), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 125000)) + 
  scale_colour_manual(values = c("darkgreen","orangered3"), labels = c("Sigma", "Zeeschelde-vallei"), name = NULL) +
  scale_fill_manual(values = c("darkgreen","orangered3"), labels = c("Sigma", "Zeeschelde-vallei"), name = NULL) +
  labs(title = "Sigmagebieden & Zeeschelde-vallei: wintergemiddelden", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85)) + 
  f_graph_theme() 

b4a.Totaal_Sigma_ZS
f_save_graph(b4a.Totaal_Sigma_ZS)

Tot_Telling_ZS_VL <- Tot_Telling_summary %>% pivot_longer(cols = c(2:10)) %>% filter(name %in% c("mean_ZS","mean_Vl"))

b5a.Totaal_ZS_VL <- ggplot() + 
  geom_point(data = Tot_Telling_ZS_VL, aes(x = Telseizoen_num, y = value, colour = name), size = 0.5) + 
  geom_smooth(data = Tot_Telling_ZS_VL, aes(x = Telseizoen_num, y = value, colour = name, fill = name), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 320000)) + 
  scale_colour_manual(values = c("yellow4","orangered3"), labels = c("Vlaanderen", "Zeeschelde-vallei"), name = NULL) +
  scale_fill_manual(values = c("yellow4","orangered3"), labels = c("Vlaanderen", "Zeeschelde-vallei"), name = NULL) +
  labs(title = "Zeeschelde-vallei & Vlaanderen: wintergemiddelden", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85)) + 
  f_graph_theme() 

b5a.Totaal_ZS_VL
f_save_graph(b5a.Totaal_ZS_VL)

################################################################################
# 5b Grafieken: Aandeel watervogels in Sigmagebieden tov Zeeschelde/Vlaanderen 
################################################################################
c1a.Aandeel_Sigma_ZS <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Aandeel_Sigma_ZS)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0, 0.4), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in Sigmagebieden tov Zeeschelde-vallei") + 
  theme_bw() +
  f_graph_theme()

c1a.Aandeel_Sigma_ZS
f_save_graph(c1a.Aandeel_Sigma_ZS)

c2a.Aandeel_ZS_VL <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Aandeel_ZS_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in de Zeeschelde-vallei tov Vlaanderen") + 
  theme_bw() +
  f_graph_theme()

c2a.Aandeel_ZS_VL
f_save_graph(c2a.Aandeel_ZS_VL)


c3a.Aandeel_Sigma_VL <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Aandeel_Sigma_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0, 0.1), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in Sigmagebieden tov Vlaanderen") + 
  theme_bw() +
  f_graph_theme()

c3a.Aandeel_Sigma_VL
f_save_graph(c3a.Aandeel_Sigma_VL)

#####################################################
# 6 Grafiek: wintergemiddelden per gebiedsklasse
#####################################################
Tot_Telling_Gebiedsklassen_temp <- Tellingen_ZS %>% 
  filter(Groepscode != "S") %>%  
  group_by(Gebiedsklasse, Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal)) 

Tot_Telling_Gebiedsklassen <- Tot_Telling_Gebiedsklassen_temp %>%
  group_by(Gebiedsklasse, Telseizoen_num) %>% 
  summarise(Wintermean = mean(aantal)) 
Tot_Telling_Gebiedsklassen

b6a.Totaal_ZS_Gebiedsklassen <- ggplot() + 
  geom_point(data = Tot_Telling_Gebiedsklassen, aes(x = Telseizoen_num, y = Wintermean, col = Gebiedsklasse), size = 0.5) + 
  geom_smooth(data = Tot_Telling_Gebiedsklassen, aes(x = Telseizoen_num, y = Wintermean, col = Gebiedsklasse, fill = Gebiedsklasse), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number) + 
  scale_colour_discrete(name = NULL) + 
  scale_fill_discrete(name = NULL) + 
  f_labs_totaal("Zeeschelde-vallei: wintergemiddelden") + 
  theme_bw() +
  theme(legend.position = c(0.8, 0.8)) +
  f_graph_theme()

b6a.Totaal_ZS_Gebiedsklassen
f_save_graph(b6a.Totaal_ZS_Gebiedsklassen)


######################
# 7 Aantallen vs IHD
######################
Tot_Telling_ESTUARIUM <- Tellingen_ZS %>% 
  filter(Groepscode != "S", Gebiedsklasse == "Estuarium") %>%  
  mutate(Gebiedsklasse = "Estuarium") %>% 
  group_by(Gebiedsklasse, Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal)) 

Tot_Telling_SIGMA_ESTUARIUM <- Tellingen_ZS %>% 
  filter(Groepscode != "S", Gebiedsklasse %in% c("Estuarium","Sigmagebied")) %>%  
  mutate(Gebiedsklasse = "Estuarium + Sigma") %>%  
  group_by(Gebiedsklasse, Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal)) 

Tot_Telling_rbind <- rbind(Tot_Telling_ESTUARIUM, Tot_Telling_SIGMA_ESTUARIUM)

Tot_Telling_summary <- Tot_Telling_rbind %>%
  group_by(Gebiedsklasse, Telseizoen_num) %>% 
  summarise(Wintermean = mean(aantal)) %>% 
  mutate(Moving_wintermean = rollmean(Wintermean, k = 5, fill = NA, align = "right"))
Tot_Telling_summary

b7.IHD <- ggplot() + 
  geom_point(data = Tot_Telling_summary, aes(x = Telseizoen_num, y = Moving_wintermean, col = Gebiedsklasse), size = 0.5) + 
  geom_smooth(data = Tot_Telling_summary, aes(x = Telseizoen_num, y = Moving_wintermean, col = Gebiedsklasse, fill = Gebiedsklasse), linewidth = 0.25, alpha = 0.3) +
  geom_hline(yintercept = 40000, colour = "black", linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 50000)) + 
  f_labs_totaal("IHD Estuarium\n(voortschrijdend gemiddelde - 5 jaar)") + 
  theme_bw() +
  f_graph_theme()

b7.IHD
f_save_graph(b7.IHD)
