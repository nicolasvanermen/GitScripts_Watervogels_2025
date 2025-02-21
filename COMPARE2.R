library(tidyverse)
library(INBOtheme)
# theme_set(theme_inbo())
conflicted::conflict_prefer("filter", "dplyr")

#################
# 1 DATA INLEZEN
#################
WV_DB <- read.csv("./Data/WV_DB_2025-01-23.csv")
Gebieden_ZS <- read_csv("./Data/Gebieden2 herwerking Wim.csv")

#checks:
Gebieden_ZS %>% anti_join(WV_DB, by = c("Gebied")) %>% select(Gebied) %>% pull()
#geen mismatches: alle gebieden zitten ook in de WV_DB

#selecteren van de tellingen in de relevante telgebieden
Tellingen_ZS <- WV_DB %>% right_join(Gebieden_ZS, by = c("Gebied" = "Gebied"))

#selecteren periode 1991/92 -> 2023/24; meeuwen worden niet betrokken in de analyse
Tellingen_ZS <- Tellingen_ZS %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4)),
         Telseizoen_chr = str_sub(Telseizoen, 1, 4)) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1990 & Telseizoen_num < 2024)

#zelfde selectie voor volledige WV_DB:
WV_DB <- WV_DB %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4)),
         Telseizoen_chr = str_sub(Telseizoen, 1, 4)) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1990 & Telseizoen_num < 2024)

Tellingen_ZS %>% 
  group_by(ProjectCode) %>% summarise(n())

WV_DB %>% 
  group_by(ProjectCode) %>% summarise(n())

WV_DB <- WV_DB %>% filter(ProjectCode %in% c("MIDMA","ZSCH"))
# WV_DB %>% filter(ProjectCode == "GTOKP") %>% group_by(NedNaam) %>% summarise(N = sum(Aantal)) %>% print(n = 100)
# WV_DB %>% filter(ProjectCode == "STELT") %>% group_by(NedNaam) %>% summarise(N = sum(Aantal)) %>% print(n = 100)
# WV_DB %>% filter(ProjectCode == "SLAAL") %>% group_by(NedNaam) %>% summarise(N = sum(Aantal)) %>% print(n = 100)
# # dit in acht genomen lijkt het wel steek te houden om enkel MIDMA & ZSCH te behouden

#check
nrow(WV_DB) == 866177
nrow(Tellingen_ZS) == 234277

##############
# 2 FUNCTIES
##############
f_labs_totaal <- function(title) {
  labs(title = str_c(title), 
       y = "Aantal watervogels", 
       x = element_blank())}  

f_labs_aandeel <- function(title) {
  labs(title = str_c(title), 
       y = "Percentage", 
       x = element_blank())}  

f_graph_theme <- function(){
  theme(plot.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))}

f_save_graph <- function(plotname){
  ggsave(
    plotname,
    filename = paste(deparse(substitute(plotname)), "(2).png", sep = ""), path = "./graphs/Grafieken Algemeen",
    width = 15, height = 10, units = "cm", dpi = 300)}

######################################
# 3 Verkenning meest getelde soorten 
######################################
Aantallen_per_soort <- Tellingen_ZS %>% 
  group_by(NedNaam, Telseizoen) %>% 
  summarise(n = n(), total = sum(Aantal), mean = total/n) 

Aantallen_per_soort %>% arrange(desc(mean)) %>% head(20)
Aantallen_per_soort %>% arrange(desc(mean)) %>% head(20) %>% distinct(NedNaam) 
# ganzen en smienten zitten in de grootste concentraties
Aantallen_per_soort %>% arrange(desc(total)) %>% head(20) 
Aantallen_per_soort %>% arrange(desc(total)) %>% head(20) %>% distinct(NedNaam) 
# de grootste totale aantallen zijn wilde eend en wintertaling

Aantallen_per_soort %>%
  group_by(NedNaam) %>% 
  summarise(total = sum(total)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  select(NedNaam, total)
# dit zijn de 5 meest getelde soorten in de periode 1991-2024

# Ter controle:
# 1 Wilde Eend   2106409
# 2 Wintertaling 1857232
# 3 Smient       1138447
# 4 Meerkoet      950949
# 5 Kievit        886092

########################################
# 4 Verkenning aantal getelde gebieden 
########################################
Gebieden_ZS %>% group_by(Sigma, Vallei, Estuarien, NOHaven) %>% summarise(n = n())
#deze catogorieën sluiten elkaar volledig uit

Tellingen_ZS <-  Tellingen_ZS %>% 
  mutate(Gebiedsklasse = ifelse(Sigma == 1, "Sigmagebied", 
                                ifelse(Vallei == 1, "Valleigebied",
                                       if_else(Estuarien == 1, "Estuarium",
                                               if_else(NOHaven == 1, "Haven", NA)))))

Telseizoen_gebied <- Tellingen_ZS %>% 
  group_by(Telseizoen_chr, Gebied, Gebiedsklasse) %>% 
  summarise()

Telseizoen_gebied %>% group_by(Telseizoen_chr, Gebied) %>% summarise(n = n()) 
Telseizoen_gebied %>% group_by(Telseizoen_chr, Gebied) %>% summarise(n = n()) %>% filter(n > 1)

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

# Wim: 
# De toename van aantal gebieden is goed te verklaren:
# - ontdubbeling LO - RO bij ScheldeTellingen_ZS (1e keer 1998, en daarna elk jaar vanaf 2000?)
# - nieuwe natuurgebieden in de haven
# - nieuwe natuurgebieden Sigmaplan
# - aantal getelde valleigebieden min of meer constant.
# 
# => de verandering in het aantal getelde gebieden heeft allicht geen invloed op de totale aantallen, deze geven een goede schatting van de aanwezige winterpopulatie

########################################################
# 5 Aantallen in Vlaanderen, Zeeschelde & Sigmagebieden
########################################################
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

############################
# 6 Aandeel Sigmagebieden
############################
Tot_Telling_summary_1 <- Tot_Telling_Sigma %>% 
  full_join(Tot_Telling_ZS, by = c("Telseizoen_num", "Telling")) %>% 
  full_join(Tot_Telling_Vl, by = c("Telseizoen_num", "Telling")) %>% 
  rename(aantal_Sigma = aantal.x,
         aantal_ZS = aantal.y,
         aantal_Vl = aantal) %>% 
  mutate(Aandeel_Sigma_ZS = aantal_Sigma / aantal_ZS,
         Aandeel_Sigma_Vl = aantal_Sigma / aantal_Vl,
         Aandeel_ZS_Vl = aantal_ZS / aantal_Vl)
Tot_Telling_summary_1

Tot_Telling_summary_2 <- Tot_Telling_summary_1 %>%
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
Tot_Telling_summary_2

#######################################################################################
# 6.1 Grafieken aantallen watervogels in Sigmagebieden (zonder meeuwen en steltlopers)
#######################################################################################
ggplot(Tot_Telling_summary_1, aes(x = Telseizoen_num, y = aantal_Sigma))  +
  geom_point() + geom_smooth() + 
  f_labs_totaal("Sigmagebieden (getelde aantallen)") 

ggplot(Tot_Telling_summary_2, aes(x = Telseizoen_num, y = max_Sigma)) +
  geom_point() + geom_smooth() +
  f_labs_totaal("Sigmagebieden (wintermaxima)") 

b1a.Totaal_Sigma <- ggplot(Tot_Telling_summary_2, aes(x = Telseizoen_num, y = mean_Sigma)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 20000)) + 
  f_labs_totaal("Sigmagebieden: wintergemiddelden") + 
  theme_bw() +
  f_graph_theme()

b1a.Totaal_Sigma
f_save_graph(b1a.Totaal_Sigma)

b2a.Totaal_ZS <- ggplot(Tot_Telling_summary_2, aes(x = Telseizoen_num, y = mean_ZS)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 125000)) + 
  f_labs_totaal("Zeeschelde-vallei: wintergemiddelden") + 
  theme_bw() +
  f_graph_theme()

b2a.Totaal_ZS
f_save_graph(b2a.Totaal_ZS)

b3a.Totaal_VL <- ggplot(Tot_Telling_summary_2, aes(x = Telseizoen_num, y = mean_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 320000)) + 
  f_labs_totaal("Vlaanderen: wintergemiddelden") + 
  theme_bw() +
  f_graph_theme()

b3a.Totaal_VL
f_save_graph(b3a.Totaal_VL)


Tot_Telling_summary_2 %>% pivot_longer(cols = c(2:10)) %>% filter(name %in% c("mean_Sigma","mean_ZS"))

b4a.Totaal_Sigma_ZS <- ggplot(Tot_Telling_summary_2 %>% pivot_longer(cols = c(2:10)) %>% filter(name %in% c("mean_Sigma","mean_ZS")), 
                          aes(x = Telseizoen_num, y = value, colour = name)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 125000)) + 
  scale_colour_manual(values = c("orangered3", "orangered4"), labels = c("Sigma", "Zeeschelde-vallei"), name = NULL) +
  labs(title = "Sigmagebieden & Zeeschelde-vallei: wintergemiddelden",y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85)) + 
  f_graph_theme() 

b4a.Totaal_Sigma_ZS
f_save_graph(b4a.Totaal_Sigma_ZS)

b5a.Totaal_ZS_VL <- ggplot(Tot_Telling_summary_2 %>% pivot_longer(cols = c(2:10)) %>% filter(name %in% c("mean_ZS","mean_Vl")), 
                          aes(x = Telseizoen_num, y = value, colour = name)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 320000)) + 
  scale_colour_manual(values = c("orangered3", "orangered4"), labels = c("Vlaanderen", "Zeeschelde-vallei"), name = NULL) +
  labs(title = "Zeeschelde-vallei & Vlaanderen: wintergemiddelden",y = "Aantal watervogels", x = element_blank()) + 
  theme_bw() +
  theme(legend.position = c(0.85, 0.85)) +
  f_graph_theme()

b5a.Totaal_ZS_VL
f_save_graph(b5a.Totaal_ZS_VL)

#################################################################################################################
# 6.2 Grafieken aantallen watervogels in Sigmagebieden tov Zeeschelde/Vlaanderen (zonder meeuwen en steltlopers)
#################################################################################################################
c1a.Aandeel_Sigma_ZS <- ggplot(Tot_Telling_summary_2, aes(x = Telseizoen_num, y = mean_Aandeel_Sigma_ZS)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0, 0.4), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in Sigmagebieden tov Zeeschelde-vallei") + 
  theme_bw() +
  f_graph_theme()

c1a.Aandeel_Sigma_ZS
f_save_graph(c1a.Aandeel_Sigma_ZS)

c2a.Aandeel_ZS_VL <- ggplot(Tot_Telling_summary_2, aes(x = Telseizoen_num, y = mean_Aandeel_ZS_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0.15, 0.45), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in de Zeeschelde-vallei tov Vlaanderen") + 
  theme_bw() +
  f_graph_theme()

c2a.Aandeel_ZS_VL
f_save_graph(c2a.Aandeel_ZS_VL)


c3a.Aandeel_Sigma_VL <- ggplot(Tot_Telling_summary_2, aes(x = Telseizoen_num, y = mean_Aandeel_Sigma_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0, 0.1), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in Sigmagebieden tov Vlaanderen") + 
  theme_bw() +
  f_graph_theme()

c3a.Aandeel_Sigma_VL
f_save_graph(c3a.Aandeel_Sigma_VL)

#################################
# 6.3 Totalen per gebiedsklasse
#################################
Tot_Telling_Gebiedsklassen <- Tellingen_ZS %>% 
  filter(Groepscode != "S") %>%  
  group_by(Gebiedsklasse, Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal)) 

Tot_Telling_Gebiedsklassen_summary <- Tot_Telling_Gebiedsklassen %>%
  group_by(Gebiedsklasse, Telseizoen_num) %>% 
  summarise(Wintermean = mean(aantal)) 
Tot_Telling_Gebiedsklassen_summary

b6a.Totaal_ZS_Gebiedsklassen <- ggplot() + 
  geom_point(data = Tot_Telling_Gebiedsklassen_summary, aes(x = Telseizoen_num, y = Wintermean, col = Gebiedsklasse), size = 0.5) + 
  geom_smooth(data = Tot_Telling_Gebiedsklassen_summary, aes(x = Telseizoen_num, y = Wintermean, col = Gebiedsklasse, fill = Gebiedsklasse), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number) + 
  f_labs_totaal("Zeeschelde-vallei: wintergemiddelden") + 
  theme_bw() +
  f_graph_theme()

b6a.Totaal_ZS_Gebiedsklassen
f_save_graph(b6a.Totaal_ZS_Gebiedsklassen)
