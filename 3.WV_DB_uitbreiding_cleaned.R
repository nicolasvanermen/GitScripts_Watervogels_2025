library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")

### functies opvragen
source("./Scripts/Functies watervogel grafieken.R")

#################
# 1 DATA INLEZEN
#################
WV_DB <- read.csv("./Data/WV_DB_2025-01-23.csv")
Gebieden_ZS <- read_csv("./Data/Gebieden2 herwerking Wim.csv")

### selecteren van de tellingen in de relevante telgebieden
Tellingen_ZS <- WV_DB %>% right_join(Gebieden_ZS, by = c("Gebied" = "Gebied"))

### gebiedsklasse definiëren
Tellingen_ZS <-  Tellingen_ZS %>%
  mutate(Gebiedsklasse = ifelse(Sigma == 1, "Sigmagebied",
                                ifelse(Vallei == 1, "Valleigebied",
                                       if_else(Estuarien == 1, "Estuarium",
                                               if_else(NOHaven == 1, "Haven", NA)))))

### selecteren periode 1999/00 -> 2023/24; meeuwen worden niet betrokken in de analyse
Tellingen_ZS <- Tellingen_ZS %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4))) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1998 & Telseizoen_num < 2024)

########################################################
# 2 Soortensamenstelling in Sigmagebieden & Zeeschelde
########################################################
Species10 <- Tellingen_ZS %>% group_by(NedNaam) %>% 
  summarise(TOT = sum(Aantal)) %>% 
  arrange(desc(TOT)) %>% 
  head(n = 10) %>% pull(NedNaam)
Species10 <- as.factor(Species10)
levels(Species10)

### wintergemiddelden per soort worden berekend door te sommeren per soort en te delen door 6, om geen overschatting te krijgen 
### (want nultellingen zijn niet mee opgenomen in de databank!!); 
### deze werkwijze komt overeen met het aan nul gelijkstellen van zowel nultellingen als ontbrekende tellingen 
Winter_Means_Sigma <- Tellingen_ZS %>% 
  filter(Gebiedsklasse == "Sigmagebied") %>%  
  mutate(NedNaam2 = ifelse(NedNaam %in% Species10, NedNaam, "Andere"),
         NedNaam2 = ifelse(NedNaam2 == "Grote Canadese Gans", "Grote \nCanadese Gans", NedNaam2)) %>%
  group_by(Telseizoen_num, NedNaam2) %>% 
  summarise(Mean = sum(Aantal)/6) %>%
  ungroup()

nrow(Winter_Means_Sigma) == 25*11

d1.Sigma_Wintergemiddelden_per_soort <- ggplot(Winter_Means_Sigma, aes(Telseizoen_num, Mean, fill = NedNaam2)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(name = "", palette = "Spectral") +
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
  scale_y_continuous(labels = scales::number) +
  labs(title = "Sigmagebieden: wintergemiddelden", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.text = element_text(size = 7)) +
  f_graph_theme() 

d1.Sigma_Wintergemiddelden_per_soort
f_save_wide_graph(d1.Sigma_Wintergemiddelden_per_soort)

Winter_Means_ZS <- Tellingen_ZS %>%  
  mutate(NedNaam2 = ifelse(NedNaam %in% Species10, NedNaam, "Andere"),
         NedNaam2 = ifelse(NedNaam2 == "Grote Canadese Gans", "Grote \nCanadese Gans", NedNaam2)) %>%
  group_by(Telseizoen_num, NedNaam2) %>% 
  summarise(Mean = sum(Aantal)/6) %>%
  ungroup()

d3.ZS_Wintergemiddelden_per_soort <- ggplot(Winter_Means_ZS, aes(Telseizoen_num, Mean, fill = NedNaam2)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(name = "", palette = "Spectral") +
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
  scale_y_continuous(labels = scales::number) +
  labs(title = "Zeeschelde-vallei: wintergemiddelden", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.text = element_text(size = 7)) +
  f_graph_theme() 

d3.ZS_Wintergemiddelden_per_soort
f_save_wide_graph(d3.ZS_Wintergemiddelden_per_soort)

########################################################
# 3 Aantal watervogels in Sigmagebieden, per cluster
########################################################
Tellingen_ZS %>% distinct(Cluster)
Tellingen_ZS %>% filter(Gebiedsklasse == "Sigmagebied") %>% distinct(Cluster)
Tellingen_ZS %>% filter(Gebiedsklasse == "Sigmagebied") %>% group_by(Cluster) %>% summarise(n = n())

### clustervolgorde aanpassen
Tellingen_ZS$Cluster <- factor(Tellingen_ZS$Cluster,
                               levels = c("Burchtse Weel","Dijlemonding","Durme","Kalkense Meersen","KBR","Noordelijk Gebied","Wal-Zwijn","Overige"))
levels(Tellingen_ZS$Cluster)

Winter_Means_Sigma_cluster <- Tellingen_ZS %>% 
  filter(Gebiedsklasse == "Sigmagebied") %>%
  group_by(Telseizoen_num, Cluster) %>% 
  summarise(Mean = sum(Aantal)/6) %>%
  ungroup()

d2.Sigma_Wintergemiddelden_per_cluster <- ggplot(Winter_Means_Sigma_cluster, aes(Telseizoen_num, Mean, fill = Cluster)) + 
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(name = "", palette = "Spectral") +
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
  scale_y_continuous(labels = scales::number) +
  labs(title = "Sigmagebieden: wintergemiddelden", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.text = element_text(size = 7)) +
  f_graph_theme() 

d2.Sigma_Wintergemiddelden_per_cluster
f_save_wide_graph(d2.Sigma_Wintergemiddelden_per_cluster)
