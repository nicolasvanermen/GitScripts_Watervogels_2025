library(tidyverse)
library(INBOtheme)
# theme_set(theme_inbo())
conflicted::conflict_prefer("filter", "dplyr")

### functies opvragen
source("./GitScripts/Functies_WV_grafieken.R")

##################
# 1 DATA INLEZEN
##################

WV_DB <- read_csv("./Data/WV_DB_2025-01-23.csv")
WV_DB <- WV_DB %>% rename(LocationWVNaam = Gebied) 

Gebieden_ZS <- read_csv("./Data/Gebieden_ZS_2025-03-27.csv")
Clusters_ZS <- read_csv("./Data/Clusters_ZS_2025-03-27.csv")

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

### selecteren periode 1991/92 -> 2023/24;
Tellingen_ZS <- Tellingen_ZS %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4))) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1990 & Telseizoen_num < 2024)

### tellingen binnen de clusters
Tellingen_clusters_ZS <- right_join(Tellingen_ZS, 
                                    Clusters_ZS %>% 
                                      select(LocationWVNaam, LocationGroupNaam), by = "LocationWVNaam")

### zelfde selectie voor volledige WV_DB:
WV_DB <- WV_DB %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4))) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1990 & Telseizoen_num < 2024)
         # ProjectCode %in% c("MIDMA","ZSCH"))

### check:
nrow(WV_DB) == 875963
nrow(Tellingen_ZS) == 234277

### 12 meest algemene soorten
Species12 <- Tellingen_ZS %>% group_by(NedNaam) %>% 
  summarise(TOT = sum(Aantal)) %>% 
  arrange(desc(TOT)) %>% 
  head(n = 12) %>% pull(NedNaam)
Species12 <- as.factor(Species12) 
levels(Species12)

### clustervolgorde aanpassen
Tellingen_clusters_ZS %>% distinct(LocationGroupNaam.y)
Tellingen_clusters_ZS <- Tellingen_clusters_ZS %>% rename(Cluster = LocationGroupNaam.y)
Tellingen_clusters_ZS %>% count(Cluster)

Tellingen_clusters_ZS <- Tellingen_clusters_ZS %>% 
  mutate(Cluster = as.factor(case_when(str_detect(Cluster, "Kruibeke") ~ "KBR", .default = Cluster)))
Tellingen_clusters_ZS %>% count(Cluster)

Tellingen_clusters_ZS$Cluster <- factor(Tellingen_clusters_ZS$Cluster,
                                        levels = c("Burchtse Weel","Dijlemonding","Durme","Kalkense Meersen",
                                                   "KBR","Noordelijk Gebied","Wal-Zwijn","Overige"))


##########################################
# 2a Grafieken per soort (zonder Kievit)
##########################################
for (i in Species12[!Species12 %in% c("Kievit")])
{
  Soort <- i
  OutputLocationName <- paste("./Graphs/Grafieken ", Soort, sep = "")
  
  if (!file.exists(OutputLocationName)) {
    dir.create(OutputLocationName) }
  
  ### soortselectie maken
  Tellingen_VL_Soort <- WV_DB %>% filter(NedNaam == Soort)
  Tellingen_ZS_Soort <- Tellingen_ZS %>% filter(NedNaam == Soort)
  Tellingen_ZS_Cluster_Soort <- Tellingen_clusters_ZS %>% filter(NedNaam == Soort)
  
  ### wintergemiddelden per deelgebied
  Winter_Means_Sigma <- Tellingen_ZS_Soort %>% 
    filter(Gebiedsklasse == "Sigmagebied") %>%
    group_by(Telseizoen_num) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  Winter_Means_ZS <- Tellingen_ZS_Soort %>%
    group_by(Telseizoen_num) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  Winter_Means_VL <- Tellingen_VL_Soort %>%
    group_by(Telseizoen_num) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  ### alles samenzetten:
  Winter_Means <- left_join(left_join(Winter_Means_Sigma %>% arrange(Telseizoen_num),
                                      Winter_Means_ZS %>% arrange(Telseizoen_num), by = "Telseizoen_num"), 
                            Winter_Means_VL %>% arrange(Telseizoen_num), by = "Telseizoen_num") %>%
    rename(Mean_Sigma = Mean.x,
           Mean_ZS = Mean.y,
           Mean_VL = Mean) %>%
    mutate(Aandeel_Sigma_ZS = Mean_Sigma / Mean_ZS,
           Aandeel_Sigma_VL = Mean_Sigma / Mean_VL)
  
  head(Winter_Means)
  
  ### eerste 4 grafieken maken en exporteren
  e1.Wintergemiddelden_Sigma <- ggplot(Winter_Means, aes(Telseizoen_num, Mean_Sigma)) + 
    geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::number) +
    labs(title = paste(Soort, " in Sigmagebieden: wintergemiddelden", sep = ""), y = "Aantal", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  
  e1.Wintergemiddelden_Sigma
  f_save_graph_species(e1.Wintergemiddelden_Sigma)
  
  e2.Wintergemiddelden_ZS <- ggplot(Winter_Means, aes(Telseizoen_num, Mean_ZS)) + 
    geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::number) +
    labs(title = paste(Soort, " in Zeeschelde-vallei: wintergemiddelden", sep = ""), y = "Aantal", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  
  e2.Wintergemiddelden_ZS
  f_save_graph_species(e2.Wintergemiddelden_ZS)
  
  e3.Aandeel_Sigma_ZS <- ggplot(Winter_Means, aes(Telseizoen_num, Aandeel_Sigma_ZS)) + 
    geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    labs(title = paste(Soort, " (Sigmagebieden tov Zeeschelde-vallei)", sep = ""), y = "Percentage", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  
  e3.Aandeel_Sigma_ZS
  f_save_graph_species(e3.Aandeel_Sigma_ZS)
    
  e4.Aandeel_Sigma_VL <- ggplot(Winter_Means, aes(Telseizoen_num, Aandeel_Sigma_VL)) + 
    geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    labs(title = paste(Soort, " (Sigmagebieden tov Vlaanderen)", sep = ""), y = "Percentage", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  
  e4.Aandeel_Sigma_VL
  f_save_graph_species(e4.Aandeel_Sigma_VL)
  
  ### wintergemiddelden per deelgebied binnen Zeeschelde-vallei
  Winter_Means_Sigma_Gebied <- Tellingen_ZS_Soort %>% 
    group_by(Telseizoen_num, Gebiedsklasse) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  e5.ZS_Wintergemiddelden <- ggplot() + 
    geom_point(data = Winter_Means_Sigma_Gebied, aes(Telseizoen_num, Mean, col = Gebiedsklasse), size = 0.5) + 
    geom_smooth(data = Winter_Means_Sigma_Gebied, aes(Telseizoen_num, Mean, col = Gebiedsklasse, fill = Gebiedsklasse), linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::number) +
    labs(title = paste(Soort, " in Zeeschelde-vallei: wintergemiddelden", sep = ""), y = "Aantal", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  e5.ZS_Wintergemiddelden
  f_save_wide_graph_species(e5.ZS_Wintergemiddelden)
  
  ### wintergemiddelden per cluster binnen Sigmagebieden
  Winter_Means_Sigma_cluster <- Tellingen_ZS_Cluster_Soort %>% 
    filter(Gebiedsklasse == "Sigmagebied") %>% 
    group_by(Telseizoen_num, Cluster) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  e6.ZS_Wintergemiddelden <- ggplot(Winter_Means_Sigma_cluster, aes(Telseizoen_num, Mean, fill = Cluster)) + 
    geom_bar(stat = 'identity') + 
    scale_fill_brewer(name = "", palette = "Dark2") +
    scale_x_continuous(breaks = seq(from = 1991, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::number) +
    labs(title = paste(Soort, " in Sigmagebieden: wintergemiddelden", sep = ""), y = "Aantal", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  e6.ZS_Wintergemiddelden
  f_save_wide_graph_species(e6.ZS_Wintergemiddelden)
}


##########################################
# 2b Grafieken per soort (Kievit & Wulp)
##########################################
for (i in c("Kievit", "Wulp"))
{
  Soort <- i
  OutputLocationName <- paste("./Graphs/Grafieken ", Soort, sep = "")
  
  if (!file.exists(OutputLocationName)) {
    dir.create(OutputLocationName) }
  
  ### soortselectie maken
  Tellingen_VL_Soort <- WV_DB %>% filter(NedNaam == Soort,
                                         Telseizoen_num > 1998)
  Tellingen_ZS_Soort <- Tellingen_ZS %>% filter(NedNaam == Soort,
                                                Telseizoen_num > 1998)
  Tellingen_ZS_Cluster_Soort <- Tellingen_clusters_ZS %>% filter(NedNaam == Soort,
                                                Telseizoen_num > 1998)
  
  ### wintergemiddelden per deelgebied
  Winter_Means_Sigma <- Tellingen_ZS_Soort %>% 
    filter(Gebiedsklasse == "Sigmagebied") %>%
    group_by(Telseizoen_num) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  Winter_Means_ZS <- Tellingen_ZS_Soort %>%
    group_by(Telseizoen_num) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  Winter_Means_VL <- Tellingen_VL_Soort %>%
    group_by(Telseizoen_num) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  ### alles samenzetten:
  Winter_Means <- left_join(left_join(Winter_Means_Sigma %>% arrange(Telseizoen_num),
                                      Winter_Means_ZS %>% arrange(Telseizoen_num), by = "Telseizoen_num"), 
                            Winter_Means_VL %>% arrange(Telseizoen_num), by = "Telseizoen_num") %>%
    rename(Mean_Sigma = Mean.x,
           Mean_ZS = Mean.y,
           Mean_VL = Mean) %>%
    mutate(Aandeel_Sigma_ZS = Mean_Sigma / Mean_ZS,
           Aandeel_Sigma_VL = Mean_Sigma / Mean_VL)
  
  head(Winter_Means)
  
  ### eerste 4 grafieken maken en exporteren
  e1.Wintergemiddelden_Sigma <- ggplot(Winter_Means, aes(Telseizoen_num, Mean_Sigma)) + 
    geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::number) +
    labs(title = paste(Soort, " in Sigmagebieden: wintergemiddelden", sep = ""), y = "Aantal", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  
  e1.Wintergemiddelden_Sigma
  f_save_graph_species(e1.Wintergemiddelden_Sigma)
  
  e2.Wintergemiddelden_ZS <- ggplot(Winter_Means, aes(Telseizoen_num, Mean_ZS)) + 
    geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::number) +
    labs(title = paste(Soort, " in Zeeschelde-vallei: wintergemiddelden", sep = ""), y = "Aantal", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  
  e2.Wintergemiddelden_ZS
  f_save_graph_species(e2.Wintergemiddelden_ZS)
  
  e3.Aandeel_Sigma_ZS <- ggplot(Winter_Means, aes(Telseizoen_num, Aandeel_Sigma_ZS)) + 
    geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    labs(title = paste(Soort, " (Sigmagebieden tov Zeeschelde-vallei)", sep = ""), y = "Percentage", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  
  e3.Aandeel_Sigma_ZS
  f_save_graph_species(e3.Aandeel_Sigma_ZS)
  
  e4.Aandeel_Sigma_VL <- ggplot(Winter_Means, aes(Telseizoen_num, Aandeel_Sigma_VL)) + 
    geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    labs(title = paste(Soort, " (Sigmagebieden tov Vlaanderen)", sep = ""), y = "Percentage", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  
  e4.Aandeel_Sigma_VL
  f_save_graph_species(e4.Aandeel_Sigma_VL)
  
  ### wintergemiddelden per deelgebied binnen Zeeschelde-vallei
  Winter_Means_Sigma_Gebied <- Tellingen_ZS_Soort %>% 
    group_by(Telseizoen_num, Gebiedsklasse) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  e5.ZS_Wintergemiddelden <- ggplot() + 
    geom_point(data = Winter_Means_Sigma_Gebied, aes(Telseizoen_num, Mean, col = Gebiedsklasse), size = 0.5) + 
    geom_smooth(data = Winter_Means_Sigma_Gebied, aes(Telseizoen_num, Mean, col = Gebiedsklasse, fill = Gebiedsklasse), linewidth = 0.25, alpha = 0.3) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::number) +
    labs(title = paste(Soort, " in Zeeschelde-vallei: wintergemiddelden", sep = ""), y = "Aantal", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  e5.ZS_Wintergemiddelden
  f_save_wide_graph_species(e5.ZS_Wintergemiddelden)
  
  ### wintergemiddelden per cluster binnen Sigmagebieden
  Winter_Means_Sigma_cluster <- Tellingen_ZS_Cluster_Soort %>% 
    filter(Gebiedsklasse == "Sigmagebied") %>% 
    group_by(Telseizoen_num, Cluster) %>% 
    summarise(Mean = sum(Aantal)/6) %>%
    ungroup()
  
  e6.ZS_Wintergemiddelden <- ggplot(Winter_Means_Sigma_cluster, aes(Telseizoen_num, Mean, fill = Cluster)) + 
    geom_bar(stat = 'identity') + 
    scale_fill_brewer(name = "", palette = "Dark2") +
    scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
    scale_y_continuous(labels = scales::number) +
    labs(title = paste(Soort, " in Sigmagebieden: wintergemiddelden", sep = ""), y = "Aantal", x = element_blank()) +
    theme_bw() +
    theme(legend.text = element_text(size = 7)) +
    f_graph_theme() 
  e6.ZS_Wintergemiddelden
  f_save_wide_graph_species(e6.ZS_Wintergemiddelden)
}
