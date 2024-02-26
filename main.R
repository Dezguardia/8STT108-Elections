library(tidyverse)
library(stringr)
library(stringi)
library(readxl)
library(dplyr)

# Récupération des résultats
dataset <- read_xlsx("dataset_elections.xlsx")

# Nettoyage des données et aggrégation des résultats
dataset_cleaned <- dataset %>%
  group_by(`Libellé du département`) %>%
  summarise(total_votes = sum(Exprimés), 
            total_blancs = sum(Blancs),
            pourcentage_blanc = total_blancs / sum(Votants) * 100, 
            total_abs = sum(as.numeric(Abstentions)), 
            pourcentage_abs = total_abs / sum(as.numeric(Inscrits))* 100,
            total_arthaud = sum(VoixArthaud), 
            pourcentage_arthaud = total_arthaud / total_votes * 100,
            total_roussel = sum(VoixRoussel),
            pourcentage_roussel = total_roussel / total_votes * 100,
            total_macron = sum(VoixMacron),
            pourcentage_macron = total_macron / total_votes * 100,
            total_lassalle = sum(VoixLassalle),
            pourcentage_lassalle = total_lassalle / total_votes * 100,
            total_lepen = sum(VoixLePen),
            pourcentage_lepen = total_lepen / total_votes * 100,
            total_zemmour = sum(VoixZemmour),
            pourcentage_zemmour = total_zemmour / total_votes * 100,
            total_melenchon = sum(VoixMelenchon),
            pourcentage_melenchon = total_melenchon / total_votes * 100,
            total_hidalgo = sum(VoixHidalgo),
            pourcentage_hidalgo = total_hidalgo / total_votes * 100,
            total_jadot = sum(VoixJadot),
            pourcentage_jadot = total_jadot / total_votes * 100,
            total_pecresse = sum(VoixPecresse),
            pourcentage_pecresse = total_pecresse / total_votes * 100,
            total_poutou = sum(VoixPoutou),
            pourcentage_poutou = total_poutou / total_votes * 100,
            total_dupontaignan = sum(VoixDupontAignan),
            pourcentage_dupontaignan = total_dupontaignan / total_votes * 100) 

#Moyenne des données
moy_votes = mean(dataset_cleaned$total_votes)
moy_abs = mean(dataset_cleaned$total_abs)
moy_blanc = mean(dataset_cleaned$total_blancs)
moy_votes_candidat <- c("N. Arthaud" = mean(dataset_cleaned$total_arthaud), 
                       "P. Poutou" = mean(dataset_cleaned$total_poutou),
                       "F. Roussel" = mean(dataset_cleaned$total_roussel),
                       "J-L. Mélenchon" = mean(dataset_cleaned$total_melenchon),
                       "A. Hidalgo" = mean(dataset_cleaned$total_hidalgo),
                       "Y. Jadot" = mean(dataset_cleaned$total_jadot),
                       "E. Macron" = mean(dataset_cleaned$total_macron), 
                       "J. Lassalle" = mean(dataset_cleaned$total_lassalle),
                       "V. Pecresse" = mean(dataset_cleaned$total_pecresse),
                       "M. Le Pen" = mean(dataset_cleaned$total_lepen),
                       "N. Dupont-Aignan" = mean(dataset_cleaned$total_dupontaignan),
                       "É. Zemmour" = mean(dataset_cleaned$total_zemmour))

print(moy_votes_candidat)

etendue = max(moy_votes_candidat) - min(moy_votes_candidat)
print(max(moy_votes_candidat))
print(min(moy_votes_candidat))
print(etendue)

#en moyenne par region on a eu environs 328 345 votants
#en moyenne par region on a eu 119 852 d'abstinence
#en moyenne par region on a eu environs 5080 vote blanc
#l'etendue des votes entre les candidat est d'environs 89588.45

# Changement de noms de départements
names(dataset_cleaned)[1] <- "region"
dataset_cleaned$region <- stri_trans_general(dataset_cleaned$region, "Latin-ASCII") %>%
  str_replace_all("Cote-d'Or", "Cote-Dor") %>%
  str_replace_all("Cotes-d'Armor", "Cotes-Darmor") %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud") %>%
  str_replace_all("Val-d'Oise", "Val-Doise") %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud")

# Récupération de la carte de France
map <- map_data("france")

# Filtrage des départements pour ne garder que la France métropolitaine
dataset_cleaned_filtered <- dataset_cleaned %>%
  filter(region %in% map$region)

# Fusion avec les données de la carte
result_map <- left_join(x = map[,-6], y = dataset_cleaned_filtered)

# On cherche ici le candidat en tête dans chaque département
result_map <- result_map %>%
  rowwise() %>%
  mutate(candidat_gagnant = case_when(
    total_arthaud == max(total_arthaud, total_roussel, total_macron, 
                         total_lassalle, total_lepen, total_zemmour, 
                         total_melenchon, total_hidalgo, total_jadot, 
                         total_pecresse, total_poutou, total_dupontaignan) ~ "N. Arthaud",
    total_roussel == max(total_arthaud, total_roussel, total_macron, 
                         total_lassalle, total_lepen, total_zemmour, 
                         total_melenchon, total_hidalgo, total_jadot, 
                         total_pecresse, total_poutou, total_dupontaignan) ~ "F. Roussel",
    total_macron == max(total_arthaud, total_roussel, total_macron, 
                        total_lassalle, total_lepen, total_zemmour, 
                        total_melenchon, total_hidalgo, total_jadot, 
                        total_pecresse, total_poutou, total_dupontaignan) ~ "E. Macron",
    total_lassalle == max(total_arthaud, total_roussel, total_macron, 
                          total_lassalle, total_lepen, total_zemmour, 
                          total_melenchon, total_hidalgo, total_jadot, 
                          total_pecresse, total_poutou, total_dupontaignan) ~ "J. Lassalle",
    total_lepen == max(total_arthaud, total_roussel, total_macron, 
                       total_lassalle, total_lepen, total_zemmour, 
                       total_melenchon, total_hidalgo, total_jadot, 
                       total_pecresse, total_poutou, total_dupontaignan) ~ "M. Le Pen",
    total_zemmour == max(total_arthaud, total_roussel, total_macron, 
                         total_lassalle, total_lepen, total_zemmour, 
                         total_melenchon, total_hidalgo, total_jadot, 
                         total_pecresse, total_poutou, total_dupontaignan) ~ "É. Zemmour",
    total_melenchon == max(total_arthaud, total_roussel, total_macron, 
                           total_lassalle, total_lepen, total_zemmour, 
                           total_melenchon, total_hidalgo, total_jadot, 
                           total_pecresse, total_poutou, total_dupontaignan) ~ "J-L. Mélenchon",
    total_hidalgo == max(total_arthaud, total_roussel, total_macron, 
                         total_lassalle, total_lepen, total_zemmour, 
                         total_melenchon, total_hidalgo, total_jadot, 
                         total_pecresse, total_poutou, total_dupontaignan) ~ "A. Hidalgo",
    total_jadot == max(total_arthaud, total_roussel, total_macron, 
                       total_lassalle, total_lepen, total_zemmour, 
                       total_melenchon, total_hidalgo, total_jadot, 
                       total_pecresse, total_poutou, total_dupontaignan) ~ "Y. Jadot",
    total_pecresse == max(total_arthaud, total_roussel, total_macron, 
                          total_lassalle, total_lepen, total_zemmour, 
                          total_melenchon, total_hidalgo, total_jadot, 
                          total_pecresse, total_poutou, total_dupontaignan) ~ "V. Pécresse",
    total_poutou == max(total_arthaud, total_roussel, total_macron, 
                        total_lassalle, total_lepen, total_zemmour, 
                        total_melenchon, total_hidalgo, total_jadot, 
                        total_pecresse, total_poutou, total_dupontaignan) ~ "P. Poutou",
    total_dupontaignan == max(total_arthaud, total_roussel, total_macron, 
                              total_lassalle, total_lepen, total_zemmour, 
                              total_melenchon, total_hidalgo, total_jadot, 
                              total_pecresse, total_poutou, total_dupontaignan) ~ "N. Dupont-Aignan",
    TRUE ~ NA_character_
  ))

# Couleur pour les candidats
candidate_colors <- c("N. Arthaud" = "red4", 
                      "P. Poutou" = "red3",
                      "F. Roussel" = "red2",
                      "J-L. Mélenchon" = "tomato2",
                      "A. Hidalgo" = "salmon2",
                      "Y. Jadot" = "springgreen4",
                      "E. Macron" = "goldenrod1", 
                      "J. Lassalle" = "lightblue",
                      "V. Pecresse" = "royalblue1",
                      "M. Le Pen" = "blue2",
                      "N. Dupont-Aignan" = "blue3",
                      "É. Zemmour" = "navyblue")

# Apparence de la carte
map_theme <- theme(title=element_text(),
                   plot.title=element_text(margin=margin(20,20,20,20), size=18, hjust = 0.5),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.grid.major= element_blank(), 
                   panel.background= element_blank()) 

# Création de la carte
ggplot(result_map, aes(long, lat, group = group, fill = candidat_gagnant)) +
  geom_polygon() +
  geom_path(color = "white", size = 0.3) + # On ajoute la frontière entre les départements
  coord_map() +
  scale_fill_manual(values = candidate_colors, name = "Candidat arrivé en tête") +
  labs(x = "", 
       y = "", 
       title = "Candidat arrivé en tête par département au premier tour des présidentielles 2022", 
       subtitle = "Données via data.gouv") +
  map_theme

