# Chargement des packages nécessaires
library(tidyverse)
library(stringr)
library(stringi)

# Récupération du jeu de données
dataset <- readxl::read_xlsx("dataset_elections.xlsx")
summary(dataset)
str(dataset)
# Récupération de la carte de France
map <- map_data("france")

# Nettoyage des données et aggrégation des résultats
dataset <- dataset %>%
  group_by(`Libellé du département`) %>%
  summarise(total_votes = sum(Exprimés), 
            total_blancs = sum(Blancs),
            pourcentage_blanc = total_blancs / sum(Votants) * 100, 
            total_abs = sum(as.numeric(Abstentions)), 
            pourcentage_abs = total_abs / sum(as.numeric(Inscrits))* 100,
            total_arthaud = sum(Voix), 
            pourcentage_arthaud = total_arthaud / total_votes * 100) 

# Changement de noms de départements
names(dataset)[1] <- "region"
dataset$region <- stri_trans_general(dataset$region, "Latin-ASCII") %>%
  str_replace_all("Cote-d'Or", "Cote-Dor") %>%
  str_replace_all("Cotes-d'Armor", "Cotes-Darmor") %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud") %>%
  str_replace_all("Val-d'Oise", "Val-Doise") %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud")

map_theme <- theme(title=element_text(),
                   plot.title=element_text(margin=margin(20,20,20,20), size=18, hjust = 0.5),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.grid.major= element_blank(), 
                   panel.background= element_blank()) 

result_map <- left_join(x = map[,-6], y = dataset)
# Carte de test
ggplot(result_map, aes(long,lat, group = group, fill = pourcentage_arthaud)) +
  geom_polygon() +
  coord_map() +
  scale_fill_gradientn(colours = c("yellow","red"), name = "Pourcentage N. Arthaud") +
  labs(x = "", 
       y = "", 
       title = "Résultats de N. Arthaud au premier tour des présidentielles 2022", 
       subtitle = "Données via data.gouv") +
  map_theme 
