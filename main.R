# Récupération du jeu de données
dataset <- readxl::read_xlsx("dataset_elections.xlsx")
summary(dataset)

# Récupération des données départementales
fra <- readRDS("Data/gadm36_FRA_2_sf.rds")
mtq <- readRDS("Data/gadm36_MTQ_0_sf.rds")
glp <- readRDS("Data/gadm36_GLP_0_sf.rds")
reu <- readRDS("Data/gadm36_REU_0_sf.rds")
myt <- readRDS("Data/gadm36_MYT_0_sf.rds")
guy <- readRDS("Data/gadm36_GUF_0_sf.rds")

# Modifier le nom des départements d'outre-mer (en français)
guy$NAME_0 <- "Guyane"
reu$NAME_0 <- "La Réunion"
