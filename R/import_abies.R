# Importation des données
# Date : 2022-11-29
# Note : les données ont déjà été importées pour vous. Vous ne devez pas
# réexécuter ce script ! 

# Configuration de l'environnement
SciViews::R()

# Les données sont importées depuis ZENODO
url <- "https://zenodo.org/record/4943286/files/Abies_bals_allom_data_Peart.csv?download=1"
abies <- read$csv(url, cache_file = "data/data_cache/abies.csv")

# Renommer les variables
abies <- janitor::clean_names(abies)

# Ajout des labels et des unités
abies <- labelise(abies,
  label = list(
    height = "Hauteur",
    diameter = 'Diamètre à 1.37m du sol',
    elevation = "Altitude",
    canopyheight = "Hauteur du plus grand arbre proche"
  ),
  units = list(
    height = "m",
    diameter = "cm",
    elevation = "m",
    canopyheight = "m"
  ))

# Sauvegarde des données localement (commenté pour éviter d'écraser les données
# originales)
#write$rds(abies, "data/abies.rds", compress = "xz")

# Nettoyage de l'environnmeent
rm(abies, url)
