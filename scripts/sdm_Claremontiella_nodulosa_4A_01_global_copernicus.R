# regroupement de modèles différents selon les une hiérarchie :
# habitat > moodèle sextant local > modèle copernicus global
# Pour cela :
# (1) Faire plusieurs runs (5) avec un algorithme pour chaque type de modèle
# ok
# (2) Les pondérer par weighted means puis committee averaging
# ok
# (3) Pondérer les raster de sorties par weighted means et committee averaging
#   (a) récupérer les rasters de sortie
#   (b) appliquer le weighted mean sur les rasters de sorties
# (4) Transformer le raster obtenu en probabilité de présence par un seuil
# (5) réaliser cela pour RF; MAXENT puis un modèle d'ensemble.
