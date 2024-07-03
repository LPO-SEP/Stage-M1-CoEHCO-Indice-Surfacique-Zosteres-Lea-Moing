# Charger les bibliothèques
library(raster)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(clustMixType)  # Pour la classification K-means
library(viridis)

# 1. Charger et traiter les données NDVI
ndvi_raster <- raster("DONNEES/NDVI/NDVI_OLERON.tif")
summary(ndvi_raster)
# Afficher les valeurs NDVI
plot(ndvi_raster)
hist(ndvi_raster, col ='seagreen3', border = FALSE)

# 2. Classification K-means sur les valeurs NDVI
ndvi_values <- getValues(ndvi_raster)
ndvi_values_non_na <- na.omit(ndvi_values)
kmeans_result <- kmeans(ndvi_values_non_na, centers=7)

# Créer un vecteur pour stocker les classes NDVI
ndvi_classified <- rep(NA, length(ndvi_values))
ndvi_classified[!is.na(ndvi_values)] <- kmeans_result$cluster

# Créer un raster avec les classes NDVI
ndvi_classified_raster <- setValues(ndvi_raster, ndvi_classified)

# Afficher les classes NDVI
plot(ndvi_classified_raster)

# 3. Importer la couche shapefile pour définir la zone d'intérêt
zone_interet <- st_read("DONNEES/ZONES_ZOSTERES/ZONAGE/OLERON_VIADUC_6.shp")

# 4. Créer une grille sur la parcelle
extent_ndvi <- extent(ndvi_raster)
grid_size <- 250  # Taille de la cellule de la grille en mètres
grille <- raster(extent_ndvi, ncol=ceiling((extent_ndvi[2] - extent_ndvi[1]) / grid_size), nrow=ceiling((extent_ndvi[4] - extent_ndvi[3]) / grid_size))
grille <- setValues(grille, 1:ncell(grille))
grille_sp <- rasterToPolygons(grille, dissolve=TRUE)
plot(grille_sp)


# Calculer les centroids
centroids <- coordinates(grille_sp)

# 5. Comparer la distribution des NDVI avec les points de prélèvement
# Utiliser la fonction extract pour obtenir les valeurs NDVI aux points échantillonnés
sampled_ndvi <- extract(ndvi_raster, centroids)

# Test de Kolmogorov-Smirnov
ks_test <- ks.test(ndvi_values_non_na, sampled_ndvi)
print(ks_test)

# Estimer la taille d'échantillon optimal
sample_size <- function(ndvi_values_non_na, alpha=0.05, var_boot_ndvi) {
  # Calculer la taille d'échantillon minimal pour détecter une différence significative
  n_min <- (var_boot_ndvi * (1 + (1/alpha))) / (ndvi^2)
  return(n_min)
}


# 6. Exporter les points de prélèvement au format GPX
sampled_points_sf <- st_as_sf(grille_sp) %>% 
  st_set_crs(32630) %>% 
  st_centroid()

st_write(sampled_points_sf, "sampled_points.gpx", driver = "GPX", overwrite = TRUE)

# Afficher la grille et les points de prélèvement sur la carte des classes NDVI
plot(ndvi_classified_raster, main="Classes NDVI et points de prélèvement")
plot(grille_sp, add=TRUE, border="black")
points(centroids, col="red", pch=20)

# Visualisation de la fonction de répartition cumulée
ecdf_ndvi <- ecdf(ndvi_values_non_na)
ecdf_sampled <- ecdf(sampled_ndvi)

plot(ecdf_ndvi, main="Fonction de répartition cumulée des NDVI", xlab="NDVI", ylab="Fréquence cumulée", col="blue")
lines(ecdf_sampled, col="red")
legend("bottomright", legend=c("Pixels NDVI", "Points de prélèvement"), col=c("blue", "red"), lty=1)
summary(ecdf_sampled)
# Comparaison avec la méthode aléatoire
set.seed(123)  # Pour reproductibilité
random_points <- spsample(as(extent_ndvi, "SpatialPolygons"), n=length(centroids), type="random")
random_ndvi <- extract(ndvi_raster, random_points)

ecdf_random <- ecdf(random_ndvi)

lines(ecdf_random, col="green")
legend("bottomright", legend=c("Pixels NDVI", "Points de prélèvement", "Points aléatoires"), col=c("blue", "red", "green"), lty=1)


