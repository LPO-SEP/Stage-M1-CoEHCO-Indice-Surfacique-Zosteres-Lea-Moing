# Charger le package nécessaire
library(sf)

# Définir le chemin du shapefile
shapefile_path <- "DONNEES/ZONES_ZOSTERES/ZONAGE/BOURCEFRANC_8.shp"

# Charger le shapefile
herbiers <- st_read(shapefile_path)

# Définir le nombre total de points aléatoires que vous souhaitez générer
nombre_de_points_total <- 16
distance_minimale <- 200  # distance minimale en mètres

# Fonction pour calculer la distance euclidienne entre deux points
distance_euclidienne <- function(point1, point2) {
  sqrt((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2)
}

# Fonction pour générer des points aléatoires dans les limites des polygones avec une distance minimale entre eux
generate_random_points <- function(polygons, num_points, min_distance) {
  points <- list() # Liste pour stocker les points générés
  
  while (length(points) < num_points) {
    # Sélectionner un polygone aléatoire
    polygon <- polygons[sample(1:nrow(polygons), 1), ]
    # Obtenir la boîte englobante (bounding box) du polygone
    bbox <- st_bbox(polygon)
    # Générer un point aléatoire à l'intérieur de la boîte englobante
    random_point <- st_point(c(runif(1, bbox[1], bbox[3]), runif(1, bbox[2], bbox[4])))
    # Convertir le point en objet spatial
    random_point_sfc <- st_sfc(random_point, crs = st_crs(polygons))
    
    # Vérifier si le point est contenu dans le polygone
    if (st_contains(polygon, random_point_sfc, sparse = FALSE)) {
      coords <- st_coordinates(random_point_sfc)
      too_close <- FALSE
      
      # Vérifier la distance avec les points existants
      for (existing_point in points) {
        existing_coords <- st_coordinates(existing_point)
        if (distance_euclidienne(coords, existing_coords) < min_distance) {
          too_close <- TRUE
          break
        }
      }
      
      # Si le point est suffisamment éloigné de tous les autres points, l'ajouter à la liste
      if (!too_close) {
        points <- append(points, list(random_point))
      }
    }
  }
  # Retourner les points générés comme un objet spatial
  return(st_sfc(points, crs = st_crs(polygons)))
}

# Générer exactement 16 points aléatoires au total avec un espacement minimal de 200 mètres
all_points <- generate_random_points(herbiers, nombre_de_points_total, distance_minimale)

# Convertir les points en un objet sf
points_sf <- st_sf(geometry = all_points, crs = st_crs(herbiers))

# Ajouter un champ ID pour identifier les points
points_sf$id <- 1:nrow(points_sf)

# Définir le chemin du nouveau shapefile
output_shapefile_path <- "BOURCEFRANC_points"

# Sauvegarder les points générés dans un nouveau shapefile avec le bon driver spécifié
st_write(points_sf, dsn = output_shapefile_path, driver = "ESRI Shapefile")

