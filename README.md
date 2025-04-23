---
editor_options: 
  markdown: 
    wrap: 72
---

# 📦 Fonctions de géomatique agricole

Ce package R regroupe une collection de fonctions utilitaires pour
l’analyse spatiale appliquée à l’**agriculture de précision au Québec**.

Il inclut des outils pour :

-   le traitement de données LiDAR, Sentinel-2, et raster agricoles ;

-   la génération de grilles orientées, rectangles, et réseaux de lignes
    ;

-   la création de fichiers de nivellement compatibles ;

-   la segmentation spatiale en zones de gestion .

## 🧩 Principales fonctionnalités

| Fonction                     | Description                                           |
|---------------------|---------------------------------------------------|
| `st_utm()`                   | Reprojetter dans la zone UTM adéquate                 |
| `aligner_rasters()`          | Aligner, lisser et normaliser une liste de rasters    |
| `lidar()`                    | Télécharger et recadrer les données LiDAR             |
| `indice_humidite()`          | Calculer le TWI, les dépressions et l'accumulation    |
| `creer_rectangles()`         | Créer des rectangles orientés à partir de points      |
| `st_grille()`                | Générer une grille orientée (option quinconce)        |
| `reseau_lignes_paralleles()` | Générer un réseau de lignes parallèles selon un angle |
| `raccourcir_lignes()`        | Raccourcir des lignes aux extrémités                  |
| `topo_lidar()`               | Générer un fichier de nivellement avec le LiDAR       |
| `sentinel2()`                | Extraire des indices Sentinel-2 (NDVI, EVI, etc.)     |
| `sf_multiplane()`            | Exporter des points vers un fichier MultiPlane        |
| `multiplane_sf()`            | Lire un fichier MultiPlane et le convertir en `sf`    |
| `zones()`                    | Grouper des superpixels en zones avec SKATER          |

## 🔧 Installation

``` r
# install.packages("devtools")
devtools::install_github("cedricbouffard/rgeoag")
```

## 🧪 Exemple

``` r
library(rgeoag)

champ <- sf::st_read("champ.shp") |> st_utm()
grille <- st_grille(champ, largeur = 10, longueur = 5)
plot(sf::st_geometry(grille))
```

## 🌱 Contexte

Ce package est conçu pour faciliter les analyses en **géomatique
agricole au Québec**, notamment dans :

-   la modélisation agro-environnementale ;

-   la gestion de la variabilité intra-champ ;

-   la création de recommandations géospatiales (fertilisation, semis,
    drainage, etc.) ;

-   l’intégration des données Sentinel, LiDAR, et MultiPlane.
