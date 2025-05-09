% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topo_lidar.R
\name{topo_lidar}
\alias{topo_lidar}
\title{Générer un fichier pour les logiciels de nivellement à partir d’un polygone et du LiDAR}
\usage{
topo_lidar(
  polygone,
  fichier_sortie,
  raster_lidar = NULL,
  imperial = FALSE,
  bm = NULL,
  bm_elev = NULL,
  densite_bordure = 3,
  largeur = 7,
  longueur = 3,
  multiplane = FALSE
)
}
\arguments{
\item{polygone}{Un objet `sf` de type POLYGON (doit contenir un seul polygone)}

\item{fichier_sortie}{Chemin du fichier texte de sortie}

\item{raster_lidar}{Un objet `SpatRaster` (optionnel). Si NULL, la fonction `lidar()` est utilisée.}

\item{imperial}{Logique : TRUE pour exporter les distances en pieds}

\item{bm}{Objet `sf` optionnel : point de référence (benchmark)}

\item{bm_elev}{Altitude du point de référence (optionnelle)}

\item{densite_bordure}{Distance entre les points sur la bordure (en mètres)}

\item{largeur}{Largeur des cellules de la grille intérieure (en mètres)}

\item{longueur}{Longueur des cellules de la grille intérieure (en mètres)}

\item{multiplane}{Logique : TRUE pour générer un fichier MultiPlane (utilise `sf_multiplane`)}
}
\value{
Un `data.frame` avec les points exportés, écrit dans un fichier texte
}
\description{
Cette fonction extrait les points de bordure et une grille intérieure depuis un polygone,
interroge un raster LiDAR pour obtenir les altitudes, puis exporte les données en format texte
compatible avec les logiciels de nivellement.
}
