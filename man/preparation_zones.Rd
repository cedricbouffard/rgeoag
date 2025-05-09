% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preparation_zone.R
\name{preparation_zones}
\alias{preparation_zones}
\title{Préparer les superpixels et données pour les zones de gestion}
\usage{
preparation_zones(
  raster_liste,
  k_supercell = 1000,
  fenetre_focale = 3,
  normaliser = TRUE,
  methode_resample = "bilinear"
)
}
\arguments{
\item{raster_liste}{Liste de `SpatRaster`}

\item{k_supercell}{Nombre de superpixels à générer}

\item{fenetre_focale}{Taille du filtre médian (>1 applique un lissage)}

\item{normaliser}{Logique. Si TRUE, normalise les rasters entre 0 et 1}

\item{methode_resample}{Méthode de rééchantillonnage (ex : "bilinear", "near")}
}
\value{
Une liste contenant :
\describe{
  \item{s_polygone}{Les superpixels (`sf`)}
  \item{poids}{Les poids spatiaux (`rgeoda`)}
  \item{donnees}{Données tabulaires sans géométrie}
}
}
\description{
Cette fonction aligne une liste de rasters, applique un lissage optionnel,
crée des superpixels (supercells), calcule les poids spatiaux et génère un
tableau de données avec les superficies.
}
