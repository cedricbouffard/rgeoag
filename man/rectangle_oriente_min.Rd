% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rectangle_oriente_min.R
\name{rectangle_oriente_min}
\alias{rectangle_oriente_min}
\title{Calculer le rectangle orienté minimal autour d'un polygone}
\usage{
rectangle_oriente_min(polygone)
}
\arguments{
\item{polygone}{Un objet `sf` de type POLYGON ou MULTIPOLYGON}
}
\value{
Une liste contenant :
\describe{
  \item{rectangle}{Le rectangle orienté (`sf`)}
  \item{angle}{Angle d’orientation en degrés}
  \item{longueur}{Longueur du rectangle}
  \item{largeur}{Largeur du rectangle}
}
}
\description{
Cette fonction retourne le plus petit rectangle orienté (MBR) qui entoure un polygone.
Elle retourne la géométrie du rectangle, son angle, sa longueur et sa largeur.
}
