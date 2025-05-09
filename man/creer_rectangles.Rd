% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creer_rectangles.R
\name{creer_rectangles}
\alias{creer_rectangles}
\title{Créer des rectangles orientés à partir de centroïdes}
\usage{
creer_rectangles(centroides, longueur, largeur, angle = 0)
}
\arguments{
\item{centroides}{Un objet `sf` de type POINT, représentant les centroïdes}

\item{longueur}{Longueur des rectangles (côté le plus long avant rotation)}

\item{largeur}{Largeur des rectangles (côté le plus court avant rotation)}

\item{angle}{Angle de rotation en degrés, dans le sens antihoraire depuis l’axe des x (par défaut : 0)}
}
\value{
Un objet `sf` contenant les rectangles orientés (`POLYGON`)
}
\description{
Cette fonction génère des rectangles orientés à partir de points centroïdes. Chaque rectangle
est défini par sa longueur, sa largeur et un angle de rotation.
}
