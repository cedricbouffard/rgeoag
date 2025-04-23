#' Créer des rectangles orientés à partir de centroïdes
#'
#' Cette fonction génère des rectangles orientés à partir de points centroïdes. Chaque rectangle
#' est défini par sa longueur, sa largeur et un angle de rotation.
#'
#' @param centroides Un objet `sf` de type POINT, représentant les centroïdes
#' @param longueur Longueur des rectangles (côté le plus long avant rotation)
#' @param largeur Largeur des rectangles (côté le plus court avant rotation)
#' @param angle Angle de rotation en degrés, dans le sens antihoraire depuis l’axe des x (par défaut : 0)
#'
#' @return Un objet `sf` contenant les rectangles orientés (`POLYGON`)
#' @export
#' @importFrom sf st_coordinates st_polygon st_sfc st_sf st_crs
creer_rectangles <- function(centroides, longueur, largeur, angle = 0) {
  stopifnot(inherits(centroides, "sf"))
  stopifnot(all(sf::st_geometry_type(centroides, by_geometry = TRUE) == "POINT"))

  angle_rad <- angle * pi / 180

  # Rectangle centré à l'origine
  rectangle_base <- matrix(c(
    -largeur / 2, -longueur / 2,
    largeur / 2, -longueur / 2,
    largeur / 2,  longueur / 2,
    -largeur / 2,  longueur / 2,
    -largeur / 2, -longueur / 2
  ), ncol = 2, byrow = TRUE)

  # Matrice de rotation
  rotation <- matrix(c(cos(angle_rad), -sin(angle_rad), sin(angle_rad), cos(angle_rad)), ncol = 2)
  coords <- sf::st_coordinates(centroides)

  # Génération des polygones orientés
  polygones <- apply(coords, 1, function(centre) {
    tourne <- rectangle_base %*% rotation
    deplace <- sweep(tourne, 2, centre, "+")
    sf::st_polygon(list(deplace))
  })

  sf::st_sf(geometry = sf::st_sfc(polygones, crs = sf::st_crs(centroides)))
}
