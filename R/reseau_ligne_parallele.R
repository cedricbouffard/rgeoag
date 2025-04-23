#' Créer un réseau de lignes parallèles orientées
#'
#' Cette fonction génère un réseau de lignes parallèles orientées selon un angle donné,
#' espacées d'une largeur fixe, et couvrant une emprise fournie.
#'
#' @param polygone Un objet `sf` de type POLYGON, MULTIPOLYGON ou `bbox` représentant la zone à couvrir
#' @param angle Angle d’orientation des lignes en degrés (par rapport à l’axe des x, dans le sens antihoraire)
#' @param largeur Espacement entre les lignes (en unités du système de coordonnées)
#'
#' @return Un objet `sf` de type LINESTRING contenant le réseau de lignes découpé à l'emprise
#' @export
#' @importFrom sf st_bbox st_as_sfc st_crs st_intersection st_sfc st_linestring st_geometry
reseau_lignes_paralleles <- function(polygone, angle, largeur) {
  stopifnot(inherits(polygone, "sf") || inherits(polygone, "sfc") || inherits(polygone, "bbox"))

  emprise <- sf::st_as_sfc(sf::st_bbox(polygone))
  crs <- sf::st_crs(emprise)

  bbox <- sf::st_bbox(emprise)
  diag_len <- sqrt((bbox$xmax - bbox$xmin)^2 + (bbox$ymax - bbox$ymin)^2)

  angle_rad <- angle * pi / 180
  dx <- cos(angle_rad)
  dy <- sin(angle_rad)

  px <- -dy
  py <- dx

  n_lignes <- ceiling((2 * diag_len) / largeur)
  lignes <- vector("list", length = n_lignes)

  for (i in seq_len(n_lignes)) {
    offset <- (i - n_lignes / 2) * largeur
    x0 <- (bbox$xmin + bbox$xmax) / 2 + offset * px
    y0 <- (bbox$ymin + bbox$ymax) / 2 + offset * py

    p1 <- c(x0 - dx * diag_len, y0 - dy * diag_len)
    p2 <- c(x0 + dx * diag_len, y0 + dy * diag_len)

    lignes[[i]] <- sf::st_linestring(rbind(p1, p2))
  }

  reseau <- sf::st_sfc(lignes, crs = crs)
  reseau_clipped <- sf::st_intersection(reseau, polygone)
  sf::st_sf(geometry = reseau_clipped)
}
