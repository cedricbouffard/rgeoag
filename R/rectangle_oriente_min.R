#' Calculer le rectangle orienté minimal autour d'un polygone
#'
#' Cette fonction retourne le plus petit rectangle orienté (MBR) qui entoure un polygone.
#' Elle retourne la géométrie du rectangle, son angle, sa longueur et sa largeur.
#'
#' @param polygone Un objet `sf` de type POLYGON ou MULTIPOLYGON
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{rectangle}{Le rectangle orienté (`sf`)}
#'   \item{angle}{Angle d’orientation en degrés}
#'   \item{longueur}{Longueur du rectangle}
#'   \item{largeur}{Largeur du rectangle}
#' }
#' @export
#' @importFrom sf st_union st_coordinates st_polygon st_sfc st_sf st_crs
rectangle_oriente_min <- function(polygone) {
  stopifnot(all(sf::st_is(polygone, c("POLYGON", "MULTIPOLYGON"))))

  coords <- sf::st_coordinates(sf::st_union(polygone))[, 1:2]
  centre <- colMeans(coords)
  coords_centre <- sweep(coords, 2, centre)

  angles <- seq(0, pi / 2, length.out = 180)
  meilleure_surface <- Inf
  meilleur_rect <- NULL
  meilleur_angle <- 0

  for (angle in angles) {
    rot <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2)
    coords_rot <- coords_centre %*% rot
    xmin <- min(coords_rot[, 1])
    xmax <- max(coords_rot[, 1])
    ymin <- min(coords_rot[, 2])
    ymax <- max(coords_rot[, 2])
    surface <- (xmax - xmin) * (ymax - ymin)

    if (surface < meilleure_surface) {
      meilleure_surface <- surface
      coin_rect <- matrix(c(
        xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin
      ), ncol = 2, byrow = TRUE)
      meilleur_rect <- coin_rect %*% t(rot)
      meilleur_angle <- angle
    }
  }

  meilleur_rect <- sweep(meilleur_rect, 2, centre, "+")
  rectangle_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(meilleur_rect)), crs = sf::st_crs(polygone)))

  cotes <- sqrt(rowSums((meilleur_rect[2:5, ] - meilleur_rect[1:4, ])^2))

  list(
    rectangle = rectangle_sf,
    angle = meilleur_angle * 180 / pi,
    longueur = max(cotes),
    largeur = min(cotes)
  )
}
