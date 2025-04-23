#' Reprojeter un objet sf dans la bonne zone UTM
#'
#' Cette fonction détecte automatiquement la zone UTM appropriée en fonction du centroïde d’un objet `sf`,
#' puis transforme la géométrie vers ce système de coordonnées métriques (UTM).
#'
#' @param x Un objet `sf` à reprojeter
#'
#' @return Un objet `sf` reprojeté dans la zone UTM correspondante
#' @export
#' @importFrom sf st_crs st_union st_transform st_centroid st_coordinates
#' @importFrom tibble as_tibble
st_utm <- function(x) {
  if (!inherits(x, "sf")) stop("L'entrée doit être un objet sf.")

  centre <- x |>
    sf::st_union() |>
    sf::st_transform(4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    tibble::as_tibble()

  zone <- floor((centre$X + 180) / 6) + 1
  hemis <- ifelse(centre$Y >= 0, 32600, 32700)
  x[[attr(x, "sf_column")]] <- sf::st_transform(sf::st_geometry(x), zone + hemis)
  x
}
