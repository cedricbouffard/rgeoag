#' Raccourcir des lignes en interpolant les extrémités
#'
#' Cette fonction permet de raccourcir une ou plusieurs lignes `LINESTRING` en retirant une distance
#' au début, à la fin, ou aux deux extrémités. Elle conserve une géométrie valide en interpolant les points.
#'
#' @param lignes Objet `sf` contenant des géométries `LINESTRING`
#' @param distance Distance à retirer (en unités du système de coordonnées)
#' @param extremite Choix de l’extrémité à couper : "debut", "fin" ou `c("debut", "fin")`
#'
#' @return Un objet `sf` avec les lignes raccourcies
#' @export
#' @importFrom sf st_geometry st_coordinates st_linestring st_length st_sfc st_as_sf
#' @importFrom dplyr filter
raccourcir_lignes <- function(lignes, distance, extremite = c("debut", "fin")) {
  extremite <- match.arg(extremite, choices = c("debut", "fin"), several.ok = TRUE)

  lignes_racc <- lapply(sf::st_geometry(lignes), function(ligne) {
    coords <- sf::st_coordinates(ligne)
    if (nrow(coords) < 2) return(sf::st_linestring())

    dists <- sqrt(rowSums(diff(coords)^2))
    cum_dists <- c(0, cumsum(dists))
    total <- cum_dists[length(cum_dists)]

    min_total <- sum(c("debut", "fin") %in% extremite) * distance
    if (total <= min_total) return(sf::st_linestring())

    dist_debut <- if ("debut" %in% extremite) distance else 0
    dist_fin <- if ("fin" %in% extremite) total - distance else total

    interp <- function(cible) {
      i <- max(which(cum_dists <= cible))
      if (i == length(cum_dists)) return(coords[i, 1:2])
      ratio <- (cible - cum_dists[i]) / (cum_dists[i + 1] - cum_dists[i])
      coords[i, 1:2] + ratio * (coords[i + 1, 1:2] - coords[i, 1:2])
    }

    nouvelles_coords <- list(interp(dist_debut))
    for (i in seq_along(cum_dists)) {
      if (cum_dists[i] > dist_debut && cum_dists[i] < dist_fin) {
        nouvelles_coords[[length(nouvelles_coords) + 1]] <- coords[i, 1:2]
      }
    }
    nouvelles_coords[[length(nouvelles_coords) + 1]] <- interp(dist_fin)

    sf::st_linestring(do.call(rbind, nouvelles_coords))
  })

  lignes_sf <- sf::st_as_sf(sf::st_sfc(lignes_racc, crs = sf::st_crs(lignes)))
  dplyr::filter(lignes_sf, as.numeric(sf::st_length(lignes_sf)) > 0)
}
