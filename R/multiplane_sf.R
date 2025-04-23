#' Convertir un fichier MultiPlane en objet sf
#'
#' Cette fonction lit un fichier texte MultiPlane (issu d’un levé GPS ou station totale) et le convertit
#' en un objet `sf` avec projection WGS84. Les coordonnées sont calculées à partir d’un point de référence.
#'
#' @param fichier Chemin vers le fichier texte MultiPlane (tabulations)
#' @param imperial Logique. Si TRUE, les distances sont en pieds (sinon en mètres)
#'
#' @return Un objet `sf` avec les colonnes `elevation`, `code` et `geometry`
#' @export
#' @importFrom sf st_sf st_sfc st_point
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select
#' @importFrom geosphere destPoint
multiplane_sf <- function(fichier, imperial = TRUE) {
  donnees <- read.table(fichier, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  ref_infos <- strsplit(donnees[1, 6], " ")[[1]]

  parse_coord <- function(coord) {
    p <- strsplit(coord, ":")[[1]]
    degres <- as.numeric(substr(p[1], 2, nchar(p[1])))
    minutes <- as.numeric(p[2])
    secondes <- as.numeric(p[3])
    valeur <- degres + minutes / 60 + secondes / 3600
    if (substr(p[1], 1, 1) %in% c("S", "W")) valeur <- -valeur
    valeur
  }

  ref_lat <- parse_coord(ref_infos[1])
  ref_lon <- parse_coord(ref_infos[3])

  donnees_pts <- donnees[-1, ]
  colnames(donnees_pts) <- c("id", "x", "y", "z", "code", "ref")
  donnees_pts <- donnees_pts[, c("x", "y", "z", "code")]

  donnees_pts <- dplyr::mutate(donnees_pts,
                               x = as.numeric(x),
                               y = as.numeric(y),
                               z = as.numeric(z)
  )

  if (imperial) {
    donnees_pts <- dplyr::mutate(donnees_pts,
                                 x = x / 3.28084,
                                 y = y / 3.28084
    )
  }

  calculer_coord <- function(dx, dy, lon0, lat0) {
    pt_x <- geosphere::destPoint(c(lon0, lat0), 90, dx)
    pt_y <- geosphere::destPoint(c(lon0, lat0), 0, dy)
    c(pt_x[1], pt_y[2])
  }

  coordonnees <- t(apply(donnees_pts[, c("x", "y")], 1, function(dist) calculer_coord(dist[1], dist[2], ref_lon, ref_lat)))

  points <- lapply(seq_len(nrow(coordonnees)), function(i) {
    sf::st_point(coordonnees[i, ])
  })

  sf::st_sf(
    elevation = donnees_pts$z,
    code = donnees_pts$code,
    geometry = sf::st_sfc(points, crs = 4326)
  )
}
