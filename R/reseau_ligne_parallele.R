#' Créer un réseau de lignes parallèles orientées avec angle et coin dynamiques
#'
#' Cette fonction génère un réseau de lignes parallèles orientées selon un angle donné ou calculé automatiquement,
#' espacées d'une largeur fixe, et couvrant une emprise fournie. Permet de choisir le coin de départ.
#'
#' @param polygone Objet `sf` (POLYGON ou MULTIPOLYGON)
#' @param angle (optionnel) Angle en degrés. Si NULL, calculé automatiquement.
#' @param largeur Espacement entre les lignes.
#' @param coin Coin d’ancrage : `"SO"`, `"SE"`, `"NE"`, `"NO"` (par défaut `"SO"`)
#'
#' @return Un objet `sf` de lignes coupées à l'emprise du polygone
#' @export
reseau_lignes_paralleles <- function(polygone, angle = NULL, largeur = 20, coin = "SO") {
  stopifnot(inherits(polygone, "sf") || inherits(polygone, "sfc"))

  crs <- sf::st_crs(polygone)
  polygone <- sf::st_union(polygone) |> sf::st_transform(4326)

  # Rectangle orienté minimal pour angle et coin
  rect_info <- rectangle_oriente_min(polygone)
  angle_calcule <- rect_info$angle
  angle <- if (is.null(angle)) angle_calcule else angle
  angle_rad <- angle * pi / 180

  # Création des lignes dans un repère tourné
  bbox <- sf::st_bbox(polygone)
  centre <- c(mean(c(bbox["xmin"], bbox["xmax"])), mean(c(bbox["ymin"], bbox["ymax"])))

  dx <- cos(angle_rad)
  dy <- sin(angle_rad)
  px <- -dy
  py <- dx

  diag_len <- sqrt((bbox["xmax"] - bbox["xmin"])^2 + (bbox["ymax"] - bbox["ymin"])^2)
  n_lignes <- ceiling((2 * diag_len) / largeur)

  # Choix du coin d'origine pour le centrage
  coins <- c("SO", "SE", "NE", "NO")
  coords <- sf::st_coordinates(rect_info$rectangle)[1:4, 1:2]
  coord_coin <- coords[match(coin, coins), ]

  lignes <- vector("list", n_lignes)
  for (i in seq_len(n_lignes)) {
    offset <- (i - n_lignes / 2) * largeur
    x0 <- coord_coin[1] + offset * px
    y0 <- coord_coin[2] + offset * py

    p1 <- c(x0 - dx * diag_len, y0 - dy * diag_len)
    p2 <- c(x0 + dx * diag_len, y0 + dy * diag_len)

    lignes[[i]] <- sf::st_linestring(rbind(p1, p2))
  }

  reseau <- sf::st_sfc(lignes, crs = 4326)
  reseau <- sf::st_transform(reseau, crs)
  polygone <- sf::st_transform(polygone, crs)

  reseau_clipped <- sf::st_intersection(reseau, polygone)
  sf::st_sf(geometry = reseau_clipped)
}
