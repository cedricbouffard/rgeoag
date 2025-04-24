#' Créer un réseau de lignes parallèles orientées à partir d'un angle ou d'une ligne
#'
#' Cette fonction génère un réseau de lignes parallèles soit :
#' - selon un angle donné ou calculé automatiquement à partir d’un polygone;
#' - à partir d’une ligne de départ, qui sert à définir l'angle et le point de référence.
#'
#' @param polygone Objet `sf` de type POLYGON ou MULTIPOLYGON.
#' @param angle (optionnel) Angle en degrés. Sera ignoré si `ligne` est fourni.
#' @param largeur Espacement entre les lignes.
#' @param coin Coin d’ancrage pour angle : `"SO"`, `"SE"`, `"NE"`, `"NO"` (par défaut `"SO"`)
#' @param ligne (optionnel) Une `LINESTRING` (ou `sf` contenant une seule ligne) pour servir de base
#'
#' @return Un objet `sf` contenant le réseau de lignes + la ligne de base (si fournie)
#' @export
reseau_lignes_paralleles <- function(polygone, angle = NULL, largeur = 20, coin = "SO", ligne = NULL) {
  stopifnot(inherits(polygone, "sf") || inherits(polygone, "sfc"))
  crs <- sf::st_crs(polygone)
  polygone <- sf::st_union(polygone)

  # Déterminer le point de départ et l'orientation
  if (!is.null(ligne)) {
    stopifnot(sf::st_geometry_type(ligne)[1] == "LINESTRING")
    coords <- sf::st_coordinates(ligne)
    p1 <- coords[1, 1:2]
    p2 <- coords[nrow(coords), 1:2]
    dx <- p2[1] - p1[1]
    dy <- p2[2] - p1[2]
    angle_rad <- atan2(dy, dx)
    centre_ref <- p1
  } else {
    # Si aucune ligne fournie, calcul basé sur le polygone
    rect_info <- rectangle_oriente_min(polygone)
    angle_calcule <- rect_info$angle
    angle <- if (is.null(angle)) angle_calcule else angle
    angle_rad <- angle * pi / 180

    # Déterminer le coin d’origine
    coins <- c("SO", "SE", "NE", "NO")
    coords <- sf::st_coordinates(rect_info$rectangle)[1:4, 1:2]
    centre_ref <- coords[match(coin, coins), ]
  }

  # Calcul de la direction principale et perpendiculaire
  dx <- cos(angle_rad)
  dy <- sin(angle_rad)
  px <- -dy
  py <- dx

  # Dimension de l’emprise
  bbox <- sf::st_bbox(polygone)
  diag_len <- sqrt((bbox["xmax"] - bbox["xmin"])^2 + (bbox["ymax"] - bbox["ymin"])^2)
  n_lignes <- ceiling((2 * diag_len) / largeur)

  # Création des lignes
  lignes <- vector("list", n_lignes)
  for (i in seq_len(n_lignes)) {
    offset <- (i - n_lignes / 2) * largeur
    x0 <- centre_ref[1] + offset * px
    y0 <- centre_ref[2] + offset * py

    p_start <- c(x0 - dx * diag_len, y0 - dy * diag_len)
    p_end   <- c(x0 + dx * diag_len, y0 + dy * diag_len)

    lignes[[i]] <- sf::st_linestring(rbind(p_start, p_end))
  }

  # Construction du réseau
  reseau <- sf::st_sfc(lignes, crs = crs)
  polygone <- sf::st_transform(polygone, crs)

  reseau_clipped <- sf::st_intersection(reseau, polygone)
  result <- sf::st_sf(geometry = reseau_clipped)



  result
}
