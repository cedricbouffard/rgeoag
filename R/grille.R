#' Générer une grille orientée selon un polygone
#'
#' Cette fonction crée une grille de polygones alignée à l'enveloppe orientée minimale d’un polygone.
#' Elle permet d’indiquer un angle, une largeur, une longueur, un coin d’origine, un décalage en quinconce,
#' et de restreindre les cellules à celles qui croisent ou sont contenues dans le polygone.
#'
#' @param polygone Objet `sf` de type POLYGON ou MULTIPOLYGON
#' @param angle Angle d’orientation en degrés. Si NULL, il est déduit automatiquement.
#' @param largeur Largeur des cellules (axe court)
#' @param longueur Longueur des cellules (axe long, optionnel)
#' @param coin Coin de départ : "SO", "SE", "NE" ou "NO"
#' @param quinconce Logique : grille en quinconce (TRUE/FALSE)
#' @param decouper Logique : découper les cellules au bord du polygone
#' @param points Logique : retourner les centroïdes au lieu des polygones
#'
#' @return Un objet `sf` avec les colonnes `ligne`, `colonne`, et `geometry`
#' @export
#' @importFrom sf st_crs st_union st_coordinates st_geometry st_make_grid st_bbox st_centroid st_intersects st_intersection st_transform
st_grille <- function(polygone, angle = NULL, largeur = 100, longueur = 100, coin = "SO", quinconce = FALSE, decouper = TRUE, points = FALSE) {
  stopifnot(inherits(polygone, "sf") || inherits(polygone, "sfc"))
  crs_polygone <- sf::st_crs(polygone)

  # Calcul du rectangle orienté
  rect_info <- rectangle_oriente_min(polygone)
  angle <- if (is.null(angle)) rect_info$angle else angle
  rect <- rect_info$rectangle
  coords <- sf::st_coordinates(rect)[1:4, 1:2]
  centre <- colMeans(coords)

  # Déterminer les dimensions effectives
  cotes <- sqrt(rowSums((coords - coords[c(2,3,4,1), ])^2))
  largeur_eff <- min(cotes)
  longueur_eff <- max(cotes)
  if (!is.null(longueur)) longueur_eff <- longueur

  # Assigner les tailles selon l’orientation
  cote1 <- sqrt(sum((coords[2, ] - coords[1, ])^2))
  cote2 <- sqrt(sum((coords[4, ] - coords[1, ])^2))
  cote1_long <- cote1 >= cote2
  taille_x <- if (cote1_long) longueur_eff else largeur
  taille_y <- if (cote1_long) largeur else longueur_eff

  # Rotation
  angle_rad <- angle * pi / 180
  rot <- matrix(c(cos(angle_rad), sin(angle_rad), -sin(angle_rad), cos(angle_rad)), 2)
  polygone_centre <- sf::st_geometry(polygone) - centre
  polygone_rot <- polygone_centre * rot

  # Grille dans l’espace pivoté
  bb <- sf::st_bbox(polygone_rot)
  n_x <- ceiling((bb["xmax"] - bb["xmin"]) / taille_x)
  n_y <- ceiling((bb["ymax"] - bb["ymin"]) / taille_y)
  grille_rot <- sf::st_make_grid(cellsize = c(taille_x, taille_y), offset = c(bb["xmin"], bb["ymin"]), n = c(n_x + 1, n_y + 1))
  grille_rot <- sf::st_sf(geometry = grille_rot)

  # Quinconce
  if (quinconce) {
    geom <- sf::st_geometry(grille_rot)
    for (i in seq_along(geom)) {
      ligne_idx <- floor((i - 1) / (n_x + 1))
      if (ligne_idx %% 2 == 1) {
        geom[[i]] <- geom[[i]] - c(taille_x / 2, 0)
      }
    }
    grille_rot <- sf::st_sf(geometry = geom)
  }

  # Numérotation
  coords_rot <- coords - centre
  coords_rot <- coords_rot %*% rot
  coins <- c("SO", "SE", "NE", "NO")
  df_coins <- data.frame(label = coins, x = coords_rot[,1], y = coords_rot[,2])
  coin_coord <- df_coins[df_coins$label == coin, c("x", "y")]
  signe_x <- switch(coin, "SO" = 1, "NO" = 1, "SE" = -1, "NE" = -1)
  signe_y <- switch(coin, "SO" = 1, "SE" = 1, "NO" = -1, "NE" = -1)
  centre_grille <- sf::st_centroid(grille_rot)
  coord_centres <- sf::st_coordinates(centre_grille)

  x0 <- coin_coord$x
  y0 <- coin_coord$y
  grille_rot$ligne <- floor((signe_x * (coord_centres[,1] - x0)) / taille_x)
  grille_rot$colonne <- floor((signe_y * (coord_centres[,2] - y0)) / taille_y)
  grille_rot$ligne <- grille_rot$ligne - min(grille_rot$ligne, na.rm = TRUE) + 1
  grille_rot$colonne <- grille_rot$colonne - min(grille_rot$colonne, na.rm = TRUE) + 1

  # Retour à la géométrie d’origine
  inv_rot <- matrix(c(cos(-angle_rad), sin(-angle_rad), -sin(-angle_rad), cos(-angle_rad)), 2)
  grille <- sf::st_geometry(grille_rot) * inv_rot + centre
  grille_rot$geometry <- grille

  # Alignement au coin du champ
  get_corners <- function(poly) {
    coords <- sf::st_coordinates(poly)[, 1:2]
    coords[!duplicated(coords), ][1:4, ]
  }
  all_corners <- do.call(rbind, lapply(grille_rot$geometry, get_corners))
  coin_cible <- coords[coins == coin, ]
  dists <- sqrt(rowSums((all_corners - matrix(coin_cible, nrow = nrow(all_corners), ncol = 2, byrow = TRUE))^2))
  coin_plus_proche <- all_corners[which.min(dists), ]
  deplacement <- coin_plus_proche - coin_cible
  grille_rot$geometry <- grille_rot$geometry - deplacement

  # Coupe ou centroïdes
  sf::st_crs(grille_rot) <- crs_polygone
  polygone <- sf::st_transform(polygone, crs_polygone)
  inters <- sf::st_intersects(grille_rot, polygone, sparse = FALSE)
  grille_rot <- grille_rot[apply(inters, 1, any), ]

  if (decouper) {
    grille_rot <- sf::st_intersection(grille_rot, polygone)
  }

  if (points) {
    grille_rot <- sf::st_centroid(grille_rot)
  }

  grille_rot$colonne <- grille_rot$colonne - min(grille_rot$colonne) + 1
  grille_rot$ligne <- grille_rot$ligne - min(grille_rot$ligne) + 1
  grille_rot
}
