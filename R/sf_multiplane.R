#' Convertir des points sf en fichier texte MultiPlane
#'
#' Cette fonction convertit un objet `sf` contenant des points en format tabulé compatible avec
#' MultiPlane, à partir d’un point de référence (benchmark). Les distances X/Y et l’altitude sont calculées
#' par rapport au benchmark.
#'
#' @param points Objet `sf` avec les points à convertir
#' @param benchmark Objet `sf` contenant un seul point de référence
#' @param colonne_z Nom de la colonne pour l’altitude
#' @param colonne_code Nom de la colonne indiquant les codes de contours
#' @param fichier_sortie Chemin du fichier `.txt` à générer
#' @param altitude_benchmark Altitude du point de référence (par défaut : 0)
#' @param imperial Logique. Si TRUE, convertit les unités en pieds
#'
#' @return Un `tibble` contenant les données formatées pour MultiPlane
#' @export
#' @importFrom sf st_coordinates st_transform
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr mutate relocate row_number if_else
#' @importFrom geosphere distVincentyEllipsoid
sf_multiplane <- function(points, benchmark, colonne_z = "Z", colonne_code = "Code",
                          fichier_sortie, altitude_benchmark = 0, imperial = FALSE) {

  if (nrow(benchmark) != 1) stop("Le point de référence doit contenir un seul point.")

  points <- sf::st_transform(points, 4326)
  benchmark <- sf::st_transform(benchmark, 4326)

  coords_points <- sf::st_coordinates(points)
  coords_bm <- sf::st_coordinates(benchmark)

  distances <- apply(coords_points, 1, function(pt) {
    dx <- geosphere::distVincentyEllipsoid(c(coords_bm[1], coords_bm[2]), c(pt[1], coords_bm[2]))
    if (pt[1] < coords_bm[1]) dx <- -dx
    dy <- geosphere::distVincentyEllipsoid(c(coords_bm[1], coords_bm[2]), c(coords_bm[1], pt[2]))
    if (pt[2] < coords_bm[2]) dy <- -dy
    if (imperial) c(dx, dy) * 3.28084 else c(dx, dy)
  })

  df <- tibble::as_tibble(t(distances), .name_repair = "minimal")
  names(df) <- c("distance_x", "distance_y")
  df <- tibble::tibble(distance_x = 0, distance_y = 0) |> rbind(df)

  df$z <- c(altitude_benchmark, points[[colonne_z]])
  df$z <- df$z - min(df$z)
  if (imperial) df$z <- df$z * 3.28084
  df$code <- c("MB", points[[colonne_code]])

  donnees <- df |> dplyr::mutate(
    id = dplyr::row_number(),
    id = dplyr::if_else(id == 1, sprintf("%04d", id), as.character(id)),
    distance_x = ifelse(id == "0001", formatC(distance_x, format = "f", digits = 3), formatC(distance_x, format = "f", digits = 2)),
    distance_y = ifelse(id == "0001", formatC(distance_y, format = "f", digits = 3), formatC(distance_y, format = "f", digits = 2)),
    z = formatC(z, format = "f", digits = 6)
  ) |> dplyr::relocate(id)

  format_coord <- function(val, type) {
    deg <- abs(val) %/% 1
    min <- (abs(val) %% 1 * 60) %/% 1
    sec <- (((abs(val) %% 1) * 60) %% 1) * 60
    paste0(
      type, format(deg, width = 2, flag = "0"), ":",
      formatC(min, width = 2, format = "d", flag = "0"), ":",
      formatC(sec, width = 6, digits = 3, format = "f", flag = "0")
    )
  }

  coord_bm <- paste0(
    format_coord(coords_bm[2], ifelse(coords_bm[2] >= 0, "N", "S")), " / ",
    format_coord(coords_bm[1], ifelse(coords_bm[1] >= 0, "E", "W")), "\t0.000"
  )

  donnees <- donnees |> dplyr::mutate(
    benchmark_coord = dplyr::if_else(id == "0001", coord_bm, "")
  )

  write.table(
    donnees,
    file = fichier_sortie,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )

  donnees
}
