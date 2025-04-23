#' Générer un fichier pour les logiciels de nivellement à partir d’un polygone et du LiDAR
#'
#' Cette fonction extrait les points de bordure et une grille intérieure depuis un polygone,
#' interroge un raster LiDAR pour obtenir les altitudes, puis exporte les données en format texte
#' compatible avec les logiciels de nivellement.
#'
#' @param polygone Un objet `sf` de type POLYGON (doit contenir un seul polygone)
#' @param raster_lidar Un objet `SpatRaster` (optionnel). Si NULL, la fonction `lidar()` est utilisée.
#' @param fichier_sortie Chemin du fichier texte de sortie
#' @param imperial Logique : TRUE pour exporter les distances en pieds
#' @param densite_bordure Distance entre les points sur la bordure (en mètres)
#' @param largeur Largeur des cellules de la grille intérieure (en mètres)
#' @param longueur Longueur des cellules de la grille intérieure (en mètres)
#' @param multiplane Logique : TRUE pour générer un fichier MultiPlane (utilise `sf_multiplane`)
#' @param bm Objet `sf` optionnel : point de référence (benchmark)
#' @param bm_elev Altitude du point de référence (optionnelle)
#'
#' @return Un `data.frame` avec les points exportés, écrit dans un fichier texte
#' @export
#' @importFrom sf st_exterior_ring st_cast st_line_sample st_as_sf st_geometry st_transform st_coordinates st_drop_geometry
#' @importFrom dplyr mutate filter select bind_rows slice rename
#' @importFrom terra extract crs global vect
#' @importFrom modelbased smoothing
#' @importFrom readr write_delim
#' @importFrom tibble as_tibble
#' @importFrom stringr str_replace_all
#' @importFrom stats na.omit
#' @importFrom utils write.table
#' @importFrom geosphere distVincentyEllipsoid
#' @importFrom lubridate year
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom glue glue
#' @importFrom utils head
#'
topo_lidar <- function(polygone, fichier_sortie,
                       raster_lidar = NULL,
                       imperial = FALSE,
                       bm = NULL,
                       bm_elev = NULL,
                       densite_bordure = 3,
                       largeur = 7,
                       longueur = 3,
                       multiplane = FALSE) {

  stopifnot(inherits(polygone, "sf") && nrow(polygone) == 1)

  if (is.null(raster_lidar)) {
    raster_lidar <- lidar(polygone, recent = TRUE)[[1]]
  }

  polygone <- sf::st_transform(polygone, terra::crs(raster_lidar))

  bordure <- polygone |>
    sf::st_exterior_ring() |>
    sf::st_cast("LINESTRING") |>
    sf::st_line_sample(density = 1 / densite_bordure) |>
    sf::st_cast("POINT") |>
    sf::st_as_sf() |>
    dplyr::mutate(Code = ifelse(multiplane, "B", "2PER"))
  sf::st_geometry(bordure) <- "geometry"

  grille_pts <- polygone |>
    st_grille(largeur = largeur, longueur = longueur, pts = TRUE) |>
    dplyr::select(geometry) |>
    dplyr::mutate(Code = ifelse(multiplane, "", "3GRD"))

  bordure <- sf::st_transform(bordure, terra::crs(raster_lidar))
  grille_pts <- sf::st_transform(grille_pts, terra::crs(raster_lidar))

  bordure$Z <- terra::extract(raster_lidar, terra::vect(bordure))[, 2]
  bordure <- dplyr::filter(bordure, !is.na(Z))
  bordure$Z <- modelbased::smoothing(bordure$Z)
  grille_pts$Z <- terra::extract(raster_lidar, terra::vect(grille_pts))[, 2]

  tous_points <- dplyr::bind_rows(bordure, grille_pts) |>
    sf::st_transform(4326)

  tous_points <- tous_points |>
    dplyr::bind_cols(sf::st_coordinates(tous_points) |> as.data.frame()) |>
    dplyr::rename(Lon = X, Lat = Y) |>
    dplyr::filter(!is.na(Lon), !is.na(Lat), !is.na(Z))

  if (multiplane) {
    if (is.null(bm)) {
      bm <- dplyr::slice(tous_points, 1)
    }
    if (is.null(bm_elev)) {
      bm_elev <- as.numeric(terra::global(lidar(bm |> st_utm() |> sf::st_buffer(3), recent = TRUE)[[1]]))
    }
    df <- sf_multiplane(
      tous_points |> dplyr::select(Lat, Lon, Z, Code),
      bm,
      dest_file = fichier_sortie,
      imperial = imperial,
      elev_bm = bm_elev
    )
  } else {
    df <- tous_points |> dplyr::select(Lat, Lon, Z, Code)
    if (!is.null(bm)) {
      if (is.null(bm_elev)) {
        bm_elev <- as.numeric(terra::global(lidar(bm |> st_utm() |> sf::st_buffer(3), recent = TRUE)[[1]]))
      }
      bm <- bm |> dplyr::select(1) |> dplyr::select(-1) |> sf::st_transform(4326)
      bm <- bm |> dplyr::bind_cols(sf::st_coordinates(bm) |> as.data.frame()) |>
        dplyr::rename(Lon = X, Lat = Y) |>
        dplyr::mutate(Z = bm_elev, Code = "0MB")
      df <- rbind(bm |> dplyr::select(Lat, Lon, Z, Code), df)
    }
    readr::write_delim(sf::st_drop_geometry(df), fichier_sortie, delim = ",")
  }

  df
}
