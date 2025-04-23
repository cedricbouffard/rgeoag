#' Télécharger et recadrer les données LiDAR pour une zone donnée
#'
#' Cette fonction interroge l'API STAC de RNCan pour télécharger les données LiDAR (MNE ou MNT)
#' correspondant à une géométrie fournie, puis les recadre à cette zone d’intérêt.
#'
#' @param polygone Un objet `sf` représentant la zone d’intérêt ou un chemin vers un fichier vectoriel (`.shp`, `.gpkg`, etc.)
#' @param dossier Dossier de sortie pour enregistrer les fichiers raster (optionnel)
#' @param mne Logique. Si TRUE, télécharge le MNE (modèle de surface). Sinon, le MNT.
#' @param recent Logique. Si TRUE, conserve uniquement la version la plus récente
#'
#' @return Une liste de `SpatRaster` (un par année)
#' @export
#' @importFrom sf st_read st_crs st_transform st_union st_bbox
#' @importFrom rstac stac stac_search get_request assets_url items_datetime
#' @importFrom lubridate as_datetime year
#' @importFrom terra vrt crop writeRaster varnames vect crs
lidar <- function(polygone, dossier = NULL, mne = FALSE, recent = FALSE) {
  if (is.character(polygone)) {
    polygone <- sf::st_read(polygone, quiet = TRUE)
  }

  if (is.na(sf::st_crs(polygone))) {
    sf::st_crs(polygone) <- 4326
  }

  polygone <- polygone |> sf::st_transform(4326) |> sf::st_union()

  stac_query <- rstac::stac("https://datacube.services.geo.ca/stac/api/", force_version = TRUE) |>
    rstac::stac_search(collections = "hrdem-lidar", bbox = as.numeric(sf::st_bbox(polygone)), limit = 100) |>
    rstac::get_request()

  datetimes <- lubridate::as_datetime(rstac::items_datetime(stac_query))
  if (length(datetimes) == 0) {
    warning("Aucune donnée LiDAR disponible pour cette zone.")
    return(NULL)
  }

  annees <- lubridate::year(datetimes)
  urls <- rstac::assets_url(stac_query, ifelse(mne, "dsm", "dtm"))

  if (recent) {
    max_annee <- max(annees)
    keep <- which(annees == max_annee)
    urls <- urls[keep]
    annees <- annees[keep]
  }

  if (!is.null(dossier)) {
    dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  }

  resultats <- lapply(unique(annees), function(annee) {
    fichiers <- urls[annees == annee]
    r <- terra::vrt(paste0("/vsicurl/", fichiers))
    r <- terra::crop(r, sf::st_transform(polygone, terra::crs(r)) |> terra::vect(), mask = TRUE)
    names(r) <- annee
    terra::varnames(r) <- annee

    if (!is.null(dossier)) {
      fichier_sortie <- file.path(dossier, paste0("lidar_", annee, ".tif"))
      terra::writeRaster(r, fichier_sortie, overwrite = TRUE)
    }

    r
  })

  resultats
}
