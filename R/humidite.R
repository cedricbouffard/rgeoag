#' Calculer les métriques topographiques d'humidité
#'
#' Cette fonction calcule l'indice d'humidité topographique (TWI), la profondeur des dépressions,
#' l'accumulation de flux et les formes du terrain (géomorphons) à partir d'un MNT ou MNE.
#'
#' @param raster_mnt Un objet `SpatRaster` ou un chemin vers un fichier `.tif`
#' @param dossier Chemin du dossier de sortie (facultatif)
#' @param prefixe Préfixe pour les noms de fichiers exportés (si `dossier` est défini)
#'
#' @return Un objet `SpatRaster` avec les couches "Geomorphons", "TWI", "Depression" et "Accumulation"
#' @export
#' @importFrom terra rast writeRaster
#' @importFrom whitebox wbt_feature_preserving_smoothing wbt_fill_depressions_wang_and_liu
#' @importFrom whitebox wbt_slope wbt_d8_flow_accumulation wbt_wetness_index wbt_geomorphons check_whitebox_binary install_whitebox
indice_humidite <- function(raster_mnt, dossier = NULL, prefixe = "") {
  if (!whitebox::check_whitebox_binary()) whitebox::install_whitebox()

  # Lecture ou écriture d’un raster temporaire
  if (inherits(raster_mnt, "SpatRaster")) {
    chemin_mnt <- tempfile(fileext = ".tif")
    terra::writeRaster(raster_mnt, chemin_mnt, overwrite = TRUE)
  } else if (is.character(raster_mnt)) {
    chemin_mnt <- raster_mnt
  } else {
    stop("L'argument 'raster_mnt' doit être un SpatRaster ou un chemin .tif")
  }

  # Chemins temporaires
  chemin_lisse <- tempfile(fileext = ".tif")
  chemin_rempli <- tempfile(fileext = ".tif")
  chemin_pente <- tempfile(fileext = ".tif")
  chemin_accum <- tempfile(fileext = ".tif")
  chemin_twi <- tempfile(fileext = ".tif")
  chemin_geo <- tempfile(fileext = ".tif")

  # Traitement avec WhiteboxTools
  whitebox::wbt_feature_preserving_smoothing(chemin_mnt, output = chemin_lisse)
  whitebox::wbt_fill_depressions_wang_and_liu(chemin_lisse, chemin_rempli)
  whitebox::wbt_slope(chemin_rempli, output = chemin_pente)
  whitebox::wbt_d8_flow_accumulation(chemin_rempli, output = chemin_accum, out_type = "specific contributing area")
  whitebox::wbt_wetness_index(sca = chemin_accum, slope = chemin_pente, output = chemin_twi)
  whitebox::wbt_geomorphons(dem = chemin_rempli, output = chemin_geo)

  # Lecture et calculs finaux
  r_lisse <- terra::rast(chemin_lisse) * 1
  r_rempli <- terra::rast(chemin_rempli) * 1
  r_twi <- terra::rast(chemin_twi) * 1
  r_geo <- terra::rast(chemin_geo) * 1
  r_accum <- terra::rast(chemin_accum) * 1
  r_depression <- r_rempli - r_lisse

  r_final <- terra::rast(list(r_geo, r_twi, r_depression, r_accum))
  names(r_final) <- c("Geomorphons", "TWI", "Depression", "Accumulation")

  # Export si requis
  if (!is.null(dossier)) {
    dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
    terra::writeRaster(r_final[[1]], file.path(dossier, paste0(prefixe, "_geomorphons.tif")), overwrite = TRUE)
    terra::writeRaster(r_final[[2]], file.path(dossier, paste0(prefixe, "_twi.tif")), overwrite = TRUE)
    terra::writeRaster(r_final[[3]], file.path(dossier, paste0(prefixe, "_depression.tif")), overwrite = TRUE)
    terra::writeRaster(r_final[[4]], file.path(dossier, paste0(prefixe, "_accumulation.tif")), overwrite = TRUE)
  }

  unlink(c(chemin_lisse, chemin_rempli, chemin_pente, chemin_accum, chemin_twi, chemin_geo))
  if (inherits(raster_mnt, "SpatRaster")) unlink(chemin_mnt)

  r_final
}
