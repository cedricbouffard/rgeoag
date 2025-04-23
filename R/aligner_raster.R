#' Aligner, lisser, tronquer et normaliser une liste de rasters
#'
#' Cette fonction prend une liste de rasters (`SpatRaster`), applique un lissage médian
#' optionnel, aligne tous les rasters à la résolution la plus fine, tronque les valeurs
#' extrêmes et normalise chaque couche si désiré.
#'
#' @param raster_liste Liste de `SpatRaster` (terra) à traiter
#' @param noms (optionnel) Noms à assigner aux couches combinées
#' @param fenetre_focale Taille du filtre médian (>1 applique un lissage)
#' @param normaliser Logique. Si TRUE, normalise chaque couche entre 0 et 1 après troncature
#'
#' @return Un `SpatRaster` combiné, aligné, tronqué et normalisé (optionnellement)
#' @export
#' @importFrom terra res focal global project c nlyr values
#' @importFrom tools file_path_sans_ext
aligner_rasters <- function(raster_liste, noms = NULL, fenetre_focale = 1, normaliser = FALSE) {
  stopifnot(all(sapply(raster_liste, inherits, what = "SpatRaster")))

  nb_couches_par_objet <- sapply(raster_liste, terra::nlyr)
  raster_lisse <- lapply(raster_liste, function(r) {
    if (fenetre_focale > 1) {
      matrice <- matrix(1, nrow = fenetre_focale * 2 + 1, ncol = fenetre_focale * 2 + 1)
      terra::focal(r, w = matrice, fun = "median", na.policy = "all", na.rm = TRUE)
    } else {
      r
    }
  })

  indice_cible <- which.min(sapply(raster_lisse, function(r) prod(terra::res(r))))
  raster_cible <- raster_lisse[[indice_cible]]

  raster_aligne <- lapply(raster_lisse, function(r) {
    terra::project(r, raster_cible, method = "bilinear")
  })

  raster_combine <- do.call(c, raster_aligne)
  if (!is.null(noms)) names(raster_combine) <- tools::file_path_sans_ext(noms)

  index_couche <- rep(seq_along(nb_couches_par_objet), times = nb_couches_par_objet)

  for (i in seq_len(terra::nlyr(raster_combine))) {
    quantiles <- terra::global(raster_combine[[i]], fun = function(x) quantile(x, c(0.05, 0.95), na.rm = TRUE))
    q_min <- as.numeric(quantiles[1, 1])
    q_max <- as.numeric(quantiles[1, 2])

    raster_combine[[i]][raster_combine[[i]] > q_max] <- q_max
    raster_combine[[i]][raster_combine[[i]] < q_min] <- q_min

    if (normaliser) {
      raster_norm <- (raster_combine[[i]] - q_min) / (q_max - q_min)
      raster_combine[[i]] <- raster_norm / nb_couches_par_objet[index_couche[i]]
    }
  }

  raster_combine
}
