#' Télécharger et calculer un indice de végétation Sentinel-2 pour une parcelle
#'
#' Cette fonction interroge le Planetary Computer pour obtenir des images Sentinel-2 L2A,
#' filtre selon la couverture nuageuse, calcule un indice de végétation (NDVI, GNDVI, EVI, KNDVI, NDRE)
#' et retourne une stack raster avec les valeurs de l'indice sélectionné.
#'
#' @param polygone Un objet `sf` ou un chemin vers un fichier vectoriel (shapefile, geojson, etc.)
#' @param dossier (optionnel) Dossier dans lequel enregistrer les fichiers raster (.tif)
#' @param annee_debut Année de début (par défaut `2018`)
#' @param annee_fin Année de fin (par défaut : année courante)
#' @param max_nuage Pourcentage maximal de nuages (0 à 100) selon les métadonnées
#' @param mois Mois d'intérêt (entiers entre 1 et 12)
#' @param nuage_dans_la_parcelle Proportion maximale de nuages dans la parcelle (0–100), ou `FALSE` pour désactiver
#' @param indice Indice de végétation à calculer (`"NDVI"`, `"GNDVI"`, `"EVI"`, `"KNDVI"`, , `"NDRE"`)
#'
#' @return Un objet `SpatRaster` contenant l'indice de végétation par date
#' @export
#' @importFrom sf st_read st_crs st_transform st_union st_geometry st_bbox
#' @importFrom lubridate year today as_date month as_datetime
#' @importFrom rstac stac ext_filter cql2_interval cql2_bbox_as_geojson post_request items_sign_planetary_computer assets_url items_datetime
#' @importFrom terra rast crop vect varnames writeRaster nlyr
#' @importFrom exactextractr exact_extract
#' @importFrom dplyr summarise_all
sentinel2 <- function(polygone, dossier = NULL, annee_debut = 2018, annee_fin = NULL,
                      max_nuage = 50, mois = 3:11, nuage_dans_la_parcelle = 5, indice = "NDVI") {

  liste_indices <- list(
    NDVI = list(bandes = c("B04", "B08"), formule = function(b) (b$B08 - b$B04) / (b$B08 + b$B04)),
    GNDVI = list(bandes = c("B03", "B08"), formule = function(b) (b$B08 - b$B03) / (b$B08 + b$B03)),
    EVI = list(bandes = c("B02", "B04", "B08"), formule = function(b) 2.5 * (b$B08 - b$B04) / (b$B08 + 6 * b$B04 - 7.5 * b$B02 + 1)),
    KNDVI = list(bandes = c("B04", "B08"), formule = function(b) tanh((b$B08 - b$B04) / (b$B08 + b$B04))^2),
    NDRE = list(
      bandes = c("B05", "B08"),
      formule = function(b) (b$B08 - b$B05) / (b$B08 + b$B05)
    )
    )

  indice <- toupper(indice)
  stopifnot(indice %in% names(liste_indices))
  bandes_utiles <- liste_indices[[indice]]$bandes
  formule_indice <- liste_indices[[indice]]$formule

  if (is.character(polygone)) polygone <- sf::st_read(polygone, quiet = TRUE)
  if (is.na(sf::st_crs(polygone))) sf::st_crs(polygone) <- 4326
  polygone <- polygone |> sf::st_transform(4326) |> sf::st_union()

  if (is.null(annee_fin)) annee_fin <- lubridate::year(lubridate::today())
  if (!is.null(dossier)) dir.create(dossier, showWarnings = FALSE, recursive = TRUE)

  planetary_computer <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
  date_debut <- as.character(lubridate::as_date(paste0(annee_debut, "-", min(mois), "-01")))
  date_fin   <- as.character(lubridate::as_date(paste0(annee_fin, "-", max(mois) + 1, "-01")))

  query <- planetary_computer |>
    rstac::ext_filter(
      collection %in%  c("sentinel-2-l2a") &&
        t_intersects(datetime, {{rstac::cql2_interval(date_debut, date_fin)}}) &&
        s_intersects(geometry, {{rstac::cql2_bbox_as_geojson(sf::st_bbox(polygone))}}) &&
        `eo:cloud_cover` < {{max_nuage}}
    ) |>
    rstac::post_request() |>
    rstac::items_sign_planetary_computer()

  if (length(query$features) == 0) {
    warning("Aucune image trouvée.")
    return(NULL)
  }

  urls_band <- lapply(bandes_utiles, function(b) rstac::assets_url(query, b))
  names(urls_band) <- bandes_utiles
  scl_urls <- rstac::assets_url(query, "SCL")
  dates <- substr(rstac::items_datetime(query), 1, 10)
  verif_mois <- lubridate::month(lubridate::as_datetime(rstac::items_datetime(query))) %in% mois

  scl_urls <- rev(scl_urls[verif_mois])
  urls_band <- lapply(urls_band, function(u) split(rev(u[verif_mois]), dates))
  dates <- rev(dates[verif_mois])
  # Regrouper les scl_urls par date
  scl_urls_grouped <- split(scl_urls, dates)

  # Redéfinir les dates (une fois par groupe unique)
  dates <- names(scl_urls_grouped)
  if (!identical(nuage_dans_la_parcelle, FALSE)) {
    scl_vrts <- lapply(seq_along(scl_urls_grouped), function(i) {
      fichiers <- scl_urls_grouped[[i]]
      if (length(fichiers) > 1) {
        terra::vrt(paste0("/vsicurl/", fichiers))
      } else {
        terra::rast(paste0("/vsicurl/", fichiers))
      }
    })

    proportion_saine <- lapply(scl_vrts, function(r) {
      suppressWarnings(exactextractr::exact_extract(
        r, polygone,
        function(values, cov_frac) mean((values %in% 4:6), na.rm = TRUE),
        force_df = TRUE
      ))
    })

    acceptables <- sapply(proportion_saine, function(df) {
      mean(df[[1]] >= ((100 - nuage_dans_la_parcelle) / 100), na.rm = TRUE)
    })

    indices_valides <- which(acceptables == 1)

    scl_urls_grouped <- scl_urls_grouped[indices_valides]
    urls_band <- lapply(urls_band, function(u) u[indices_valides])
    dates <- dates[indices_valides]

    if (length(indices_valides) == 0) return(NULL)
  }

  groupes_par_date <- lapply(seq_along(dates), function(i) {
    lapply(names(urls_band), function(b) urls_band[[b]][i]) |> setNames(names(urls_band))
  })

  # Étape 2 : Pour chaque bande, créer une stack de VRT par date
  couches <- list()

  for (bande in names(urls_band)) {
    vrts_bande <- list()
    for (i in seq_along(groupes_par_date)) {
      fichiers <- groupes_par_date[[i]][[bande]]

      # Si plusieurs fichiers pour une même date/bande (chevauchement de tuiles)
      if (length(fichiers[[1]]) > 1) {
        vrt <- terra::vrt(paste0("/vsicurl/",fichiers[[1]]))
      } else {
        vrt <- terra::rast(paste0("/vsicurl/",fichiers[[1]]))
      }
      vrts_bande[[i]] <- vrt
    }
    couches[[bande]] <- terra::sprc(vrts_bande)
  }

  names(couches) <- names(urls_band)
  vecteur <- sf::st_transform(polygone, terra::crs(couches[[1]][1])) |> terra::vect()
  couches <- lapply(couches, function(r) terra::vrt(terra::crop(r, vecteur), options = "-separate"))
  if (indice == "NDRE") {
    couche_ref <- couches[["B08"]]
    couches[["B05"]] <- terra::resample(couches[["B05"]], couche_ref, method = "bilinear")
  }
  vi <- do.call(formule_indice, list(couches))
  vi = terra::crop(vi , vecteur, mask=T)
  names(vi) <- dates
  terra::varnames(vi) <- indice

  if (!is.null(dossier)) {
    for (i in 1:terra::nlyr(vi)) {
      terra::writeRaster(vi[[i]], file.path(dossier, paste0(indice, "_", names(vi[[i]]), ".tif")), overwrite = TRUE)
    }
  }

  vi
}
