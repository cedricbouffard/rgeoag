#' Générer une carte topographique avec effets d’ombrage et lignes de contour
#'
#' Cette fonction crée une carte topographique à partir d’un raster d’élévation,
#' avec la possibilité d’ajouter un effet d’ombrage (hillshade) et des lignes de
#' contours selon un intervalle spécifié.
#'
#' @param raster_elevation Un objet `SpatRaster` ou un chemin vers un fichier `.tif`
#' @param intervalle (optionnel) Intervalle pour les lignes de contour (en mètres)
#' @param ombrage Logique. Si TRUE, ajoute un effet d’ombrage
#'
#' @return Une liste de couches ggplot à ajouter à une carte
#' @export
#' @importFrom terra focal terrain clamp values minmax as.contour rast
#' @importFrom purrr map
#' @importFrom sf st_as_sf
#' @importFrom ggplot2 geom_sf scale_fill_gradientn scale_fill_distiller
#' @importFrom tidyterra geom_spatraster
#' @importFrom ggnewscale new_scale_fill
geom_topo <- function(raster_elevation, intervalle = NULL, ombrage = TRUE) {
  r_smooth <- NULL

  if (inherits(raster_elevation, "SpatRaster")) {
    r <- raster_elevation
  } else if (is.character(raster_elevation)) {
    r <- terra::rast(raster_elevation)
  } else {
    stop("L'argument 'raster_elevation' doit être un objet SpatRaster ou un chemin vers un fichier .tif.")
  }

  # Générer un ombrage si demandé
  if (ombrage) {
    r_smooth <- terra::focal(r, w = 5, fun = "mean")
    pente <- terra::terrain(r_smooth * 30, "slope", unit = "radians")
    orientation <- terra::terrain(r_smooth * 30, "aspect", unit = "radians")
    hillmulti <- purrr::map(c(270, 15, 60, 330), function(dir) {
      terra::shade(pente, orientation, angle = 45, direction = dir, normalize = TRUE)
    }) |> terra::rast() |> sum()
    couche_ombrage <- hillmulti
  }

  # Générer les courbes de niveau si l'intervalle est fourni
  courbes <- NULL
  if (!is.null(intervalle)) {
    if (is.null(r_smooth)) {
      r_smooth <- terra::focal(r, w = 5, fun = "mean")
    }
    minmax_vals <- terra::minmax(r)
    niveaux <- seq(floor(minmax_vals[1, 1]), ceiling(minmax_vals[2, 1]), by = intervalle)
    courbes <- terra::as.contour(r, levels = niveaux) |> sf::st_as_sf()
  }

  # Calcul des quantiles pour couper les valeurs extrêmes
  q <- quantile(terra::values(r), probs = c(0.03, 0.97), na.rm = TRUE)
  r_clipped <- terra::clamp(r, lower = q[1], upper = q[2], values = TRUE)
  breaks <- pretty(c(q[1], q[2]), n = 7)

  # Palette de couleurs
  couleurs <- c("#800080", "#9A31FF", "#0066FF", "#1BF7C9", "#00CD00",
                "#FFFF00", "#FF8000", "#FF0000", "#B40000")
  palette <- if (ombrage) scales::alpha(couleurs, 0.7) else couleurs

  couches_base <- list(
    tidyterra::geom_spatraster(data = r_clipped, maxcell = Inf),
    ggplot2::scale_fill_gradientn(
      name = "Élévation (m)",
      colours = palette,
      na.value = "transparent",
      limits = c(q[1], q[2]),
      breaks = breaks,
      labels = round(breaks, 1)
    )
  )

  if (!is.null(courbes)) {
    couches_base <- append(couches_base, list(ggplot2::geom_sf(data = courbes, size = 0.1)))
  }

  if (ombrage) {
    return(c(
      list(
        tidyterra::geom_spatraster(data = couche_ombrage, maxcell = Inf, show.legend = FALSE),
        ggplot2::scale_fill_distiller(palette = "Greys", na.value = "transparent"),
        ggnewscale::new_scale_fill()
      ),
      couches_base
    ))
  } else {
    return(couches_base)
  }
}
