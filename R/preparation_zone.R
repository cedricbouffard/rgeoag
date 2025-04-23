#' Préparer les superpixels et données pour les zones de gestion
#'
#' Cette fonction aligne une liste de rasters, applique un lissage optionnel,
#' crée des superpixels (supercells), calcule les poids spatiaux et génère un
#' tableau de données avec les superficies.
#'
#' @param raster_liste Liste de `SpatRaster`
#' @param k_supercell Nombre de superpixels à générer
#' @param fenetre_focale Taille du filtre médian (>1 applique un lissage)
#' @param normaliser Logique. Si TRUE, normalise les rasters entre 0 et 1
#' @param methode_resample Méthode de rééchantillonnage (ex : "bilinear", "near")
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{s_polygone}{Les superpixels (`sf`)}
#'   \item{poids}{Les poids spatiaux (`rgeoda`)}
#'   \item{donnees}{Données tabulaires sans géométrie}
#' }
#' @export
#' @importFrom dplyr mutate select
#' @importFrom sf st_area st_drop_geometry
preparation_zones <- function(raster_liste,
                              k_supercell = 1000,
                              fenetre_focale = 3,
                              normaliser = TRUE,
                              methode_resample = "bilinear") {

  r_stack <- aligner_rasters(
    raster_liste = raster_liste,
    fenetre_focale = fenetre_focale,
    normaliser = normaliser
  )

  s_polygone <- supercells::supercells(
    x           = r_stack,
    k           = k_supercell,
    compactness = 1,
    dist_fun    = "euclidean"
  )

  poids <- rgeoda::knn_weights(s_polygone, k = 8)

  donnees <- s_polygone |>
    dplyr::mutate(area = as.numeric(sf::st_area(s_polygone))) |>
    dplyr::select(-x, -y, -supercells) |>
    sf::st_drop_geometry()

  list(
    s_polygone = s_polygone,
    poids      = poids,
    donnees    = donnees
  )
}
