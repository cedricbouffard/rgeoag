#' Écrire un fichier AGS à partir d'un objet sf
#'
#' @param sf_obj Un objet `sf` avec des géométries de points
#' @param fichier_sortie Chemin de base pour le fichier de sortie (sans extension)
#' @param colonnes Liste nommée : noms internes des colonnes à écrire = noms d'en-tête souhaités
#'                 Exemple : list(lat = "*Lattitude (deg)", lon = " Longitude (deg)", elev = "Elevation Existing (m)", info = "Code")
#' @return Rien. Écrit un fichier `.ags`.
#' @export
sf_ags <- function(sf_obj, fichier_sortie,
                       colonnes = list(
                         lat = "*Lattitude (deg)",
                         lon = " Longitude (deg)",
                         elev = "Elevation Existing (m)",
                         info = "Code"
                       )) {
  # Vérifie et ajoute lat/lon si absent
  noms_col <- names(sf_obj)
  if (!("lat" %in% noms_col && "lon" %in% noms_col)) {
    coords <- sf_obj |>
      sf::st_transform(4326) |>
      sf::st_coordinates() |>
      tibble::as_tibble() |>
      dplyr::rename(lon = X, lat = Y)

    sf_obj <- sf_obj |>
      dplyr::bind_cols(coords)
  }

  # Construction du tableau de sortie avec noms choisis
  tableau <- sf_obj |> sf::st_drop_geometry() |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::all_of(names(colonnes))) |>
    rlang::set_names(unlist(colonnes))

  # Écriture du fichier .ags
  utils::write.table(
    tableau,
    file = paste0(tools::file_path_sans_ext(fichier_sortie), ".ags"),
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE,
    sep = "  "
  )
}
