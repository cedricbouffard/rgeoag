#' Charger un fichier SVY et retourner un objet sf
#'
#' @param fichier Chemin vers un fichier .SVY
#' @return Un objet `sf` avec les colonnes lat, lon, elev et info
#' @export

svy_sf <- function(fichier) {
  donnees <- read.delim(fichier, quote = "\"", header = FALSE) |>
    dplyr::as_tibble() |>
    rlang::set_names("a") |>
    dplyr::filter(stringr::str_detect(a, "lla")) |>
    dplyr::slice(-1) |>
    tidyr::separate(a,
                    into = c('x','y','z','lat','lon','elev','t','dt','a','b','c','d','e','f','g','h','i','j','k','l'),
                    sep = ",", fill = "right") |>
    dplyr::select(where(~ !all(is.na(.x))))
  names(donnees)[length(names(donnees))]  <-  'info'
  donnees <- donnees|>
    dplyr::mutate(
      lat = as.numeric(stringr::str_remove(lat, "lla\\(")),
      lon = as.numeric(lon),
      elev = as.numeric(elev),
      info = stringr::str_remove_all(stringr::str_remove(info, " ]."), "'")
    ) |>
    dplyr::select(lat, lon, elev, info) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  return(donnees)
}
