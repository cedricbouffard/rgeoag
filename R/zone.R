#' Générer des zones de gestion à partir de superpixels
#'
#' Cette fonction applique l’algorithme SKATER sur des superpixels et leurs poids spatiaux,
#' afin de générer des zones de gestion respectant une contrainte de superficie minimale.
#'
#' @param s_poly Objet `sf` contenant les superpixels
#' @param w Objet de poids spatiaux (issu de `rgeoda::knn_weights` ou équivalent)
#' @param df Tableau de données associé, sans géométrie, incluant la colonne `area`
#' @param n_zones Nombre de zones de gestion souhaitées
#' @param superficie_min Surface minimale par zone (en mètres carrés)
#'
#' @return Un objet `sf` représentant les zones fusionnées par groupe
#' @export
#' @importFrom dplyr group_by summarise select
#' @importFrom rgeoda skater
zones <- function(s_poly, w, df, n_zones = 5, superficie_min = 10000) {
  z <- rgeoda::skater(
    k              = n_zones,
    w              = w,
    df             = df |> dplyr::select(-area),
    bound_variable = list(df$area),
    min_bound      = min(c(sum(df$area) / (n_zones + 1), superficie_min))
  )

  s_poly$cl <- z$Clusters

  s_poly |>
    dplyr::group_by(cl) |>
    dplyr::summarise()
}
