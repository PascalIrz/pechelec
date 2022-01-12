#' # fonction de mise en forme du df
#'
#' @param df Caractère. Nom du dataframe contenant les données.
#' @param ordre Vecteur entier servant à réordonner les colonnes.
#'
#' @return Le dataframe avec les colonnes réordonnées.
#' @export
#'
#' @importFrom dplyr select all_of
#'
#' @examples
mef_ordonner_vars <- function(df, ordre) {
  df %>%
    select(cond, temp, volt, puiss, intens, pen15c) %>%
    select(all_of(ordre))
}
