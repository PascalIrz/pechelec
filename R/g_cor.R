#' Graphique de matrice de corrélation
#'
#' @param df Dataframe contenant uniquement les variables à corréler.
#' @param order Caractère. Ordre des variables sur le graphique qui est soit celui
#'     du dataframe ("original") soit fonction des proximités en CAH ("hclust").
#'
#' @return La graphique.
#' @export
#'
#' @importFrom corrplot corrplot.mixed
#' @importFrom Hmisc rcorr
#' @importFrom misty na.as
#'
#' @examples
g_cor <- function(df, order = "original") {

  M <- df %>%
    as.matrix() %>%
    Hmisc::rcorr(type = c("spearman"))

  corrplot.mixed(
    corr = M$r,
    p.mat = M$P %>%
      misty::na.as(na = 1),
    sig.level = .05,
    upper = "ellipse",
    tl.cex = 0.8,
    order = order
  )
}
