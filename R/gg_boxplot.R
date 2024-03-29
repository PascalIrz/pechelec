#' Produire un boxplot sur les stations ou les observations
#'
#' @param df Dataframe contenant les données.
#' @param var_quant Variable quantitative à représenter (ordonnée).
#' @param var_quant_lab Caractère. Etiquette de l'axe des y.
#' @param var_qual Variable qualitative à représenter (abscisse).
#' @param var_qual_lab Caractère. Etiquette de l'axe des x.
#' @param couleur Caractère. Couleur de remplissage des barres de l'histogramme.
#'
#' @return Un boxplot ggplot2.
#' @export
#' @importFrom dplyr filter pull enquo
#' @importFrom ggplot2 ggplot geom_boxplot labs aes theme element_text
#'
#' @examples \dontrun{
#' gg_boxplot(
#'   df = data,
#'   var_quant = voltage,
#'   var_quant_lab = "Voltage (V)",
#'   var_qual = cran,
#'   var_qual_lab = "Cran de réglage du Héron"
#' )
#' }
gg_boxplot <- function(df,
                       var_quant,
                       var_quant_lab,
                       var_qual,
                       var_qual_lab,
                       couleur = "#7570B3")
{
  var_quant <- enquo(var_quant)
  var_qual <- enquo(var_qual)

  df <- df %>%
    filter(!is.na(!!var_quant) &
             !is.na(!!var_qual))

  quantiles <- df %>%
    pull(!!var_quant) %>%
    quantile(probs = c(0.05, 0.95))

  ggplot(data = df,
         aes(x = as.factor(!!var_qual),
             y = !!var_quant)) +
    geom_boxplot(fill = couleur) +
    labs(x = var_qual_lab,
         y = var_quant_lab) +
    theme(text = element_text(size = 20)) #+
  #  coord_cartesian(ylim = quantiles)

}
