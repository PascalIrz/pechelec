#' Produire un résumé mis en forme du modèle penny_15_centre ~ variable
#'
#' @param df Dataframe contenant les données.
#' @param var Caractère. Nom de la variable explicative.
#'
#' @return Une liste contenant les dirrérents éléments du modèle (R2, p-value, etc.).
#' @export
#'
#' @importFrom stats lm summary.lm anova
#' @importFrom magrittr set_names
#' @importFrom dplyr select everything slice
#'
#' @examples
#' #' @examples \dontrun{
#' model_bivar_penny(df = prov, var = "intensite")
#' }
model_bivar_penny <- function(df, var)

  {

  mod <- lm(penny_15_centre ~ get(var), data = df)

  coef <- mod %>%
    summary() %>%
    .$coefficients %>%
    .[1:2] %>%
    t() %>%
    as.data.frame() %>%
    set_names(c("b", "a"))

  pval <- mod %>%
    anova() %>%
    .$"Pr(>F)"

  rsq <- mod %>%
    summary() %>%
    .$"r.squared"

  cbind(coef, pval, rsq) %>%
    slice(1) %>%
    mutate_at(vars(a, b), round, 3) %>%
    mutate(
      rsq = round(rsq, 2),
      sig = ifelse(pval > 0.05, "NS",
                   ifelse(
                     pval > 0.01, "*",
                     ifelse(pval > 0.001, "**", "***")
                   )),
      variable = var,
      pval = round(pval, 5)
    ) %>%
    select(variable,
           a,
           everything())

}
