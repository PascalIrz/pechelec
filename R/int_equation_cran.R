#' Produire l'équation mise en forme d'une régression, pour un cran de réglage donné.
#'
#' @param df Dataframe contenant les données.
#' @param y Caractère. Nom de la variable quantitative dépendante (ordonnée).
#' @param x Caractère. Nom de la variable quantitative explicative (abscisse).
#' @param sel_cran Entier. Cran de réglage sélectionné (entre 2 et 4).
#'
#' @return L'équation au format texte.
#' @export
#' @importFrom dplyr filter mutate slice mutate_at vars mutate case_when
#' @importFrom stats lm summary.lm anova
#' @importFrom magrittr set_names
#'
#' @examples
int_equation_cran <- function(df,
                              y,
                              x,
                              sel_cran)

{
  mod <- lm(formula = get(y) ~ get(x),
            data = df %>%
              filter(cran == sel_cran))

  coef <- mod %>%
    summary() %>%
    .$coefficients %>%
    .[1:2] %>%
    t() %>%
    as.data.frame() %>%
    set_names(c("b", "a")) %>%
    mutate(cran = sel_cran)

  pval <- mod %>%
    anova() %>%
    .$"Pr(>F)"

  rsq <- mod %>%
    summary() %>%
    .$"r.squared"

  tab <- cbind(coef, pval, rsq) %>%
    slice(1) %>%
    mutate_at(vars(a, b), round, 3) %>%
    mutate(
      rsq = round(rsq, 2),
      sig = case_when(
        pval > 0.05 ~ "NS",
        pval <= 0.05 & pval > 0.01 ~ "*",
        pval <= 0.01 & pval > 0.001 ~ "**",
        TRUE ~ "***"
      )
    )

  signe <- ifelse(tab$b > 0, "+", "")

  label <- paste0(
    "Cran ",
    tab$cran,
    " : y=",
    tab$a,
    "x",
    signe,
    tab$b,
    "(",
    tab$sig,
    ")  ",
    "r\U00B2=",
    round(rsq, 2)
  )

}
