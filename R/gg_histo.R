#' Produire l'histogramme d'une des variables du jdd
#'
#' @param x Caract-re. Nom de la variable à représenter.
#' @param data Caractère. Nom du dataframe contenant les données.
#' @param xlab Caractère. Etiquette de l'axe des x.
#' @param type Caractère. Type de dataframe, soit "pêches", soit "stations".
#' @param couleur Caractère. Couleur de remplissage des barres de l'histogramme.
#'
#' @return Un histogramme ggplot
#' @export
#' @importFrom dplyr select pull
#' @importFrom ggplot2 ggplot geom_histogram labs geom_vline aes theme element_text scale_y_continuous
#' @importFrom scales number_format
#'
#' @examples
gg_histo <-
  function(x, data, xlab, type = c("pêches", "stations"), couleur = "#D95F02") {
    if (type == "stations") {
      data <- data %>%
        select(station, x) %>%
        unique()
    }
    if (type == "stations") {
      ylab <- "Nb de stations"
    } else {
      ylab <- "Nb de pêches"
    }

    moy <- data %>%
      pull(x) %>%
      mean(na.rm = TRUE)

    range <- data %>%
      pull(x) %>%
      range(na.rm = TRUE)

    amp <- range[2] - range [1]

    label = paste0("moyenne : ",
                   round(moy, digits = 2))

    ggplot(data = data,
           aes(x = get(x))) +
      geom_histogram(bins = 10,
                     fill = couleur) +
      labs(x = paste0(xlab, " - ", label),
           y = ylab) +
      geom_vline(
        xintercept = moy,
        linetype = "longdash",
        size = 0.7,
        col = "limegreen"
      ) +
      theme(text = element_text(size = 20)) +
      scale_y_continuous(labels = number_format(accuracy = 1))

  }