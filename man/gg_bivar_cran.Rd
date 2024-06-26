% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_bivar_cran.R
\name{gg_bivar_cran}
\alias{gg_bivar_cran}
\title{Produire un diagramme bivarié par cran de réglage}
\usage{
gg_bivar_cran(df, x, y, y_lab)
}
\arguments{
\item{df}{Dataframe contenant les données.}

\item{x}{Caractère. Nom de la variable à représenter en abscisse.}

\item{y}{Caractère. Nom de la variable à représenter en ordonnée}

\item{y_lab}{Caractère. Etiquette de l'axe des ordonnées.}
}
\value{
Le graphique ggplot2.
}
\description{
Produire un diagramme bivarié par cran de réglage
}
\examples{
\dontrun{
gg_bivar_cran(df = prov,
x = "conductivite",
y = "intensite",
y_lab = "Intensité (A)")
}
}
