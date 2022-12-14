% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gapminder.R
\name{calcular_estatisticas}
\alias{calcular_estatisticas}
\title{Calcular estatisticas}
\usage{
calcular_estatisticas(continentes, anos = 2000:2022)
}
\arguments{
\item{continentes}{um vetor com o nome do(s) continente(s)}

\item{anos}{um vetor com os anos. o padrão é de 2000 até 2022.}
}
\value{
uma tibble
}
\description{
Essa função é usada para calcular estatísticas de população e
expectativa de vida, segundo os continentes e ano. Ela utiliza a base
gapminder.
}
\examples{
calcular_estatisticas("África")
}
