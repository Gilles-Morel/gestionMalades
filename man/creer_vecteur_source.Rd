% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gradient_conjugue.R
\name{creer_vecteur_source}
\alias{creer_vecteur_source}
\title{Create Source Vector for Patients' Arrival}
\usage{
creer_vecteur_source(n, arrivee_constante = 10)
}
\arguments{
\item{n}{Number of resources (doctors)}

\item{arrivee_constante}{Constant arrival rate for patients}
}
\value{
Vector b representing patients' arrival rates
}
\description{
This function creates a source vector b for patients' arrival rates based on the number of resources (doctors) n
}
\examples{
# n <- 5
# Créons- un vecteur source b avec un taux d'arrivée constant de 10
# creer_vecteur_source(n, arrivee_constante=10)

}
