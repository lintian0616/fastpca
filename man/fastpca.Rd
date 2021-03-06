% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fastpca.R
\name{fastpca}
\alias{fastpca}
\title{Fast PCA}
\usage{
fastpca(expr, scale = FALSE, k = 50)
}
\arguments{
\item{expr}{numeric matrix with data.}

\item{scale}{logical. Whether to scale the data.}

\item{k}{numeric. The number of components to compute.}
}
\value{
numberic matrix with the principal components.
}
\description{
This function implements a fast version of PCA
}
\examples{
x <- matrix(data=rnorm(30), nrow=10, ncol=3)
pca <- fastpca(x, k=2)

}
