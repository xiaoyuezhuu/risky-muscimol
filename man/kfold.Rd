% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{kfold}
\alias{kfold}
\title{kfold}
\usage{
kfold(K_FOLD = 10, fit_model, model = "rho-only")
}
\arguments{
\item{K_FOLD:}{the number of folds}

\item{fit_model:}{a compiled stan model object}

\item{model:}{'rho-only', 'omega-only', 'rho-and-omega'}
}
\value{
dataframe of ELPD
}
\description{
the generic function for manual K-fold cross validation
}
