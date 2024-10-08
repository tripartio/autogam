% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autogam.R
\name{plot.autogam}
\alias{plot.autogam}
\title{Plot Method for autogam Objects}
\usage{
\method{plot}{autogam}(x, ...)
}
\arguments{
\item{x}{An object of class \code{autogam}.}

\item{...}{Additional arguments passed to other methods.}
}
\value{
Same return object as \code{\link[mgcv:print.gam]{mgcv::print.gam()}}.
}
\description{
This function plots an \code{autogam} object. It calls the \code{mgcv::gam} object \code{\link[mgcv:plot.gam]{mgcv::plot.gam()}} method.
}
