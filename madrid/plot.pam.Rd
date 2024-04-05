\name{plot.pam}

\alias{plot.pam}

\title{Plot a p-splines additive model}

\description{
  Plot a p-splines additive model.
}

\usage{
plot.pam <- function(model, components = 1:length(model$xlabels), plotinfo, 
                     options = list(), ...)
\method{plot}{pam}(x, ...)
}

\arguments{
\item{model, x}{the fitted p-splines additive model.  This is created by a call to the \code{sm} function.  It should be of class \code{pam}.}
\item{components}{a vector of indices which specify which of the model components should be plotted.  The names of the model components can be viewed by calling the function \code{summary} with \code{model} as its argument.}
\item{options}{options which control the plotting.}
\item{...}{Other control arguments.}
}

\details{
}

\value{
}

\examples{
\dontrun{
}}
