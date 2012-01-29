\name{mirrorRR}
\alias{mirrorRR}
\title{Label a relative risk axis on a plot of probabilities}
\description{
  If given probabilities which the user wishes to plot, suggests tick marks and labels for axes showing relative risks across the range of probabilities with respect to a reference level
}
\usage{
mirrorRR(limits, reference, type="auto", fd=TRUE,
         cut=c(1.175, 1.4, 2, 3.5, 11, 50, 500),
         base=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{limits}{}
  \item{reference}{}
\item{type}{}
\item{fd}{}
\item{cut}{}
\item{base}{}
}
\details{


}

\value{
  A list object containing \code{at}, a vector of locations for the relative risk tick marks, and \code{labels}, a vector of labels to plot at those tick marks.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph \email{cadolph@uw.edu}}

\seealso{\link{tile}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}

