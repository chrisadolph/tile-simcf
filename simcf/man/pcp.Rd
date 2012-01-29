\name{pcp}
\alias{pcp.glm}
\alias{pcp.oprobit}
\alias{concord.glm}
\alias{concord.oprobit}
\title{Prediction-based goodness of fit measures for categorical models}
\description{Compute percent correctly predicted and concordance indexes for logistic regression, ordered probit, and similar models.}
\usage{
pcp.glm(res, y, type="model")
pcp.oprobit(x, y, b, constant=1, ncat=3, type="model")
concord.glm(res, y, type="model")
concord.oprobit(x, y, b, constant=1, ncat=3, type="model")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{A glm object, e.g., as produced by \code{\link{glm}}.}
  \item{y}{A numeric vector containing the categorical response variable.  Not implemented for factors.}
  \item{type}{Character, whether the function should return the goodness of fit for the estimated \code{model}, the goodness of fit for a \code{null} model, or the percentage \code{improve}-ment from the latter to the former.} 
  \item{x}{The matrix of covariates.}
  \item{b}{The vector of estimated parameters, with cutpoints at the end.  There should be k-2 cutpoints for a model with k response categories.  Not currently compatible with \code{polr}.}
  \item{constant}{Numeric, the position of the element of \code{b} containing the constant term; set to \code{NA} for no constant.}
  \item{ncat}{Number of response categories.}
}
\details{To add.
}
\value{
  Returns either the percent correctly predicted, the concordance index, or the percent improvement in these goodness of fit measures.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph <\email{cadolph@u.washington.edu}>}
%\seealso{\code{\link{}}, \code{\link{}}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}


