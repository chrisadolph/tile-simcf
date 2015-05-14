#' Create a matrix of dummy variables
#' 
#' Creates a matrix of dummy variables for use in model matrix of fixed effects
#' specifications
#' 
#' 
#' @param unit numeric or character vector of group numbers or names, one for
#' each observation
#' @param names character vector of group names, one for each unique group
#' number in \code{unit}
#' @return Returns a matrix of binary variables, one row for each observation,
#' and one column for each group
#' @author Christopher Adolph <\email{cadolph@@u.washington.edu}>
#' @keywords design
#' @export
makeFEdummies <- function(unit,names=NULL) {
  fe <- model.matrix(~factor(unit)-1)
  if (is.null(names)) {
    colnames(fe) <- unique(as.character(unit))
  } else {
    colnames(fe) <- names
  }
  fe
}
