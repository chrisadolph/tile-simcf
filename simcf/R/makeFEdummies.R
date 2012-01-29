

makeFEdummies <- function(unit,names=NULL) {
  fe <- model.matrix(~factor(unit)-1)
  if (is.null(names)) {
    colnames(fe) <- unique(as.character(unit))
  } else {
    colnames(fe) <- names
  }
  fe
}
