# Make a simcf function to do all this from forumla, etc)
extractdata <- function(formula, data, extra=NULL, na.rm = FALSE) {
  subdata <- get_all_vars(formula,data)
  if (!is.null(extra)) {
    if (is.data.frame(extra)) {
      subdata <- cbind(subdata,extra)
    } else {
      if (any(class(extra)=="formula")) {
        subdata <- cbind(subdata, get_all_vars(extra,data) )
      } else {
                                        # to implement
        stop("Extra must be a dataframe or formula object")
      }
    }
  }
  if (na.rm)
    subdata <- na.omit(subdata)
  subdata
}
