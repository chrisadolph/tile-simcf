#' ~~function to do ... ~~
#' 
#' ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' ~~ If necessary, more details than the description above ~~
#' 
#' @param \dots ~~Describe \code{\dots{}} here~~
#' @return ~Describe the value returned If it is a LIST, use \item{comp1
#' }{Description of 'comp1'} \item{comp2 }{Description of 'comp2'} ...
#' @author ~~who you are~~
#' @seealso ~~objects to See Also as \code{\link{help}}, ~~~
#' @references ~put references to the literature/web site here ~
#' @keywords dplot list
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' 
#' 
#' @export riceplot
"riceplot" <-
function(...){  
  args <- list(...,graphic="riceplot")
  class(args) <- c(class(args),"tileTrace","riceplot")
  args
}

