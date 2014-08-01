#' Base preferences
#' 
#' Base preferences are used to describe the different goals of a preference query. 
#' 
#'
#' @name base_pref
#' @param expr Value which is the term for the current preference and should be minimized/maximized or, for boolean preferences, \code{TRUE}. 
#' The term \code{expr} can be just a single attribute an arbitrary expression, e.g., 
#' \code{low(a+2*b+f(c))}, where \code{a}, \code{b} and \code{c} are columns of the addressed dataset and \code{f} is a previously defined function.
#' 
#' @details
#' 
#' All base preferences are mathematically strict weak orders (irreflexive, transitive and negative transitive).
#' 
#' The three fundamental base preferences are:
#' 
#' \describe{
#'   \item{\code{low(a), high(a)}}{Search for minimal/maximal values of \code{a}, 
#'         i.e., the induced order is the "smaller than" or "greater than" order on the values of \code{a}.
#'         The values of \code{a} must be numeric values.}
#'   \item{\code{true(a)}}{Search for true values in logical expressions, i.e. \code{TRUE} is better than \code{FALSE}.
#'         The values of \code{a} must be logical values.}
#' }
#' 
#' Functions contained in \code{expr} are evaluated over the entire dataset, i.e., 
#' it is possible to use aggregate functions (\code{min}, \code{mean}, etc.). 
#' Note that all functions (and also variables not occuring in the dataset, where \code{pref} will be evaluated on)
#' must be defined in the same environment (e.g. function scope) as the base preference.
#' 
#' @seealso See \code{\link{complex_pref}} how to compose complex preferences to retrieve e.g. the Skyline.
#' See \code{\link{base_pref_macros}} for more base preferences.
#' 
#' @examples
#' # Define a preference with a score value combining mpg and hp
#' p1 <- high(4*mpg + hp)
#' # Perform the preference selection
#' psel(mtcars, p1)
#' 
#' # Define a preference with a given function
#' f <- function(x, y) (abs(x-mean(x))/max(x) + abs(y-mean(y))/max(y))
#' p2 <- low(f(mpg, hp))
#' psel(mtcars, p2)
NULL


#' @rdname base_pref
#' @export
low <- function(expr) {
  expr <- as.expression(substitute(expr))
  return(lowpref(expr, parent.frame()))
}

#' @rdname base_pref
#' @export
high <- function(expr) {
  expr <- as.expression(substitute(expr))
  return(highpref(expr, parent.frame()))
}

#' @rdname base_pref
#' @export
true <- function(expr) {
  expr <- as.expression(substitute(expr))
  return(truepref(expr, parent.frame()))
}
