#' Useful base preference macros
#' 
#' In addition to the fundamental base preferences, rPref offers some macros to define preferences where a given interval or point is preferred. 
#'
#' @name base_pref_macros
#' @param expr The value which should be in the preferred interval, layer, etc. The same requirements as for \code{\link{base_pref}} apply.
#' @param center Preferred value, where a values from the given \code{expr} should be near by.
#' @param left Lower limit for a preferred interval.
#' @param right Upper limit for a preferred interval.
#' @param pos_value The preferred value or set for a pos-preference.
#' @param ... Layers (sets) for a layered-Preference, where the first set are the most preferred values.
#' 
#' @section Definition of the preference macros:
#' 
#' \describe{
#'   \item{\code{between(expr, left, right)}}{Those tuples are preferred, where \code{expr} evaluates to a value between l and r.
#'   For values not in this interval, the values nearest to [l, r] are preferred.}
#'   \item{\code{around(expr, center)}}{Same as \code{between(expr, center, center)}.}
#'   \item{\code{pos(expr, pos_value)}}{If \code{expr} evaluates to a value which is contained in \code{pos_value}, these tuples are preferred.}
#'   \item{\code{layered(expr, layer1, layer2, ..., layerN)}}{For the most preferred tuples, \code{expr} must evaluate to a value in \code{layer1}. 
#'   The second-best tuples are those, where \code{expr} evaluates to a value in \code{layer2}, and so forth. 
#'   Values occuring in non of the layers are considered worse than those in \code{layerN}.
#'   Technically, this is realized by a Prioritization (lexicographical order) chain of boolean preferences.}
#' }
#'
#' @examples 
#' # Search for cars where mpg is near to 25
#' psel(mtcars, around(mpg, 25))
#' 
#' # cyl = 2 and cyl = 4 are equally good, cyl = 6 is worse
#' psel(mtcars, layered(cyl, c(2, 4), 6))
NULL


#' @rdname base_pref_macros
#' @export
around <- function(expr, center) {
  res <- expression(abs(X - Y))
  expr <- as.expression(substitute(expr))
  res[[1]][2][[1]][2] <- expr
  res[[1]][2][[1]][3] <- center
  return(lowpref(res, parent.frame()))
}

#' @rdname base_pref_macros
#' @export
between <- function(expr, left, right) {
  res <- expression(pmax(L - X, 0, X - R))
  expr <- as.expression(substitute(expr))
  res[[1]][2][[1]][2] <- left
  res[[1]][2][[1]][3] <- expr
  res[[1]][4][[1]][2] <- expr
  res[[1]][4][[1]][3] <- right
  return(lowpref(res, parent.frame()))
}

#' @rdname base_pref_macros
#' @export
pos <- function(expr, pos_value) {
  res <- expression(X %in% Y)
  expr <- as.expression(substitute(expr))
  res[[1]][2] <- expr
  res[[1]][3] <- pos_value
  return(truepref(res, parent.frame()))
}

#' @rdname base_pref_macros
#' @export
layered <- function(expr, ...) {
  sl <- substitute(list(...))
  if (length(sl) < 2) stop("Empty layered preference is not allowed!")
  expr <- as.expression(substitute(expr))
  prefs <- list()
  pos_expr <- expression(X %in% Y)
  pos_expr[[1]][2] <- expr
  for(i in 2:length(sl)) {
    pos_expr[[1]][3] <- as.expression(sl[[i]])
    prefs[[i-1]] = truepref(pos_expr, parent.frame())
  }
  return(Reduce(`&`, prefs))
}
