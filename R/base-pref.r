#' Base Preferences
#' 
#' Base preferences are used to describe the different goals (dimensions, in case of a Skyline query)
#' of a preference query. 
#' 
#'
#' @name base_pref
#' @param expr A numerical/logical expression which is the term to evaluate for the current preference. 
#'       The objective is to search for minimal/maximal values of this expression (for \code{low}/\code{high}) or for 
#'       logical \code{TRUE} values (for \code{true}).
#'       For \code{low_}, \code{high_} and \code{true_}, the argument must be an expression, a call or a string.
#' @param df (optional) A data frame, having the same structure (i.e., columns)
#'        like that data frame, where this preference is evaluated later on. 
#'        Causes a partial evaluation of the preference and the preference is associated with this data frame.
#'        See below for details.
#' @param x An object to be tested if it is a base preference.
#' 
#' @details
#' 
#' Mathematically, all base preferences are strict weak orders (irreflexive, transitive and negative transitive).
#' 
#' The three fundamental base preferences are:
#' 
#' \describe{
#'   \item{\code{low(a), high(a)}}{Search for minimal/maximal values of \code{a}, 
#'         i.e., the induced order is the "smaller than" or "greater than" order on the values of \code{a}.
#'         The values of \code{a} must be numeric values.}
#'   \item{\code{true(a)}}{Search for true values in logical expressions, i.e., \code{TRUE} is considered to be better than \code{FALSE}.
#'         The values of \code{a} must be logical values.
#'         For a tuplewise evaluation of a complex logical expression one has to use the \code{&} and \code{|} operators for logical AND/OR
#'         (and not the \code{&&} and \code{||} operators).}
#' }
#' 
#' 
#' The term \code{expr} may be just a single attribute or may contain an arbitrary expression,
#' depending on more than one attribute, e.g., \code{low(a+2*b+f(c))}.
#' There \code{a}, \code{b} and \code{c} are columns of the addressed data set and \code{f} has to be a previously defined function.
#' 
#' Functions contained in \code{expr} are evaluated over the entire data set, i.e., 
#' it is possible to use aggregate functions (\code{min}, \code{mean}, etc.). 
#' Note that all functions (and also variables which are not columns of the data set, where \code{expr} will be evaluated on)
#' must be defined in the same environment (e.g., environment of a function or global environment) as the base preference is defined.
#' 
#' The function \code{is.base_pref} returns \code{TRUE} if \code{x} is a preference object and \code{FALSE} otherwise.
#' 
#' 
#' @section Using Expressions in Preferences:
#' 
#' The \code{low_}, \code{high_} and \code{true_} preferences have the same functionality
#' as \code{low}, \code{high} and \code{true} 
#' but expect an expression, a call or a string as argument.
#' For example, \code{low(a)} is equivalent to \code{low_(expression(a))} or \code{low_("a")}. 
#' Lazy expressions (see the lazyeval package) are also possible.
#' 
#' 
#' This is helpful for developing your own base preferences. Assume you want to define a base Preference \code{false}
#' as the dual of \code{true}. A definition like \code{false <- function(x) -true(x)} is the wrong approach, as 
#' \code{psel(data.frame(a = c(1,2)), false(a == 1))} will result in the error "object 'a' not found".
#' This is because \code{a} is considered as a variable and not as an (abstract) symbol to be evaluated later.
#' By defining
#' 
#' \code{false <- function(x, ...) -true_(substitute(x), ...)}
#' 
#' one gets a preference which behaves like a "built-in" preference.  
#' Additional optional parameters (like \code{df}) are bypassed.
#' The object \code{false(a == 1)} will output 
#' \code{[Preference] -true(a == 1)} on the console and 
#' \code{psel(data.frame(a = c(1,2)), false(a==1))} returns correctly the second tuple with \code{a==2}.
#' 
#' There is a special symbol \code{df__} which can be used in preference expression to access the given 
#' data set \code{df}, when \code{\link{psel}} is called on this data set. 
#' For example, on a data set where the first column has the name \code{A}
#' the preference \code{low(df__[[1]])} is equivalent to \code{low(A)}.
#' 
#' 
#' @section Partial Evaluation and Associated Data Frames:
#' 
#' If the optional parameter \code{df} is given, 
#' then the expression is evaluated at the time of definition as far as possible.
#' All variables occurring as columns in \code{df} remain untouched. For example, consider
#' 
#' \code{f <- function(x) 2*x} \cr
#' \code{p <- true(cyl == f(1), mtcars)}
#' 
#' Then \code{p} is equivalent to the preference \code{true(cyl == 2)} as the variable \code{cyl} is a column in \code{mtcars}.
#' Additionally the data set \code{mtcars} is associated with the preference \code{p}, 
#' implying that the preference selection can be done with \code{\link{peval}}. 
#' See \code{\link{assoc.df}} for details on associated data sets.
#' 
#' The preference selection, i.e., \code{psel(mtcars, p)} can be invoked without the partial evaluation.
#' But this results in an error, if the function \code{f} has meanwhile removed from the current environment.
#' Hence it is safer to do an early partial evaluation of all preferences, as far as they contain user defined functions.
#' 
#' The partial evaluation can be done manually by \code{\link{partial.eval.pref}}.
#' 
#' 
#' @seealso See \code{\link{complex_pref}} how to compose complex preferences to retrieve e.g., the Skyline.
#' See \code{\link{general_pref}} for functions applying to all kind of preferences.
#' See \code{\link{base_pref_macros}} for more base preferences.
#' 
#' @examples
#' # Defines a preference with a score value combining mpg and hp.
#' p1 <- high(4 * mpg + hp)
#' # Perform the preference selection:
#' psel(mtcars, p1)
#' 
#' # Defines a preference with a given function.
#' f <- function(x, y) (abs(x - mean(x))/max(x) + abs(y - mean(y))/max(y))
#' p2 <- low(f(mpg, hp))
#' psel(mtcars, p2)
#' 
#' # Use partial evaluation for weighted scoring.
#' p3 <- high(mpg/sum(mtcars$mpg) + hp/sum(mtcars$hp), df = mtcars)
#' p3
#' # Select Pareto optima.
#' peval(p3)
NULL

#' @rdname base_pref
#' @export
low <- function(expr, df = NULL) {
  p <- methods::new("lowpref", get.lazy(substitute(expr), parent.frame()))
  return(assoc.composed.df(p, compose.df(df, substitute(df))))
}

#' @rdname base_pref
#' @export
low_ <- function(expr, df = NULL) {
  p <- methods::new("lowpref", get.lazy(expr, parent.frame()))
  return(assoc.composed.df(p, compose.df(df, substitute(df))))
}

#' @rdname base_pref
#' @export
high <- function(expr, df = NULL) {
  p <- methods::new("highpref", get.lazy(substitute(expr), parent.frame()))
  return(assoc.composed.df(p, compose.df(df, substitute(df))))
}

#' @rdname base_pref
#' @export
high_ <- function(expr, df = NULL) {
  p <- methods::new("highpref", get.lazy(expr, parent.frame()))
  return(assoc.composed.df(p, compose.df(df, substitute(df))))
}

#' @rdname base_pref
#' @export
true <- function(expr, df = NULL) {
  p <- methods::new("truepref", get.lazy(substitute(expr), parent.frame()))
  return(assoc.composed.df(p, compose.df(df, substitute(df))))
}

#' @rdname base_pref
#' @export
true_ <- function(expr, df = NULL) {
  p <- methods::new("truepref", get.lazy(expr, parent.frame()))
  return(assoc.composed.df(p, compose.df(df, substitute(df))))
}

#' @rdname base_pref
#' @export
is.base_pref <- function(x) {
  inherits(x, "basepref")
}

# Helper functions
# ----------------


# transform chracter/expression in lazy_eval
#' @importFrom lazyeval as.lazy
get.lazy <- function(expr_chr, env) {
  # Unfold expression, if expression object given
  if (is.expression(expr_chr)) expr_chr <- expr_chr[[1]]
  # Create lazy_eval object
  # Use given environment if expr_chr does not already have an associated environment
  return(as.lazy(expr_chr, env))
}

# Compose a data frame and its provenience to a list
compose.df <- function(df, df_call) {
  if (is.null(df)) {
    return(list())
  } else {
    df_name <- as.character(as.expression(df_call))
    return(list(df = df, info_str = paste0(class(df)[1], ' "', df_name, '" [', nrow(df), ' x ', ncol(df), ']')))
  }
}

# Add a composed data.frame to a preference object and return it (this is not more a reference!)
assoc.composed.df <- function(object, composed_df) {
  object@df_src <- composed_df
  if (length(composed_df) != 0) {
    object <- evaluate(object, get_static_terms(composed_df$df))
  }
  return(object)
}
