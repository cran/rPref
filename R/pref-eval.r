

#' Preference selection
#' 
#' Evaluate a preference on a given dataset, i.e. return the maximal elements of a dataset for a given preference order.
#' 
#' @param df A dataframe or, for grouped preference selection, a grouped dataframe. See below for details.
#' @param pref The preference order, constructed via \code{\link{complex_pref}} and \code{\link{base_pref}}. 
#'             All variables occuring in \code{pref} must be columns of the dataframe \code{df} 
#'             (or can be variables of the environment, where \code{pref} was defined).
#' @param top By default \code{NULL}, which means that the maxima are returned. 
#'            For \code{top = k} the k-best elements according to the preference are returned.
#' 
#' @details
#' 
#' The difference between the two variants of the preference selection is:
#' 
#' \itemize{
#' \item The \code{psel} function returns a subset of the dataset which are the maxima according to the given preference. 
#' \item The function \code{psel.indices} returns just the row indices of the maxima.
#' Hence \code{psel(df,pref,top)} is equivalent to \code{df[psel(df,pref,top),]} for non-grouped dataframes. 
#' For grouped dataframes, the groups are restored after the preference selection.
#' }
#' 
#' For a given \code{top} value "k", the k best elements are returned. 
#' By definition, this is non-deterministic. A top-1 query of two equivalent tuples (according to \code{pref}) can return on both of these tuples.
#' In the rPref implementation, in this case the first occuring tuple in the dataset is picked.
#' 
#' If the \code{top} value is greater than the number of elements in \code{df}, 
#' i.e., \code{top > nrow(df)} then all elements of \code{df} will be returned without further warning.
#' 
#' @section Grouped preference selection:
#' 
#' With \code{psel} it is also possible to perform a preference selection, where the maxima are calculated for every group seperatly. 
#' The groups have to be created with \code{\link{group_by}} from the dplyr package. The preference selection preserves the grouping, i.e.,
#' the \code{summarize} function from dplyr refers to the set of maxima of each group. 
#' This can be used to e.g. calculate the number of maxima in each group, see examples below.
#' 
#' A given top value k in connection with a grouped preference selection returns the k best values for each group. 
#' Hence if there are three groups in \code{df}, each containing at least 2 elements, and we have \code{top = 2} then 6 tuples will be returned.
#' 
#' @seealso See \code{\link{complex_pref}} how to construct a Skyline preference.
#' 
#' @name psel
#' @importFrom dplyr is.grouped_df regroup
#' @export
#' 
#' @examples
#' 
#' # Skyline and Top-K skyline
#' psel(mtcars, low(mpg) * low(hp))
#' psel(mtcars, low(mpg) * low(hp), top = 5)
#' 
#' # Visualize the skyline in a plot
#' sky1 <- psel(mtcars, high(mpg) * high(hp))
#' plot(mtcars$mpg, mtcars$hp)
#' points(sky1$mpg, sky1$hp, lwd=3)
#' 
#' # Grouped preference with dplyr
#' library(dplyr)
#' psel(group_by(mtcars, cyl), low(mpg))
#' 
#' # Return size of each maxima group
#' summarise(psel(group_by(mtcars, cyl), low(mpg)), n())
psel <- function(df, pref, top = NULL) {  
  
  res <- psel.indices(df, pref, top)
  
  # Do the preference selection
  if (!is.grouped_df(df)) return(df[res,]) # Usual preference selection
  else return(dplyr::regroup(df[res,], attr(df, 'vars'))) # Grouped preference selection - regroup!
   
}

#' @export
#' @rdname psel
#' @importFrom dplyr is.grouped_df 
psel.indices <- function(df, pref, top = NULL) {
  
  # Consistency check
  if (!is.null(top) && !is.singleint(top)) stop("Top must be a single integer value")
  
  # Precalculate score values for given preference
  scores <- pref$get_scorevals(1, df)$scores
  pref_serial <- pref$serialize()
    
  # Do the preference selection
  if (!is.grouped_df(df)) { # Usual preference selection
  
    if (is.null(top)) # No Top-K selection
      res <- pref_select_impl(scores, pref_serial, -1)
    else { # Top-K
      if (top <= 0) res <- NULL
      else res <- pref_select_impl(scores, pref_serial, top)
    }
  
  } else { # Grouped preference selection
   
    if (is.null(top)) # No Top-K selection
      res <- grouped_pref_sel_impl(df, scores, pref_serial, -1)
    else { # Grouped Top-K selection (i.e., Top-k per group)
      if (top <= 0) res = NULL
      else res <- grouped_pref_sel_impl(df, scores, pref_serial, top)
    }
  }
  
  # All C indices start at 0, and all R indices start at 1
  return(res+1)
}


# Helper
is.singleint <- function(val) (is.numeric(val) && length(val) == 1 && round(val) == val)
