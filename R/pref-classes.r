

# Preference classes
# ------------------

# All functions are internal!

# General preference (only for internal use)
preference <- setRefClass("preference",
  methods = list(
    show = function() {
      return(cat(paste0('[Preference] ', unbrace(.self$get_str()))))
    },
    
    get_str = function(parent_op = "") {
      return("((empty))")
    }
  )
)
is.preference <- function(x) inherits(x, "preference")

# cmp/eq functions are not needed for C++ BNL algorithms, but for igraph, etc.

# Base preferences 
basepref <- setRefClass("basepref", 
  contains = "preference",
  fields = list(expr = "expression", score_id = "numeric", eval_frame = "environment"),
  methods = list(
    initialize = function(expr_ = expression(), eval_frame_ = environment()) {
      .self$expr <- expr_
      .self$eval_frame <- eval_frame_
      return(.self)
    },
    
    get_scorevals = function(next_id, df) {
      .self$score_id = next_id
      # Calc score of base preference
      scores <- .self$calc_scores(df, .self$eval_frame)
      # Check if length ok
      if (length(scores) != nrow(df)) {
        if (length(scores) == 1) scores <- rep(scores, nrow(df))
        else stop(paste0("Evaluation of base preferences ", .self$get_str(), " does not have the same length as the dataset!"))
      }
      return(list(next_id = next_id+1, scores = as.data.frame(scores)))
    },
    
    cmp = function(i, j, score_df) { # TRUE if i is better than j
      return(score_df[i, score_id] < score_df[j, score_id])
    },
    
    eq = function(i, j, score_df) { # TRUE if i is equal to j
      return(score_df[i, score_id] == score_df[j, score_id])
    },
    
    get_str = function(parent_op = "") {
      return(paste0(.self$op(), '(', as.character(.self$expr), ')'))
    },
    
    serialize = function() {
      return(list(kind = 's'));
    }
  )
)
is.basepref <- function(x) inherits(x, "basepref")


lowpref <- setRefClass("lowpref", 
  contains = "basepref",
  methods = list(
    op = function() 'low',
    
    calc_scores = function(df, frm) {
      res = eval(.self$expr, df, frm)
      if (!is.numeric(res)) stop("For a low preference the expression must be numeric!")
      return(res)        
    }
  )
)
is.lowpref <- function(x) inherits(x, "lowpref")


highpref <- setRefClass("highpref", 
  contains = "basepref",
  methods = list(
    op = function() 'high',
    
    calc_scores = function(df, frm) {
      res = eval(.self$expr, df, frm)
      if (!is.numeric(res)) stop("For a high preference the expression must be numeric!")
      return(-res)
    }
  )
)
is.highpref <- function(x) inherits(x, "highpref")


truepref <- setRefClass("truepref", 
  contains = "basepref",
  methods = list(
    op = function() 'true',
    
    calc_scores = function(df, frm) {
      res = eval(.self$expr, df, frm)
      if (!is.logical(res)) stop("For a true preference the expression must be logical!")
      return(1 - as.numeric(res))
    }
  )
)
is.truepref <- function(x) inherits(x, "truepref")


# Reverse preference (revrsing the order)
reversepref <- setRefClass("reversepref",
  contains = "preference",
  fields = list(p = "preference"),
  methods = list(
    initialize = function(p_) {
      .self$p <- p_
      return(.self)
    },
    
    get_scorevals = function(next_id, df) {
      res <- p$get_scorevals(next_id, df)
      return(list(next_id = res$next_id + 1, scores = res$scores))
    },
    
    get_str = function(parent_op = "") {
      return(paste0('-', .self$p$get_str(parent_op)))
    },
    
    cmp = function(i, j, score_df) { # TRUE if i is better than j
      return(p$eq(j, i, score_df))
    },
    
    eq = function(...) p$eq(...),
    
    serialize = function() {
      return(list(kind = '-', p = .self$p$serialize()));
    }
  )
)
is.reversepref <- function(x) inherits(x, "reversepref")


# Binary complex preferences
complexpref <- setRefClass("complexpref",
  contains = "preference",
  fields = list(p1 = "preference", p2 = "preference", op = "character", 
                cmp = "function", eq = "function"),
  methods = list(
    initialize = function(p1_, p2_, op_, cmp_, eq_) {
      .self$p1  <- p1_
      .self$p2  <- p2_
      .self$op  <- op_
      .self$cmp <- cmp_
      .self$eq  <- eq_
      return(.self)
    },
    
    get_scorevals = function(next_id, df) {
      res1 <- p1$get_scorevals(next_id, df)
      res2 <- p2$get_scorevals(res1$next_id, df)
      return(list(next_id = res2$next_id + 1, scores = cbind(res1$scores, res2$scores)))
    },
    
    get_str = function(parent_op = "") {
      res <- paste0(.self$p1$get_str(.self$op), ' ', op, ' ', .self$p2$get_str(.self$op))
      if (.self$op != parent_op) res <- embrace(res)
      return(res)
    },
    
    # Serialization for C++ Interface
    serialize = function() {
      return(list(kind = .self$op, p1 = .self$p1$serialize(), p2 = .self$p2$serialize()));
    }
  )
)
is.complexpref <- function(x) inherits(x, "complexpref")


# Some helper functions
# ---------------------

# Remove brackets from string if existing
unbrace <- function(x) {
  if (substr(x, 1, 1) == "(" && substr(x, nchar(x), nchar(x)) == ")") return(substr(x, 2, nchar(x)-1))
  else return(x)
}

# Add brackets to string if not existing
embrace <- function(x) {
  if (!(substr(x, 1, 1) == "(" && substr(x, nchar(x), nchar(x)) == ")")) return(paste0('(', x, ')'))
  else return(x)
}
  
  