#' Constructor of a terminator object
#'
#' Terminator objects are used to write termination conditions
#' that can be tested via a callback function within another function
#' during its evaluation.
#'
#' Terminator objects are similar to tracer objects but serve a different
#' purpose. Like tracer objects, they can be used to trace, print and
#' plot values of a variable within the evaluation environment of another
#' function during its evaluation. The primary purpose of a terminator
#' object is, however, to evaluate a termination condition, which can
#' trigger termination of a loop. Terminator objects do not save trace
#' information and do not trace runtime.
#'
#' @param cond  a termination condition. Either an expression or call that
#'              evaluates to a logical.
#' @param Delta an integer specifying how often the termination condition
#'              is evaluated. \code{Delta = 0} means never, and otherwise
#'              the condition is evaluated every \code{Delta}-th iteration.
#'              \code{Delta = 1} is the default.
#' @param print a variable name to print or a logical. If FALSE, nothing is
#'              printed. If a variable name is given, that variable (if it exists)
#'              will be printed every \code{Delta}-th iteration. If TRUE (the
#'              default) the last variable in \code{cond} is printed
#'              every \code{Delta}-th iteration.
#' @param plotter an expression, possibly created by the \code{plotter} function.
#' @param ...     other arguments passed to \code{format} for printing and
#'                \code{plot.window}
#'
#' @return A terminator object containing the functions \code{terminator} and
#' \code{clear}.
#' @export
#'
terminator <- function(
  cond = FALSE,
  Delta = 1,
  print = TRUE,
  plotter = NULL,
  ...
) {
  if (isTRUE(print)) {
    var <- all.names(cond, functions = FALSE)
    var <- var[length(var)]
  } else {
    if (is.character(print)) {
      var <- print
      print <- TRUE
    } else {
      print <- FALSE
    }
  }
  cond_expr <- is.call(cond) || is.expression(cond)
  plotter_expr <- is.call(plotter) || is.expression(plotter)
  # Environment for evaluation of the condition
  env <- new.env()
  env$.__n <- 1
  if (plotter_expr) {
    new_plotter_window(...)
  }

  terminator <- function() {
    terminate <- isTRUE(cond)
    n <- env$.__n
    if (isTRUE(Delta > 0) && n %% Delta == 0) {
      # The condition is evaluated in its own environment to ensure that it
      # does not accidentally overwrite variables in the calling environment.
      # To give the expression access to the calling environment it is assigned
      # as the enclosing environment.
      parent.env(env) <- parent.frame()
      if (cond_expr) {
        terminate <- tryCatch(eval(cond, envir = env),
          error = function(e) warning(e))
      }
      terminate <- isTRUE(terminate)
      if (plotter_expr) {
        tryCatch(eval(plotter, envir = env),
          error = function(e) warning(e))
      }

      if (print) {
        values <- mget(var, envir = env, ifnotfound = NA)
        cat(paste("n = ", n, ": ", var, " = ", format(values, ...), "; ", sep = ""),
          "terminate: ", terminate,
          "\n", sep = "")
      }
    }
    env$.__n <- n + 1
    invisible(terminate)
  }

  clear <- function(plotter = FALSE) {
    env <<- new.env()
    env$.__n <- 1
    plotter_expr_mod <- is.call(plotter) || is.expression(plotter)
    if (plotter_expr_mod) {
      plotter_expr <<- plotter_expr_mod
      plotter <<- plotter
      plotter <- FALSE
    }
    if (plotter_expr && isTRUE(plotter)) {
      new_plotter_window(...)
    }
  }

  structure(list(terminator = terminator, clear = clear), class = "terminator")
}

#' Print terminator information
#'
#' @param x   a terminator object
#' @param ... other arguments (currently ignored)
#'
#' @return \code{print}
#' @export
print.terminator <- function(x, ...) {
  env <- environment(x$terminator)$env
  values <- mget(ls(env, ...), env)
  print(values)
}
