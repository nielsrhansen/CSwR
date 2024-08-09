#' Constructor of a tracer object
#'
#' Tracer objects can collect, print and summarize trace information from
#' the evaluation environment of other functions during their evaluation.
#'
#' The function \code{tracer} constructs a tracer object containing a
#' \code{tracer}, a \code{get} and a \code{clear} function. A call of
#' the objects \code{tracer} function can
#' be inserted in other functions and used to collect, print and plot trace information
#' about the internals of that function during its evaluation. The objects \code{get}
#' function can access that information afterwards, and its \code{clear} function deletes
#' all stored values in the tracer object.
#'
#' A tracer object can trace time (in seconds) between \code{tracer} calls,
#' which are measured by
#' the \code{hires_time} function from the bench package. There are \code{print}
#' and \code{summary} methods available for summarizing the trace information.
#' A call of the \code{tracer} function can be manually inserted into the body of
#' the function that is to be traced, it can be inserted using \code{base::trace},
#' or it can be passed as an argument to any function with a callback argument.
#'
#' @param objects a character vector of names of the objects that are to be traced.
#'                The objects are searched for in the calling environment of the
#'                \code{tracer} function. Objects created by the \code{expr} argument
#'                can also be traced.
#' @param Delta   an integer specifying if and how often trace information is printed.
#'                \code{Delta = 0} means never, and otherwise trace information is printed
#'                every \code{Delta}-th iteration. \code{Delta = 1} is the default.
#' @param save    a logical value. Determines if the trace information is stored.
#' @param time    a logical value. Determines if runtime information in seconds
#'                is traced.
#' @param expr    an expression that will be evaluated in an environment that has
#'                the calling environment of the \code{tracer} function as parent.
#' @param plotter an expression, possibly created by the \code{plotter} function.
#' @param ...     other arguments passed to \code{format} for printing and
#'                \code{plot.window}
#'
#' @return A tracer object containing the functions \code{tracer}, \code{get} and
#' \code{clear}.
#' @export
#'
#' @examples
#'
#' test_tracer <- tracer(c("m", "m_sq"), expr = quote(m_sq <- m^2))
#'
#' test <- function(n, cb = NULL) {
#'   for(i in 1:n) {
#'     m <- 2 * i
#'     Sys.sleep(0.1)
#'     if(!is.null(cb)) cb()
#'     }
#' }
#'
#' test(10, test_tracer$tracer)
#' summary(test_tracer)
tracer <- function(
  objects = NULL,
  Delta = 1,
  save = TRUE,
  time = TRUE,
  expr = NULL,
  plotter = NULL,
  ...
) {
  force(objects)
  force(Delta)
  force(time)
  force(save)
  gc_last_time <- gc.time()[3]
  values_save <- list()
  last_time <- bench::hires_time()
  eval_expr <- is.call(expr) || is.expression(expr)
  plotter_expr <- is.call(plotter) || is.expression(plotter)
  # Environment for evaluation of the expression or plotter
  env <- new.env()
  if (plotter_expr) {
    new_plotter_window(...)
  }
  env$.__n <- 1

  tracer <- function() {
    time_diff <- bench::hires_time() - last_time
    n <- env$.__n
    gc_time <- gc.time()[3]
    gc_time_diff <- gc_time - gc_last_time
    if (gc_time_diff > 0) {
      time_diff <- max(time_diff - gc_time_diff, 0)
    }
    # The expression is evaluated in its own environment to ensure that it
    # does not accidentally overwrite variables in the calling environment.
    # To give the expression access to the calling environment it is assigned
    # as the enclosing environment.
    envir <- parent.frame()
    parent.env(env) <- envir
    if (eval_expr) {
      tryCatch(eval(expr, envir = env),
               error = function(e) warning(e))
    }
    if (is.null(objects))
      objects <- ls(envir)

    values <- mget(objects, envir = envir, ifnotfound = list(NA))
    if (eval_expr) {
      na_val <- unlist(lapply(values, function(x) all(is.na(x))))
      values <- values[!na_val]
      expr_objects <- objects[!(objects %in% names(values))]
      values <- c(values, mget(expr_objects, envir = env, ifnotfound = list(NA)))
    }
    if (isTRUE(Delta > 0) && (n  == 1 || n %% Delta == 0)) {
      if (plotter_expr) {
        tryCatch(eval(plotter, envir = env),
          error = function(e) warning(e))
      }
      cat("n = ", n, ": ",
        paste(names(values), " = ", format(values, ...), "; ", sep = ""),
        "\n", sep = "")
    }
    if (save) {
      if (time)
        values[[".time"]] <- time_diff
      values_save[[n]] <<- values
    }
    env$.__n <- n + 1
    gc_last_time <<- gc.time()[3]
    last_time <<- bench::hires_time()
    invisible(NULL)
  }

  get <- function(simplify = FALSE) {
    if (simplify) {
      col_names <- unique(unlist(lapply(values_save, names)))
      values_save <- lapply(values_save,
                            function(entry) lapply(entry, col_to_row))
      values_save <- lapply(
        col_names,
        function(x) {
          do.call(
            rbind,
            unlist(lapply(values_save, function(y) y[x]), recursive = FALSE)
          )
        }
      )
      names(values_save) <- col_names
      values_save <- lapply(
        col_names,
        function(x) {
          x_val <- values_save[[x]]
          if (!is.null(ncol(x_val)) && ncol(x_val) == 1) {
            colnames(x_val) <- x
          } else {
            if (is.null(colnames(x_val)))
              colnames(x_val) <- seq_len(ncol(x_val))
            colnames(x_val) <- paste(x, ".", colnames(x_val), sep = "")
          }
          x_val
        }
      )
      values_save <- do.call(cbind, values_save)
      row.names(values_save) <- seq_len(nrow(values_save))
    }
    values_save
  }

  clear <- function(plotter = FALSE) {
    env <<- new.env()
    env$.__n <- 1
    gc_last_time <<- gc.time()[3]
    values_save <<- list()
    last_time <<- bench::hires_time()
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

  structure(list(tracer = tracer, get = get, clear = clear), class = "tracer")
}

# If x is a matrix object which encodes a column vector, then the transpose of x
# is returned. Otherwise, x is returned.
col_to_row <- function(x) {
  if (inherits(x, "matrix") && nrow(x) > 1 && ncol(x) == 1) {
    t(x)
  } else {
    x
  }
}

# Setting up a new plot window
new_plotter_window <- function(...) {
  graphics::plot.new()
  graphics::plot.window(...)
  graphics::Axis(side = 1)
  graphics::Axis(side = 2)
}

#' Subsetting tracer objects
#'
#' @param x a tracer object.
#' @param i the indices of the trace information to extract.
#' @param j currently ignored.
#' @param ... other arguments passed on to \code{get}.
#' @param drop simplify a list with one element to a vector if TRUE.
#'
#' @return a list
#' @export
'[.tracer' <- function(x, i, j, ..., drop = TRUE) {
  values <- x$get(...)[i]
  if (length(values) == 1 && drop)
    values <- values[[1]]
  values
}

#' Summarize and print trace information
#'
#' @param object   a tracer object
#' @param x        a tracer object
#' @param ...      other arguments (currently ignored)
#'
#' @return \code{summary} returns a data frame (of class trace) with columns
#' containing the values
#' of the traced objects, and if time is traced an additional column, \code{.time},
#' containing the cumulative runtime in seconds.
#' @export
summary.tracer <- function(object, ...) {
  object <- suppressWarnings(object$get(simplify = TRUE))
  if (".time" %in% colnames(object)) {
    object[, ".time"] <- c(0, cumsum(object[-1, ".time"]))
  }
  structure(as.data.frame(object), class = c("trace", "data.frame"))
}

#' @rdname summary.tracer
#' @export
print.tracer <- function(x, ...) print(x$get(simplify = TRUE))

#' Plot results from a trace
#'
#' Plots the value of a traced object on a log-scale against runtime.
#'
#' @param object a trace or tracer object
#' @param y      the name of the traced object to plot
#' @param log    logical. Should the y-axis be on a log-scale. Default is TRUE.
#' @param ...    additional arguments passed to
#'
#' @return a ggplot object
#' @import ggplot2
#' @export
autoplot.tracer <- function(object, y, ...) {
  autoplot(summary(object), {{y}}, ...)
}

#' @rdname autoplot.tracer
#' @import ggplot2
#' @importFrom rlang .data
#' @export
autoplot.trace <- function(object, y, log = TRUE, ...) {
  p <- ggplot(object, aes(x = .data$.time, {{y}})) + geom_point() + xlab("time (s)")
  if (log) p <- p + scale_y_log10()
  p
}

#' Constructor of plotter expression
#'
#' The plotter function returns a quoted expression that adds points
#' to a current plot. For use with tracer and terminator objects to
#' iteratively update plots during long running function evaluations.
#'
#' @param y a name of a symbol to plot
#' @param col point and line color
#' @param lty line type
#' @param pch plot symbol
#'
#' @return a quoted expression
#' @export
plotter <- function(y, col = "black", lty = "solid", pch = 1) {
  bquote(
    {
      .__y_val <- get(.(y))
      points(.__n, .__y_val, col = .(col), pch = .(pch))
      if (exists(".__n_old") && exists(".__y_val_old"))  {
        lines(c(.__n_old, .__n), c(.__y_val_old, .__y_val), col = .(col), lty = .(lty))
      }
      .__n_old <- .__n
      .__y_val_old <- .__y_val
    }
  )
}
