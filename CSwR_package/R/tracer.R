#' Constructor of a tracer object
#'
#' Tracer objects can collect, print and summarize trace information from
#' the evaluation environment of other functions during their evaluation.
#'
#' The function \code{tracer} constructs a tracer object containing a
#' \code{tracer}, a \code{get} function and a \code{clear} function. A call of \code{tracer} can
#' be inserted in other functions and used to collect and print trace information
#' about the internals of that function during its evaluation. The \code{get}
#' function can access that information afterwards, and \code{clear} deletes
#' all stored values in the tracer object.
#'
#' The time between \code{tracer} calls can also be measured and stored using
#' the \code{hires_time} function from the bench package. There are \code{print}
#' and \code{summary} methods available for summarizing the trace information.
#' The call of the \code{tracer} function can be manually inserted into the body of
#' the function that is to be traced, it can be inserted using \code{base::trace},
#' or it can be passed as an argument to any function with a callback argument.
#'
#' @param objects a character vector of names of the objects that are to be traced.
#'                The objects are searched for in the calling environment of the
#'                \code{tracer} function. Objects created by the \code{expr} argument
#'                can also be traced.
#' @param N       an integer specifying if and how often trace information is printed.
#'                \code{N = 0} means never, and otherwise trace information is printed
#'                every \code{N}-th iteration. \code{N = 1} is the default.
#' @param save    a logical value. Determines if the trace information is stored.
#' @param time    a logical value. Determines if runtime information is traced.
#' @param expr    an expression that will be evaluated in an environment that has
#'                the calling environment of the \code{tracer} function as parent.
#' @param ...     other arguments passed to \code{format} for printing.
#'
#' @return A tracer object containing the functions \code{tracer} and \code{get}.
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
  N = 1,
  save = TRUE,
  time = TRUE,
  expr = NULL,
  ...
) {
  force(objects)
  force(N)
  force(time)
  force(save)
  n <- 1
  gc_last_time <- gc.time()[3]
  values_save <- list()
  last_time <- bench::hires_time()
  eval_expr <- is.call(expr) || is.expression(expr)
  if (eval_expr) {
    # Environment for evaluation of the expression
    expr_envir <- new.env()
  }

  tracer <- function() {
    time_diff <- bench::hires_time() - last_time
    gc_time <- gc.time()[3]
    gc_time_diff <- gc_time - gc_last_time
    if (gc_time_diff > 0) {
      time_diff <- max(time_diff - gc_time_diff, 0)
    }
    envir <- parent.frame()
    if (eval_expr) {
      # The expression is evaluated in its own environment to ensure that it
      # does not accidentally overwrite variables in the calling environment.
      # To give the expression access to the calling environment it is assigned
      # as the enclosing environment.
      parent.env(expr_envir) <- envir
      tryCatch(eval(expr, envir = expr_envir),
               error = function(e) warning(e))
    }
    if (is.null(objects))
      objects <- ls(envir)

    values <- mget(objects, envir = envir, ifnotfound = list(NA))
    if (eval_expr) {
      na_val <- unlist(lapply(values, function(x) all(is.na(x))))
      values <- values[!na_val]
      expr_objects <- objects[!(objects %in% names(values))]
      values <- c(values, mget(expr_objects, envir = expr_envir, ifnotfound = list(NA)))
    }
    if (N && (n == 1 || n %% N == 0))
      cat("n = ", n, ": ",
          paste(names(values), " = ", format(values, ...), "; ", sep = ""),
          "\n", sep = "")
    if (save) {
      if (time)
        values[[".time"]] <- time_diff
      values_save[[n]] <<- values
    }
    n <<- n + 1
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
              colnames(x_val) <- 1:ncol(x_val)
            colnames(x_val) <- paste(x, ".", colnames(x_val), sep = "")
          }
          x_val
        }
      )
      values_save <- do.call(cbind, values_save)
      row.names(values_save) <- 1:nrow(values_save)
    }
    values_save
  }

  clear <- function() {
    n <<- 1
    values_save <<- list()
    last_time <<- bench::hires_time()
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
#' @param x   a tracer object
#' @param ... other arguments (currently ignored)
#'
#' @return \code{summary} returns a data frame (of class trace) with columns
#' containing the values
#' of the traced objects, and if time is traced an additional column, \code{.time},
#' containing the cumulative runtime.
#' @export
summary.tracer <- function(x, ...) {
  x <- suppressWarnings(x$get(simplify = TRUE))
  if (".time" %in% colnames(x))
    x[, ".time"] <- c(0, cumsum(x[-1, ".time"]))
  structure(as.data.frame(x), class = c("trace", "data.frame"))
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
#' @return
#' @importFrom ggplot2 autoplot
#' @export
autoplot.tracer <- function(object, y, ...) {
  autoplot(summary(object), {{y}}, ...)
}

#' @rdname autoplot.tracer
#' @export
autoplot.trace <- function(object, y, log = TRUE, ...) {
  p <- ggplot(object, aes(x = .time, {{y}})) + geom_point() + xlab("time")
  if(log) p <- p + scale_y_log10()
  p
}

