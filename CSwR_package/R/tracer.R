#' Constructor of a tracer object
#'
#' Tracer objects can collect, print and summarize trace information from
#' the evaluation environment of other functions during their evaluations.
#'
#' The function \code{tracer} constructs a tracer object containing a
#' \code{tracer} and a \code{get} function. A call of \code{tracer} can
#' be inserted in other functions and used to collect and print trace information
#' about the internals of that function during its evaluation. The \code{get}
#' function can access that information afterwards.
#' The time between \code{tracer} calls can also be measured and stored using
#' the \code{hires_time} function from the bench package. There are \code{print}
#' and \code{summary} methods available for summarizing the trace information.
#' The call of the \code{tracer} function can be manually inserted into the body of
#' the function that is to be traced, it can be inserted using \code{base::trace},
#' or it can be passed as an argument to any function with a callback argument.
#'
#' @param objects a character vector of names of the objects in the calling
#'                environment of the \code{tracer} function that are to be traced.
#'                Objects created by the \code{expr} argument can also be traced.
#' @param N       an integer specifying if and how often trace information is printed.
#'                \code{N = 0} means never, and otherwise trace information is printed
#'                every \code{N}-th iteration. \code{N = 1} is the default.
#' @param save    a logical value. Determines if the trace information is saved.
#' @param time    a logical value. Determines if run time information is traced.
#' @param expr    an expression that will be evaluated in an environment that has
#'                the calling environment of the \code{tracer} function as parent.
#' @param ...     other arguments passed to \code{format} for printing.
#'
#' @return A tracer object containing the functions \code{tracer} and \code{get}.
#' @export
#'
#' @examples
#'
#' test_tracer <- tracer(c("m", "m_sq"), expr = expression(m_sq <- m^2))
#'
#' test <- function(n) {
#'   for(i in 1:n) {
#'     m <- 2 * i
#'     Sys.sleep(0.1)
#'     test_tracer$tracer()
#'     }
#' }
#'
#' test(10)
#' summary(test_tracer)
tracer <- function(
  objects = NULL,
  N = 1,
  save = TRUE,
  time = TRUE,
  expr = NULL,
  ...
) {
  n <- 1
  values_save <- list()
  last_time <- bench::hires_time()

  tracer <- function() {
    time_diff <- bench::hires_time() - last_time
    envir <- parent.frame()
    if(is.expression(expr)) {
      # This construction ensures that the evaluation of the expression does
      # not accidentally overwrite variables in the calling environment but
      # still has access to it
      envir <- new.env(parent = envir)
      eval(expr, envir = envir)
    }
    if(is.null(objects))
      objects <- ls(envir)

    # inherits = TRUE to also find values in the parent of envir
    values <- mget(objects, envir = envir, ifnotfound = list(NA), inherits = TRUE)
    if(N && (n == 1 || n %% N == 0))
      cat("n = ", n, ": ",
          paste(names(values), " = ", format(values, ...), "; ", sep = ""),
          "\n", sep = "")
    if(save) {
      if(time)
        values[[".time"]] <- time_diff
      values_save[[n]] <<- values
    }
    n <<- n + 1
    last_time <<- bench::hires_time()
    invisible(NULL)
  }

  get <- function(simplify = FALSE) {
    if(simplify) {
      col_names <- unique(unlist(lapply(values_save, names)))
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
          if(!is.null(ncol(x_val)) && ncol(x_val) == 1) {
            colnames(x_val) <- x
          } else {
            if(is.null(colnames(x_val)))
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

  structure(list(tracer = tracer, get = get), class = "tracer")
}



#' Subsetting tracer objects
#'
#' @param x a tracer object.
#' @param i the indices of the trace information to extract.
#' @param j currently ignored.
#' @param ... other arguments passed on to \code{get}.
#' @param drop currently ignored.
#'
#' @return a list
#' @export
'[.tracer' <- function(x, i, j, ..., drop = TRUE) {
  values <- x$get(...)[i]
  if (drop && length(i) == 1)
    values <- values[[1]]
  values
}

#' Summarize and print trace information
#'
#' @param x   a tracer object
#' @param ... other arguments (currently ignored)
#'
#' @return \code{summary} returns a data frame with columns containing the values
#' of the traced objects, and if time is traced an additional column, \code{.time},
#' containing the cumulative run time.
#' @export
summary.tracer <- function(x, ...) {
  x <- suppressWarnings(x$get(simplify = TRUE))
  x[, ".time"] <- c(0, cumsum(x[-1, ".time"]))
  as.data.frame(x)
}

#' @rdname summary.tracer
#' @export
print.tracer <- function(x, ...) print(x$get(...))
