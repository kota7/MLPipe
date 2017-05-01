
#' @export
Pipeline <- R6::R6Class(
  'Pipeline', inherit=PipeComponent,

  public = list(
    steps=list(), invert_prediction=TRUE,

    fit = function(x=NULL, y=NULL)
    {
      dat <- list(x=x, y=y)
      for (k in seq_along(self$steps))
      {
        self$steps[[k]]$fit(dat$x, dat$y)
        if (k < length(self$steps)) dat <- self$steps[[k]]$transform(dat$x, dat$y)
      }
      invisible(self)
    },

    transform = function(x=NULL, y=NULL)
    {
      dat <- list(x=x, y=y)
      for (k in seq_along(self$steps)) dat <- self$steps[[k]]$transform(dat$x, dat$y)

      dat
    },

    inv_transform = function(x=NULL, y=NULL)
    {
      dat <- list(x=x, y=y)
      for (k in rev(seq_along(self$steps))) dat <- self$steps[[k]]$inv_transform(dat$x, dat$y)

      dat
    },

    predict = function(x)
    {
      dat <- list(x=x, y=y)
      for (k in seq_along(self$steps))
      {
        if (k < length(self$steps)) {
          dat <- self$steps[[k]]$transform(dat$x, dat$y)
        } else {
          pred <- self$steps[[k]]$predict(dat$x)
        }
      }
      if (self$invert_prediction) {
        dat$y <- pred
        for (k in rev(seq_along(self$steps)))
        {
          if (k < length(self$steps)) dat <- inv_transform(dat$x, dat$y)
        }
        pred <- dat$y
      }
      pred
    },

    incr_fit = function(x=NULL, y=NULL)
    {
      dat <- list(x=x, y=y)
      for (k in seq_along(self$steps))
      {
        self$steps[[k]]$incr_fit(dat$x, dat$y)
        if (k < length(self$steps)) dat <- self$steps[[k]]$transform(dat$x, dat$y)
      }
      invisible(self)
    },

    initialize = function(..., invert_prediction=FALSE)
    {
      self$steps <- list(...)
      self$invert_prediction <- invert_prediction

      invisible(self)
    },

    evaluate = function(funcname, x=NULL, y=NULL, ...)
    {
      dat <- list(x=x, y=y)
      for (k in seq_along(self$steps))
      {
        if (k < length(self$steps)) {
          dat <- self$steps[[k]]$transform(dat$x, dat$y)
        } else {
          out <- self$steps[[k]][[funcname]](dat$x, dat$y, ...)
        }
      }
      out
    }
  )
)



#' Machine learning pipeline
#'
#' @name pipeline
#' @aliases Pipeline
#'
#' @section Usage:
#' \preformatted{pipline(..., invert_prediction = FALSE)}
#'
#' @section Arguments:
#' \describe{
#' \item{...}{Arbitrary number of pipeline components}
#' \item{invert_prediction}{if TRUE, then \code{predict} function inverts predicted values}
#' }
#'
#' @section Value:
#' \code{Pipeline} class object
#'
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x = NULL, y = NULL)}}{fit and transform each component}
#' \item{\preformatted{transform(x = NULL, y = NULL)}}{transform from beginning to end}
#' \item{\preformatted{predict(x = NULL, y = NULL)}}{return predicted values}
#' \item{\preformatted{incr_fit(x = NULL, y = NULL)}}{fit incrementally each component}
#' \item{\preformatted{inv_transform(x = NULL, y = NULL)}}{invert transformation from end to beginning}
#' \item{\preformatted{evaluate(funcname, x = NULL, y = NULL, ...)}}{evaluate arbitrary function at the last component}
#' }
#'
#' @examples
#' set.seed(123)
#' data(Sonar, package='mlbench')
#' X <- Sonar[, -ncol(Sonar)]
#' y <- Sonar[, ncol(Sonar)]
#' tr <- c(sample(1:111,75), sample(112:200,75))
#'
#' p <- pipeline(pc=pca_extractor(30),
#'               ml=mlp_classifier(hidden_sizes=c(5, 5), num_epoch=1000))
#' p$fit(X[tr,], y[tr])
#' table(y[-tr], p$predict(X[-tr,]))
#' p$evaluate('accuracy', X[-tr,], y[-tr])
NULL

#' @export
pipeline <- Pipeline$new
