
#' @export
Pipeline <- R6::R6Class(
  'Pipeline', inherit=PipeComponent,

  public = list(
    steps=list(),

    fit = function(x, y=NULL)
    {
      for (k in seq_along(self$steps))
      {
        self$steps[[k]]$fit(x, y)
        if (k < length(self$steps)) {
          tmp <- self$steps[[k]]$transform(x, y)
          x <- tmp$x
          y <- tmp$y
        }
      }
    },

    transform = function(x, y=NULL)
    {
      dat <- list(x=x, y=y)
      for (k in seq_along(self$steps))
      {
        dat <- self$steps[[k]]$transform(dat$x, dat$y)
      }
      dat
    },

    predict = function(x)
    {
      dat <- list(x=x, y=y)
      for (k in seq_along(self$steps))
      {
        if (k != length(self$steps)) {
          dat <- self$steps[[k]]$transform(dat$x, dat$y)
        } else {
          return(self$steps[[k]]$predict(dat$x))
        }
      }
    },

    incfit = function(x, y=NULL)
    {
      dat <- list(x=x, y=y)
      for (k in seq_along(self$steps))
      {
        self$steps[[k]]$incfit(dat$x, dat$y)
        dat <- self$steps[[k]]$transform(dat$x, dat$y)
      }
    },

    initialize = function(...) { self$steps <- list(...) }
  )
)



#' Machine learning pipeline
#'
#' @aliases Pipeline
#'
#' @param ... Arbitrary number of pipeline components
#' @return \code{Pipeline} class object
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
#' @export
pipeline <- Pipeline$new
