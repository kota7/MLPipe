#' Machine learning pipeline
#'
#' @param ... Arbitrary number of pipeline components
#' @examples
#' library(mlbench)
#' data(Sonar)
#' X <- Sonar[, -ncol(Sonar)]
#' y <- Sonar[, ncol(Sonar)]
#' samp <- c(sample(1:100,75), sample(101:200,75))
#' m <- mlp_classifier(size=10)
#' m$fit(list(x=X[samp,], y=class.ind(y[samp])))
#' table(y[-samp], m$predict(list(x=X[-samp,])))
#'
#' p <- pipeline(pc=pca(30), ml=mlp_classifier(size=10))
#' p$fit((list(x=X[samp,], y=class.ind(y[samp]))))
#' table(y[-samp], p$predict(list(x=X[-samp,])))
#' @export
pipeline <- function(...) { Pipeline$new(...) }


Pipeline <- R6::R6Class(
  'Pipeline',
  inherit=PipeComponent,

  public = list(
    steps=list(),

    fit = function(data)
    {
      for (k in seq_along(self$steps))
      {
        self$steps[[k]]$fit(data)
        data <- self$steps[[k]]$transform(data)
      }
    },

    transform = function(data)
    {
      for (k in seq_along(self$steps))
      {
        data <- self$steps[[k]]$transform(data)
      }
      data
    },

    predict = function(data)
    {
      for (k in seq_along(self$steps))
      {
        if (k != length(self$steps)) {
          data <- self$steps[[k]]$transform(data)
        } else {
          return(self$steps[[k]]$predict(data))
        }
      }
    },

    incfit = function(data)
    {
      for (k in seq_along(self$steps))
      {
        self$steps[[k]]$incfit(data)
        data <- self$steps[[k]]$transform(data)
      }
    },

    initialize = function(...) { self$steps <- list(...) }
  )
)
