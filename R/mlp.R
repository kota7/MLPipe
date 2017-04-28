#' Multilayer perceptron with a single hidden layer
#'
#' @param ... Parameters passed to \code{nnet::nnet}
#' @examples
#' # replication of \code{\link{nnet::nnet}} example
#' X <- iris[,-5]
#' y <- iris[,5]
#' samp <- c(sample(1:50,25),
#'           sample(51:100,25),
#'           sample(101:150,25))
#' m <- mlp(size=2, rang=0.1, decay=5e-4, maxit=200)
#' m$fit(list(x=X[samp,], y=y[samp]))
#' table(y[-samp], max.col(m$predict(list(x=X[-samp,]))))
#' @export
mlp <- function(...) { MLP$new(...) }


MLP <- R6::R6Class(
  'MLP', inherit=PipeComponent,
  public=list(
    parameters=list(size=1),

    fit = function(data) {
      if (is.character(data$y) | is.factor(data$y))
        data$y <- nnet::class.ind(data$y)
      self$object <- do.call(
        nnet::nnet, c(list(data$x, data$y), self$parameters))
    },

    predict = function(data) {
      predict(self$object, data$x)
    },

    incfit = function(data) {
      if (is.null(self$object)) {
        self$fit(data)
      } else {
        self$object <- do.call(
          nnet::nnet, c(list(data$x, data$y, Wts=self$object$wts),
                        self$parameters))
      }
    },

    initialize = function(...)
    {
      super$initialize(...)
    }
  )
)




#' @rdname mlp
#' @details \code{mlp_classifier} is a wrapper of \code{mlp}, which sets up
#' appropriate parameters for classification models.
#' @examples
#'
#' # Classification
#' m <- mlp_classifier(size=3, maxit=200)
#' m$fit(list(x=X[samp,], y=y[samp]))
#' table(y[-samp], m$predict(list(x=X[-samp,])))
#' @export
mlp_classifier <- function(...) { MLPClassifier$new(...) }


MLPClassifier <- R6::R6Class(
  'MLPClassifier', inherit = MLP,
  public = list(
    parameters = list(size=1, softmax=TRUE),

    predict = function(data) {
      predict(self$object, data$x, type='class')
    }
  )
)



#' @rdname mlp
#' @details \code{mlp_regressor} is a wrapper of \code{mlp}, which sets up
#' appropriate parameters for regression (i.e. numeric outcome) models.
#' @examples
#'
#' # Regression
#' library(mlbench)
#' data(BostonHousing)
#' X <- BostonHousing[, names(BostonHousing) != c('medv')]
#' y <- BostonHousing[, 'medv']
#' samp <- sample.int(length(y), ceiling(length(y)*0.8))
#' m <- mlp_regressor(size=5, maxit=200, skip=TRUE)
#' m$fit(list(x=X[samp,], y=y[samp]))
#' tbl <- cbind(y[-samp], m$predict(list(x=X[-samp,])))
#' cor(tbl)[2,1]
#' \dontrun{
#' plot(tbl, xlab='actual', ylab='predicted')}
#' @export
mlp_regressor <- function(...) { MLPRegressor$new(...) }


MLPRegressor <- R6::R6Class(
  'MLPRegressor', inherit = MLP,
  public = list(
    parameters = list(size=1, linout=TRUE),

    predict = function(data) {
      predict(self$object, data$x)
    }
  )

)
