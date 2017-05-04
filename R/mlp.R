
#' @export
MLP <- R6::R6Class(
  'MLP', inherit=PipeComponent,

  public=list(
    outlabels=NULL,

    # network architecture
    output='sigm', hidden_sizes=10, activation='sigm',

    # learning parameters
    learn_rate=0.9, learn_rate_decay=1, momentum=0.5,
    num_epoch=5L, batch_size=100L,
    hidden_dropout=0, visible_dropout=0,


    fit = function(x, y)
    {
      y <- private$.format_y(y)
      x <- private$.format_x(x)

      # set output labels by the column names of y
      # if y has no column name, then assign numbers
      if (self$output == 'softmax') {
        self$outlabels <- if (is.null(colnames(y))) 1:ncol(y) else colnames(y)
      }
      # now fit
      self$object <- private$.fit_helper(x, y, NULL, NULL)
      invisible(self)
    },

    predict = function(x, ...)
    {
      x <- private$.format_x(x)

      p <- deepnet::nn.predict(self$object, x)
      if (is.null(self$outlabels)) p else private$.to_class_label(p)
    },

    predict_proba = function(x, ...)
    {
      x <- self$.format_x(x)
      if (self$output=='linear') warning('predicted value may not be in [0, 1]')

      deepnet::nn.predict(self$object, x)
    },

    incr_fit = function(x, y)
    {
      y <- private$.format_y(y)
      x <- private$.format_x(x)

      self$object <- private$.fit_helper(x, y, self$object$W, self$object$B)
      invisible(self)
    },

    initialize = function(
      output='sigm', hidden_sizes=10, activation='sigm',
      learn_rate=0.9, learn_rate_decay=1, momentum=0.5,
      num_epoch=5, batch_size=100,
      hidden_dropout=0, visible_dropout=0, ...)
    {
      self$output <- output
      self$hidden_sizes <- hidden_sizes
      self$activation <- activation

      self$learn_rate <- learn_rate
      self$learn_rate_decay <- learn_rate_decay
      self$momentum <- momentum
      self$num_epoch <- num_epoch
      self$batch_size <- batch_size

      self$hidden_dropout <- hidden_dropout
      self$visible_dropout <- visible_dropout
      invisible(self)
    },


    # performance evaluation
    mse = function(x, y)
    {
      y <- private$.format_y(y)
      x <- private$.format_x(x)

      p <- deepnet::nn.predict(self$object, x)
      mean((p-y)^2)
    },

    cross_entropy = function(x, y)
    {
      y <- private$.format_y(y)
      x <- private$.format_x(x)

      p <- deepnet::nn.predict(self$object, x)
      if (any(p <= 0) || any(p >= 1)) {
        stop('cross entropy loss requires prediction in (0,1)')
      }

      if (self$output == 'softmax') {
        return(-mean(rowSums(y*log(p), na.rm=TRUE), na.rm=TRUE))
      } else {
        return(-mean(y*log(p) + (1-y)*log(1-p), na.rm=TRUE))
      }
    },

    accuracy = function(x, y)
    {
      if (is.null(self$outlabels)) stop('accuracy is not computed for continuous prediction')

      p <- self$predict(x)
      y <- private$.to_class_label(y)
      mean(y==p)
    }
  ),

  private = list(
    .fit_helper = function(x, y, initW, initB)
    {
      # assumes x and y are already formatted by ".format" functions
      deepnet::nn.train(
        x, y, initW, initB,

        hidden=self$hidden_sizes,
        activationfun=self$activation,
        output=self$output,

        learningrate=self$learn_rate,
        learningrate_scale=self$learn_rate_decay,
        momentum=self$momentum,
        numepochs=self$num_epoch,
        batchsize=self$batch_size,

        hidden_dropout=self$hidden_dropout,
        visible_dropout=self$visible_dropout
      )
    },

    .format_y = function(y)
    {
      # softmax output assumes that y is a matrix
      # if y is a factor or vector, then apply nnet::class.ind to
      # make it to one-hot form
      if (self$output == 'softmax') {
        if (is.factor(y) || is.vector(y)) y <- nnet::class.ind(y)
      }
      y
    },

    .format_x = function(x)
    {
      # deepnet::nn.train does not accept x as data.frame
      # so convert x to a matrix
      if (!is.matrix(x)) x <- as.matrix(x)
      x
    },

    .to_class_label = function(y)
    {
      # if y is matrix, then convert it to class label by row-wise max
      # otherwise return y as-is
      # error is outlabels is not defined
      if (is.null(self$outlabels)) stop('class label not defined')
      if (is.matrix(y)) self$outlabels[max.col(y)] else y
    }
  )
)



#' Multilayer perceptron
#'
#' @name mlp
#' @aliases mlp_classifier mlp_regressor MLP
#'
#' @section Usage:
#' \preformatted{
#' mlp(output = 'sigm', hidden_sizes = 10, activation = 'sigm',
#'   learn_rate = 0.9, learn_rate_decay = 1, momentum = 0.5,
#'   num_epoch = 5, batch_size = 100,
#'   hidden_dropout = 0, visible_dropout = 0)
#'
#' mlp_classifier(hidden_sizes = 10, activation = 'sigm',
#'   learn_rate = 0.9, learn_rate_decay = 1, momentum = 0.5,
#'   num_epoch = 5, batch_size = 100,
#'   hidden_dropout = 0, visible_dropout = 0)
#'
#' mlp_regressor(hidden_sizes = 10, activation = 'sigm',
#'   learn_rate = 0.9, learn_rate_decay = 1, momentum = 0.5,
#'   num_epoch = 5, batch_size = 100,
#'   hidden_dropout = 0, visible_dropout = 0)}
#'
#' @section Arguments:
#' \describe{
#' \item{\code{output}}{output unit form. \code{'sigm'}, \code{'linear'} or \code{'softmax'}}
#' \item{\code{hidden_sizes}}{integer vector of hidden unit sizes}
#' \item{\code{activation}}{activation function. \code{'sigm'}, \code{'tanh'} or \code{'linear'}}
#' \item{\code{learn_rate}}{learning rate}
#' \item{\code{learn_rate_decay}}{scale multipled to learning rate after each iteration}
#' \item{\code{momentum}}{momentum for gradient descent}
#' \item{\code{num_epoch}}{number of iteration}
#' \item{\code{batch_size}}{mini-batch size}
#' \item{\code{hidden_dropout}}{drop out fraction for hidden layer}
#' \item{\code{visible_dropout}}{drop out fraction for input layer}
#' }
#'
#' @section Value:
#' \code{MLP} class object
#'
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x, y)}}{train neural network}
#' \item{\preformatted{predict(x, ...)}}{return predicted values}
#' \item{\preformatted{incr_fit(x, y)}}{train neural network incrementally}
#' \item{\preformatted{predict_proba(x, ...)}}{return probability prediction}
#'
#' \item{\preformatted{mse(x, y)}}{return the mean-squared error}
#' \item{\preformatted{cross_entropy(x, y)}}{return the cross entropy loss if appropriate}
#' \item{\preformatted{accuracy(x, y)}}{return the classification accuracy if appropriate}
#' }
#'
#' @section Details:
#' Uses \code{\link[deepnet]{nn.train}} as the backend.
#' \code{fit} method trains the network from the scratch; Use \code{incr_fit} method for incremental learning.
#'
#' @examples
#' set.seed(123)
#' # example from \code{\link{deepnet::nn.train}}
#' Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
#' Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
#' x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)
#' y <- c(rep(1, 50), rep(0, 50))
#' m <- mlp(hidden_sizes=5, learn_rate=0.8, num_epoch=3)
#' m$fit(x, y)
#' m$mse(x, y)
#'
#' # classification example
#' data(iris)
#' x <- iris[,-5]
#' y <- iris[,5]
#' tr <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
#' m <- mlp_classifier(num_epoch=300)
#' m$fit(x[tr,], y[tr])
#' table(y[-tr], m$predict(x[-tr,]))
#' m$accuracy(x[-tr,], y[-tr])
#' m$cross_entropy(x[-tr,], y[-tr])
#'
#' \dontrun{
#' # regression example (takes a few seconds)
#' n <- 1000
#' x <- runif(2*n)
#' dim(x) <- c(n, 2)
#' y <- pmin(x[,1], x[,2])
#' m <- mlp_regressor(hidden_sizes=c(10), num_epoch=500, batch_size=25)
#' m$fit(x, y)
#' newx <- expand.grid(x1=seq(0, 1, length=50), x2=seq(0, 1, length=50))
#' pred <- m$predict(newx)
#' true <- pmin(newx[,1], newx[,2])
#' cor(true, pred)
#' dim(pred) <- c(50, 50)
#' dim(true) <- c(50, 50)
#' par(mfrow=c(1, 2))
#' contour(true)
#' contour(pred)
#' m$mse(newx, as.numeric(true))
#' }
NULL

#' @export
mlp <- MLP$new


#' @export
mlp_classifier <- function(...) { MLP$new(output='softmax', ...) }


#' @export
mlp_regressor <- function(...) { MLP$new(output='linear', ...) }



