

MLP <- R6::R6Class(
  'MLP', inherit=PipeComponent,
  public=list(
    outlabels=NULL,

    # network architecture
    hidden_sizes=10, activation='sigm', output='sigm',

    # learning parameters
    learn_rate=0.9, learn_rate_decay=1, momentum=0.5,
    num_epoch=5L, batch_size=100L,
    hidden_dropout=0, visible_dropout=0,

    .fit_helper = function(x, y, initW, initB)
    {
      # deepnet::nn.train does not accept x as data.frame
      if (!is.matrix(x)) x <- as.matrix(x)

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

    fit = function(x, y)
    {
      # set output labels by the column names of y
      # if y has no column name, then assign numbers
      if (self$output == 'softmax') {
        # if y is a factor or vector, then apply nnet::class.ind to
        # make it to one-hot form
        if (is.factor(y) || is.vector(y)) y <- nnet::class.ind(y)
        self$outlabels <- if (is.null(colnames(y))) 1:ncol(y) else colnames(y)
      }
      # now fit
      self$object <- self$.fit_helper(x, y, NULL, NULL)
    },

    predict = function(x)
    {
      if (!is.matrix(x)) x <- as.matrix(x)

      if (is.null(self$outlabels)) {
        return(deepnet::nn.predict(self$object, x))
      } else {
        return(self$outlabels[max.col(deepnet::nn.predict(self$object, x))])
      }
    },

    incfit = function(x, y)
    {
      self$object <- self$.fit_helper(x, y, self$object$W, self$object$B)
    },

    initialize = function(
      hidden_sizes=10, activation='sigm', output='sigm',
      learn_rate=0.9, learn_rate_decay=1, momentum=0.5,
      num_epoch=5L, batch_size=100L,
      hidden_dropout=0, visible_dropout=0, ...)
    {
      self$set_parameters(...)

      self$hidden_sizes <- hidden_sizes
      self$activation <- activation
      self$output <- output

      self$learn_rate <- learn_rate
      self$learn_rate_decay <- learn_rate_decay
      self$momentum <- momentum
      self$num_epoch <- num_epoch
      self$batch_size <- batch_size

      self$hidden_dropout <- hidden_dropout
      self$visible_dropout <- visible_dropout
    }
  )
)



#' Multilayer perceptron
#' @param ... initialization arguments for \code{MLP} class
#' @examples
#' # example from \code{\link{deepnet::nn.train}}
#' Var1 <- c(rnorm(50, 1, 0.5), rnorm(50, -0.6, 0.2))
#' Var2 <- c(rnorm(50, -0.8, 0.2), rnorm(50, 2, 1))
#' x <- matrix(c(Var1, Var2), nrow = 100, ncol = 2)
#' y <- c(rep(1, 50), rep(0, 50))
#'
#' m <- mlp(hidden_sizes=5, learn_rate=0.8, num_epoch=3)
#' m$fit(x, y)
#' @export
mlp <- MLP$new


#' Multilayer perceptron for classification
#' @param ... initialization arguments for \code{MLP} class
#' @examples
#' data(iris)
#' x <- iris[,-5]
#' y <- iris[,5]
#' tr <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
#'
#' m <- mlp_classifier(num_epoch=100)
#' m$fit(x[tr,], y[tr])
#' table(y[-tr], m$predict(x[-tr,]))
#' @export
mlp_classifier <- function(...) { MLP$new(output='softmax', ...) }


#' Multilayer perceptron for regression
#' @param ... initialization arguments for \code{MLP} class
#' @examples
#' set.seed(123)
#' n <- 1000
#' x <- runif(2*n)
#' dim(x) <- c(n, 2)
#' y <- pmin(x[,1], x[,2])
#' m <- mlp_regressor(hidden_sizes=10, num_epoch=500, batch_size=25)
#' m$fit(x, y)
#'
#' newx <- expand.grid(x1=seq(0, 1, length=50), x2=seq(0, 1, length=50))
#' pred <- m$predict(newx)
#' true <- pmin(newx[,1], newx[,2])
#' cor(true, pred)
#' \dontrun{
#' dim(pred) <- c(50, 50)
#' dim(true) <- c(50, 50)
#' par(mfrow=c(1,2))
#' contour(true)
#' contour(pred)}
#' @export
mlp_regressor <- function(...) { MLP$new(output='linear', ...) }



