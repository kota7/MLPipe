
#' @export
GLM <- R6::R6Class(
  'LinearRegression', inherit=PipeComponent,

  public = list(
    object_cv = NULL,

    family='gaussian', intercept=TRUE, standardize=TRUE,
    offset_index=0,

    # regularization related
    alpha=1, lambda=0.01,

    choose_lambda=FALSE, # if true, fit function invokes cross validation

    # cross validation controls.
    # - lambda_candidates are optional user supplied candidate values
    # - lambda_min_ratio is the minimum lambda as fraction to the max (auto-determined)
    # - nlambda controls number of lambdas to check
    lambda_candidates=NULL, lambda_min_ratio=0.0001, nlambda=100,
    nfolds=10, parallel=FALSE, loss='deviance',

    # decided not to support:
    # - foldid
    # - grouped
    # - keep

    group_multinom = FALSE,
    modified_newton = FALSE,
    standardize_response = FALSE,

    lower=-Inf, upper=+Inf,

    max_included=NULL, force_exclude=integer(0),

    tol=1e-7, maxit=1e+5,


    fit = function(x, y)
    {
      if (self$choose_lambda) {
        private$.fit_cv(x, y)
      } else {
        private$.fit_ncv(x, y)
      }
      invisible(self)
    },

    predict = function(x, lambda=NULL, ...)
    {
      tmp <- private$.format_x(x)
      x <- tmp$x
      os <- tmp$offset
      lambda <- if (is.null(lambda)) self$lambda else lambda
      ty <- if (self$family %in% c('binomial', 'multinomial')) 'class' else 'response'
      if (is.null(os)) {
        out <- predict(self$object, x, s=lambda, type=ty, ...)
      } else {
        out <- predict(self$object, x, s=lambda, type=ty, offset=os, ...)
      }
      drop(out)
    },

    predict_proba = function(x, lambda=NULL, ...)
    {
      tmp <- private$.format_x(x)
      x <- tmp$x
      os <- tmp$offset
      lambda <- if (is.null(lambda)) self$lambda else lambda
      if (is.null(os)) {
        out <- predict(self$object, x, s=lambda, type='response', ...)
      } else {
        out <- predict(self$object, x, s=lambda, type='response', offset=os, ...)
      }
      if (!(self$family %in% c('binomial', 'multinomial'))) {
        warning('probability prediction may not be appropriate for family ', self$family)
      }
      drop(out)
    },

    get_coef = function(lambda=NULL, nonzero_only=FALSE, ...)
    {
      lambda <- if (is.null(lambda)) self$lambda else lambda
      ty <- if (nonzero_only) 'nonzero' else 'coefficients'
      predict(self$object, s=lambda, type=ty)
    },


    initialize = function(...)
    {
      self$not_parameters <- c(self$not_parameters, 'object_cv')
      self$set_parameters(...)
      invisible(self)
    },


    mse = function(x, y)
    {
      y <- private$.format_y(y)
      p <- self$predict(x)
      mean((p-y)^2, na.rm=TRUE)
    },

    cross_entropy = function(x, y)
    {
      if (!(self$family %in% c('binomial', 'multinomial'))) {
        stop('cross entropy is only available for classification models')
      }
      y <- private$.format_y(y)
      p <- self$predict_proba(x) %>% drop()
      if (any(p >= 1) || any(p <= 0)) stop('predicted probability not in (0,1)')

      if (self$family == 'binomial') p <- cbind(1-p, p)
      -mean(rowSums(y*log(p), na.rm=TRUE), na.rm=TRUE)
    },

    accuracy = function(x, y)
    {
      if (!(self$family %in% c('binomial', 'multinomial'))) {
        stop('accuracy is only available for classification models')
      }
      y <- private$.format_y(y)
      p <- self$predict(x) %>% drop() %>% private$.format_y()
      mean(max.col(y) == max.col(p), na.rm=TRUE)
    }
  ),

  private = list(
    .format_x = function(x)
    {
      # pick offset. there may be multiple offsets
      index <- self$offset_index
      index <- index[index >= 1 & index <= ncol(x)]

      if (length(index) == 0) return(list(x=x, offset=NULL))
      list(x=x[, -index, drop=FALSE], offset=x[, index])
    },

    .format_y = function(y)
    {
      if (self$family %in% c('binomial', 'multinomial')) {
        y <- drop(y)
        if (is.factor(y) || is.vector(y)) y <- nnet::class.ind(y)
      }
      y
    },

    .fit_cv = function(x, y)
    {
      # called by fit when choose_lambda is true
      tmp <- private$.format_x(x)
      x <- tmp$x
      os <- tmp$offset
      y <- private$.format_y(y)

      res <- glmnet::cv.glmnet(
        x, y, offset=os,
        lambda=self$lambda_candidates,
        type.measure=self$loss, nfolds=self$nfolds, parallel=self$parallel,
        # parameters for glmnet
        family=self$family, alpha=self$alpha,
        intercept=self$intercept, standardize=self$standardize,

        nlambda=self$nlambda, lambda.min.ratio=self$lambda_min_ratio,
        thresh=self$tol, maxit=self$maxit,
        lower.limits=self$lower, upper.limits=self$upper,

        type.logistic=if (self$modified_newton) 'modified.Newton' else 'Newton',
        type.multinomial=if (self$group_multinom) 'grouped' else 'ungrouped',
        standardize.response=self$standardize_response,

        pmax=if (is.null(self$max_included)) ncol(x)+1 else self$max_included,
        exclude=self$force_exclude
      )

      self$object_cv <- res
      self$lambda <- res$lambda.min # best lambda
      self$object <- res$glmnet.fit # full model

      invisible(self)
    },

    .fit_ncv = function(x, y)
    {
      # called by fir when choose_lambda is false
      tmp <- private$.format_x(x)
      x <- tmp$x
      os <- tmp$offset
      y <- private$.format_y(y)

      #lambda <- if (is.null(self$lambda)) self$lambda_candidates else self$lambda
      if (!is.numeric(self$lambda) || length(self$lambda) != 1) {
        stop('lambda must be a single numeric value')
      }
      res <- glmnet::glmnet(
        x, y, family=self$family, offset=os,
        intercept=self$intercept, standardize=self$standardize,
        alpha=self$alpha,

        lambda=self$lambda, #nlambda=self$nlambda, lambda.min.ratio=self$lambda_min_ratio,
        thresh=self$tol, maxit=self$maxit,
        lower.limits=self$lower, upper.limits=self$upper,

        type.logistic=if (self$modified_newton) 'modified.Newton' else 'Newton',
        type.multinomial=if (self$group_multinom) 'grouped' else 'ungrouped',
        standardize.response=self$standardize_response,

        pmax=if (is.null(self$max_included)) ncol(x)+1 else self$max_included,
        exclude=self$force_exclude
      )
      self$object <- res
      invisible(self)
    }
  )
)


#' Generalized linear model
#'
#' Fit a generalized linear model, possibly with L1 or elastic net regularization.
#' Supports linear regression for continous outputs, poisson regression for count outputs, logistic and multinomial regression for classification.
#' Currently not supports cox regression model.
#'
#' @name GLM
#' @aliases linear_regression linear_regression_multi logistic_regression multinomial_regression poisson_regression
#'
#' @section Usage:
# #' \preformatted{glm_fit(family = 'gaussian', intercept = TRUE, standardize = TRUE,
# #'     offset_index = 0, alpha = 1, lambda = 0.01, choose_lambda = FALSE,
# #'     lambda_candidates = NULL, lambda_min_ratio = 0.0001, nlambda = 100,
# #'     nfolds = 10, parallel = FALSE, loss = 'deviance',
# #'     group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
# #'     lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
# #'     tol = 1e-7, maxit = 1e+5)}
#' \preformatted{linear_regression(intercept = TRUE, standardize = TRUE,
#'     offset_index = 0, alpha = 1, lambda = 0.01, choose_lambda = FALSE,
#'     lambda_candidates = NULL, lambda_min_ratio = 0.0001, nlambda = 100,
#'     nfolds = 10, parallel = FALSE, loss = 'deviance',
#'     group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
#'     lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
#'     tol = 1e-7, maxit = 1e+5)}
#' \preformatted{logistic_regression(intercept = TRUE, standardize = TRUE,
#'     offset_index = 0, alpha = 1, lambda = 0.01, choose_lambda = FALSE,
#'     lambda_candidates = NULL, lambda_min_ratio = 0.0001, nlambda = 100,
#'     nfolds = 10, parallel = FALSE, loss = 'deviance',
#'     group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
#'     lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
#'     tol = 1e-7, maxit = 1e+5)}
#' \preformatted{poisson_regression(intercept = TRUE, standardize = TRUE,
#'     offset_index = 0, alpha = 1, lambda = 0.0001, choose_lambda = FALSE,
#'     lambda_candidates = NULL, lambda_min_ratio = 0.01, nlambda = 100,
#'     nfolds = 10, parallel = FALSE, loss = 'deviance',
#'     group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
#'     lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
#'     tol = 1e-7, maxit = 1e+5)}
#' \preformatted{multinomial_regression(intercept = TRUE, standardize = TRUE,
#'     offset_index = 0, alpha = 1, lambda = 0.0001, choose_lambda = FALSE,
#'     lambda_candidates = NULL, lambda_min_ratio = 0.01, nlambda = 100,
#'     nfolds = 10, parallel = FALSE, loss = 'deviance',
#'     group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
#'     lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
#'     tol = 1e-7, maxit = 1e+5)}
#' \preformatted{linear_regression_multi(intercept = TRUE, standardize = TRUE,
#'     offset_index = 0, alpha = 1, lambda = 0.01, choose_lambda = FALSE,
#'     lambda_candidates = NULL, lambda_min_ratio = 0.0001, nlambda = 100,
#'     nfolds = 10, parallel = FALSE, loss = 'deviance',
#'     group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
#'     lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
#'     tol = 1e-7, maxit = 1e+5)}
#'
#' @section Arguments:
#' \describe{
# #' \item{\code{family}}{Reponse type. \itemize{
# #'   \item{\code{'gaussian'} for linear regression}
# #'   \item{\code{'mgaussian'} for linear regression with multiple outputs}
# #'   \item{\code{'binomial'} for logistic regression}
# #'   \item{\code{'multinomial'} for multinomial regression}
# #'   \item{\code{'poisson'} for poisson regression}
# #'   \item{\code{'cox'} for cox model}
# #' }}
#' \item{\code{intercept}}{logical indicating if the model has constant term}
#' \item{\code{standardize}}{lgical indicating if the explanatory variables should be standardized before fitted}
#' \item{\code{offset_index}}{integer, specifies which column of \code{x} is used as the offset. This is often used for poisson regression, to accomodate the difference in the periods across observations. Note that \code{family=='multinomial'} requires multiple offset variables.}
#' \item{\code{alpha}}{numeric value between 0 and 1, specifies the weight for the L1 regularization versus L2. \code{alpha=1} means L1 regularization, whle \code{alpha=0} is L2 regularization.}
#' \item{\code{lambda}}{initial values of lambda, possibly multiple values. if NULL, automatically chosen}
#' \item{\code{choose_lambda}}{if TRUE, lambda is chosen by cross validation when fitted}
#' \item{\code{lambda_candidates}}{numeric vector, if specified, used as the lambda values with which models are fitted. if NULL, automatically chosen}
#' \item{\code{lambda_min_ratio}}{smallest value of lambda as a fraction to the maximum, which is computed automatically.}
#' \item{\code{nlambda}}{number of lambda values evaluated}
#' \item{\code{nfolds}}{number of cross validation folds, used if \code{choose_lambda=TRUE}}
#' \item{\code{parallel}}{logical indicating parallel computation when conducting cross validation, used if \code{choose_lambda=TRUE}}
#' \item{\code{loss}}{loss function used for cross validation. either 'deviance', 'class', 'auc', 'mse', or 'mae', used if \code{choose_lambda=TRUE}}
#' \item{\code{modified_newton}}{if TRUE, uses an upper bound on the hessian instead of the exact}
#' \item{\code{group_multinom}}{if TRUE, uses group lasso for a variable in multinomial model}
#' \item{\code{standardize_response}}{if TRUE, output variables are standardized in multiple gaussian model}
#' \item{\code{tol}}{numeric value of convergence criterion}
#' \item{\code{maxit}}{maximum number of iteration}
#'
#' }
#' @section Value:
#' \code{GLM} class object
#'
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x, y)}}{fit the model}
#' \item{\preformatted{predict(x, ...)}}{return predicted values}
#' \item{\preformatted{incr_fit(x, y)}}{not implemented}
#' \item{\preformatted{predict_proba(x, ...)}}{return probability prediction}
#'
#' \item{\preformatted{get_coef(lambda = NULL, nonzero_only = FALSE)}}{return the coefficients}
#' \item{\preformatted{mse(x, y)}}{return the mean squared error}
#' \item{\preformatted{cross_entropy(x, y)}}{return the cross entropy loss if appropriate}
#' \item{\preformatted{accuracy(x, y)}}{return the classification accuracy if appropriate}
#' }
#'
#' @section Details:
#' uses \code{\link[glmnet]{glmnet}} and \code{\link[glmnet]{cv.glmnet}} as the backend
#'
#' @examples
#' set.seed(123)
#' x <- matrix(rnorm(100*20),100,20)
#' y <- rnorm(100)
#' g <- linear_regression()
#' g$fit(x, y)
#' cor(y, g$predict(x))
#' g$mse(x, y)
#'
#' # setting lambda=0 is equivalent to lm
#' g <- linear_regression(lambda=0)
#' g$fit(x, y)
#' lmfit <- lm(y ~ x)
#' cbind(g$get_coef(), coefficients(lmfit))
#'
#' # logistic regression
#' y <- sample(0:1, 100, replace=TRUE)
#' g <- logistic_regression()
#' g$fit(x, y)
#' table(g$predict(x), y)
#' g$accuracy(x, y)
#' g$cross_entropy(x, y)
#' # y can be factor or character
#' y <- sample(c('A', 'B'), 100, replace=TRUE)
#' g$fit(x, y)
#' table(g$predict(x), y)
#' g$accuracy(x, y)
#' g$cross_entropy(x, y)
#'
#' # multinomial regression
#' y <- sample(c('l', 'm', 's'), 100, replace=TRUE)
#' g <- multinomial_regression()
#' g$fit(x, y)
#' table(g$predict(x), y)
#' g$accuracy(x, y)
#' g$cross_entropy(x, y)
#'
#' # poisson regression
#' y <- sample(0:5, 100, prob=c(2,3,3,2,1,1), replace=TRUE)
#' g <- poisson_regression()
#' g$fit(x, y)
#' cor(y, g$predict(x))
NULL

# #' @export
#glm_fit <- GLM$new

#' @export
linear_regression <- function(...) { GLM$new(family='gaussian', ...) }

#' @export
linear_regression_multi <- function(...) { GLM$new(family='mgaussian', ...) }

#' @export
logistic_regression <- function(...) { GLM$new(family='binomial', ...) }

#' @export
multinomial_regression <- function(...) { GLM$new(family='multinomial', ...) }

#' @export
poisson_regression <- function(...) { GLM$new(family='poisson', ...) }
