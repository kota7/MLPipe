#' @export
BoxCoxTransform <- R6::R6Class(
  'BoxCoxTransform', inherit=PipeComponent,

  public = list(
    lambda=1, lambda2=NULL, tol=1e-5, skipfit=FALSE,
    beta=NULL,
    transform = function(x=NULL, y)
    {
      if (is.null(self$lambda2)) {
        y2 <- if (self$lambda < self$tol) log(y) else ((y^self$lambda)-1)/self$lambda
      } else {
        y2 <- if (self$lambda < self$tol) log(y+self$lambda2) else (((y+self$lambda2)^self$lambda)-1)/self$lambda
      }
      list(x=x, y=y2)
    },

    inv_transform = function(x=NULL, y)
    {
      if (is.null(self$lambda2)) {
        y2 <- if (self$lambda < self$tol) exp(y) else (self$lambda*y+1)^(1/self$lambda)
      } else {
        y2 <- if (self$lambda < self$tol) exp(y)-self$lambda2 else (self$lambda*y+1)^(1/self$lambda)-self$lambda2
      }
      list(x=x, y=y2)
    },

    predict = function(x=NULL, ...)
    {
      if (is.null(self$beta)) stop('coefficients have not been estimated')

      if (length(self$beta)==1) {
        pred <- self$beta
      } else {
        x <- cbind(1, as.matrix(x))
        pred <- x %*% matrix(self$beta, ncol=1)
      }
      self$inv_transform(y=pred)
    },

    fit = function(x=NULL, y)
    {
      # can skip fitting if we want to use fixed lambda
      if (self$skipfit) {
        # update beta with the current lambda
        y2 <- self$transform(y=y)$y
        if (is.null(x)) {
          self$beta <- stats::coefficients(stats::lm(y2 ~ 1))
        } else {
          dat <- cbind(y2, x)
          colnames(dat) <- c('y', sprintf('x%d', 1:ncol(x)))
          self$beta <- stats::coefficients(stats::lm(y ~ ., data=dat))
        }
        return(invisible(self))
      }

      if (is.null(x)) x <- rep(1, length(y))
      self$object <- geoR::boxcoxfit(y, x, self$lambda, self$lambda2)

      self$lambda <- self$object$lambda[1]
      if (length(self$object$lambda) >= 2) self$lambda2 <- self$object$lambda[2]
      self$beta <- self$object$beta.normal
      invisible(self)
    },

    initialize = function(lambda=1, lambda2=NULL, skipfit=FALSE, tol=1e-5, ...)
    {
      self$lambda <- lambda
      self$lambda2 <- lambda2
      self$skipfit <- skipfit
      self$tol <- tol

      invisible(self)
    }
  )
)


#' Box-Cox Transformation
#'
#' Conduct Box-Cox Transformation. This can be used as
#' \itemize{
#' \item{an estimation process where data are fitted to the linear regression model of the transformed variable; or}
#' \item{a transformer that transforms dependent variable}
#' }
#' Transformation parameters can be fixed by users, or estimated by the maximum likelihood.
#'
#' @name box_cox_transform
#' @aliases BoxCoxTransform
#'
#' @section Usage:
#' \preformatted{box_cox_transform(lambda = 1, lambda2 = NULL,
#'   skipfit = FALSE, tol = 1e-5)}
#'
#' @section Arguments:
#' \describe{
#' \item{lambda}{initial value for lambda parameter}
#' \item{lambda2}{initial value for lambda2 parameter or logical that indicates if lambda2 should be estimated. If \code{NULL} or \code{FALSE}, then lambda2 is fixed to 0}
#' \item{skipfit}{logical. If TRUE, \code{fit} method does nothing and the parameters are fixed to the initial values.}
#' \item{tol}{lambda smaller than this level is regarded as 0 and log function is applied}
#' }
#'
#' @section Value:
#' \code{BoxCoxTransform} class object
#'
#' @section Class Attributes:
#' \describe{
#' \item{beta}{regression coefficients}
#' }
#'
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x = NULL, y)}}{if \code{skipfit} is \code{FALSE}, then estimate the lambda parameter(s) by the maximum likelihood, otherwise, the parameters are fixed. In either case, regression coeffients beta are estimated by the least squares}
#' \item{\preformatted{transform(x = NULL, y)}}{transform \code{y} and returns list of \code{x} and \code{y}}
#' \item{\preformatted{inv_transform(x = NULL, y)}}{inverse transform \code{y} and returns list of \code{x} and \code{y}}
#' \item{\preformatted{predict(x, ...)}}{return predicted values of \code{y} in the pre-transfom scale}
#' \item{\preformatted{incr_fit(x, y)}}{not available}
#' }
#'
#' @section Details:
#' uses \code{\link[geoR]{boxcoxfit}} as the backend paramter estimator
#'
#' @examples
#' simul <- geoR::rboxcox(100, lambda=0.5, mean=10, sd=2)
#' b <- box_cox_transform()
#' b$fit(y=simul)
#' cat(b$lambda, '\n')
#'
#' b <- box_cox_transform(lambda2=TRUE)
#' b$fit(y=seq(-1, 1, 1/20))
#' cat(b$lambda, b$lambda2, '\n')
#'
#' data(trees)
#' b <- box_cox_transform()
#' x <- trees[,1:2]
#' y <- trees[,3]
#' b$fit(x, y)
#' pred <- b$predict(x)$y
#' cor(y, pred)
#' \dontrun{
#' plot(y, pred)}
#'
#' b <- box_cox_transform(lambda=0, skipfit=TRUE)
#' b$fit(y=1:10)
#' cat(b$lambda, b$beta, '\n')
NULL

#' @export
box_cox_transform <- BoxCoxTransform$new
