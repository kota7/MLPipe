% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm.R
\name{GLM}
\alias{GLM}
\alias{linear_regression}
\alias{linear_regression_multi}
\alias{logistic_regression}
\alias{multinomial_regression}
\alias{poisson_regression}
\title{Generalized linear model}
\description{
Fit a generalized linear model, possibly with L1 or elastic net regularization.
Supports linear regression for continous outputs, poisson regression for count outputs, logistic and multinomial regression for classification.
Currently not supports cox regression model.
}
\section{Usage}{

\preformatted{linear_regression(intercept = TRUE, standardize = TRUE,
    offset_index = 0, alpha = 1, lambda = 0.01, choose_lambda = FALSE,
    lambda_candidates = NULL, lambda_min_ratio = 0.0001, nlambda = 100,
    nfolds = 10, parallel = FALSE, loss = 'deviance',
    group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
    lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
    tol = 1e-7, maxit = 1e+5)}
\preformatted{logistic_regression(intercept = TRUE, standardize = TRUE,
    offset_index = 0, alpha = 1, lambda = 0.01, choose_lambda = FALSE,
    lambda_candidates = NULL, lambda_min_ratio = 0.0001, nlambda = 100,
    nfolds = 10, parallel = FALSE, loss = 'deviance',
    group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
    lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
    tol = 1e-7, maxit = 1e+5)}
\preformatted{poisson_regression(intercept = TRUE, standardize = TRUE,
    offset_index = 0, alpha = 1, lambda = 0.0001, choose_lambda = FALSE,
    lambda_candidates = NULL, lambda_min_ratio = 0.01, nlambda = 100,
    nfolds = 10, parallel = FALSE, loss = 'deviance',
    group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
    lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
    tol = 1e-7, maxit = 1e+5)}
\preformatted{multinomial_regression(intercept = TRUE, standardize = TRUE,
    offset_index = 0, alpha = 1, lambda = 0.0001, choose_lambda = FALSE,
    lambda_candidates = NULL, lambda_min_ratio = 0.01, nlambda = 100,
    nfolds = 10, parallel = FALSE, loss = 'deviance',
    group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
    lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
    tol = 1e-7, maxit = 1e+5)}
\preformatted{linear_regression_multi(intercept = TRUE, standardize = TRUE,
    offset_index = 0, alpha = 1, lambda = 0.01, choose_lambda = FALSE,
    lambda_candidates = NULL, lambda_min_ratio = 0.0001, nlambda = 100,
    nfolds = 10, parallel = FALSE, loss = 'deviance',
    group_multinom = FALSE, modified_newton = FALSE, standardize_response = FALSE,
    lower = -Inf, upper = +Inf, max_included = NULL, force_exclude=integer(0),
    tol = 1e-7, maxit = 1e+5)}
}

\section{Arguments}{

\describe{
\item{\code{intercept}}{logical indicating if the model has constant term}
\item{\code{standardize}}{lgical indicating if the explanatory variables should be standardized before fitted}
\item{\code{offset_index}}{integer, specifies which column of \code{x} is used as the offset. This is often used for poisson regression, to accomodate the difference in the periods across observations. Note that \code{family=='multinomial'} requires multiple offset variables.}
\item{\code{alpha}}{numeric value between 0 and 1, specifies the weight for the L1 regularization versus L2. \code{alpha=1} means L1 regularization, whle \code{alpha=0} is L2 regularization.}
\item{\code{lambda}}{initial values of lambda, possibly multiple values. if NULL, automatically chosen}
\item{\code{choose_lambda}}{if TRUE, lambda is chosen by cross validation when fitted}
\item{\code{lambda_candidates}}{numeric vector, if specified, used as the lambda values with which models are fitted. if NULL, automatically chosen}
\item{\code{lambda_min_ratio}}{smallest value of lambda as a fraction to the maximum, which is computed automatically.}
\item{\code{nlambda}}{number of lambda values evaluated}
\item{\code{nfolds}}{number of cross validation folds, used if \code{choose_lambda=TRUE}}
\item{\code{parallel}}{logical indicating parallel computation when conducting cross validation, used if \code{choose_lambda=TRUE}}
\item{\code{loss}}{loss function used for cross validation. either 'deviance', 'class', 'auc', 'mse', or 'mae', used if \code{choose_lambda=TRUE}}
\item{\code{modified_newton}}{if TRUE, uses an upper bound on the hessian instead of the exact}
\item{\code{group_multinom}}{if TRUE, uses group lasso for a variable in multinomial model}
\item{\code{standardize_response}}{if TRUE, output variables are standardized in multiple gaussian model}
\item{\code{tol}}{numeric value of convergence criterion}
\item{\code{maxit}}{maximum number of iteration}

}
}

\section{Value}{

\code{GLM} class object
}

\section{Class Methods}{

\describe{
\item{\preformatted{fit(x, y)}}{fit the model}
\item{\preformatted{predict(x, ...)}}{return predicted values}
\item{\preformatted{incr_fit(x, y)}}{not implemented}
\item{\preformatted{predict_proba(x, ...)}}{return probability prediction}

\item{\preformatted{get_coef(lambda = NULL, nonzero_only = FALSE)}}{return the coefficients}
\item{\preformatted{mse(x, y)}}{return the mean squared error}
\item{\preformatted{cross_entropy(x, y)}}{return the cross entropy loss if appropriate}
\item{\preformatted{accuracy(x, y)}}{return the classification accuracy if appropriate}
}
}

\section{Details}{

uses \code{\link[glmnet]{glmnet}} and \code{\link[glmnet]{cv.glmnet}} as the backend
}
\examples{
set.seed(123)
x <- matrix(rnorm(100*20),100,20)
y <- rnorm(100)
g <- linear_regression()
g$fit(x, y)
cor(y, g$predict(x))
g$mse(x, y)

# setting lambda=0 is equivalent to lm
g <- linear_regression(lambda=0)
g$fit(x, y)
lmfit <- lm(y ~ x)
cbind(g$get_coef(), coefficients(lmfit))

# logistic regression
y <- sample(0:1, 100, replace=TRUE)
g <- logistic_regression()
g$fit(x, y)
table(g$predict(x), y)
g$accuracy(x, y)
g$cross_entropy(x, y)
# y can be factor or character
y <- sample(c('A', 'B'), 100, replace=TRUE)
g$fit(x, y)
table(g$predict(x), y)
g$accuracy(x, y)
g$cross_entropy(x, y)

# multinomial regression
y <- sample(c('l', 'm', 's'), 100, replace=TRUE)
g <- multinomial_regression()
g$fit(x, y)
table(g$predict(x), y)
g$accuracy(x, y)
g$cross_entropy(x, y)

# poisson regression
y <- sample(0:5, 100, prob=c(2,3,3,2,1,1), replace=TRUE)
g <- poisson_regression()
g$fit(x, y)
cor(y, g$predict(x))
}

