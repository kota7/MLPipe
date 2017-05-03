
GLM <- R6::R6Class(
  'LinearRegression', inherit=PipeComponent,

  public = list(
    family='gaussian', intercept=TRUE, standardize=TRUE,
    alpha=1, lambda=0.01, lambda_candidates=NULL,

    offset_index=0,

    choose_lambda=TRUE,

    # decided to fix lambda to a single value
    #nlambda=100,
    #lambda_min_ratio=0.01,

    group_multinom = FALSE,
    modified_newton = FALSE,

    lower=-Inf, upper=+Inf,

    max_included=NULL, max_excluded=NULL, force_exclude=integer(0),

    tol=1e-7, maxit=1e+5,


    fit = function(x, y)
    {
      self$object <- glmnet::glmnet(
        x, y,
        familiy=self$family, alpha=self$alpha, lambda=self$lambda,
        standardize=self$standardize, intercept=self$intercept,

        lower.limits=self$lower, upper.limits=self$upper,

        thresh=self$tol, maxit=self$maxit,

        type.logistic = if (self$modified_newton) 'modified.Newton' else 'Newton',
        type.multinomial = if (self$group_mutinom) 'grouped' else 'ungrouped'
      )
    }
  )
)
