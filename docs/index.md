MLPipe package
================
Kota Mori

Overview
--------

`MLPipe` privides interface for creating machine learning pipeline in the style of Python's [scikit-learn](http://scikit-learn.org/) library.

Installation
------------

Install from GitHub:

``` r
devtools::install_github('kota7/MLPipe')
```

Quick Start
-----------

Let's use `Sonar` data from `mlbench` package as an example. `Sonar` data contain 60 numeric variables and 1 label (factor variable). The label represents the types of sonar targets: "rock" by `"R"` and "metal" by `"M"`.
Our goal is to predict the label from the other variables. See `?(mlbench::Sonar)` for more details about the data.

``` r
set.seed(123)
data(Sonar, package='mlbench')
head(Sonar[1:8])
```

    ##       V1     V2     V3     V4     V5     V6     V7     V8
    ## 1 0.0200 0.0371 0.0428 0.0207 0.0954 0.0986 0.1539 0.1601
    ## 2 0.0453 0.0523 0.0843 0.0689 0.1183 0.2583 0.2156 0.3481
    ## 3 0.0262 0.0582 0.1099 0.1083 0.0974 0.2280 0.2431 0.3771
    ## 4 0.0100 0.0171 0.0623 0.0205 0.0205 0.0368 0.1098 0.1276
    ## 5 0.0762 0.0666 0.0481 0.0394 0.0590 0.0649 0.1209 0.2467
    ## 6 0.0286 0.0453 0.0277 0.0174 0.0384 0.0990 0.1201 0.1833

``` r
head(Sonar[52:61])
```

    ##      V52    V53    V54    V55    V56    V57    V58    V59    V60 Class
    ## 1 0.0027 0.0065 0.0159 0.0072 0.0167 0.0180 0.0084 0.0090 0.0032     R
    ## 2 0.0084 0.0089 0.0048 0.0094 0.0191 0.0140 0.0049 0.0052 0.0044     R
    ## 3 0.0232 0.0166 0.0095 0.0180 0.0244 0.0316 0.0164 0.0095 0.0078     R
    ## 4 0.0121 0.0036 0.0150 0.0085 0.0073 0.0050 0.0044 0.0040 0.0117     R
    ## 5 0.0031 0.0054 0.0105 0.0110 0.0015 0.0072 0.0048 0.0107 0.0094     R
    ## 6 0.0045 0.0014 0.0038 0.0013 0.0089 0.0057 0.0027 0.0051 0.0062     R

We pick 80% of the samples as the training data, and use the rest for the performance test.

``` r
X <- Sonar[, -ncol(Sonar)]
y <- Sonar[, ncol(Sonar)]
tr <- c(sample(1:111,111*0.8), sample(112:200,89*0.8))
```

Suppose that we would like to first conduct the dimensionality reduction of the features, and then fit to a classification model. For example, the first step can be done by extraction of principal components, and the second step can be a neural network model. We can construct a **pipeline** of this modelling procedure by the code below:

``` r
library(MLPipe)
p <- pipeline(pc=pca_extractor(ncomp=30),
              ml=mlp_classifier(hidden_sizes=c(5, 5), num_epoch=1000))
```

 function takes arbitrary number of `pipe components` objects. Each pipe component represents a modelling step, and they are in the order of procedure.

We can now fit the the model to the training data by `fit` method of the pipeline. `fit` method takes exactly two inputs, `x` and `y`,

``` r
p$fit(X[tr,], y[tr])
```

By this single call of `fit`, each component of the pipeline is fitted.
In this example, first, principal component analysis for `x` is conducted, and the first 30 (specified as `ncomp`) principal components are extracted as the features. Then, the neural network model is estimated with these new features and `y`.

`mlp_classifier` has the `predict` function, which returns the predicted labels for new data. We can make a confusion matrix using it.

``` r
table(y[-tr], p$predict(X[-tr,]))
```

    ##    
    ##      M  R
    ##   M 23  4
    ##   R  2 20

We can also use `mlp_classifier`'s accuracy method to see the fraction of correct classifications.

``` r
p$evaluate('accuracy', X[-tr,], y[-tr])
```

    ## [1] 0.877551
