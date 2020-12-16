
Overview
--------

The neatoutput package offers solutions for journal-quality LaTeX tables of linear model output in R. This package is designed to be used in R Markdown documents that will be knitted to .pdf files; however, other formats such as HTML are also possible.

**Note:** This package is in an early development stage. The package currently contains functions for producing LaTeX, HTML, and Pandoc tables of linear models (lm), general linear models (GLM), and best subsets regression (regsubsets). There are plans for additional methods for other R objects.

Installation
------------

``` r
devtools::install_github("ebrist/neatoutput")
```

Usage
-----

### print\_lm

#### PDF Documents

``` r
# load the package
library(neatoutput)
# fit a linear model
lm_fit <- lm(mpg ~ hp + wt + qsec + gear, data = mtcars) 
# print a journal-quality latex table of the LM results
print_lm(lm_fit, format = "latex")
# residual plots
plot_lm(lm_fit)
```

#### HTML Documents

The default format is LaTeX. For html documents, specify `format = "html"`:

``` r
# print a journal-quality html table of the LM results
print_lm(lm_fit, format = "html")
# residual plots
plot_lm(lm_fit)
```

### print\_glm

#### PDF Documents

``` r
# load the package
library(neatoutput)
# fit a logistic regression model
glm_fit <- glm(as.factor(vs) ~ mpg, 
               family = binomial, data = mtcars) 
# print a journal-quality latex table of the GLM results
print_glm(glm_fit, format = "latex")
```

#### HTML Documents

The default format is LaTeX. For html documents, specify `format = "html"`:

``` r
# print a journal-quality html table of the GLM results
print_glm(lm_fit, format = "html")
```
