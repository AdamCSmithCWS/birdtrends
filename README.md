
<!-- README.md is generated from README.Rmd. Please edit that file -->

# birdtrends

<!-- badges: start -->
<!-- badges: end -->

The goal of **birdtrends** package is to provide a flexible toolkit to
estimate population trends (based on annual indices and uncertainty or
similar data inputs), predict future trends, and compare temporal trends
over time.

## Data Input Types

This package currently accommodates three types of input data:

1)  Annual indices of relative abundance with CI estimates (i.e. index,
    upper and lower confidence interval)

2)  Matrix of estimated index based on Bayesian modeled posterior draws
    (rows) for each year (columns) representing the full annual indices
    of relative abundance.

3)  Matrix of posterior draws (rows) for each year (columns) based on
    the smoothed annual indices of relative abundance.

## Estimating annual indices

Depending on the input data type, various methods are available to
estimate the trend for a given time period. Note in the case of datatype
3 this already represents a modeled smooth generated from the original
modeled relative population abundance.

For data input 1: we can fit a hierarchical Bayesian General Additive
Model (HGAM), using annual indices and uncertainty values.

For data input 2: We can fit a GAM for each posterior draw

For data input 3: This data represents a smoothed output and can be used
directly for trend assessment.

## Estimate trend between given time points

We can use two time points to estimate a trend. The default method uses
a geometric mean to estimate the average change in values over the time
period. Alternatively we can assess the trend based on a linear
regression between two points.

## Predict trends into the future

## Installation

You can install the development version of birdtrends from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ninoxconsulting/birdtrends")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#library(birdtrends)
## basic example code
```
