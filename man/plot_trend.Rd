% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_trend.R
\name{plot_trend}
\alias{plot_trend}
\title{Plot predicted trends}
\usage{
plot_trend(
  raw_indices = NULL,
  model_indices = fitted_data,
  pred_indices = preds_sm,
  start_yr = 2014,
  end_yr = 2022,
  ref_yr = 2014,
  targets = NULL
)
}
\arguments{
\item{raw_indices}{tibble with raw annual index value of observed data columns = year, index, index_q_0.25 = 2.5\% confidence interval and index_q_0.975 = 97.5 \% confidence interval. If raw indices are unavavilable these will be generated based on 5 and 95 percent range}

\item{model_indices}{tibble of estimated indices drawn from fit_* function. Columns are draw, year, and proj_y}

\item{pred_indices}{datatable with modeled and predicted values into the future}

\item{start_yr}{numeric value of the first year in which trend will be calculated. Default is first available year within the dataset}

\item{end_yr}{numeric value representing the last year in which trend will be calculated. Default is first available year within the dataset}

\item{ref_yr}{numeric value representing the reference year in which trends are compared to, default is 2014 for pif calculations}

\item{targets}{optional tibble of short and term targets generated by get_targets()}
}
\value{
plot
}
\description{
Plot predicted trends
}
\examples{
\dontrun{
  hgams_plot <- plot_trend(raw_indices = input_option_1, model_indices = ldf_hgam,
  pred_indices = preds_hgam, start_yr = 2014, end_yr = 2022)
}
}
