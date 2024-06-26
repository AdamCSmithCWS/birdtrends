% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_trend.R
\name{plot_trend}
\alias{plot_trend}
\title{Plot predicted trends}
\usage{
plot_trend(
  raw_indices = indat1,
  model_indices = ldf_hgam,
  pred_indices = preds_hgam,
  start_yr = 2014,
  end_yr = 2022
)
}
\arguments{
\item{raw_indices}{tibble with raw annual index value of observed data columns = year, index, index_q_0.25 = 2.5\% confidence interval and index_q_0.975 = 97.5 \% confidence interval}

\item{model_indices}{tibble of estimated indices drawn from fit_* function. Columns are draw, year, and proj_y}

\item{pred_indices}{datatable with modeled and predicted values into the future}

\item{start_yr}{numeric value of the first year in which trend will be calculated. Default is first available year within the dataset}

\item{end_yr}{numeric vlaue representing the last year in which trend will be calculated. Default is first available year within the dataset}
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
