% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efx_to_obs.R
\name{efx_to_obs}
\alias{efx_to_obs}
\title{efx_to_obs}
\usage{
efx_to_obs(
  model_summary,
  observed_data,
  group = "ccode",
  filter_obs_to = "att_item == 'grhseff2'",
  add_stars_on = slope_pos95,
  apply_order = TRUE
)
}
\arguments{
\item{model_summary}{A model summary created by 
\code{summarise_trends} or a related function.}

\item{observed_data}{The data used to fit the model summarised in 
\code{model_summary}.}

\item{group}{A string identifying the grouping variable being 
summarised from the model.}

\item{filter_obs_to}{An optional string specifying a filter condition for 
the \code{observed_data} object. It is only really necessary when 
plotting points, in which case it prevents data that that isn't part
of the model from being plotted.}

\item{add_stars_on}{A variable indicating which group-slopes have a 
credible interval above zero (see details).}
}
\description{
Joins the summary of a set of group effects from a multilevel model 
to the observed data used to fit it.
}
\details{
The \code{\link{summarise_trends}} function adds dummy variables to
the posterior summary of a model, indicating which groups have 90% and
95% credible intervals above zero. Providing one of these variables to
the \code{add_stars_on} argument of \code{efx_to_obs} will paste an
asterisk to group names showing that the credible interval of their
slope is positive.
}
