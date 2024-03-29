% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drug_quantity.R
\name{inventory_reorder}
\alias{inventory_reorder}
\title{Forecast and run inventory management algorithm, returning reorder quantities
by site and drug}
\usage{
inventory_reorder(
  site,
  supplier,
  product,
  w_order,
  requis,
  holidays,
  updateProgress = NULL
)
}
\arguments{
\item{site}{String. Site code (selected from dynamic UI listing all sites)}

\item{supplier}{String. Supplier (changes weekly, selected in Shiny interface)}

\item{product}{dataframe. Contents of product_sup_profile, loaded in
app_server.R}

\item{w_order}{dataframe. Contents of w_order_log_df1, loaded in
app_server.R}
}
\value{
dataframe containing drug name, stock level, days to order, etc.
}
\description{
Forecast and run inventory management algorithm, returning reorder quantities
by site and drug
}
