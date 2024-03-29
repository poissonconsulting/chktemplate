% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/row_convert.R
\name{chkrow_to_expression}
\alias{chkrow_to_expression}
\title{Convert chk row to list of expressions}
\usage{
chkrow_to_expression(x)
}
\arguments{
\item{x}{A data.frame with chk values for each column.
Data.frame must have a column called 'name' and a value of 'chk'
within that column corresponding to the row of chk values.}
}
\value{
A named list of the chk values by column.
}
\description{
Convert chk row to list of expressions
}
