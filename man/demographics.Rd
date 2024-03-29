% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrollment.R
\name{demographics}
\alias{demographics}
\title{Demographics}
\usage{
demographics(rcon, cond)
}
\arguments{
\item{rcon}{REDCap connection object exported from tcoRs::build_rcon()}

\item{cond}{data frame of conditions exported from tcoRs::get_maintrial_conditions()}
}
\value{
data frame of demographic values
}
\description{
get demographic information for the main trial
}
\examples{
\dontrun{
demographics(rcon, cond)
}
}
