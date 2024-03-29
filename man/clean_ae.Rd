% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adverse_events.R
\name{clean_ae}
\alias{clean_ae}
\title{Clean Main Trial Adverse Events}
\usage{
clean_ae(ae_df)
}
\arguments{
\item{ae_df}{Adverse Event data frame exported from tcoRs::get_maintrial_aes()}
}
\value{
data frame of clean adverse events
}
\description{
Clean Main Trial Adverse Events
}
\examples{
\dontrun{
rcon <- build_rcon("my_secret_token")
ae_df <- get_maintrial_aes(rcon)
clean_ae_df <- clean_ae(ae_df)
}
}
