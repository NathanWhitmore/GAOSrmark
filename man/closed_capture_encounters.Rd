\name{closed_capture_encounters}
\alias{closed_capture_encounters}

\title{closed_capture_encounters}

\usage{
closed_capture_encounters(file.name,
                          site,
                          include1s = FALSE,
                          start_date,
                          finish_date)
}

\description{

Generates the encounter history required for a closed capture mark recapture analysis.


}

\format{

\itemize{
\item \code{file.name}: the .csv extract which contains the information for the species.
\item \code{site}: the site name (capitalised).
\item \code{include1s}: whether or not to include or exclude size1s (young-of-the-year). Unless there is a special reason  you should use \code{include1s = FALSE}.
\item \code{start_date}: the start date for the sampling session in the format (YYYY-MM-DD). Note \code{start_date} is a string and needs to be in quotes.
\item \code{finish_date}: the finish date for the sampling session in the format (YYYY-MM-DD). Note \code{start_date} is a string and needs to be in quotes.
}
}

\details{

The function will save the encounter history in memory as 'encounter.his'.
}


\examples{
closed_capture_encounters(file.name = "Extract_Grand.csv",
                          site = "Airport",
                          include1s = FALSE,
                          start_date = "2016-02-10",
                          finish_date = "2016-02-16")
                          
}





